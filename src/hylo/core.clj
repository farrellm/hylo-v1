(ns hylo.core
  (require [clojure.pprint :refer [pprint]]
           [clojure.string :refer [join]]
           [hylo.pprint :refer :all]))

(defn mk-prim [prim]
  {:class ::primitive
   :type prim})

(defn mk-ref [tgt]
  {:class ::ref
   :target tgt})

(defn mk-poly
  ([lbl] {:class ::polymorphic
          :label lbl})
  ([lbl constraints] {:class ::polymorphic
                      :label lbl
                      :constraints constraints}))

(defn mk-unknown
  ([] {:class ::unknown})
  ([constraints] {:class ::unknown
                  :constraints constraints}))

(defn mk-type [x]
  (cond
    (map? x)     x
    (class? x)   (mk-prim x)
    (keyword? x) (mk-poly x)
    (symbol? x)  (mk-ref x)
    :else        (throw (Exception. (str "Can't mk-type " x)))))

(defn mk-fn [ret args]
  {:class       ::fn
   :return      (mk-type ret)
   :parameters  (map mk-type args)})

(defn mk-poly-fn
  ([ret args] {:class       ::poly-fn
               :constraints {}
               :return      (mk-type ret)
               :parameters  (map mk-type args)}))

(def root-context {:assumptions {'Math/sqrt (mk-fn Double [Double])
                                 #'identity  (mk-fn :a [:a])
                                 #'*         (mk-fn Double [Double Double])
                                 'if         (mk-fn :a [Boolean :a :a])
                                 }})

(def type-keywords
  (->> (int \a) (iterate inc) (map (comp keyword str char))))

(defn primitive? [expr]
  (or (instance? Boolean expr)
      (number? expr)
      (string? expr)
      (keyword? expr)))

(defn context-contains? [context k]
  (or (contains? (:assumptions context) k)
      (when-let [parent (:parent context)]
        (recur parent k))))

(defn context-get
  ([context] (partial context-get context))
  ([context k]
   (or (get-in context [:assumptions k])
       (when-let [parent (:parent context)]
         (recur parent k)))))

(defn context-deref [context t]
  "recursively deref a symbol; for unknown symbol, return deepest ref"
  (loop [cur t
         prev nil]
    (case (:class cur)
      ::ref     (recur (context-get context (:target cur)) cur)
      ::unknown prev
      cur)))

(defn context-set [context k t]
  (if-let [k-type ((:assumptions context) k)]
    (cond
      (= k-type t)
      context

      (and (= ::unknown (:class k-type))
           (empty? (:constraints k-type)))
      (assoc-in context [:assumptions k] t)

      :else
      (throw (Exception. (str "Type mismatch: expected " [t]
                              ", found " [k-type]))))

    (if-let [parent (:parent context)]
      (assoc context :parent (context-set parent k t))

      (throw (Exception. (str "Missing key: " k))))))

(defn context-sort [context a b]
  (cond
    (contains? (:assumptions context) a) [a b]
    (contains? (:assumptions context) b) [b a]
    :else (recur (:parent context) a b)))

(declare unify)

(defn unify-ref [context tgt b]
  (let [next (context-get context tgt)]
    (case (:class next)
      ::primitive (unify context next b)
      ::unknown   (context-set context tgt b)
      ::ref       (recur context (:target next) b)
      (throw (Exception. (str "Can't handle ref to " next))))))

(defn unify-refs [context a b]
  (let [tgt-a (:target a)
        tgt-b (:target b)
        next-a (context-get context tgt-a)
        next-b (context-get context tgt-b)]
    (case [(:class next-a) (:class next-b)]
      [::ref       ::unknown] (recur context next-a b)
      [::primitive ::unknown] (context-set context tgt-b next-a)
      [::unknown   ::unknown] (context-set context tgt-a b)
      [::fn        ::unknown] (recur context b a)
      [::unknown   ::fn]      (unify context a next-b)
      (throw (Exception. (str "Can't handle refs to " [next-a next-b]))))))

(defn unify
  "unify two types.  should never see unknown types here, only refs to
  unknown"
  [context a b]
  (case [(:class a) (:class b)]
    [::primitive ::primitive]
    (if (= (:type a) (:type b)) context
        (throw (Exception. (str "Type mismatch, expected " (:type a)
                                " found " (:type b)))))
    [::primitive ::ref]       (recur context b a)
    [::ref ::primitive]       (unify-ref context (:target a) b)
    [::ref ::fn]              (unify-ref context (:target a) b)
    [::ref ::ref]             (unify-refs context a b)
    (throw (Exception. (str "Could not unify " a " with " b)))))

(declare type-of-form)
(declare type-of-apply)
(declare type-of-fn)
(declare abstract-poly-fn)

(defn type-of [context expr]
  (cond (primitive? expr)
        {:type (mk-prim (class expr))
         :ast expr
         :context context}

        (seq? expr)
        (type-of-form context (first expr) (rest expr))

        (and (symbol? expr)
             (context-contains? context (or (resolve expr) expr)))
        {:type (context-get context (or (resolve expr) expr))
         :ast expr
         :context context}

        :else
        (throw (Exception. (str "Unknown type: [" expr "]")))))

(defn type-of-form [context f args]
  (case f
    'fn (type-of-fn context args)
    (type-of-apply context f args)))

(defn context-add-fn [context f n-args ast]
  (let [ret (gensym "ret_")
        params (repeatedly n-args (partial gensym "param_"))
        t (mk-fn ret params)
        context {:parent context
                 :assumptions (into {ret (mk-unknown)}
                                    (map vector
                                         params
                                         (repeatedly mk-unknown)))}
        context (unify context f t)]
    [(context-deref context f) ast context]))

(defn type-of-function [context f n-args]
  (let [{:keys [type ast context]} (type-of context f)
        f-type (context-deref context type)]
    (if (= ::ref (:class f-type))
      (context-add-fn context f-type n-args ast)
      [f-type ast context])))

(defn type-of-apply [context f args]
  (let [[f-type f-ast context] (type-of-function context f (count args))
        rtn (:return f-type)

        ;; include return type in unknowns
        free-types (->> (conj (:parameters f-type) rtn)
                        (filter #(= (:class %) ::polymorphic))
                        (map :label)
                        (into #{}))
        mapping (zipmap free-types
                        (map #(gensym (str f "_" (name %) "_"))
                             free-types))

        refs (zipmap (vals mapping) (repeatedly #(mk-ref (gensym))))
        context {:parent context
                 :assumptions
                 (into refs (zipmap (map :target (vals refs))
                                    (repeatedly mk-unknown)))}

        {:keys [arg-types arg-ast context]}
        (reduce (fn [{:keys [arg-types arg-ast context]} arg]
                  (let [{:keys [type ast context]} (type-of context arg)]
                    {:arg-types (conj arg-types type)
                     :arg-ast (conj arg-ast ast)
                     :context context}))
                {:context context
                 :arg-types []
                 :arg-ast []}
                args)

        context (reduce
                 (fn [c [p a]]
                   (unify c (or (some->> (:label p) mapping refs) p) a))
                 context
                 (map vector (:parameters f-type) arg-types))]

    {:type (or (some->> (:label rtn)
                        mapping
                        refs
                        (context-deref context))
               rtn)
     :ast `(~f-ast ~@arg-ast)
     :context context}))

(defn context-excise [context assump]
  (if (= (:assumptions context) assump)
    (:parent context)
    (assoc context :parent (context-excise (:parent context) assump))))

(defn type-of-fn [context [ps expr]]
  (let [refs     (zipmap ps (map #(mk-ref (gensym (str % "_"))) ps))
        bindings (map :target (vals refs))
        context  {:parent context
                  :assumptions (zipmap bindings
                                       (repeatedly mk-unknown))}
        context  {:parent context
                  :assumptions refs}

        {:keys [type ast context]} (type-of context expr)]
    {:type    (mk-fn (context-deref context type)
                     (map (comp (partial context-deref context) refs) ps))
     :ast     `(:fn [~@ps] ~ast)
     :context (context-excise context refs)}))

(defmulti constituent-types (fn [context type] (:class type)))

(defmethod constituent-types ::fn [context type]
  (->> (conj (:parameters type) (:return type))
       (map (partial context-deref context))
       (mapcat (partial constituent-types context))))

(defmethod constituent-types :default [_ type]
  [type])

(defn- abstract-poly-type [context free-mapping type]
  (let [d-type (context-deref context type)]
    (case (:class d-type)
      ::primitive d-type
      ::ref       (free-mapping (:target d-type))
      ::fn        (mk-fn (abstract-poly-type context free-mapping
                                             (:return d-type))
                         (map (partial abstract-poly-type context free-mapping)
                              (:parameters d-type)))
      (throw (Exception. (str "Unexpected unknown type " type))))))

(defn abstract-poly-fn [{:keys [type ast context]}]
  (let [ps (constituent-types context type)

        free (->> (filter #(= ::ref (:class %)) ps)
                  (map :target)
                  distinct)

        free-mapping (zipmap free type-keywords)

        calc-type (partial abstract-poly-type context free-mapping)

        rtn (calc-type (:return type))
        args (map calc-type (:parameters type))]

    (if (empty? free)
      {:type (mk-fn rtn args)
       :ast ast
       :context context}
      {:type (mk-poly-fn rtn args)
       :ast `(:poly-fn [~@free] ~ast)
       :context context})))

(defmacro hylo [body]
  `(type-of root-context '~body))

(defmacro hylo-fn [body]
  `(abstract-poly-fn (hylo ~body)))
