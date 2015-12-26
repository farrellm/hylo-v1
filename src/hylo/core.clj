(ns hylo.core
  (require [clojure.pprint :refer [pprint]]
           [clojure.string :refer [join]]
           [hylo.pprint :refer :all]))

(defn sqrt [x] (Math/sqrt x))
(defn id [x] x)

(defn mk-prim [prim]
  {:class :primitive
   :type prim})

(defn mk-ref [tgt]
  {:class :ref
   :target tgt})

(defn mk-poly
  ([lbl] {:class :polymorphic
          :label lbl})
  ([lbl constraints] {:class :polymorphic
                      :label lbl
                      :constraints constraints}))
(defn mk-unknown
  ([] {:class :unknown})
  ([constraints] {:class :unknown
                  :constraints constraints}))

(defn mk-type [x]
  (cond
    (map? x) x
    (class? x) (mk-prim x)
    (keyword? x) (mk-poly x)
    (symbol? x) (mk-ref x)
    :else (throw (Exception. (str "Can't mk-type " x)))))

(defn mk-fn
  ([ret args] {:class :fn
               :constraints {}
               :return (mk-type ret)
               :parameters (map mk-type args)}))

(def root-context {:assumptions {#'sqrt (mk-fn Double [Double])
                                 #'id (mk-fn :a [:a])
                                 'if (mk-fn :a [Boolean :a :a])
                                 }})

(def type-keywords
  (->> (int \a) (iterate inc) (map (comp keyword str char))))

(defn primitive? [expr]
  (or (instance? Boolean expr)
      (number? expr)
      (string? expr)
      (keyword? expr)))

(defn context-contains? [context k]
  (cond
    (contains? (:assumptions context) k)
    true

    (:parent context)
    (recur (:parent context) k)

    :else
    false))

(defn context-get
  ([context] (partial context-get context))
  ([context k]
   (or (get-in context [:assumptions k])
       (when-let [parent (:parent context)]
         (recur parent k)))))

(defn context-deref [context k]
  "recursively deref a symbol"
  (let [t (context-get context k)]
    (if (= :ref (:class t))
      (recur context (:target t))
      t)))

(defn context-deref-known [context t]
  "if type is known, same as context-deref.  for unknown symbol,
   return deepest ref"
  (loop [cur t
         prev nil]
    (case (:class cur)
      :ref     (recur (context-get context (:target cur)) cur)
      :unknown prev
      cur)))

(defn context-deref-known-type [context t]
  "if ref, do deref-known; otherwise, self"
  (if (= :ref (:class t))
    (context-deref-known context (:target t))
    t))

(defn context-set [context k t]
  (if-let [k-type ((:assumptions context) k)]
    (cond
      (= k-type t)
      context

      (and (= :unknown (:class k-type))
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
      :primitive (unify context next b)
      :unknown   (context-set context tgt b)
      :ref       (recur context (:target next) b)
      (throw (Exception. (str "Can't handle ref to " next))))))

(defn unify-refs [context a b]
  (let [tgt-a (:target a)
        tgt-b (:target b)
        next-a (context-get context tgt-a)
        next-b (context-get context tgt-b)]
    (case [(:class next-a) (:class next-b)]
      [:ref       :unknown] (recur context next-a b)
      [:primitive :unknown] (context-set context tgt-b next-a)
      [:unknown   :unknown] (context-set context tgt-a b)
      (throw (Exception. (str "Can't handle refs to " [next-a next-b]))))))

(defn unify
  "unify two types.  should never see unknown types here, only refs to
  unknown"
  [context a b]
  (case [(:class a) (:class b)]
    [:primitive :primitive]
    (if (= (:type a) (:type b)) context
        (throw (Exception. (str "Type mismatch, expected " (:type a) " found "
                                (:type b)))))
    [:primitive :ref]       (recur context b a)
    [:ref :primitive]       (unify-ref context (:target a) b)
    [:ref :fn]              (unify-ref context (:target a) b)
    [:ref :ref]             (unify-refs context a b)
    (throw (Exception. (str "Could not unify " a " with " b)))))

(declare type-of-form)
(declare type-of-apply)
(declare type-of-fn)
(declare abstract-poly-fn)

(defn type-of [context expr]
  (cond (primitive? expr)
        {:type (mk-prim (class expr))
         :context context}

        (seq? expr)
        (type-of-form context (first expr) (rest expr))

        (and (symbol? expr)
             (context-contains? context (or (resolve expr) expr)))
        {:type (context-get context (or (resolve expr) expr))
         :context context}

        :else
        (throw (Exception. (str "Unknown type: [" expr "]")))))

(defn type-of-form [parent-context f args]
  (case f
    'fn (type-of-fn parent-context args)
    (type-of-apply parent-context f args)))

(defn context-add-fn [context f n-args]
  (let [ret (gensym "ret_")
        params (repeatedly n-args (partial gensym "param_"))
        t (mk-fn ret params)
        ctx {:parent context
             :assumptions (into {ret (mk-unknown)}
                                (map vector
                                     params
                                     (repeatedly mk-unknown)))}
        ctx-prime (unify ctx f t)]
    [(context-deref ctx-prime (:target f)) ctx-prime]))

(defn type-of-apply [parent-context f args]
  (let [{:keys [type context]} (type-of parent-context f)
        f-type (context-deref-known context type)

        [f-type f-context] (if (= :ref (:class f-type))
                             (context-add-fn context f-type (count args))
                             [f-type context])

        ;; include return type in unknowns
        free-types (->> (conj (:parameters f-type) (:return f-type))
                        (filter #(= (:class %) :polymorphic))
                        (map :label)
                        (into #{}))
        mapping (zipmap free-types
                        (map #(gensym (str f "_"(name %) "_"))
                             free-types))

        refs (zipmap (vals mapping) (repeatedly #(mk-ref (gensym))))
        ctx {:parent f-context
             :assumptions
             (into refs (zipmap (map :target (vals refs))
                                (repeatedly mk-unknown)))}

        param-types (map #(or (mapping (:label %)) %)
                         (:parameters f-type))

        {:keys [arg-types ctx-prime]}
        (reduce (fn [{:keys [arg-types ctx-prime]} arg]
                  (let [{:keys [type context]} (type-of ctx-prime arg)]
                    {:arg-types (conj arg-types type)
                     :ctx-prime context}))
                {:ctx-prime ctx
                 :arg-types []}
                args)

        ctx-prime (reduce
                   (fn [c [p a]]
                     (unify c (if (symbol? p) (context-get c p) p)
                            a))
                   ctx-prime
                   (map vector param-types arg-types))]

    {:type (if (= :polymorphic (get-in f-type [:return :class]))
             (->> (get-in f-type [:return :label])
                  mapping
                  (context-get ctx-prime)
                  (context-deref-known ctx-prime))
             (:return f-type))
     :context ctx-prime}))

(defn type-of-fn [parent-context [ps expr]]
  (let [refs (zipmap ps (map #(mk-ref (gensym (str % "_"))) ps))
        ctx {:parent parent-context
             :assumptions (zipmap (map :target (vals refs))
                                  (repeatedly mk-unknown))}

        ctx-prime {:parent ctx
                   :assumptions refs}

        {:keys [type context]} (type-of ctx-prime expr)]

    {:type    (mk-fn type (map refs ps))
     :context context}))

#_(defn mk-poly-fn [f])

(defn- calculate-type [context free-mapping type]
  (loop [t type]
    (if (symbol? t) (recur (context-deref context t))
        (case (:class t)
          :primitive t
          :ref (recur (context-deref context (:target t)))
          :fn t
          :unknown
          (free-mapping
           (some #(and (= t (context-deref context %)) %)
                 (keys free-mapping)))))))

(defn abstract-poly-fn [{:keys [type context]}]
  (let [ps (map (partial context-deref-known-type context)
                (:parameters type))

        free (distinct (filter symbol? ps))
        free-mapping (zipmap free type-keywords)

        calc-type (partial calculate-type context free-mapping)]

    {:type (mk-fn (calc-type (:return type))
                  (map calc-type (:parameters type)))
     :context (:parent context)}))

(defmacro hylo [body]
  `(type-of root-context '~body))

(defmacro hylo-fn [body]
  `(abstract-poly-fn (hylo ~body)))

#_(-> (hylo (fn [f x] (sqrt (f x))))
      :type
      pretty-type)

#_(pprint-ret (hylo (fn [f x] (sqrt (f x)))))

;; (pprint-ret (hylo (fn [x] x)))
;; (prn " ")
