(ns hylo.core
  (require [clojure.pprint :refer [pprint]]))

(defn sqrt [x] (Math/sqrt x))
(defn id [x] x)

(defmacro hylo [body]
  `(type-of root-context '~body))

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
    :else (throw (Exception. (str "Can't mk-type " x)))))

(defn mk-fn
  ([ret args] {:class :fn
               :constraints {}
               :return (mk-type ret)
               :arguments (map mk-type args)}))

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
    (context-contains? (:parent context) k)

    :else
    false))

(defn context-get
  ([context] #(context-get context %))
  ([context k]
   (or ((:assumptions context) k)
       (when (:parent context)
         (context-get (:parent context) k)))))

(defn context-deref [context k]
  (let [t (context-get context k)]
    (if (= :ref (:class t))
      (context-deref context (:target t))
      t)))

(defn context-set [context k t]
  (cond
    (contains? (:assumptions context) k)
    (let [k-type (context-get context k)]
      (cond
        (= k-type t)
        context

        (and (= :unknown (:class k-type))
             (-> (:constraints k-type) seq not))
        (assoc-in context [:assumptions k] t)

        :else
        (throw (Exception. (str "Type mismatch: expected " [t]
                                ", found " [k-type])))))

    (:parent context)
    (update-in context [:parent] context-set k t)

    :else
    (throw (Exception. (str "Missing key: " k)))))

(defn context-sort [context a b]
  (cond
    (contains? (:assumptions context) a) [a b]
    (contains? (:assumptions context) b) [b a]
    :else (context-sort (:parent context) a b)))

(declare unify)

(defn unify-ref [context tgt b]
  (let [next (context-get context tgt)]
    (case (:class next)
      :primitive (unify context next b)
      :unknown (context-set context tgt b)
      :ref (recur context (:target next) b)
      (throw (Exception. (str "Can't handle ref to " next))))))

(defn unify-refs [context a b]
  (let [tgt-a (:target a)
        tgt-b (:target b)
        next-a (context-get context tgt-a)
        next-b (context-get context tgt-b)]
    (case [(:class next-a) (:class next-b)]
      [:ref :unknown] (recur context next-a b)
      [:primitive :unknown] (context-set context tgt-b next-a)
      [:unknown :unknown] (context-set context tgt-a b)
      (throw (Exception. (str "Can't handle refs to " [next-a next-b]))))))

(defn unify [context a b]
  (case [(:class a) (:class b)]
    [:primitive :primitive]
    (if (= (:type a) (:type b)) context
        (throw (Exception.
                (str "Type mismatch, expected " (:type a) " found " (:type b)))))
    [:primitive :ref] (recur context b a)
    [:ref :primitive] (unify-ref context (:target a) b)
    [:ref :ref] (unify-refs context a b)
    (throw (Exception. (str "Could not unify " a " with " b)))))

(declare type-of-form)
(declare type-of-apply)

(defn type-of [context expr]
  (cond (primitive? expr)
        {:type (mk-prim (class expr))
         :context context}

        (seq? expr)
        (type-of-form context (first expr) (rest expr))

        (context-contains? context expr)
        {:type (context-get context expr)
         :context context}

        (and (symbol? expr)
             (context-contains? context (or (resolve expr) expr)))
        {:type (context-get context (or (resolve expr) expr))
         :context context}

        :else
        (throw (Exception. (str "Unknown type: [" expr "]")))))

(defn type-of-form [parent-context f args]
  (cond
    (= f 'fn)
    (let [[ps expr] args
          mapping (zipmap ps (map #(gensym (str % "_")) ps))

          refs (zipmap ps (map #(mk-ref (gensym (str % "_"))) ps))
          ctx {:parent parent-context
               :assumptions (zipmap (map :target (vals refs))
                                    (repeatedly mk-unknown))}

          ctx-prime {:parent ctx
                     :assumptions refs}

          {:keys [type context]} (type-of ctx-prime expr)

          free (remove #(not= :unknown (:class (context-deref context %))) ps)
          free-mapping (zipmap free type-keywords)

          calc-type (fn [t]
                      (if (symbol? t) (recur (context-deref context t))
                          (case (:class t)
                            :primitive t
                            :ref (recur (context-deref context (:target t)))
                            :unknown
                            (free-mapping
                             (some #(and (= t (context-deref context %)) %)
                                   free)))))]
      {:type (mk-fn (calc-type type) (map calc-type ps))
       :context (:parent context)})

    :else
    (type-of-apply parent-context f args)))

(defn type-of-apply [parent-context f args]
  (let [f-type (:type (type-of parent-context f))
        _ (if-not (and (map? f-type) (= (:class f-type) :fn))
            (throw (Exception. (str "Cannot apply '" f
                                    "' of type " [f-type]))))

        free-types (->> (:arguments f-type)
                        (filter #(= (:class %) :polymorphic))
                        (map :label)
                        (into #{}))
        mapping (zipmap free-types
                        (map #(gensym (str f "_"(name %) "_"))
                             free-types))

        refs (zipmap (vals mapping) (repeatedly #(mk-ref (gensym))))
        ctx {:parent parent-context
             :assumptions
             (into refs (zipmap (map :target (vals refs))
                                (repeatedly mk-unknown)))}

        param-types (map #(or (mapping (:label %)) %)
                         (:arguments f-type))

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
                  (context-deref ctx-prime))
             (:return f-type))
     :context ctx-prime}))
