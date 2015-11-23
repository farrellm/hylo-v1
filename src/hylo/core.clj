(ns hylo.core
  (require [clojure.core.match :refer [match]]))

(defn sqrt [x] (Math/sqrt x))
(defn id [x] x)

(defn id2 [x y] x)
(defn id3 [x y] x)

(defmacro hylo [body]
  `(type-of root-context '~body))

(def root-context {:assumptions {#'sqrt
                                 {:class :fn
                                  :constraints {}
                                  :return Double
                                  :arguments [Double]}

                                 #'id
                                 {:class :fn
                                  :constraints {}
                                  :return :a
                                  :arguments [:a]}

                                 'if
                                 {:class :fn
                                  :constraints {}
                                  :return :a
                                  :arguments [Boolean :a :a]}}})

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

(defn context-get [context k]
  (cond
    (contains? (:assumptions context) k)
    (let [value ((:assumptions context) k)]
      (or (context-get context value) value))

    (:parent context)
    (context-get (:parent context) k)

    :else
    nil))

(defn context-set [context k t]
  (cond
    (contains? (:assumptions context) k)
    (let [cur (context-get context k)]
      (cond
        (or (nil? cur) (symbol? cur))
        (update-in context [:assumptions k] (constantly t))

        (= t cur)
        context

        :else
        (throw (Exception. (str "Type mismatch: expected [" t "], found [" cur "]")))))

    (:parent context)
    (update-in context [:parent] context-set k t)

    :else
    (throw (Exception. (str "Missing key: " k)))))

(defn context-specify [context k t]
  (if-let [v (context-get context k)]
    (if (= v t) context
        (throw (Exception. (str "Type " k " is " v ", cannot be " t))))
    (let [a (into {} (map (fn [[q v :as e]] (if (= v k) [q t] e))
                          (:assumptions context)))]
      (if (contains? (:assumptions context) k)
        {:assumptions (assoc a k t)
         :parent (:parent context)}
        {:assumptions a
         :parent (context-specify (:parent context) k t)}))))

(defn context-sort [context a b]
  (cond
    (contains? (:assumptions context) a) [a b]
    (contains? (:assumptions context) b) [b a]
    :else (context-sort (:parent context) a b)))

(defn unify [context a b]
  (prn [:unify a b])
  (cond
    (= a b)
    context

    (and (class? a) (symbol? b))
    (if-let [b-val (context-get context b)]
      (unify context a b-val)
      (context-specify context b a))

    (and (symbol? a) (class? b))
    (unify context b a)

    (and (symbol? a) (symbol? b))
    (let [a-val (context-get context a)
          b-val (context-get context b)]
      (prn [a-val b-val])
      (prn context)
      (cond
        (and (nil? a-val) (nil? b-val))
        (apply context-specify context (context-sort context a b))

        (and a-val (nil? b-val))
        (context-specify context b a-val)

        (and (nil? a-val) b-val)
        (context-specify context a b-val)

        (and a-val b-val)
        (apply context-specify context (context-sort context a-val b-val))))

    :else
    (throw (Exception. (str "Could not unify [" a "] with [" b "]")))))

(declare type-of-form)
(declare type-of-apply)

(defn type-of [context expr]
  (prn :type-of expr)
  (cond (primitive? expr)
        {:type (class expr)
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
          mapping (zipmap ps (repeatedly gensym))
          ctx-prime {:parent parent-context
                     :assumptions (merge mapping
                                         (zipmap (vals mapping) (repeat nil)))}
          {:keys [type context]} (type-of ctx-prime expr)
          free (remove (partial context-get context) (vals mapping))
          free-mapping (zipmap free type-keywords)

          calc-type #(if (class? %) %
                         (or (context-get context %)
                             (free-mapping %)
                             %))]
      {:type {:class :fn
              :constraints {}
              :return (calc-type type)
              :arguments (map (comp calc-type (partial context-get context)) ps)}
       :context (:parent context)}
      )

    :else
    (type-of-apply parent-context f args)))

(defn type-of-apply [parent-context f args]
  (let [f-type (:type (type-of parent-context f))
        free-types (->> (:arguments f-type) (filter keyword?) (into #{}))
        mapping (zipmap free-types
                        (map #(gensym (str f "_"(name %) "_")) free-types))

        ctx {:parent parent-context
             :assumptions (zipmap (vals mapping)
                                  (repeat nil))}

        param-types (map #(or (mapping %) %) (:arguments f-type))

        {:keys [arg-types ctx-prime]}
        (reduce (fn [{:keys [arg-types ctx-prime]} arg]
                  (let [{:keys [type context]} (type-of ctx-prime arg)]
                    {:arg-types (conj arg-types type)
                     :ctx-prime context}))
                {:ctx-prime ctx
                 :arg-types []}
                args)

        _ (if-not (and (map? f-type) (= (:class f-type) :fn))
            (throw (Exception. (str "Cannot apply '" f "' of type [" f-type "]"))))

        ctx-prime (reduce
                   (fn [c [a v]] (unify c a v))
                   ctx-prime
                   (map vector param-types arg-types))]

    {:type (if (keyword? (:return f-type))
             ((:assumptions ctx-prime) (mapping (:return f-type)))
             (:return f-type))
     :context (:parent ctx-prime)}))
