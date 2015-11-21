(ns hylo.core
  (require [clojure.core.match :refer [match]]))

(defn sqrt [x] (Math/sqrt x))
(defn id [x] x)

(defn id2 [x y] x)
(defn id3 [x y] x)

(defmacro hylo [body]
  `(type-of root-context '~body))

(def root-context {:assumptions {`sqrt
                                 {:class :fn
                                  :constraints {}
                                  :return Double
                                  :arguments [Double]}

                                 `id
                                 {:class :fn
                                  :constraints {}
                                  :return :a
                                  :arguments [:a]}

                                 ;; `if
                                 ;; {:class :fn
                                 ;;  :constraints {}
                                 ;;  :return :a
                                 ;;  :arguments [Boolean :a :a]}

                                 ;; `id2
                                 ;; (let [a (gensym)
                                 ;;       b (gensym)]
                                 ;;   {:class :fn
                                 ;;    :constraints {}
                                 ;;    :return a
                                 ;;    :arguments [a b]})

                                 ;; `id3
                                 ;; (let [a (gensym)]
                                 ;;   {:class :fn
                                 ;;    :constraints {}
                                 ;;    :return a
                                 ;;    :arguments [a a]})
                                 }})

(defn context-contains? [context key]
  (cond
    (contains? (:assumptions context) key)
    true

    (:parent context)
    (context-contains? (:parent context) key)

    :else
    false))

(defn context-get [context key]
  (cond
    (contains? (:assumptions context) key)
    ((:assumptions context) key)

    (:parent context)
    (context-contains? (:parent context) key)

    :else
    nil))

(defn context-set [context key type]
  (cond
    (contains? (:assumptions context) key)
    (let [cur (context-get context key)]
      (cond
        (or (nil? cur) (symbol? cur))
        (update-in context [:assumptions key] (constantly type))

        (= type cur)
        context

        :else
        (throw (Exception. (str "Type mismatch: expected [" type "], found [" cur "]")))))

    (:parent context)
    (assoc context :parent (context-set (:parent context) key type))

    :else
    (throw (Exception. (str "Missing key: " key)))))

(declare unify)

(defn unify-current [context a b]
  "a is defined on this level, so be is either on this or a parent
  level, so set a to equal b"
  (let [cur-a (context-get context a)]
    (cond
      (nil? cur-a)
      (context-set context a (or (context-get context b) b))

      (symbol? cur-a)
      (let [ctx-prime (unify cur-a b)]
        (context-set ctx-prime a (or (context-get ctx-prime b) b)))

      :else
      (context-set context b cur-a))))

(defn unify [context a b]
  (cond
    (and (class? a) (class? b))
    (if (= a b) context
        (throw (Exception. (str "Type mismatch: expected [" a "], found [" b "]"))))

    (class? a) (context-set context b a)
    (class? b) (context-set context a b)

    (contains? (:assumptions context) a) (unify-current context a b)
    (contains? (:assumptions context) b) (unify-current context b a)

    (:parent context) (unify (:parent context) a b)

    :else (throw (Exception. (str "Could not find [" a "] or [" b "] to unify")))))

(declare type-of-form)
(declare type-of-apply)

(defn type-of [context expr]
  (cond (or (instance? Boolean expr)
            (number? expr)
            (string? expr)
            (keyword? expr))
        {:type (class expr)
         :context context}

        (seq? expr)
        (type-of-form context (first expr) (rest expr))

        (context-contains? context expr)
        {:type (context-get context expr)
         :context context}

        :else
        (throw (Exception. (str "Unknown type: [" expr "]")))))

(defn type-of-form [context f args]
  (cond
    (= f `fn)
    :yay

    :else
    (type-of-apply context f args)))

(defn type-of-apply [parent-context f args]
  (let [f-type (:type (type-of parent-context f))
        free-types (->> (:arguments f-type) (filter keyword?) (into #{}))
        mapping (zipmap free-types (repeatedly gensym))

        context {:parent parent-context
                 :assumptions (zipmap (vals mapping)
                                      (repeat nil))}

        param-types (map #(or (mapping %) %) (:arguments f-type))
        arg-types (map (comp :type (partial type-of context)) args)

        _ (if-not (and (map? f-type) (= (:class f-type) :fn))
            (throw (Exception. (str "Cannot apply '" f "' of type [" f-type "]"))))

        ctx-prime (reduce
                   (fn [c [a v]] (unify c a v))
                   context
                   (map vector param-types arg-types))]

    {:type (if (keyword? (:return f-type))
             ((:assumptions ctx-prime) (mapping (:return f-type)))
             (:return f-type))
     :context (:parent ctx-prime)}))


(type-of root-context `(fn [x] x))

;; (:type (type-of root-context `(if true :a :b)))

(:type (type-of root-context `(id 3.14)))
(:type (type-of root-context `(id 8)))
;; (type-of root-context `(id2 :a 8))
;; (type-of root-context `(id3 :a :b))
;; (type-of root-context `(id3 :a 8))

(:type (type-of root-context `(sqrt 3.14)))
;; (type-of root-context `(sqrt 3))

;; (type-of root-context `(8 3.14))

(:type (type-of root-context `3.14))
(:type (type-of root-context `sqrt))

(:type (type-of root-context `id))
;; (type-of root-context `id2)
;; (type-of root-context `id3)

;; (hylo (sqrt 3.14))
