(ns hylo.core
  (require [clojure.core.match :refer [match]]))

(defn sqrt [x] (Math/sqrt x))
(defn id [x] x)

(defn id2 [x y] x)
(defn id3 [x y] x)

(defmacro compile [body]
  `(type-of root-context ~body))

(def root-context {`sqrt
                   {:class :fn
                    :constraints {}
                    :return Double
                    :arguments [Double]}

                   `id
                   (let [a (gensym)]
                     {:class :fn
                      :constraints {}
                      :return a
                      :arguments [a]})

                   `if
                   (let [a (gensym)]
                     {:class :fn
                      :constraints {}
                      :return a
                      :arguments [Boolean a a]})

                   `id2
                   (let [a (gensym)
                         b (gensym)]
                     {:class :fn
                      :constraints {}
                      :return a
                      :arguments [a b]})

                   `id3
                   (let [a (gensym)]
                     {:class :fn
                      :constraints {}
                      :return a
                      :arguments [a a]})})

(defn type-of [context expr]
  (cond (or (instance? Boolean expr)
            (number? expr)
            (string? expr)
            (keyword? expr))
        (class expr)

        (seq? expr)
        (type-of-form context (first expr) (rest expr))

        (contains? context expr)
        (context expr)

        (= `fn expr)
        (name expr)

        :else
        (throw (Exception. (str "Unknown type: [" expr "]")))))

(defn type-of-form [context f args]
  (cond
    (= f `fn)
    :yay

    :else
    (type-of-apply context f args)
    ))

(defn type-of-apply [context f args]
  (let [f-type (type-of context f)
        args-type (map (partial type-of context) args)

        _ (if-not (and (map? f-type) (= (:class f-type) :fn))
            (throw (Exception. (str "Cannot apply '" f "' of type [" f-type "]"))))

        poly (reduce
              (fn [p [a v]]
                (cond
                  (instance? Class a)
                  (if (= a v) p
                      (throw (Exception. (str "type mismatch, [" a "] != [" v "]"))))

                  (symbol? a)
                  (if (contains? p a)
                    (if (= (p a) v) p
                        (throw (Exception. (str "type mismatch, [" (p a) "] != [" v "]"))))
                    (assoc p a v))

                  :else (throw (Exception. (str "Unsupported argument type [" a "]")))
                  ))
              {}
              (map vector (:arguments f-type) args-type))]

    (if (symbol? (:return f-type))
      (poly (:return f-type))
      (:return f-type))))

(type-of root-context `(fn [x] x))

(type-of root-context `(if true :a :b))

(type-of root-context `(id 8))
(type-of root-context `(id2 :a 8))
(type-of root-context `(id3 :a :b))
;; (type-of root-context `(id3 :a 8))

(type-of root-context `(sqrt 3.14))
;; (type-of root-context `(sqrt 3))

;; (type-of root-context `(8 3.14))

(type-of root-context `3.14)
(type-of root-context `sqrt)

(type-of root-context `id)
(type-of root-context `id2)
(type-of root-context `id3)

(class `(sqrt 3.14))

(compile (sqrt 3.14))
