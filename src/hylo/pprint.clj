(ns hylo.pprint
  (require [clojure.pprint :refer [pprint]]
           [clojure.string :refer [join]]))

(defn pretty-type [t]
  (case (:class t)
    ::hylo.core/primitive   (.getSimpleName (:type t))
    ::hylo.core/fn          (let [ts (map pretty-type (concat (:parameters t)
                                                              [(:return t)]))]
                              (str "(" (join " -> " ts) ")"))
    ::hylo.core/polymorphic (:label t)
    t))

(defn pretty-context [c]
  {:parent      (if-let [p (:parent c)]
                  (pretty-context p))
   :assumptions (if-let [a (:assumptions c)]
                  (into {} (map #(vector (key %) (pretty-type (val %)))) a))})

(defn pretty-ret [r]
  {:type    (pretty-type (:type r))
   :ast     (:ast r)
   :context (pretty-context (:context r))})

(defn pprint-type [t]
  (pprint (pretty-type t)))

(defn pprint-context [c]
  (pprint (pretty-context c)))

(defn pprint-ret [r]
  (pprint (pretty-ret r)))
