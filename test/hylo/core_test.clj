(ns hylo.core-test
  (:require [clojure.test :refer :all]
            [hylo.core :refer :all]))

(prn *ns*)
(prn (resolve 'id))

#_(deftest a-test
    (testing "FIXME, I fail."
      (is (= 0 1))))

(deftest simple
  (is (= (mk-prim clojure.lang.Keyword) (:type (hylo :x))))
  (is (= (mk-prim Boolean) (:type (hylo true))))
  (is (= (mk-prim Long) (:type (hylo 3))))
  (is (= (mk-prim Double) (:type (hylo 3.14))))

  (is (= (mk-fn Double [Double]) (:type (hylo hylo.core/sqrt))))
  (is (= (mk-fn :a [:a]) (:type (hylo hylo.core/id)))))

(deftest apply
  (is (= (mk-prim Long) (:type (hylo (hylo.core/id 8)))))
  (is (= (mk-prim Double) (:type (hylo (hylo.core/id 3.14)))))
  (is (= (mk-prim Double) (:type (hylo (hylo.core/sqrt 3.14)))))
  (is (= (mk-prim clojure.lang.Keyword) (:type (hylo (if true :a :b))))))

(deftest function
  (is (= (mk-fn :a [:a])
         (:type (hylo (fn [x] x)))))
  (is (= (mk-fn Double [Double])
         (:type (hylo (fn [x] (hylo.core/sqrt x))))))
  (is (= (mk-fn :a [:a :a])
         (:type (hylo (fn [x y] (if true x y))))))
  (is (= (mk-fn Double [Double Double Boolean])
         (:type (hylo (fn [x y z] (if z x (hylo.core/sqrt y))))))))
