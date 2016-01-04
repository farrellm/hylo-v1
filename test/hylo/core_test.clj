(ns hylo.core-test
  (:refer-clojure :exclude [apply])
  (:require [clojure.test :refer :all]
            [hylo.core :refer :all]))

#_(deftest a-test
    (testing "FIXME, I fail."
      (is (= 0 1))))

(deftest simple
  (is (= (mk-prim clojure.lang.Keyword) (:type (hylo :x))))
  (is (= (mk-prim Boolean) (:type (hylo true))))
  (is (= (mk-prim Long) (:type (hylo 3))))
  (is (= (mk-prim Double) (:type (hylo 3.14))))

  (is (= (mk-fn Double [Double]) (:type (hylo Math/sqrt))))
  (is (= (mk-fn :a [:a]) (:type (hylo identity)))))

(deftest apply
  (is (= (mk-prim Long) (:type (hylo (identity 8)))))
  (is (= (mk-prim Double) (:type (hylo (identity 3.14)))))
  (is (= (mk-prim Double) (:type (hylo (Math/sqrt 3.14)))))
  (is (= (mk-prim clojure.lang.Keyword) (:type (hylo (if true :a :b))))))

(deftest function
  (is (= (mk-fn :a [:a])
         (:type (hylo-fn (fn [x] x)))))
  (is (= (mk-fn Double [Double])
         (:type (hylo-fn (fn [x] (Math/sqrt x))))))
  (is (= (mk-fn :a [:a :a])
         (:type (hylo-fn (fn [x y] (if true x y))))))
  (is (= (mk-fn Double [Double Double Boolean])
         (:type (hylo-fn (fn [x y z] (if z x (Math/sqrt y)))))))
  (is (= (mk-fn Double [Double Double Boolean])
         (:type (hylo-fn (fn [x y z] (if z (Math/sqrt y) x))))))
  (is (= (mk-fn Double [(mk-fn Double [:a]) :a])
         (:type (hylo-fn (fn [f x] (Math/sqrt (f x))))))))
