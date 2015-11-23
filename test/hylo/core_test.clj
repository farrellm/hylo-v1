(ns hylo.core-test
  (:require [clojure.test :refer :all]
            [hylo.core :refer :all]))

(prn *ns*)
(prn (resolve 'id))

#_(deftest a-test
    (testing "FIXME, I fail."
      (is (= 0 1))))

(deftest simple
  (is (= clojure.lang.Keyword (:type (hylo :x))))
  (is (= Boolean (:type (hylo true))))
  (is (= Long (:type (hylo 3))))
  (is (= Double (:type (hylo 3.14))))

  (is (= {:class :fn, :constraints {}, :return Double, :arguments [Double]}
         (:type (hylo hylo.core/sqrt))))
  (is (= {:class :fn, :constraints {}, :return :a, :arguments [:a]}
         (:type (hylo hylo.core/id)))))

(deftest apply
  (is (= Long (:type (hylo (hylo.core/id 8)))))
  (is (= Double (:type (hylo (hylo.core/id 3.14)))))
  (is (= Double (:type (hylo (hylo.core/sqrt 3.14)))))
  (is (= clojure.lang.Keyword (:type (hylo (if true :a :b))))))

(deftest function
  (is (= {:class :fn, :constraints {}, :return :a, :arguments [:a]}
         (:type (hylo (fn [x] x)))))
  (is (= {:class :fn, :constraints {}, :return Double, :arguments [Double]}
         (:type (hylo (fn [x] (hylo.core/sqrt x))))))
  (is (= {:class :fn, :constraints {}, :return :a, :arguments [:a :a]}
         (:type (hylo (fn [x y] (if true x y))))))
  (is (= {:class :fn, :constraints {}, :return Double, :arguments [Double Double Boolean]}
         (:type (hylo (fn [x y z] (if z x (hylo.core/sqrt y))))))))
