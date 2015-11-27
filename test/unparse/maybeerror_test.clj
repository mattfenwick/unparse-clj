(ns unparse.maybeerror-test
  (:require [clojure.test :refer :all]
            [unparse.maybeerror :refer :all]))

(defn f_b
  [x]
  (cond (= x 3) (pure (+ x 1))
        (= x 4) zero
        :else (error "e1")))

(defn f_a
  [x y z]
  [x z y z])

(def g1 (pure "g1"))
(def g2 (pure "g2"))
(def g3 (pure 3))
(def g4 (pure 4))
(def e1 (error "e1"))
(def e2 (error "e2"))
(def e3 (error 3))
(def e4 (error 4))
(def b1 zero)


(deftest test-pure
  (testing "pure"
    (is (= "success" (:status g1)))
    (is (= "g1" (:value g1)))))

(deftest test-zero
  (testing "zero"
    (is (= "failure" (:status b1)))
    (is (= nil (:value b1)))))
