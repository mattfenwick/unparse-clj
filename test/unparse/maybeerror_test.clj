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
    (is (= "g1" (:value g1))))
  (testing "zero"
    (is (= "failure" (:status b1)))
    (is (= nil (:value b1))))
  (testing "error"
    (is (= "error" (:status e1)))
    (is (= "e1" (:value e1))))
  (testing "constructors"
    (is (= g1 (maybeerror "success" "g1")))
    (is (= b1 (maybeerror "failure" nil)))
    (is (= e1 (maybeerror "error" "e1"))))
  (testing "equality"
    (is (= g1 g1))
    (is (= b1 b1))
    (is (= e1 e1)))
  (testing "inequality"
    (is (not (= g1 g2)))
    (is (not (= g1 b1)))
    (is (not (= g1 e1)))
    (is (not (= b1 g1)))
    (is (not (= b1 e1)))
    (is (not (= e1 g1)))
    (is (not (= e1 b1)))
    (is (not (= e1 e2))))
  (testing "fmap"
    (is (= (fmap inc g3) g4))
    (is (= (fmap inc b1) b1))
    (is (= (fmap inc e3) e3)))
  (testing "bind"
    (is (= (bind g2 f_b) e1))
    (is (= (bind g3 f_b) g4))
    (is (= (bind g4 f_b) b1))
    (is (= (bind b1 f_b) b1))
    (is (= (bind e1 f_b) e1)))
  (testing "app"
    (is (= (app f_a g1 g2 g3) (pure ["g1" 3 "g2" 3])))
    (is (= (app f_a g1 b1 g2) b1))
    (is (= (app f_a g1 g3 e1) e1)))
  (testing "plus"
    (is (= (plus g1 g2) g1))
    (is (= (plus g1 b1) g1))
    (is (= (plus g1 e1) g1))
    (is (= (plus b1 g1) g1))
    (is (= (plus b1 b1) b1))
    (is (= (plus b1 e1) e1))
    (is (= (plus e1 g1) e1))
    (is (= (plus e1 b1) e1))
    (is (= (plus e1 e2) e1)))
  (testing "mapError"
    (is (= (mapError inc g1) g1))
    (is (= (mapError inc b1) b1))
    (is (= (mapError inc e3) e4))))
