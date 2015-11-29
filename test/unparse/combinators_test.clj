(ns unparse.combinators-test
  (:refer-clojure :exclude [count seq get])
  (:require [clojure.test :refer :all]
            [unparse.combinators :as C]
            [unparse.maybeerror :as M]))

(def iz1 C/basic)
(def iz2 C/position)
(def iz3 C/count)

(defn good
  [rest state result]
  (M/pure {:rest rest, :state state, :result result}))

(deftest test-basic
  (testing "basic"
    (testing "item"
      (is (= ((:parse (:item iz1)) "" nil) M/zero))
      (is (= ((:parse (:item iz1)) "abcdef" nil)
             (good (list \b \c \d \e \f) nil \a))))
    (testing "literal"
      (let [val ((:literal iz1) 3)]
        (is (= ((:parse val) [3 4 5] {})
            (good [4 5] {} 3)))
        (is (= ((:parse val) [4 5] {})
               M/zero))))
    (testing "satisfy"
      (let [v1 ((:parse ((:satisfy iz1) #(> %1 3))) [1 2 3] "bye")
            v2 ((:parse ((:satisfy iz1) #(< %1 3))) [1 2 3] "hi")]
        (is (= v1 M/zero))
        (is (= v2 (good [2 3] "hi" 1)))))
    (testing "string"
      (let [parser ((:string iz1) "abc")
            v1 ((:parse parser) "abcdef" nil)
            v2 ((:parse parser) "abdef" nil)]
        (is (= v1 (good (list \d \e \f) nil "abc")))
        (is (= v2 M/zero))))
; TODO need to test what this depends on first
;    (testing "not1"
;      (let [val ((:not1 iz1) ((:literal iz1) 2))]
;        (is (= ((:parse val) [2 3 4] {}) M/zero))
;        (is (= ((:parse val) [3 4 5] {}) (good [4 5] {} 3)))))
    (testing "oneOf"
      (let [p ((:oneOf iz1) "abc")]
        (is (= ((:parse p) "cqrs" nil) (good (list \q \r \s) nil \c)))
        (is (= ((:parse p) "aqrs" nil) (good (list \q \r \s) nil \a)))
        (is (= ((:parse p) "dqrs" nil) M/zero))))))

(deftest test-general
  (testing "general combinators"
    (testing "fmap"
      (let [f #(+ %1 7)
            v1 ((:parse (C/fmap f (C/pure 3))) "ab" 81)
            v2 ((:parse (C/fmap f C/zero)) "ab" 81)
            v3 ((:parse (C/fmap f (C/error "oops"))) "ab" 81)]
        (is (= v1 (good "ab" 81 10)))
        (is (= v2 M/zero))
        (is (= v3 (M/error "oops")))))
    (testing "pure"
      (let [val ((:parse (C/pure 3)) "abc" 2)]
        (is (= val (good "abc" 2 3)))))
    (testing "bind"
      (let [two (C/bind (:item iz1) (:literal iz1))]
        (is (= ((:parse two) "abcde" {}) M/zero))
        (is (= ((:parse two) "aabcde" {}) 
               (good (list \b \c \d \e) {} \a)))))
    (testing "AltBinaryRules"
      (let [g1 (C/pure 3)
            g2 (C/pure "hi")
            b C/zero
            e (C/error "oops")
            e2 (C/error "2nd")
            r1 (good "abc" nil 3)
            r3 M/zero
            r4 (M/error "oops")]
        (is (= ((:parse (C/alt g1 g2)) "abc" nil) r1))
        (is (= ((:parse (C/alt g1 b)) "abc" nil) r1))
        (is (= ((:parse (C/alt g1 e)) "abc" nil) r1))
        (is (= ((:parse (C/alt b g1)) "abc" nil) r1))
        (is (= ((:parse (C/alt b b)) "abc" nil) r3))
        (is (= ((:parse (C/alt b e)) "abc" nil) r4))
        (is (= ((:parse (C/alt e g1)) "abc" nil) r4))
        (is (= ((:parse (C/alt e b)) "abc" nil) r4))
        (is (= ((:parse (C/alt e e2)) "abc" nil) r4))))
    (testing "AltCornerCases"
      (is (= ((:parse (C/alt)) [1 2 3] nil) 
             M/zero))
      (is (= ((:parse (C/alt (C/pure \h))) [1 2 3] nil)
             (good [1 2 3] nil \h)))
      (is (= ((:parse (C/alt (C/error "oops"))) [1 2 3] nil)
             (M/error "oops")))
      (is (= ((:parse (C/alt C/zero)) [1 2 3] nil)
             M/zero))
      (let [p1 (C/alt C/zero ((:literal iz1) 1) ((:literal iz1) 2) (C/error \d))]
        (is (= ((:parse p1) [1 3 4] nil)
               (good [3 4] nil 1)))
        (is (= ((:parse p1) [2 3 4] nil)
               (good [3 4] nil 2)))
        (is (= ((:parse p1) [3 3 4] nil)
               (M/error \d)))))
    (testing "Error"
      (let [v1 ((:parse (C/error "uh-oh")) "abc" 123)]
        (is (= v1 (M/error "uh-oh")))))))
