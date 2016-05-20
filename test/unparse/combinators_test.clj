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
    (testing "not1"
      (let [val ((:not1 iz1) ((:literal iz1) 2))]
        (is (= ((:parse val) [2 3 4] {}) M/zero))
        (is (= ((:parse val) [3 4 5] {}) (good [4 5] {} 3)))))
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
               (M/error \d)))))))

(deftest test-errors
  (testing "error combinators"
    (testing "Error"
      (let [v1 ((:parse (C/error "uh-oh")) "abc" 123)]
        (is (= v1 (M/error "uh-oh")))))
    (testing "CatchError"
      (let [f1 (fn [e] (C/pure 3))
            f2 (fn [e] (C/error "dead again"))]
        (is (= ((:parse (C/catchError f1 (C/error "dead 1"))) "123" [2 4])
               (good "123" [2 4] 3)))
        (is (= ((:parse (C/catchError f1 (C/pure 18))) "123" [2 4])
               (good "123" [2 4] 18)))
        (is (= ((:parse (C/catchError f2 (C/error "dead 1"))) "123" [2 4])
               (M/error "dead again")))))
    (testing "MapError"
      (let [f #(clojure.core/count %1)
            v1 ((:parse (C/mapError f (C/error "abcdef"))) "123abc" nil)
            v2 ((:parse (C/mapError f C/zero)) "123abc" nil)
            v3 ((:parse (C/mapError f (C/pure 82))) "123abc" nil)]
        (is (= v1 (M/error 6)))
        (is (= v2 M/zero))
        (is (= v3 (good "123abc" nil 82)))))))

(deftest test-putstate
  (testing "putting and state"
    (testing "put"
      (let [val (C/put "xyz")]
        (is (= ((:parse val) "abc" [])
               (good "xyz" [] nil)))))
    (testing "putState"
      (let [v1 ((:parse (C/putState 29)) "abc123" 2)]
        (is (= v1 (good "abc123" 29 nil)))))
    (testing "updateState"
      (let [v1 ((:parse (C/updateState #(* %1 4))) "abc" 18)]
        (is (= v1 (good "abc" 72 nil)))))))

(deftest test-checkseq
  (testing "check and sequencing"
    (testing "Check"
      (let [val (C/check #(> (clojure.core/count %1) 3) C/get)]
        (is (= ((:parse val) "abcde" [])
               (good "abcde" [] "abcde")))
        (is (= ((:parse val) "abc" [])
               M/zero))))
    (testing "Many0"
      (let [val (C/many0 ((:literal iz1) 3))]
        (is (= ((:parse val) [4 4 4] {})
               (good [4 4 4] {} [])))
        (is (= ((:parse val) [3 3 4 5] {})
               (good [4 5] {} [3 3])))))
    (testing "Many1"
      (let [val (C/many1 ((:literal iz1) 3))]
        (is (= ((:parse val) [4 4 4] {})
               M/zero))
        (is (= ((:parse val) [3 3 4 5] {})
               (good [4 5] {} [3 3])))))
    (testing "Seq"
      (let [val (C/seq (:item iz1) ((:literal iz1) 2) ((:literal iz1) 8))]
        (is (= ((:parse val) [3 2 4] {})
               M/zero))
        (is (= ((:parse val) [3 2 8 16] {})
               (good [16] {} [3 2 8])))))
    (testing "App"
      (let [parser (C/app (fn [x y z] (+ x (* y z)))
                          (:item iz1)
                          ((:satisfy iz1) #(> %1 2))
                          (:item iz1))
            v1 ((:parse parser) [1 2 3 4 5] "hi")
            v2 ((:parse parser) [5 6 7 8 9] "bye")
            v3 ((:parse parser) [5 6] "goodbye")]
        (is (= v1 M/zero))
        (is (= v2 (good [8 9] "bye" 47)))
        (is (= v3 M/zero))))
    (testing "AppP"
      (let [parser (C/appP (C/pure (fn [x y z] (+ x (* y z))))
                           (:item iz1)
                           ((:satisfy iz1) #(> %1 2))
                           (:item iz1))
            v1 ((:parse parser) [1 2 3 4 5] "hi")
            v2 ((:parse parser) [5 6 7 8 9] "bye")
            v3 ((:parse parser) [5 6] "goodbye")]
        (is (= v1 M/zero))
        (is (= v2 (good [8 9] "bye" 47)))
        (is (= v3 M/zero))))
    (testing "AppP -- type error"
      (is (thrown? Exception (C/appP (fn [x] x) (:item iz1)))))))

(deftest test-more-combinators
  (testing "more combinators"
    (testing "Optional"
      (let [parser (C/optional ((:literal iz1) 3) "blargh")
            v1 ((:parse parser) [1 2 3] "hi")
            v2 ((:parse parser) [3 2 1] "bye")]
        (is (= v1 (good [1 2 3] "hi" "blargh")))
        (is (= v2 (good [2 1] "bye" 3)))))
; TODO remove these -- they're not needed for the Clojure library
;    (testing "optional -- no value"
;      (let [p (C/optional ((:literal iz1) 3))
;            v1 ((:parse p) [3 2 1] nil)
;            v2 ((:parse p) [1 2 3] nil)]
;        (is (= v1 (good [2 1] nil 3)))
;        (is (= v2 (good [1 2 3] nil nil)))))
    (testing "Seq2R"
      (let [val (C/seq2R ((:literal iz1) 2) ((:literal iz1) 3))]
        (is (= ((:parse val) [4 5] {})
               M/zero))
        (is (= ((:parse val) [2 4 5] {})
               M/zero))
        (is (= ((:parse val) [2 3 4] {})
               (good [4] {} 3)))))
    (testing "Seq2L"
      (let [val (C/seq2L ((:literal iz1) 2) ((:literal iz1) 3))]
        (is (= ((:parse val) [4 5] {})
               M/zero))
        (is (= ((:parse val) [2 4 5] {})
               M/zero))
        (is (= ((:parse val) [2 3 4] {})
               (good [4] {} 2)))))
    (testing "SepBy0"
      (let [parser (C/sepBy0 ((:oneOf iz1) "pq") ((:oneOf iz1) "st"))
            val1 ((:parse parser) "abc" {})
            val2 ((:parse parser) "ppabc" {})
            val3 ((:parse parser) "psabc" {})
            val4 ((:parse parser) "psqtqabc" {})]
        (is (= val1 (good "abc" {} {:separators [], :values []})))
        (is (= val2 (good (list \p \a \b \c) {} {:separators [], :values [\p]})))
        (is (= val3 (good (list \s \a \b \c) {} {:separators [], :values [\p]})))
        (is (= val4 (good (list \a \b \c) {} {:separators [\s \t], :values [\p \q \q]})))))
    (testing "SepBy1"
      (let [parser (C/sepBy1 ((:oneOf iz1) "pq") ((:oneOf iz1) "st"))
            val1 ((:parse parser) "abc" {})
            val2 ((:parse parser) "ppabc" {})
            val3 ((:parse parser) "psabc" {})
            val4 ((:parse parser) "psqtqabc" {})]
        (is (= val1 M/zero))
        (is (= val2 (good (list \p \a \b \c) {} {:separators [], :values [\p]})))
        (is (= val3 (good (list \s \a \b \c) {} {:separators [], :values [\p]})))
        (is (= val4 (good (list \a \b \c) {} {:separators [\s \t], :values [\p \q \q]})))))
    (testing "Lookahead"
      (let [parser (C/lookahead ((:oneOf iz1) [2 3]))]
        (is (= ((:parse parser) [2 3 4 5] 41)
               (good [2 3 4 5] 41 2)))
        (is (= ((:parse parser) [3 4 5] 41)
               (good [3 4 5] 41 3)))
        (is (= ((:parse parser) [4 5] nil)
               M/zero))))
    (testing "Not0"
      (let [val (C/not0 ((:literal iz1) 2))]
        (is (= ((:parse val) [2 3 4] {})
               M/zero))
        (is (= ((:parse val) [3 4 5] {})
               (good [3 4 5] {} nil)))))
    (testing "Commit"
      (let [val (C/commit "bag-agg" ((:literal iz1) 2))]
        (is (= ((:parse val) [2 3 4] "hi")
               (good [3 4] "hi" 2)))
        (is (= ((:parse val) [3 4 5] "hi")
               (M/error "bag-agg")))))
    (testing "Zero"
      (is (= ((:parse C/zero) nil nil)
             M/zero)))
    (testing "Get"
      (is (= ((:parse C/get) "abc" {})
             (good "abc" {} "abc"))))
    (testing "GetState"
      (is (= ((:parse C/getState) "abc" 123)
             (good "abc" 123 123))))))
