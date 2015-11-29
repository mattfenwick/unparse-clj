(ns unparse.combinators
  (:refer-clojure :exclude [seq count get])
  (:require [unparse.maybeerror :as maybeerror]))

; wrapper around a callable of type `[t] -> s -> ME ([t], s, a)`.
(defrecord Parser [parse])

(defn checkFunction
  [fName actual]
  (if (not (fn? actual))
      (throw (new Exception (str {:message "type error" :function fName 
                                  :expected "function" :actual actual})))))

(defn checkParser
  [fName actual]
  (if (not (= (type actual) unparse.combinators.Parser))
      (throw (new Exception (str {:message "type error" :function fName
                                  :expected "Parser" :actual actual})))))

(defn result
  [value rest state]
  {:result value :rest rest :state state})

(defn good
  [value rest state]
  (maybeerror/pure (result value rest state)))

(defn compose
  [f g]
  #(f (g %1)))

(defn fmap
  [g parser]
  (checkParser "fmap" parser)
  (checkFunction "fmap" g)
  (let [h (fn [r] (result (g (:result r)) (:rest r) (:state r)))
        f (fn [xs s] (maybeerror/fmap h ((:parse parser) xs s)))]
    (->Parser f)))

(defn pure
  [x]
  (->Parser (fn [xs s] (good x xs s))))

(defn bind
  [parser g]
  (checkParser "bind" parser)
  (checkFunction "bind" g)
  (let [f (fn [xs s]
            (let [r ((:parse parser) xs s)
                  val (:value r)]
              (if (= "success" (:status r))
                  ((:parse (g (:result val))) (:rest val) (:state val))
                  r)))]
    (->Parser f)))

(defn error
  [e]
  (->Parser (fn [_xs_ _s_] (maybeerror/error e))))

(defn catchError
  [f parser]
  (checkFunction "catchError" f)
  (checkParser "catchError" parser)
  (->Parser (fn [xs s]
              (let [v ((:parse parser) xs s)]
                (if (= "error" (:status v))
                    ((:parse (f (:value v))) xs s)
                    v)))))

(defn mapError
  [f parser]
  (checkFunction "mapError" f)
  (checkParser "mapError" parser)
  (catchError (compose error f) parser))

(defn put
  [xs]
  (->Parser (fn [_xs_ s] (good nil xs s))))

(defn putState
  [s]
  (->Parser (fn [xs _s_] (good nil xs s))))

(defn updateState
  [g]
  (checkFunction "updateState" g)
  (->Parser (fn [xs s] (good nil xs (g s)))))

(def zero (->Parser (fn [_xs_ _s_] maybeerror/zero)))
(def get (->Parser (fn [xs s] (good xs xs s))))
(def getState (->Parser (fn [xs s] (good s xs s))))

(defn check
  [predicate parser]
  (checkFunction "check" predicate)
  (checkParser "check" parser)
  (->Parser (fn [xs s]
              (let [r ((:parse parser) xs s)]
                (cond
                  (not (= "success" (:status r))) r
                  (predicate (:result (:value r))) r
                  :else maybeerror/zero)))))

(defn many0
  [parser]
  (checkParser "many0" parser)
  (->Parser (fn [xs s]
              (loop [vals []
                     tokens xs
                     state s]
                (let [r ((:parse parser) tokens state)]
                  (cond
                    (= (:status r) "success")
                      (recur (conj vals (:result (:value r)))
                             (:rest (:value r))
                             (:state (:value r)))
                    (= (:status r) "failure") (good vals tokens state)
                    :else r))))))

(defn many1
  [parser]
  (checkParser "many1" parser)
  (check #(> (clojure.core/count %1) 0) (many0 parser)))

(defn seq
  [& parsers]
  (for [p parsers]
    (checkParser "seq" p))
  (->Parser
    (fn [xs s]
      (loop [vals [], tokens xs, state s, ps parsers]
        (if (empty? ps)
            (good vals tokens state)
            (let [r ((:parse (first ps)) tokens state)]
              (if (= "success" (:status r))
                  (recur (conj vals (:result (:value r)))
                         (:rest (:value r))
                         (:state (:value r))
                         (rest ps))
                  r)))))))

(defn appP
  [p & parsers]
  (checkParser "appP" p)
  (for [parser parsers]
    (checkParser "appP" parser))
  (bind
    p
    (fn [f]
      (let [g (fn [args] (apply f args))] ; TODO [g (partial apply f)] ?
        (fmap g (apply seq parsers))))))

(defn app
  [f & parsers]
  (apply appP (cons (pure f) parsers)))

(defn _first
  [x _]
  x)

(defn _second
  [_ y]
  y)

(defn seq2L
  [p1 p2]
  (checkParser "seq2L" p1)
  (checkParser "seq2L" p2)
  (app _first p1 p2))

(defn seq2R
  [p1 p2]
  (checkParser "seq2R" p1)
  (checkParser "seq2R" p2)
  (app _second p1 p2))

(defn lookahead
  [parser]
  (checkParser "lookahead" parser)
  (bind get 
        (fn [xs]
          (bind getState
                (fn [s]
                  (seq2L parser (seq (put xs) (putState s))))))))

(defn not0
  [parser]
  (checkParser "not0" parser)
  (->Parser (fn [xs s]
              (let [r ((:parser parser) xs s)]
                (cond
                  (= "error" (:status r)) r
                  (= "success" (:status r)) maybeerror/zero
                  :else (good nil xs s))))))

(defn alt
  [& parsers]
  (for [p parsers]
    (checkParser "alt" p))
  (->Parser (fn [xs s]
              (loop [ps parsers]
                (if (empty? ps)
                    maybeerror/zero
                    (let [r ((:parse (first ps)) xs s),
                          status (:status r)]
                      (cond
                        (= "success" status) r
                        (= "error" status) r
                        :else (recur (rest ps)))))))))

(defn optional
  [parser default_v]
  (checkParser "optional" parser)
  (alt parser (pure default_v)))

(defn commit
  [e parser]
  (checkParser "commit" parser)
  (alt parser (error e)))

(defn _buildSepByValue
  [fst pairs]
  (loop [vals [fst], seps [], ps pairs]
    (if (empty? ps)
        {:values vals :separators seps}
        (recur (conj vals (clojure.core/get (first ps) 0))
               (conj seps (clojure.core/get (first ps) 1))
               (rest ps)))))

(defn _pair
  [x y]
  [x y])

(defn sepBy1
  [parser separator]
  (app _buildSepByValue parser (many0 (app _pair separator parser))))

(defn sepBy0
  [parser separator]
  (optional (sepBy1 parser separator) {:values [] :separators []}))

(defn Itemizer
  [item]
  (checkParser "Itemizer" item)
  (let [literal (fn [x]
                  (check (fn [y] (= x y)) item))
        satisfy (fn [pred]
                  (checkFunction "satisfy" pred)
                  (check pred item))
        not1 (fn [parser]
               (checkParser "not1" parser)
               (seq2R (not0 parser) item))
        string (fn [elems] ; TODO is this necessary?  can I just do '(seq (map literal) elems)'?
                 (seq2R (apply seq (map literal elems)) (pure elems)))
        oneOf (fn [elems]
                (let [elem-set (set elems)]
                  (satisfy (fn [x] (contains? elem-set x)))))]
    {:item item,
     :literal literal,
     :satisfy satisfy,
     :not1 not1,
     :string string,
     :oneOf oneOf}))

(defn _item_basic
  [xs s]
  (if (empty? xs)
      maybeerror/zero
      (good (first xs) (rest xs) s)))

(defn _bump
  [char position]
  (let [[line col] position]
    (if (= char \newline)
        [(+ line 1) 1]
        [line (+ col 1)])))

(defn _item_position
  [xs position]
  (if (empty? xs)
      maybeerror/zero
      (good (first xs) (rest xs) (_bump (first xs) position))))

(defn _item_count
  [xs ct]
  (if (empty? xs)
      maybeerror/zero
      (good (first xs) (rest xs) (+ ct 1))))

(def basic (Itemizer (->Parser _item_basic)))
(def position (Itemizer (->Parser _item_position)))
(def count (Itemizer (->Parser _item_count)))

(defn run
  [parser input_string state]
  ((:parse parser) input_string state))
