(ns unparse.maybeerror)

; TODO can I add this to Functor and Monad?
(defrecord MaybeError [status value])

(def statuses #{"success" "failure" "error"})

(defn maybeerror
  [status value]
  (if (contains? statuses status)
      (->MaybeError status value)
      (throw (new Exception (str "invalid MaybeError constructor name: " status)))))

(defn pure
  [x]
  (maybeerror "success" x))

(defn error
  [x]
  (maybeerror "error" x))

(defn fmap
  [f me]
  (if (= (:status me) "success")
      (pure (f (:value me)))
      me))

(defn app
  [f & args]
  (loop [vals [] xs args]
    (if (empty? xs)
        (pure (apply f vals))
        (let [fst (first xs)]
          (if (= (:status fst) "success")
              (recur (conj vals (:value fst)) (rest xs))
              fst)))))

(defn bind
  [me f]
  (if (= (:status me) "success")
      (f (:value me))
      me))

(defn mapError
  [f me]
  (if (= (:status me) "error")
      (error (f (:value me)))
      me))

(defn plus
  [me that]
  (if (= (:status me) "failure")
      that
      me))

(def zero (maybeerror "failure" nil))
