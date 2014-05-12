(ns stone.interpreter
  (:use [midje.sweet]
        [stone.parser]
        [stone.env]))

(defn- dispatch 
  ([ast e]
     (dispatch ast))
  ([ast]
     (cond 
      (number? ast) :number
      (keyword? ast) :id
      (string? ast) :string
      :else (:token ast))))

(defmulti stone-eval dispatch)

(defmethod stone-eval :root [ast e]
  (loop [xs (:children ast) res nil]
    (if (zero? (count xs)) 
      res
      (let [res (stone-eval (first xs) e)]
        (println (str " => " res))
        (recur (rest xs) res)))))

(defmethod stone-eval :number [ast e]
  ast)

(defmethod stone-eval :string [ast e]
  ast)

(defmethod stone-eval :id [ast e]
  (get-env @e ast))

(defn- compute-assign [left right e]
  (if (or
       (keyword? left)
       (and (= (:token left) :primary-expr)
            (zero? (count (second (:children left))))
            (= (:token (first (:children left))) :id)))
    (let [rvalue (stone-eval right e)]
      (dosync (alter e put-env left rvalue))
      rvalue)
    (throw (Exception. "bad assignment"))))

(defn- compute-op [op left right]
  (condp = op
    :+ (+ left right)
    :- (- left right)
    :* (* left right)
    (keyword "/") (/ left right)
    :% (mod left right)
    :== (=  left right)
    :> (> left right)
    :< (< left right)))

(defmethod stone-eval :binary-expr [ast e]
  (let [op (:op ast)
        left (:left ast)
        right (:right ast)]
    (if (= op :=) 
      (compute-assign left right e)
      (compute-op op (stone-eval left e) (stone-eval right e)))))

(defmethod stone-eval :while-statement [ast e]
  (let [{:keys [condition body]} ast]
    (loop [res nil]
      (if (stone-eval condition e)
        (recur (stone-eval body e))
        res))))

(defmethod stone-eval :block [ast e]
  (let [c (:children ast)]
    (loop [xs c res nil]
      (if (zero? (count xs))
        res
        (recur (rest xs) (stone-eval (first xs) e))))))

(defmethod stone-eval :if-statement [ast e]
  (let [{:keys [condition body else-body]} ast]
    (if (stone-eval condition e)
      (stone-eval body e)
      (when else-body
        (stone-eval else-body e)))))
