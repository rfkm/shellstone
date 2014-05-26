(ns stone.interpreter
  (:use [clojure.contrib.seq-utils :only [indexed]]
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

(defrecord Function [param-list block env])

(defmethod stone-eval :def-statement [ast e]
  (dosync (alter e put-new-env (:name ast) (->Function (:params ast) (:body ast) e)))
  (:name ast))

(defn eval-params [ps idx value e]
  (let [name ((:children ps) idx)] 
    (dosync (alter e put-new-env name value))))

(defn eval-args [ast tgt e]
  (when (not (instance? Function tgt))
    (throw (Exception. "bad function")))
  (let [params (:param-list tgt)
        ne (ref (->Env {} (:env tgt)))]
    (when (not= (count (:children ast)) 
                (count (:children params)))
      (throw (Exception. "bad number of arguments")))
    (doseq [[idx a] (indexed (:children ast))]
      (eval-params params idx (stone-eval a e) ne))

    (stone-eval (:block tgt) ne)))

(defmethod stone-eval :primary-expr [ast e]
  (letfn [(operand []
            (-> ast :children first))
          (postfix [nest]
            (let [c (:children ast)] 
              (c (- (count c) nest 1))))
          (has-postfix [nest]
            (> (- (count (:children ast)) nest) 1))
          (eval-sub [e nest]
            (if (has-postfix nest)
              (let [tgt (eval-sub e (inc nest))]
                (eval-args (postfix nest) tgt e))
              (stone-eval (operand) e)))]
    (eval-sub e 0)))

(defmethod stone-eval :lambda [ast e]
  (->Function (:params ast) (:body ast) e))
