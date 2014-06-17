(ns stone.interpreter
  (:use [clojure.contrib.seq-utils :only [indexed]]
        [stone.parser]
        [stone.env]))


(defrecord Function [param-list block env])
(defrecord ClassInfo [definition super env])
(defrecord StoneObject [env])

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

(declare set-field has-postfix get-postfix eval-sub)
(defn- compute-assign [left right e]
  (if (or
       (keyword? left)
       (and (= (:token left) :primary-expr)
            (zero? (count (second (:children left))))
            (= (:token (first (:children left))) :id)))
    (let [rvalue (stone-eval right e)]
      (swap! e put-env left rvalue)
      rvalue)
    (throw (Exception. "bad assignment"))))

(defn- compute-assign-1 [left right e]
  (if (and (= (:token left) :primary-expr)
           (has-postfix left 0)
           (= (:token (get-postfix left 0)) :dot))
    (let [t (eval-sub left e 1)]
      (if (instance? StoneObject t)
        (set-field t (get-postfix left 0) right)
        (compute-assign left right e)))
    (compute-assign left right e)
    ))

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
      (compute-assign-1 left right e)
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

(defmethod stone-eval :def-statement [ast e]
  (swap! e put-new-env (:name ast) (->Function (:params ast) (:body ast) e))
  (:name ast))

(defn eval-params [ps idx value e]
  (let [name ((:children ps) idx)] 
    (swap! e put-new-env name value)))

(defn eval-args [ast tgt e]
  (when (not (instance? Function tgt))
    (throw (Exception. "bad function")))
  (let [params (:param-list tgt)
        ne (atom (->Env {} (:env tgt)))]
    (when (not= (count (:children ast)) 
                (count (:children params)))
      (throw (Exception. "bad number of arguments")))
    (doseq [[idx a] (indexed (:children ast))]
      (eval-params params idx (stone-eval a e) ne))

    (stone-eval (:block tgt) ne)))

(defn dispatch-postfix [pst value e]
  (:token pst))

(defmulti eval-postfix dispatch-postfix)
(defmethod eval-postfix :arguments [pst value e]
  (eval-args pst value e))

(defn operand [ast]
  (-> ast :children first))
(defn get-postfix [ast nest]
  (let [c (:children ast)] 
    (c (- (count c) nest 1))))
(defn has-postfix [ast nest]
  (> (- (count (:children ast)) nest) 1))
(defn eval-sub [ast e nest]
  (if (has-postfix ast nest)
    (let [tgt (eval-sub ast e (inc nest))]
      (eval-postfix (get-postfix ast nest) tgt e))
    (stone-eval (operand ast) e)))
(defmethod stone-eval :primary-expr [ast e]
  (eval-sub ast e 0))

(defn create-class-info [ast e]
  (let [sp (:super-class ast)
        sc (when sp
             (let [spci (get-env @e sp)]
               (if (instance? ClassInfo spci) 
                 spci
                 (throw (Exception. "unknown super class")))))]
    
    (->ClassInfo ast sc e)))
(defmethod stone-eval :defclass [ast e]
  (let [ci (create-class-info ast e)
        n (:name ast)]
    (swap! e put-env n ci)
    n))

(defprotocol Revised
  (add-reviser [self reviser]))

(extend-type ClassInfo
  Revised
  (add-reviser [self reviser]
    (let [old (:reviser self)] 
      (assoc self :reviser (assoc reviser :super old)))))

(defmethod stone-eval :revise [ast e]
  (let [n (:name ast)
        ci (get-env @e n)
        reviser (create-class-info ast e)
        ]
    (if (and (satisfies? Revised ci)
             reviser
             (instance? ClassInfo reviser))
      (swap! e put-env n (add-reviser ci reviser))
      (throw (Exception. "unknown target class")))
    n))

(defmethod stone-eval :revise-body [ast e]
  (let [c (:children ast)]
    (loop [xs c res nil]
      (if (zero? (count xs))
        nil
        (recur (rest xs) (stone-eval (first xs) e))))))

(defmethod stone-eval :class-body [ast e]
  (let [c (:children ast)]
    (loop [xs c res nil]
      (if (zero? (count xs))
        nil
        (recur (rest xs) (stone-eval (first xs) e))))))

(defn init-object [ci e]
  (when-let [sp (:super ci)]
    (init-object sp e))
  (stone-eval (:body (:definition ci)) e)
  (when-let [rv (:reviser ci)]
    (init-object rv e)))

(defn get-env-object [obj member]
  (if (contains? (:values @(:env obj)) member)
    (:env obj)
    (throw (Exception. "bad access")) ;; TODO: custom exception class
    ))
(defn read-object [obj member]
  (get-env @(get-env-object obj member) member))

(defn write-object [obj member value]
  (swap! (get-env-object obj member) put-new-env member value))

(defn set-field [obj, dot, rvalue]
  (let [n (:name dot)]
    (try
      (write-object obj n rvalue)
      rvalue
      (catch Exception e (throw (Exception. (str "bad member access: " n)))))))

(defn create-instance [ast e ci]
  (let [e (atom (->Env {} (:env ci)))
        so (->StoneObject e)]
    (swap! e put-new-env :this so)
    (init-object ci e)
    so))

(defn read-instance-member [ast e value]
  (try
    (read-object value (:name ast))))

(defn eval-dot [ast value e]
  (let [n (:name ast)]
    (cond
     (and (instance? ClassInfo value)
          (= n :new)) 
     (create-instance ast e value)
     
     (instance? StoneObject value) 
     (read-instance-member ast e value)
     
     :else (throw (Exception. (str "bad member access:" n))))))

(defmethod eval-postfix :dot [ast value e]
  (eval-dot ast value e))

(defmethod stone-eval :lambda [ast e]
  (->Function (:params ast) (:body ast) e))
