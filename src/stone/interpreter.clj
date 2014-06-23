(ns stone.interpreter
  (:use [clojure.contrib.seq-utils :only [indexed]]
        [stone.parser]
        [stone.env]))

(defrecord Function [param-list block env])
(defrecord Method [param-list block env])
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
  (if (= ast :super) 
    :super
    (find-env e ast true)))

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
    (compute-assign left right e)))

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

(defmethod stone-eval :defmethod [ast e]
  (swap! e put-new-env (:name ast) (->Method (:params ast) (:body ast) e))
  (:name ast))

(defn eval-params [ps idx value e]
  (let [name ((:children ps) idx)] 
    (swap! e put-new-env name value)))

(defn eval-args [ast tgt e]
  (when (not (or (instance? Function tgt)
                 (instance? Method tgt)))
    (throw (Exception. (str "bad function: " tgt))))
  (let [params (:param-list tgt)
        ne (if (instance? Function tgt)
             (atom (create-env (:env tgt)))
             (atom (create-env (get-bottom (:env tgt)))))]
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
        ms (:methodshell ast)
        sc (when sp
             (let [spci (get-env @e sp)]
               (if (instance? ClassInfo spci) 
                 spci
                 (throw (Exception. "unknown super class")))))]
    
    (assoc (->ClassInfo ast sc e) :__methodshell__ ms)))

(defmethod stone-eval :defclass [ast e]
  (let [ci (create-class-info ast e)
        n (:name ast)]
    (swap! e put-env n ci)
    n))

(defprotocol Revised
  (add-reviser [self methodshell reviser]))

(extend-type ClassInfo
  Revised
  (add-reviser [self methodshell reviser]
    (let [old (:reviser self)]
      (if (contains? old methodshell)
        (throw (Error. "The reviser with such name already exsists"))
        (assoc-in self [:reviser methodshell] reviser)))))

(defmethod stone-eval :revise [{:keys [name methodshell] :as ast} e]
  (let [ci (get-env @e name)
        reviser (create-class-info ast e)
        ]
    (if (and (satisfies? Revised ci)
             reviser
             (instance? ClassInfo reviser))
      (swap! e put-env name (add-reviser ci methodshell reviser))
      (throw (Exception. "unknown target class")))
    name))

(defmethod stone-eval :revise-body [ast e]
  (let [c (:children ast)]
    (loop [xs c res nil]
      (if (zero? (count xs))
        nil
        (recur (rest xs) (stone-eval (first xs) e))))))

(defmethod stone-eval :include [ast e]
  (let [v (:target ast)] 
    (swap! e put-new-env :__include__ (vec (conj (get-new-env @e :__include__) v)))))

(defmethod stone-eval :link [ast e]
  (let [v (:target ast)] 
    (swap! e put-new-env :__link__ (cons v (get-new-env @e :__link__)))))

(defmethod stone-eval :class-body [ast e]
  (let [c (:children ast)]
    (loop [xs c res nil]
      (if (zero? (count xs))
        nil
        (recur (rest xs) (stone-eval (first xs) e))))))


(defn init-reviser [rvs rv e caller-env]
  (let [mss (->> rv
                 :definition
                 :body
                 :children
                 (filter #(= (:token %) :include))
                 (map :target)
                 vec)]
    (doseq [ms mss]
      (when-let [rv (ms rvs)]
        (if (instance? ClassInfo rv)
          (init-reviser rvs rv e caller-env)
          (throw (Exception. "invalid reviser"))))))
  
  (let [mss (->> rv
                 :definition
                 :body
                 :children
                 (filter #(= (:token %) :link))
                 (map :target)
                 vec)]
    (doseq [ms mss]
      (when-let [rv (ms rvs)]
        (if (instance? ClassInfo rv)
          (let [se (atom (put-new-env (create-env (:env rv)) :__link_down__ e))]
            (swap! e put-new-env :__link_up__ (cons (init-reviser rvs rv se caller-env) (get-new-env @e :__link_up__))))
          (throw (Exception. "invalid reviser"))))))
  
  (stone-eval (:body (:definition rv)) e)
  e)

(defn init-revisers [rvs e caller-env]
  (when-let [mss (get-new-env @caller-env :__include__ )]  ; include entry points
    (doseq [ms mss]
      (when-let [rv (ms rvs)]
        (if (instance? ClassInfo rv)
          (init-reviser rvs rv e caller-env)
          (throw (Exception. "invalid reviser"))))))
  e)


(defn init-object [ci e caller-env] ; `e' is being kept in the class info
  (when-let [sp (:super ci)]
    (let [se (atom (put-new-env (create-env (:env sp)) :__sub__ e))]
      (swap! e put-new-env :__super__ (init-object sp se caller-env))))
  
  (stone-eval (:body (:definition ci)) e)
  (when-let [rvs (:reviser ci)]
    (init-revisers rvs e caller-env))
  e)


(defn get-env-object [obj member]
  (or (find-object (:env obj) member)
      (throw (Exception. "bad access"))))

(defn read-object [obj member]
  (get-env-super @(get-env-object obj member) member))

(defn write-object [obj member value]
  (swap! (get-env-object obj member) put-new-env member value))

(defn set-field [obj, dot, rvalue]
  (let [n (:name dot)]
    (try
      (write-object obj n rvalue)
      rvalue
      (catch Exception e (throw (Exception. (str "bad member access: " n)))))))

(defn create-instance [ast e ci]
  (let [ne (atom (create-env (:env ci)))
        so (->StoneObject ne)]
    (swap! ne put-new-env :this so)
    (reset! ne @(init-object ci ne e))
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
     
     (and (= value :super))
     (if-let [s @(get-env @e :__super__)]
       (get-env-super s n)
       (throw (Exception. (str "super class not found"))))
     
     (instance? StoneObject value) 
     (read-instance-member ast e value)
     
     :else (throw (Exception. (str "bad member access:" n))))))

(defmethod eval-postfix :dot [ast value e]
  (eval-dot ast value e))

(defmethod stone-eval :lambda [ast e]
  (->Function (:params ast) (:body ast) e))
