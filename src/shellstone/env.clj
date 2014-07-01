(ns shellstone.env)

(defrecord Env [values outer])

(defprotocol BasicEnv
  (put-new-env [this name value])
  (put-env [this name value])
  (get-new-env [this name])
  (get-env [this name])
  (get-env-super [this name])
  (get-env-link [this name])
  (set-outer [this e])
  (get-outer [this])
  (where [this name]))


(extend-type Env
  BasicEnv
  (put-new-env [this name value]
    (update-in this [:values] assoc name value))
  
  (put-env [this name value]
    (let [e2 (where this name)]
      (if (nil? e2)
        (put-new-env this name value)
        (do (swap! e2 put-new-env name value)
            this))))
  
  (get-new-env [this name]
    ((:values this) name))
  
  (get-env [this name]
    (let [v ((:values this) name)]
      (if (and (nil? v)
               (:outer this))
        (get-env @(:outer this) name)
        v)))
  
  (get-env-link [this name]
    (when-let [sps (get-env this :__link_up__)]
      (let [ret (for [sp sps] 
                  (letfn [(gs [this name]
                            (let [v (get-new-env this name)]
                              (if-let [sp (and (nil? v)
                                               (get-env this :__link_up__))]
                                (gs @sp name)
                                v)))] 
                    (gs @sp name)))]
        (cond 
         (> (count ret) 1)
         (throw (Exception. "cannot identify a method"))
         
         :else
         (first ret)))))
  (get-env-super [this name]
    
    (or (get-env-link this name)
        (get-env this name) 
        (if-let [sp (get-env this :__super__)]
          (letfn [(gs [this name]
                
                    (let [v (get-new-env this name)]
                      (if-let [sp (and (nil? v)
                                       (get-env this :__super__))]
                        (gs @sp name)
                        v)))] 
            (gs @sp name))
          (throw (Exception. (str "super class not found:" name))))))
  
  (set-outer [this e]
    (assoc this :outer e))
  
  (get-outer [this]
    (:outer this))
  
  (where [this name]
    (letfn [(where-sub [e name child]
              (cond ((:values e) name) (:outer child)
                    (nil? (:outer e)) nil
                    :else (where-sub @(:outer e) name e)))]
      (cond (nil? (:outer this)) nil
            :else (where-sub @(:outer this) name this)))))

(defn- assign-uid [env]
  (let [uuid (str (java.util.UUID/randomUUID))]
    (assoc env :id uuid)))

(defn create-env 
  ([] 
     (create-env {} nil))
  ([outer]
     (create-env {} outer))
  ([values outer] 
     (assign-uid (->Env values outer))))

(defn get-bottom [e]
  (or 
   (when-let [s (get-new-env @e :__link_down__)]
     (get-bottom s))
   (if-let [s (get-new-env @e :__sub__)]
     (get-bottom s)
     e)))

(defn find-object [env name & [include-outer?]]
  (or (when-let [links (get-new-env @env :__link_up__)]
        (first (for [link (reverse links)]
                 (find-object link name include-outer?))))
      (when (get-new-env @env name) env)
      
      (and include-outer? 
           (get-outer @env)
           (find-object (get-outer @env) name include-outer?))
      (when-let [super (get-new-env @env :__super__)]
        (find-object super name include-outer?))
      (when-let [super (and (get-outer @env)
                            (get-new-env @(get-outer @env) :__super__))] ; Method
        (find-object super name include-outer?))
      ))

(defn find-env [env name & [include-outer?]]
  (when-let [e (find-object env name include-outer?)]
    (get-new-env @e name)))
