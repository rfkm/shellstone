(ns stone.env)

(defrecord Env [values outer])

(defprotocol BasicEnv
  (put-new-env [this name value])
  (put-env [this name value])
  (get-env [this name])
  (set-outer [this e])
  (get-outer [this e])
  (where [this name]))

(extend-type Env
  BasicEnv
  (put-new-env [this name value]
    (update-in this [:values] assoc name value))
  
  (put-env [this name value]
    (let [e2 (where this name)]
      (if (nil? e2)
        (put-new-env this name value)
        (put-new-env e2 name value))))
  
  (get-env [this name]
    (let [v ((:values this) name)]
      (if (and (nil? v)
               (:outer this))
        (get-env @(:outer this) name)
        v)))
  
  (set-outer [this e]
    (assoc this :outer e))
  
  (get-outer [this]
    (:outer this))
  
  (where [this name]
    (cond ((:values this) name) this
          (nil? (:outer this)) nil
          :else (where @(:outer this) name))))
