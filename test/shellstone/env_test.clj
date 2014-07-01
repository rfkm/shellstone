(ns shellstone.env-test
  (:require [shellstone.env :refer :all])
  (:use [midje.sweet]))

(fact "env"
  (let [e (create-env {:b 3} (atom (create-env {:a 1} nil)))]
    (let [e2 (atom e)] 
      (swap! e2 put-env :a 2)
      (-> @e2 :outer deref :values :a)) => 2))

(fact "id"
  (let [e (atom (create-env))
        e-id (:id @e)
        e2 (atom (create-env e))
        e2-id (:id @e2)]
    (swap! e2 put-env :a 2)
    (swap! e put-env :b 3)
    (:id @e)  => e-id
    (:id @e2) => e2-id
    (:id @e) =not=>  e2-id))
