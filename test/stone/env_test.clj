(ns stone.env-test
  (:require [stone.env :refer :all])
  (:use [midje.sweet]))

(fact "env"
  (let [e (->Env {:b 3} (atom (->Env {:a 1} nil)))]
    (let [e2 (atom e)] 
      (swap! e2 put-env :a 2)
      (-> @e2 :outer deref :values :a)) => 2))
