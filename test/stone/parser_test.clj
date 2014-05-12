(ns stone.parser-test
  (:require [clojure.test :refer :all]
            [stone.parser :refer :all])
  (:use [midje.sweet]
        [blancas.kern.core]))

;; (use 'blancas.kern.core)

(fact "number"
  (value number "100") => 100)

(fact "string"
  (value string "\"hoge\"") => "hoge")

(fact "id"
  (value id "foo") => :foo)

(fact "primary"
  (value primary "100")     => 100
  (value primary "\"foo\"") => "foo"
  (value primary "foo")     => :foo
  (value primary "(1 + 2)") => {:token :binary-expr 
                                :op :+ 
                                :left 1
                                :right 2})

(fact "factor"
  (value factor "-100") => {:token :negative :operand 100}
  (value factor "100")  => 100
  (value factor "-a")   => {:token :negative :operand :a}
  )

(fact "expr"
  (value expr "1 + 3") => {:token :binary-expr 
                           :op :+ 
                           :left 1
                           :right 3}
  (value expr "1 + 3 - a") => {:token :binary-expr
                               :op :-
                               :left {:token :binary-expr 
                                      :op :+
                                      :left 1
                                      :right 3}
                               :right :a}
  (value expr "1 + 3 * a") => {:token :binary-expr
                               :op :+
                               :left 1
                               :right {:token :binary-expr
                                       :op :*
                                       :left 3 
                                       :right :a}}
  (value expr "1 + (3 + a)") => {:token :binary-expr
                                 :op :+
                                 :left 1
                                 :right {:token :binary-expr
                                         :op :+ 
                                         :left 3
                                         :right :a}}
  )

(fact "block"
  (value block "{ a }") => {:token :block 
                            :children [:a]}
  (value block "{ a;b }") => {:token :block 
                              :children [:a :b]}
  (value block "{ a;b; }") => {:token :block 
                               :children [:a :b]}
  (value block "{ a

                  b;
                }") => {:token :block
                        :children [:a :b]})
(fact "if-statement"
  (value if-statement "if a > 10 {b}") => {:token :if-statement
                                           :condition {:token :binary-expr
                                                       :op :>
                                                       :left :a
                                                       :right 10}
                                           :body {:token :block
                                                  :children [:b]}}
  (value if-statement "if a > 10 {b} else {c}") => {:token :if-statement
                                                    :condition {:token :binary-expr
                                                                :op :>
                                                                :left :a
                                                                :right 10}
                                                    :body {:token :block
                                                           :children [:b]}
                                                    :else-body {:token :block
                                                                :children [:c]}})
(fact "while-statement"
  (value while-statement "while a > 10 { b }") => {:token :while-statement
                                                   :condition {:token :binary-expr
                                                               :op :>
                                                               :left :a
                                                               :right 10}
                                                   :body {:token :block
                                                          :children [:b]}})


(fact "statement"
  (value statement "b") => :b
  (value statement "while a > 10 { b }") => {:token :while-statement
                                             :condition {:token :binary-expr
                                                         :op :>
                                                         :left :a
                                                         :right 10}
                                             :body {:token :block
                                                    :children [:b]}})

(fact "program"
  (value program "i = 0
         
                  i = 1;;
                  i = 2;") => {:token :root
                               :children [{:token :binary-expr
                                           :op :=
                                           :left :i
                                           :right 0}
                                          {:token :binary-expr
                                           :op :=
                                           :left :i
                                           :right 1}
                                          {:token :binary-expr
                                           :op :=
                                           :left :i
                                           :right 2}]})
