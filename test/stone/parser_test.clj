(ns stone.parser-test
  (:require [clojure.test :refer :all]
            [stone.parser :refer :all])
  (:use [midje.sweet]
        [blancas.kern.core]))

(require '[clojure.walk :as w])
;; (use 'blancas.kern.core)

(fact "number"
  (value number "100") => 100)

(fact "string"
  (value string "\"hoge\"") => "hoge")

(fact "id"
  (value id "foo") => :foo)

(fact "primary"
  (value primary "100")       => 100
  (value primary "\"foo\"")   => "foo"
  (value primary "foo")       => :foo
  (value primary "(1 + 2)")   => {:token :binary-expr 
                                  :op :+ 
                                  :left 1
                                  :right 2}
  (value primary "foo(a)")    => {:token :primary-expr
                                  :children [:foo {:token :arguments
                                                   :children [:a]}]}
  (value primary "foo(a)(b)") => {:token :primary-expr
                                  :children [:foo 
                                             {:token :arguments
                                              :children [:a]}
                                             {:token :arguments
                                              :children [:b]}]}
  (value primary "foo.bar(100)") => {:token :primary-expr
                                    :children [:foo
                                               {:token :dot
                                                :name :bar}
                                               {:token :arguments
                                                :children [100]}]})

(fact "simple"
  (value simple "foo a") => {:token :primary-expr
                             :children [:foo {:token :arguments
                                              :children [:a]}]})

(fact "factor"
  (value factor "-100") => {:token :negative :operand 100}
  (value factor "100")  => 100
  (value factor "-a")   => {:token :negative :operand :a}
  )

(fact "expr"
  (value expr "1 + 3")       => {:token :binary-expr 
                                 :op :+ 
                                 :left 1
                                 :right 3}
  (value expr "1 + 3 - a")   => {:token :binary-expr
                                 :op :-
                                 :left {:token :binary-expr 
                                        :op :+
                                        :left 1
                                        :right 3}
                                 :right :a}
  (value expr "1 + 3 * a")   => {:token :binary-expr
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
  (value block "{ a }")    => {:token :block 
                               :children [:a]}
  (value block "{ a;b }")  => {:token :block 
                               :children [:a :b]}
  (value block "{ a;b; }") => {:token :block 
                               :children [:a :b]}
  (value block "{ a

                  b;
                }")        => {:token :block
                               :children [:a :b]})
(fact "if-statement"
  (value if-statement "if a > 10 {b}")          => {:token :if-statement
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
  (value statement "b")                  => :b
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
                  i = 2;")  => {:token :root
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
                                            :right 2}]}
  (value program "def foo(x){ x }
                  foo 100") => {:token :root
                                :children [{:token :def-statement
                                            :name :foo
                                            :params {:token :param-list
                                                     :children  [:x]}
                                            :body {:token :block
                                                   :children [:x]}}
                                           {:token :primary-expr
                                            :children [:foo {:token :arguments
                                                             :children [100]}]}]}
  (value program "class Foo{ x = 100 }
                  foo 100") => {:token :root
                                :children [{:token :defclass
                                            :name :Foo
                                            :super-class nil
                                            :body {:token :class-body
                                                   :children  [{:token :binary-expr
                                                                :op :=
                                                                :left :x
                                                                :right 100}]}}
                                           {:token :primary-expr
                                            :children [:foo {:token :arguments
                                                             :children [100]}]}]})


(fact "params"
  (value params "a,b, c") => {:token :param-list 
                              :children [:a :b :c]})

(fact "param-list"
  (value param-list "()")      => {:token :param-list
                                   :children []}
  (value param-list "(a)")     => {:token :param-list
                                   :children [:a]}
  (value param-list "(a,b,c)") => {:token :param-list 
                                   :children [:a :b :c]})

(fact "defun"
  (value defun "def a (x) { x }") => {:token :def-statement 
                                      :name :a
                                      :params {:token :param-list 
                                               :children [:x]}
                                      :body {:token :block
                                             :children [:x]}})
(fact "args"
  (value args "100, 200") => {:token :arguments 
                              :children [100 200]})

(fact "postfix"
  (value postfix "()")         => {:token :arguments
                                   :children []}
  (value postfix "(100, 200)") => {:token :arguments
                                   :children [100 200]}
  (value postfix ".foo") => {:token :dot
                             :name :foo})

(fact "lambda"
  (value lambda "fun(x){x}")  => {:token :lambda
                                  :params {:token :param-list
                                           :children [:x]}
                                  :body {:token :block
                                         :children [:x]}}
  (value lambda "fun (x){x}") => {:token :lambda
                                  :params {:token :param-list
                                           :children [:x]}
                                  :body {:token :block
                                         :children [:x]}})
(fact "member"
  (value member "def a (x) { x }") => {:token :defmethod
                                      :name :a
                                      :params {:token :param-list 
                                               :children [:x]}
                                      :body {:token :block
                                             :children [:x]}}
  (value member "foo a") => {:token :primary-expr
                             :children [:foo {:token :arguments
                                              :children [:a]}]})


(fact "class-body"
  (value class-body "{ def a (x) { x }; x = 100}") => {:token :class-body
                                                       :children [{:token :defmethod 
                                                                   :name :a
                                                                   :params {:token :param-list 
                                                                            :children [:x]}
                                                                   :body {:token :block
                                                                          :children [:x]}}
                                                                  {:token :binary-expr 
                                                                   :op := 
                                                                   :left :x
                                                                   :right 100}]})

(fact "def-class"
  (value defclass "class Foo { x = 100 }") => {:token :defclass
                                               :name :Foo
                                               :super-class nil
                                               :body {:token :class-body
                                                      :children [{:token :binary-expr
                                                                  :op :=
                                                                  :left :x
                                                                  :right 100}]}}
  (value defclass "class Foo extends Hoge {}") => {:token :defclass
                                                   :name :Foo
                                                   :super-class :Hoge
                                                   :body {:token :class-body
                                                          :children []}})

(fact "include"
  (value dec-include "include foo") => {:token :include :target :foo})

(fact "link"
  (value dec-link "link foo") => {:token :link :target :foo})

(fact "revise-member"
  (value dec-include "include foo") => {:token :include :target :foo}
  (value dec-link "link foo")       => {:token :link :target :foo}
  (value defun "def a (x) { x }")   => {:token :def-statement 
                                        :name :a
                                        :params {:token :param-list 
                                                 :children [:x]}
                                        :body {:token :block
                                               :children [:x]}})

(fact "revise"
  (value revise "revise ms:Foo { def a (x) { x } }") => {:token :revise
                                                         :name :Foo
                                                         :methodshell :ms
                                                         :body {:token :revise-body
                                                                :children [{:token :defmethod
                                                                            :name :a
                                                                            :params {:token :param-list 
                                                                                     :children [:x]}
                                                                            :body {:token :block
                                                                                   :children [:x]}}]}})
