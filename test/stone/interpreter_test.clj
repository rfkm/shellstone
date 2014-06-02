(ns stone.interpreter-test
  (:require [clojure.test :refer :all]
            [stone.parser :refer :all]
            [stone.interpreter :refer :all]
            [stone.env :refer :all])
  (:use [midje.sweet]))

(fact "eval"
  (let [e #(stone-eval (ast %) (ref (->Env {} nil)))] 
    (e "1")                   => 1
    (e "1.1")                 => 1.1
    (e "\"foo\"")             => "foo"
    (e "i = 0")               => 0
    (e "i = 0;i")             => 0
    (e "i = 10 + 20;i")       => 30
    (e "a = 10;b = 20;a - b") => -10
    (e "10 + 20 * 30")        => 610
    (e "(10 + 20) * 30")      => 900
    (e "10 * (20 + 30)")      => 500
    (e "a = 200;a < 2000")    => true
    (e "acc = 0;i = 0
        while i < 11 {
          acc = acc + i
          i = i + 1
        }
        acc")                 => 55
    (e "acc = 0;i = 0
        while i < 11 {
          if i % 2 == 0 {
            acc = acc + i
          }
          i = i + 1
        }
        acc")                 => 30
    (e "def foo(){100}")      => :foo
    (e "def foo(){100}
        foo()")               => 100
    (e "def add(a, b) {a+b}
        add(10, 20)")         => 30
    (e "def x2(a) {a*2}
        x2(10)")              => 20
    (e "x = 1
        def foo (y) {x}
        def bar (x) { foo(x + 1) }
        bar(3)")              => 1
    (e "def fib(n) {
          if n < 2 {
            n
          } else {
            fib(n - 1) + fib(n - 2)
          }
        }
        fib(10)")             => 55
    (e "x = 0
        def incX(){
          x = x + 1
        }
        incX()
        x")                   => 1
    (e "inc = fun(x) {x + 1}
        inc(3)")              => 4
    (e "def counter (c) {
          fun () { c = c + 1 }
        }
        c1 = counter(0)
        c1()")                => 1
    (e "def counter (c) {
          fun () { c = c + 1 }
        }
        c1 = counter(0)
        c1()
        c1()")                => 2
    (e "def counter (c) {
          fun () { c = c + 1 }
        }
        c1 = counter(0)
        c2 = counter(0)
        c1()
        c1()
        c2()")                => 1
    (e "a = fun(x){ fun(y){ x + y } }
            a(10)(20)")       => 30
    (e "class Position {
          x = y = 0
          def move (nx, ny) {
            x = nx; y = ny;
          }
        }")                   => :Position
    (e "class Position {
          x = y = 0
          def move (nx, ny) {
            x = nx; y = ny;
          }
        }
        p = Position.new
        p.move(3, 4)
        p.y")                 => 4
    (e "class Position {
          x = 3
          def foo(){x}
        }
        p = Position.new
        p.x = 10
        p.foo()") => 10
    (e "a = 10
        class Position {
          x = 30
          def foo() { a + x}
        }
        p = Position.new
        a = 20
        p.foo()")             => 50
    (e "class Foo {
          x = 30
          def foo() {x * 2}
        }
        class Bar extends Foo {
          y = 20
          def bar() { foo() + y }
        }
        p = Bar.new
        p.bar()")             => 80))



