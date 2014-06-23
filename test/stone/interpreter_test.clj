(ns stone.interpreter-test
  (:require [clojure.test :refer :all]
            [stone.parser :refer :all]
            [stone.interpreter :refer :all]
            [stone.env :refer :all])
  (:use [midje.sweet]))

(fact "eval"
  (let [e #(stone-eval (ast %) (atom (create-env)))] 
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
    (ast "p.move(3,4)")
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
        p.bar()")             => 80
    (e "class Foo {
          x = 30
          def foo() {x * 2}
        }
        class Bar extends Foo {
          y = 20
          x = 40
          def bar() { foo() + y }
        }
        p = Bar.new
        p.bar()")             => 100
    (e "class Fuga{
          x = 30
          def foo() {x * 2}
        }
        class Foo extends Fuga{
        }
        class Bar extends Foo {
          y = 20
          def bar() { foo() + y }
        }
        p = Bar.new
        p.bar()")             => 80
    (e "class Foo {
          x = 30
          def foo() { x * 2 }
        }
        revise ms1:Foo {
          y = 100
          def foo() { y + x }
        }
        revise ms2:Foo {
          y = 50
          z = 200
          def bar() { z * 2 }
          def foo() { y + x + bar() }
        }
        include ms2
        f = Foo.new
        f.foo()")             => 480
    (e "class Fuga {
          def foo() {100}
        }
        class Foo extends Fuga{
        }
        class Bar extends Foo {
          def foo() {super.foo() * 2}
        }
        b = Bar.new
        b.foo()")             => 200
    (e "class Foo {
          x = 100
        }
        f = Foo.new
        y = 20
        f.y")                 => (throws Exception "bad access")
    (e "class Foo { x = 1 }
        revise ms:Foo{ x = 2 }
        Foo.new.x")           => 1
    (e "class Foo { x = 1 }
        revise ms1:Foo{ x = 2 }
        revise ms2:Foo{ x = 3 }
        include ms1
        include ms2
        Foo.new.x")           => 3
    (e "class Foo { x = 1 }
        revise ms1:Foo{ x = 2 }
        revise ms2:Foo{ x = 3 }
        include ms2
        include ms1
        Foo.new.x")           => 2
    (e "class Foo { x = 1 }
        revise ms1:Foo{ x = 2; y = 0}
        revise ms2:Foo{ include ms1; x = 3 }
        include ms2
        Foo.new.y")           => 0
    (e "class Foo { x = 1 }
        revise ms1:Foo{ def f(){ x } }
        revise ms2:Foo{ include ms1; x = 3 }
        include ms2
        Foo.new.f()")         => 3
    (e "class Foo { x = 1 }
        revise ms1:Foo{ y = 2 }
        revise ms2:Foo{ y = 3 }
        revise ms3:Foo{ 
          include ms1; 
          include ms2; 
          x = 4 }
        include ms3
        Foo.new.y")           => 3
    (e "class Foo {
          x = 0
        }
        revise ms1:Foo {
          def a(){ x }
        }
        revise ms2:Foo {
          link ms1

        }
        include ms2
        Foo.new.a()")             => 0
    (e "class Foo {
          x = 0
        }
        revise ms1:Foo {
          x = 1
        }
        revise ms2:Foo {
          link ms1
          x = 2
        }
        include ms2
        Foo.new.x")             => 1
    (e "class Foo {
          x = 10
          def foo() {
            x * 2
          }
        }
        revise ms1:Foo {
          def foo() {
            x * 3
          }
        }
        revise ms2:Foo {
          def foo() {
            x * 4
          }
        }
        revise ms3:Foo {
          include ms1
          link ms2
        }
        revise ms4:Foo {
          include ms3
          def bar() {
            x * foo() // 10 * 10 * 4
          }
        }
        include ms4
        f = Foo.new
        f.bar()")             => 400
    (e "class Foo {
          x = 0
        }
        revise ms1:Foo {
          def inc() {
            x = x + 1
          }
        }
        revise ms2:Foo {
          link ms1
          def a() { inc() }
          def b() { x }
        }
        include ms2
        f = Foo.new
        f.a(); f.b()")        => 1))


(fact "scope test"
  (let [env (atom (create-env))
        e #(do (stone-eval (ast %) env) @env)]
    
    ;; Define a class
    (e "class Foo {
          x = 10
          def foo() {
            x * 2
          }
        }") => truthy
         
    (fact 
      "Environment in which class holds is identical to the
      environment when the class is defined."
      (-> @env
          :values 
          :Foo
          :env
          deref
          :id) => (:id @env))
    
    ;; Create an instance
    (e "f = Foo.new") => truthy
    
    (let [v (-> @env
                :values
                :f
                :env
                deref
                :values)]
      (fact 
        (:x v) => 10
        (:foo v) => #(instance? stone.interpreter.Method %))
      (fact 
        (-> v
            :foo
            :env
            deref
            :outer
            deref
            :id) => (:id @env)
        (-> v
            :foo
            :env
            deref
            :values
            :x
            ) => 10))
    
    ;; Define a sub class
    (e "class Bar extends Foo {
          x = 20
        }") => truthy
         
    (fact 
      (-> @env
          :values
          :Bar
          :super
          ) => #(instance? stone.interpreter.ClassInfo %))
          
    ;; Create an instance of the subclass
    (e "b = Bar.new") => truthy
    
    (fact
      (-> @env
          :values
          :b
          :env
          deref
          :values
          :x) => 20
          
      (-> @env
          :values
          :b
          :env
          deref
          :values
          :foo) => falsey
          
      (-> @env
          :values
          :b
          :env
          deref
          :id) => 
      (-> @env 
          :values
          :b
          :env
          deref
          :values
          :__super__
          deref
          :values
          :__sub__
          deref
          :id)
      
      (-> @env
          :values
          :b
          :env
          deref
          :values
          :__super__
          deref
          :values
          :foo) => #(instance? stone.interpreter.Method %)
      (-> @env
          :values
          :b
          (read-object :foo)) => #(instance? stone.interpreter.Method %))
    
    ;; Define a reviser
    (e "revise ms1:Foo {
          def getX(){ x }
        }
        revise ms2:Foo {
          link ms1
        }") => truthy    
         
    (fact
      (-> @env
          :values
          :Foo
          :reviser
          :ms1
          ) => truthy)
    
    ;; Create an instance of revised Foo
    (e "include ms2
        f = Foo.new")
    
    (fact
      (-> @env
          :values
          :__include__) => [:ms2]
      (-> @env
          :values
          :f
          :env
          deref
          :values
          :__link_up__
          first
          deref
          :values
          :getX) => (get-new-env @(find-object (-> @env
                                                   :values
                                                   :f
                                                   :env) :getX) :getX))
    
    (e "class Foo {
          x = 30
          def foo() {x * 2}
        }
        class Bar extends Foo {
          y = 20
          def bar() { foo() + y }
        }
        b = Bar.new")
    (fact 
      (find-env (get-bottom (-> @env
                                :values
                                :b
                                :env
                                deref
                                :values
                                :bar
                                :env)) :foo) => truthy)
    (e "a = 10
        class Position {
          x = 30
          def foo() { a + x }
        }
        p = Position.new")
    (fact 
      (-> @env
          :values
          :p
          :env ;; => object's env
          deref
          :values
          :foo
          :env ;; => foo method's env (= object's env)
          deref
          :outer ;; => global env
          deref
          :values
          :a
          ) => 10
      (find-env (-> @env
                    :values
                    :p
                    :env
                    deref
                    :values
                    :foo
                    :env
                    create-env
                    atom
                    ) :a true) => 10)
    (e "class Foo {
          x = 0
        }
        revise ms1:Foo {
          def a(){ x }
        }
        revise ms2:Foo {
          link ms1
        }
        include ms2
        f = Foo.new")
    
    (fact
      (-> @env
          :values
          :f
          :env
          deref
          :id) => 
      (-> @env
          :values
          :f
          :env
          deref
          :values
          :__link_up__
          first
          deref
          :values
          :__link_down__
          deref
          :id))
    
    (-> @env
        :values
        :f
        :env
        deref
        :values
        :__link_up__
        first
        deref
        :values
        :a
        :env
        deref
        :values
        :__link_down__
        deref
        :id) =>       
    (-> @env
        :values
        :f
        :env
        deref
        :id)
    (-> @env
        :values
        :f
        :env
        deref
        :values
        :__link_up__
        first
        deref
        :values
        :a
        :env
        (get-bottom)
        (find-env :x true) ) => 0
    ))

