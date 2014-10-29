# ShellStone

ShellStone is an experimental programming language in which [Method Shells][] is implemented.

[Method Shells]: http://www.csg.ci.i.u-tokyo.ac.jp/paper/takeshita-sc13.pdf  "Method Shells: Avoiding Conflicts on Destructive Class Extensions by Implicit Context Switches"

## Examples

```{.ruby}
# Basic expressions
a = 100
b = a * 2

# Loop and conditions
acc = 0;i = 0
while i < 11 {
  if i % 2 == 0 {
    acc = acc + i
  }
  i = i + 1
}
acc # => 55

# Function
def foo(){100}
foo() # => 100

# Recursive function
def fib(n) {
  if n < 2 {
    n
  } else {
    fib(n - 1) + fib(n - 2)
  }
}
fib(10) # => 55

# Closure
def counter (c) {
  fun () { c = c + 1 }
}
c1 = counter(0)
c1() # => 1

# Class
class Position {
  x = y = 0
  def move (nx, ny) {
    x = nx; y = ny;
  }
}

# Inheritance
class Foo {
  x = 30
  def foo() {x * 2}
}
class Bar extends Foo {
  y = 20
  x = 40
  def bar() { foo() + y }
}
p = Bar.new
p.bar() # => 80

# Reviser (like Ruby's open class)
class Foo {
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
f.foo() # => 480

# Method Shell's include
class Foo { x = 1 }
revise ms1:Foo{ def f(){ x } }
revise ms2:Foo{ include ms1; x = 3 }
include ms2
Foo.new.f() # => 3

# Method Shell's link
class Foo {
  x = 0
}
revise ms1:Foo {
  def a(){ x }
}
revise ms2:Foo {
  link ms1

}
include ms2
Foo.new.a() # => 0
```

## License

Copyright © 2014 Ryo Fukumuro

Distributed under the Eclipse Public License, the same as Clojure.