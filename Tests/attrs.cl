class Main inherits IO {
  main() : Object {
      let c : C <- new C in {
        out_int(c.getA());
        out_string("\n");
        out_int(c.getB());
        out_string("\n");
        out_int(c.getC());
        out_string("\n");
        out_int(c.getA());
        out_string("\n");
        out_int(c.getB());
        out_string("\n");
        out_int(c.getC());
        out_string("\n");
      }
  };
};

class A {
  innerClass : SELF_TYPE <- self;
  getInnerClass() : SELF_TYPE {innerClass};
  a : Int <- 6;
  getA() : Int {a};
};

class B inherits A {
  getB() : Int {b};
  b : Int <- {self; if (a < 5) then a else b+1 fi;};
};

class C inherits B {
  c : Int <- {a <- 17; getA(); 16;};
  getC() : Int {~innerClass.getInnerClass().getInnerClass().getC2()};
  getC2() : Int {5};
};
