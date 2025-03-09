class Main {
  main(): Object {
    let c: B <- new C in (c.getSelf() = new A)
  };
};

class A {
  getSelf(): SELF_TYPE { self };
};

class B inherits A { };

class C inherits B { };


