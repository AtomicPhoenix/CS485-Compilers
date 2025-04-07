class A {
    a : Int;
};
class B inherits A {
    b : Int;
};
class C inherits B {
    c: Int;
};
class D inherits C {
    d: Int;
};
class Main inherits IO {
    main() : Object { out_string("Hello, World\n") };
};
