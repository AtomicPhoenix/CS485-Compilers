class A inherits IO {
    a : Int;
};
class B inherits A {
    b : Int;
};
class C inherits B {
    c: Int;
    cfun() : Object { out_string("Hi\n") };
    c(o : Int) : Object { c <- o };
};

class D inherits C {
    d: Int;
    cfun() : Object {{ out_string("Hello\n"); out_int(c); }};
};

class Main {
    main() : Object { 
        let d : C <- new D in 
        { 
            d.c(5); 
            d.cfun();
            d@C.cfun();
        } 
    };
};
