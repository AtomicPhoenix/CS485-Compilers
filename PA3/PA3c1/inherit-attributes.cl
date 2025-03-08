class A inherits IO {
    a : Int;
};
class B inherits A {
    b : Int;
};
class C inherits B {
    c: Int <- 5;
    setC(o : Int) : Object { c <- o };
    printC(o : Int) : Object { out_int(c) };
};

class D inherits C {
    d: Int <- 15;
    setC(o : Int) : Object { c <- o + 5 };
    setD(o : Int) : Object { d <- o + c };
    print() : Object {{ out_int(c);   out_int(d);}};
};

class Main {
    main() : Object { 
        let d : D <- new D in 
        { 
            d.setD(5); 
            d.print();
        }
     };
};
