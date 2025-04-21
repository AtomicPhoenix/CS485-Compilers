class C  inherits IO {
    c: Int <- 5;
};

class D inherits C {
    d: Int <- 15;
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
