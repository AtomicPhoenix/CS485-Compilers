class Main inherits IO {
    main() : Object {
        let a : A <- (new A) in {
            a.out_string("a");
            a@IO.out_string("a");
            a@A.out_string("a");
        }
    };

};

class A inherits IO {
    out_string(x : String) : SELF_TYPE { self@IO.out_string("Hi!") };
};
