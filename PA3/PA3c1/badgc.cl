class Main inherits IO {
a : Int;
    init(num : Int) : SELF_TYPE {{ a <- num; self; }};
    main() : Object {
        let a : Main in {
            let b : Main <- (new Main).init(7) in
                a <- b;
            a.printa();
        }
    };
    printa() : Object { out_int(a) };
};
