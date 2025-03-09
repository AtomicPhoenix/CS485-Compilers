class Main inherits IO {
    main() : Object {
        let a : Int <- 5 in {
            let a : Int <- 7, b : Int <- a, a : Int <- 3 in
            {out_int(a);
                out_int(b);};
                out_int(a);
        }
    };
};
