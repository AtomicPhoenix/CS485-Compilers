class Main inherits IO {
    main() : Object {
        let a : Int <- 3 in {
            a <- a  <- 7 + a; 
            out_int(a); 
        }
    };
};
