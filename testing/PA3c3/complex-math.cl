class Main inherits IO {
  main() : Object {
    {
      let x : Int <- 5 in
        let y : Int in 
        { 
          y <- in_int();
          out_int(y);
        };
    }
  } ;
} ;

