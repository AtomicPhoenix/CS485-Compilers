class Main inherits IO {
  a : Int <- 5;
  b : Int <- 7;
  main() : Object { {
      let c : Int <- a + b in
          {
          b <- 2;
          out_int(a);  
          };
      out_string("\n");
    } 
  } ;
};
