class Main inherits IO {
  a : Int <- 5;
  b : Int <- 7;
  main() : Object { {
      if (true) then
        {
          b <- 4;
          out_int(a);
        }
      else
        let c : Int <- a + b in
          out_int(c)  
      fi;
      out_string("\n");
    } 
  } ;
};
