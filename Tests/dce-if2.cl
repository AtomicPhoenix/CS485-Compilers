class Main inherits IO {
  main() : Object {  
  let a : Int <- 5 in
  let b : Int <- 7 in
    {
      b <- 5;
      if (true) then
        {
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
