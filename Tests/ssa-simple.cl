class Main inherits IO {
  main() : Object { 
    let x : Int <- 5, y : Int, w : Int, z : Int  in
         {
         if (x<3) then 
         {
           y <- 2 * x;
           w <- y;
         }
         else 
           y <- x-3
         fi;
         w <- x - y;
         out_int(w);
         out_string("\n");
      }
  } ;
};
