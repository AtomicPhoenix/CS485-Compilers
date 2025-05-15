class Main inherits IO {
  main() : Object { 
    let x : Int <- 5 in
     let x : Int <- x - 3 in
      let y : Int in
       let w : Int in
        let z : Int in {
         w <- x - y;
         z <- x + y;
         out_int(z);
         out_string("\n");
      }
  } ;
};
