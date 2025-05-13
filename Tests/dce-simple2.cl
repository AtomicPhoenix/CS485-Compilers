class Main inherits IO {

  main() : Object { 

  let a : Int <- 1 in
  let b : Int <- 3 in
  let c : Int <- a + b in
  let d : Int <- 7 in
  let e : Int <- d + d in
    {
      out_int(a);
      out_string("\n");
    } 
  } ;
};

-- a <- int 1
-- b <- int 3
-- c <- + a b 
-- d <- int 7
-- e <- + d d 
-- retval <- call out_int c 
-- return retval
