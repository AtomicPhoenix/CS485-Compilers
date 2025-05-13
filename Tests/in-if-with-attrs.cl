class Main inherits IO {
  my_attribute : Int;
  main() : Object {
    out_int(
      if (true) then
        my_attribute <- 1985
      else
        my_attribute <- 2005
      fi)
  };
} ;

