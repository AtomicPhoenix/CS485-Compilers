class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    {
    out_int(my_attribute/0);
    }
  } ;
} ;

