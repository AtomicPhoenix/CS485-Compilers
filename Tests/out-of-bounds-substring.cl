class Main inherits IO {
  my_attribute : String <- "Hello" ;
  main() : Object {
    {
      out_string(my_attribute.substr(5,1));
    }
  } ;
} ;

