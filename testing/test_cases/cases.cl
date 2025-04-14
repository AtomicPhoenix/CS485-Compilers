class Main inherits IO {
  my_attribute : Int <- 5 ;
  
  main() : Object { {
      case my_attribute of
       c1 : Int => out_int(c1);
       c2 : String => out_string(c2);
      esac;
}
  } ;
} ;

