class Main inherits IO {
  void_obj : A <- new A;
  
  main() : Object { {
      case void_obj of
       c1 : Int => out_int(c1);
       c2 : String => out_string(c2);
      esac;
}
  } ;
} ;

class A {
  a: Int <- 5;
};
