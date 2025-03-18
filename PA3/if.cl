class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    if (true) then
      out_string("true")
      --1
    else
      out_string("not true")
      --2
    fi
  } ;
} ;

class A {
  a: Int <- 5;
};
