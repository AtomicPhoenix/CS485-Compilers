class Main inherits IO {
  
  main() : Object { {
      out_string(((new B).increment()).type_name());
      out_string(((new A).increment()).type_name());
      let c : A  <- new B in 
        out_string(((c).increment()).type_name());
    }
  } ;
} ;

class A inherits Main {
  a: Int <- 5;
  
  increment() : SELF_TYPE {
    { 
      a = a+5;
      out_int(a);
    }
  };
};


class B inherits A {
  b: Int <- 5;
  
  increment() : SELF_TYPE {
    {
      b = a+5;
      out_int(b);
    }
  };
};
