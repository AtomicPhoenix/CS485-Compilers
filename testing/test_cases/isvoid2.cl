class Main inherits IO {
  main() : Object {
    {
      if (isvoid(5)) then 
        out_int(0)
      else 
        out_int(1)
      fi;

      if (isvoid(new A)) then 
        out_int(0)
      else 
        out_int(1)
      fi;
      
    }
  } ;
} ;

class A {
};

