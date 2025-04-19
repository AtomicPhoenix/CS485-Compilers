class Main inherits IO {
  x : Int <- 5;
  y : Int <- 4;
  z : Int;

  main() : Object {
    {
      out_int(x);
      incX();
      out_int(x);


      out_int(y);
      decY();
      out_int(y);


      setZ(0);
      out_int(z);
    }
  } ;

  incX() : SELF_TYPE {
      {
      x <- x + 1;
      self;
      }
    };

  decY() : SELF_TYPE {
      {
      y <- y - 1;
      self;
      }
    };

  setZ(arg : Int) : SELF_TYPE {
      {
      z <- arg;
      self;
      }
    };
} ;


