class Main inherits IO {

  main() : SELF_TYPE  {
    {
      let p : Plane <- (new Plane).init(2,4,6) in
        {
        p.print().triple().print().init(5,6,7).print().triple().print().init(5,6,7).print().triple().print().init(5,6,7).print().triple().print().init(5,6,7).print().triple().print().init(5,6,7).print().triple().print().init(5,6,7).print().triple().print().init(5,6,7).print().triple().print();
      if p.triple() = p.init(1,2,3)
        then p.print()
      else
        out_string("not equal")
      fi;
      };
      self;
    }
  };
};

class Plane inherits IO {
  x : Int;
  y : Int;
  z : Int;

  init(x : Int, y1 : Int, z : Int) : Plane {
    {
      x <- x;
      y <- y1;
      z <- z;
      self;
    }
  };

  triple() : Plane {
    {
    x <- 3*x;
    y <- 3*y;
    z <- 3*z;
    self;
    }
  };

  print() : Plane {
    {
    out_int(x).
    out_int(y).
    out_int(z);
    self;
    }
  };


};
