class Main inherits IO {
  a : A <- new A;
  b : B <- new B;
  c : C <- new C;
  d : D <- new D;

  main() : Object  {
    {
      a.print();
      b.print();
      c.print();
      d.print();
    }
  };
};

class A inherits IO {
  attr : A <- case self of
	a : A => (new B);
	b : B => (new C);
	c : C => (new D);
	d : D => d;
  esac;
  
  print() : SELF_TYPE {
    out_string(attr.type_name())
  };
};


class B inherits A {
};

class C inherits B {
};

class D inherits C {
};





