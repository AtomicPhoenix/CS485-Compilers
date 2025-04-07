class Main inherits IO {
  main() : Object {
    if (new A)<(new B) then
      out_string("(new A)<(new B)")
    else
      out_string("(new A)>=(new B)")
    fi
  };
};


class A {};
class B {};
