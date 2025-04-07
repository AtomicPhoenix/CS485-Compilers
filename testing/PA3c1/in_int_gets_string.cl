class Main inherits IO {
  s : Int <- in_int();
  main() : Object {
    {
        out_int(s/s/s);
        out_int(s+s);
        out_int(s*s);
        out_int(s-2*s);
        out_int(~s);
        out_int(~3*s);
    }
  } ;
} ;
