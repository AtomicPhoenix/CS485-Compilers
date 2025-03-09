class Main inherits IO {
  main() : Object {
        let a : Int <- (~2147483647)-1 in {
            out_int(a);
            out_string("\n");
            out_int(~a);
            out_string("\n");
            out_int((~a)-1);
            out_string("\n");
            out_int(~(a-1));
            out_string("\n");
            out_int(~((~2147483647)-1));
            out_string("\n");
            out_int(~(2147483647+1));
            out_string("\n");
            out_int((2147483647+1));
            out_string("\n");
        }
  };
};
