class Main inherits IO {
  main() : Object {
      let a : Main <- new Main, b : Object <- a in
    if (a=b) then
      out_string("(new Main)==0")
    else
      out_string("(new Main)!=0")
    fi
  };
};
