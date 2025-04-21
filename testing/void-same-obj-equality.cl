class Main inherits IO {
  main() : Object {
    let a : Main <- (new Main) in
    if a=a then
      out_string("(new Main)==(new Main)")
    else
      out_string("(new Main)!=(new Main)")
    fi
  };
};
