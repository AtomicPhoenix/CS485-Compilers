class Main inherits IO {
  main() : Object {
    if (new Main)=(new Main) then
      out_string("(new Main)==(new Main)")
    else
      out_string("(new Main)!=(new Main)")
    fi
  };
};
