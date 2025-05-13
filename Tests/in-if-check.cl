class Main inherits IO {
  main() : Object {
    {
    if false<=true then
      out_string("So true bestie")
    else
      out_string("no")
    fi;

    let my_attribute : Int <- 5 in
      case my_attribute of
       c2 : Object => my_attribute;
       c1 : Int => out_int(c1);
       c2 : String => out_string(c2);
      esac;
    }
  };
};


