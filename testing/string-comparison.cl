class Main inherits IO {
  main() : Object {
    {
      if "X" = "X" then
        out_string("This is correct\n")
      else
        out_string("This is wrong\n")
      fi;
      if "X" = "Y" then
        out_string("This is wrong\n")
      else
        out_string("This is correct\n")
      fi;
      out_string("Enter 'q': ");
      let char : String <- in_string() in
        if "q" = char then
          out_string("This is correct\n")
        else
          out_string("This is wrong\n")
        fi;
    }
  } ;
} ;

