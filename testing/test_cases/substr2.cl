class Main inherits IO {
  s : String <- "Hello world!a";

  main() : Object {
    {
      out_int(attrLength());

      let s2 : String <- attrCell(0) in
        out_string(s2);

      let s2 : String <- attrCell(1) in
        out_string(s2);

      let s2 : String <- attrCell(2) in
        out_int(s2.length());
      
      
    }
   };
  
   attrLength() : Int {
       s.length()
   };
   
   attrCell(position : Int) : String {
       s.substr(position, 1)
   };
} ;

