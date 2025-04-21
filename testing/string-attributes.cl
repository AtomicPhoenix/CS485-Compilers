class Main inherits IO {
   
    main() : Object {
        {
                let str : StrObject <- new StrObject in
                    {
                        str.init("Hello World!");
                        out_string(str.foo().get());
                    };
        }
    };
};

class StrObject {
    data : String;

    init (str : String) : SELF_TYPE {
        {
            data <- str;
            self;
        }
    };
    
    get() : String {
        data
    };

    getPos(position : Int) : String {
        data.substr(position, 1)
    };

    getLength() : Int {
        data.length()
    };

    foo() : SELF_TYPE {
        let position : Int <- 0 in
            let  temp : String in
            { 
                {
                    while position < getLength() loop
                    {
                        temp <- temp.concat(getPos(position).concat("\n"));
                        position <- position + 1;
                    }
                    pool;
                    data <- temp;
                    self;
                };
            }
    };
};
