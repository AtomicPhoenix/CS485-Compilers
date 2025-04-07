class HeapOverflow {
  run() : Object {
    (new HeapOverflow).run()
  };
};

class Main inherits IO {
  main() : Object {
    (new HeapOverflow).run()
  };
};
