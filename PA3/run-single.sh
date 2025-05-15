./build.sh
cp "$1" "./test-case.cl-type"
./a.out "./test-case.cl-type"
gcc -static -fno-pie -ggdb -o program "./test-case.s"
