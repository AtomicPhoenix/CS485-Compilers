../cool --x86 --opt --cfg "$1"
dot -Tpng main_pre.dot >filename.png
feh ./filename.png
rm *".dot"
rm ./filename.png
