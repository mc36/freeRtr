make recompile-compilers.make
make developer\makefile.all
ifplat i80386 make i80386\makefile.all
ifplat mips make mips\makefile.all
make system\makefile.all
make filesystem\makefile
make utils\makefile.all
make internet\makefile.all
