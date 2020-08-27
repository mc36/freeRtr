#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
FILE* f;
int i;
f=fopen("/dev/ttyS0","r+");
for (i=0;i<8;i++) fputs("Router>\n",f);
for (;;) {
  i=fgetc(f);
///  fputc(i,f);
  if ((i==13)||(i==10)) fputs("Router>",f);
}
}
