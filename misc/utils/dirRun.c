#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>


void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}


int main(int argc, char **argv) {

    if (argc < 3) err("using: cdr <dir> <bin> [args]");

    if (chdir(argv[1]) == -1) err("error changing directory");

    if (execvp(argv[2], &(argv[2])) == -1) err("error executing process");
    return 0;
}
