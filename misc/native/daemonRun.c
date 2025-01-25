#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>


void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}


int main(int argc, char **argv) {

    if (argc < 2) err("using: daemon <bin> [args]");

    if (close(STDIN_FILENO) != 0) err("error closing stdin");
    if (close(STDOUT_FILENO) != 0) err("error closing stdout");
    if (close(STDERR_FILENO) != 0) err("error closing stderr");

    sigset_t mask;
    if (sigfillset(&mask) != 0) err("error setting mask");
    if (sigprocmask(SIG_SETMASK, &mask, NULL) != 0) err("error masking signals");

    int i = fork();
    if (i == -1) err("failed to fork");
    if (i != 0) err("normal termination");

    i = fork();
    if (i == -1) err("failed to fork");
    if (i != 0) err("normal termination");

    i = open("/dev/null", O_RDONLY);
    if (i < 0) err("error opening stdin");
    if (dup2(i, STDIN_FILENO) == -1) err("error redirecting stdin");
    i = open("/dev/null", O_RDWR);
    if (i < 0) err("error opening stdout");
    if (dup2(i, STDOUT_FILENO) == -1) err("error redirecting stdout");
    i = open("/dev/null", O_RDWR);
    if (i < 0) err("error opening stderr");
    if (dup2(i, STDERR_FILENO) == -1) err("error redirecting stderr");

    if (chdir("/") == -1) err("error changing directory");

    if (execvp(argv[1], &(argv[1])) == -1) err("error executing process");
    return 0;
}
