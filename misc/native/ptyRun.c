#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <poll.h>
#include <pty.h>
#include <sys/wait.h>


void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}


int main(int argc, char **argv) {

    if (argc < 2) err("using: pty <bin> [args]");

    int status = 1;

    struct pollfd* fds = malloc(sizeof(struct pollfd)*2);
    if (fds == NULL) err("error allocating memory");
    int commSock = 0;
    int childPid = forkpty(&commSock, NULL, NULL, NULL);
    if (childPid == -1) err("error creating pty");
    if (childPid == 0) {
        printf("child started\r\n");
        if (execvp(argv[1], &(argv[1])) == -1) err("error executing process");
        return 0;
    }

    int i = fork();
    if ((i != -1) && (i != 0)) {
        if (waitpid(childPid, &status, 0) == -1) err("error waiting for process");
        sleep(1);
        printf("\r\nchild exited with %i code\r\n", status);
        fflush(stdout);
        return WEXITSTATUS(status);
    }

    printf("child %i created on %i\r\n", childPid, commSock);
    fflush(stdout);

    unsigned char buf[1024];
    for (;;) {
        fds[0].fd = commSock;
        fds[0].events = POLLIN;
        fds[0].revents = 0;
        fds[1].fd = STDIN_FILENO;
        fds[1].events = POLLIN;
        fds[1].revents = 0;
        if (poll(fds, 2, 100) < 0) err("error in poll");
        if ((fds[0].revents&POLLIN) != 0) {
            i = read(commSock, buf, sizeof(buf));
            write(STDOUT_FILENO, buf, i);
        }
        if ((fds[1].revents&POLLIN) != 0) {
            i = read(STDIN_FILENO, buf, sizeof(buf));
            write(commSock, buf, i);
        }
        i = fds[0].revents|fds[1].revents;
        if ((i & POLLERR) != 0) err("error on sockets");
        if ((i & POLLHUP) != 0) break;
    }

    printf("\r\nchild closed stdio lines\r\n");
    fflush(stdout);
    sleep(10);
    return status;
}
