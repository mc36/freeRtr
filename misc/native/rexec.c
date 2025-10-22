#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <poll.h>


void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}


#define add2buf(a) {int q=strlen(a); memcpy(&buf[i],&a[0],q+1); i+=q+1;}


int main(int argc, char **argv) {
    if (argc < 6) err("using: re <addr> <port> <user> <pass> <cmd> [cmd]");
    int port = atoi(argv[2]);
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof (addr));
    if (inet_aton(argv[1], &addr.sin_addr) == 0) err("bad addr address");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    printf("connecting %s %i.\n", inet_ntoa(addr.sin_addr), port);
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) err("unable to open socket");
    if (connect(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) err("failed to connect socket");

    unsigned char buf[4096];
    int i = 1;
    buf[0]=0;
    add2buf(argv[3]);
    add2buf(argv[4]);
    printf("running");
    for (int o=5; o<argc; o++) {
        printf(" %s", argv[o]);
        add2buf(argv[o]);
        buf[i-1]=32;
    }
    printf(" as %s\n", argv[3]);
    buf[i-1]=0;
    if (write(sock, buf, i) < 0) err("error writing command");
    i = read(sock, buf, 1);
    if (i < 1) err("error reading reply");

    struct pollfd fds[2];
    for (;;) {
        fds[0].fd = sock;
        fds[0].events = POLLIN;
        fds[0].revents = 0;
        fds[1].fd = STDIN_FILENO;
        fds[1].events = POLLIN;
        fds[1].revents = 0;
        if (poll(fds, 2, 100) < 0) err("error in poll");
        if ((fds[0].revents&POLLIN) != 0) {
            i = read(sock, buf, sizeof(buf));
            if (i < 1) err("error reading remote");
            if (write(STDOUT_FILENO, buf, i) < 0) err("error writing stdout");
        }
        if ((fds[1].revents&POLLIN) != 0) {
            i = read(STDIN_FILENO, buf, sizeof(buf));
            if (i < 1) err("error reading stdin");
            if (write(sock, buf, i) < 0) err("error writing remote");
        }
        i = fds[0].revents|fds[1].revents;
        if ((i & POLLERR) != 0) err("error on sockets");
        if ((i & POLLHUP) != 0) err("hangup on sockets");
    }
}
