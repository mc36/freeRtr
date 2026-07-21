#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <alsa/asoundlib.h>

#include "io_util.h"
#include "in_rtp.h"
#include "out_rtp.h"


int main(int argc, char**argv) {
    rec_init(argv[1], argv[2], argv[3]);
    ply_init(argv[4], argv[5], argv[6]);
    iou_loop();
    return 0;
}
