#include "io_incl.h"
#include "io_util.h"
#include "in_udp.h"
#include "in_rtp.h"
#include "out_vu.h"


int main(int argc, char**argv) {
    if (argc <= 3) err("usage this <group> <source> <port>");
    rec_init(argv[1], argv[2], argv[3]);
    ply_init();
    iou_loop();
    return 0;
}
