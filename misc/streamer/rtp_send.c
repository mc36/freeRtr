#include "io_incl.h"
#include "io_util.h"
#include "in_mpg.h"
#include "out_udp.h"
#include "out_rtp.h"


int main(int argc, char**argv) {
    if (argc <= 5) err("usage this <file> <seek> <group> <source> <port>");
    ply_init(argv[3], argv[4], argv[5]);
    rec_init(argv[1], argv[2]);
    iou_loop();
    return 0;
}
