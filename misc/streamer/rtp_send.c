#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn2.h"
#include "io_util.h"
#include "in_mpg.h"
#include "out_udp.h"
#include "out_rtp.h"


int main(int argc, char**argv) {
    if (argc <= 6) err("usage this <file> <seek> <vol> <group> <source> <port>");
    ply_init(argv[4], argv[5], argv[6]);
    rec_init(argv[1], argv[2], argv[3]);
    iou_loop();
    return 0;
}
