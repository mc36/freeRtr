#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn0.h"
#include "io_util.h"
#include "in_dev.h"
#include "out_udp.h"
#include "out_scr.h"


int main(int argc, char**argv) {
    if (argc <= 4) err("usage this <device> <group> <source> <port>");
    ply_init(argv[2], argv[3], argv[4]);
    rec_init(argv[1]);
    iou_loop();
    return 0;
}
