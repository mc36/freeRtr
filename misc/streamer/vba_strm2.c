#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn2.h"
#include "io_util.h"
#include "in_dev.h"
#include "out_udp.h"
#include "out_vba.h"


int main(int argc, char**argv) {
    if (argc <= 5) err("usage this <device> <volume> <group> <source> <port>");
    ply_init(argv[3], argv[4], argv[5]);
    rec_init(argv[1], argv[2]);
    iou_loop();
    return 0;
}
