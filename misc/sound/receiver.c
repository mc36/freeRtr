#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn.h"
#include "io_util.h"
#include "in_net.h"
#include "out_dev.h"


int main(int argc, char**argv) {
    if (argc <= 5) err("usage this <device> <kind> <group> <source> <port>");
    rec_init(argv[2], argv[3], argv[4], argv[5]);
    ply_init(argv[1]);
    iou_loop();
    return 0;
}
