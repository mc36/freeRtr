#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn.h"
#include "io_util.h"
#include "in_dev.h"
#include "out_net.h"


int main(int argc, char**argv) {
    if (argc <= 5) err("usage this <device> <kind> <group> <source> <port>");
    ply_init(argv[2], argv[3], argv[4], argv[5]);
    rec_init(argv[1], "1.0");
    iou_loop();
    return 0;
}
