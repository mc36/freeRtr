#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn.h"
#include "io_util.h"
#include "in_net.h"
#include "out_vu.h"


int main(int argc, char**argv) {
    if (argc <= 4) err("usage this <kind> <group> <source> <port>");
    rec_init(argv[1], argv[2], argv[3], argv[4]);
    ply_init();
    iou_loop();
    return 0;
}
