#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn2.h"
#include "io_util.h"
#include "in_dev.h"
#include "out_vu.h"


int main(int argc, char**argv) {
    if (argc <= 1) err("usage this <device>");
    rec_init(argv[1]);
    ply_init();
    iou_loop();
    return 0;
}
