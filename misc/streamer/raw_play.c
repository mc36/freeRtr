#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn2.h"
#include "io_util.h"
#include "in_raw.h"
#include "out_dev.h"


int main(int argc, char**argv) {
    if (argc <= 2) err("usage this <file> <device>");
    ply_init(argv[1]);
    rec_init(argv[2]);
    iou_loop();
    return 0;
}
