#include "io_incl.h"
#include "io_cnst.h"
#include "io_tim0.h"
#include "io_util.h"
#include "io_chn0.h"
#include "in_raw.h"
#include "out_dev.h"


int main(int argc, char**argv) {
    if (argc <= 2) err("usage this <device> <file>");
    ply_init(argv[1]);
    rec_init(argv[2]);
    iou_loop();
    return 0;
}
