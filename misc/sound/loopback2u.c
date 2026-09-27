#include "io_incl.h"
#include "io_cnst.h"
#include "io_bit2u.h"
#include "io_tim0.h"
#include "io_util.h"
#include "io_chn0.h"
#include "in_dev.h"
#include "out_dev.h"


int main(int argc, char**argv) {
    if (argc <= 2) err("usage this <device> <device>");
    ply_init(argv[2]);
    rec_init(argv[1], "1.0");
    iou_loop();
    return 0;
}
