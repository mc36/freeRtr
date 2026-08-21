#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn2.h"
#include "io_util.h"
#include "in_mpg.h"
#include "out_raw.h"


int main(int argc, char**argv) {
    if (argc <= 4) err("usage this <file> <seek> <vol> <device>");
    ply_init(argv[4]);
    rec_init(argv[1], argv[2], argv[3]);
    iou_loop();
    return 0;
}
