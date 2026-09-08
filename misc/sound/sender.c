#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn.h"
#include "io_util.h"
#include "in_mpg.h"
#include "out_net.h"


int main(int argc, char**argv) {
    if (argc <= 7) err("usage this <file> <seek> <vol> <kind> <group> <source> <port>");
    ply_init(argv[4], argv[5], argv[6], argv[7]);
    rec_init(argv[1], argv[2], argv[3]);
    iou_loop();
    return 0;
}
