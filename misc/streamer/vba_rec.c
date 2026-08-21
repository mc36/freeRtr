#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn2.h"
#include "io_util.h"
#include "in_udp.h"
#include "in_vba.h"
#include "out_raw.h"


int main(int argc, char**argv) {
    if (argc <= 4) err("usage this <file> <group> <source> <port>");
    rec_init(argv[2], argv[3], argv[4]);
    ply_init(argv[1]);
    iou_loop();
    return 0;
}
