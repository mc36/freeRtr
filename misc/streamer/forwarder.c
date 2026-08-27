#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn.h"
#include "io_util.h"
#include "in_net.h"
#include "out_net.h"


int main(int argc, char**argv) {
    if (argc <= 8) err("usage this <kind> <group> <source> <port> <kind> <group> <source> <port>");
    rec_init(argv[1], argv[2], argv[3], argv[4]);
    ply_init(argv[5], argv[6], argv[7], argv[8]);
    iou_loop();
    return 0;
}
