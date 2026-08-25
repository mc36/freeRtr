#include "io_incl.h"
#include "io_cnst.h"
#include "io_chn.h"
#include "io_util.h"
#include "in_udp.h"
#include "in_vba.h"
#include "out_udp.h"
#include "out_wfa.h"


int main(int argc, char**argv) {
    if (argc <= 6) err("usage this <group> <source> <port> <group> <source> <port>");
    rec_init(argv[1], argv[2], argv[3]);
    ply_init(argv[4], argv[5], argv[6]);
    iou_loop();
    return 0;
}
