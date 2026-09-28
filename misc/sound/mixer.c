#include "io_incl.h"
#include "io_cnst.h"
#include "io_tim0.h"
#include "io_util.h"
#include "in_net.h"
#include "out_net.h"
#include "mixer.h"


int main(int argc, char**argv) {
    if (argc <= 11) err("usage: java this  <bufs> <thrs> <kind>  <group> <source> <port> <vol>  <group> <source> <port> <volL> <volR>   <group> <source> <port> <volL> <volR>  ...");
    mixDly = atoi(argv[1]);
    mixThr = atoi(argv[2]);
    ply_init(argv[3], argv[4], argv[5], argv[6]);
    mixSrc = (argc - 8) / 5;
    mixVolO = atoi(argv[7]);
    vol2rng(&mixVolO, 0);
    mixSel = -1;
    for (int i = mixSrc - 1 ; i >= 0 ; i--) {
        int p = (i * 5) + 8;
        rec_init(argv[3], argv[p + 0], argv[p + 1], argv[p + 2]);
        mixIni(&mixChn[i], atoi(argv[p + 3]), atoi(argv[p + 4]));
        rec_blk(i == 0);
    }
    iou_loop();
    return 0;
}
