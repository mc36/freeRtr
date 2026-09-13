#include "io_incl.h"
#include "io_cnst.h"
#include "io_tim0.h"
#include "io_util.h"
#include "in_net.h"
#include "out_net.h"
#include "mixer.h"


int main(int argc, char**argv) {
    if (argc <= 11) err("usage: java this  <bufs> <kind>  <group> <source> <port> <vol>  <group> <source> <port> <volL> <volR>   <group> <source> <port> <volL> <volR>  ...");
    mixDly = atoi(argv[1]);
    ply_init(argv[2], argv[3], argv[4], argv[5]);
    mixSrc = (argc - 6) / 5;
    mixVolO = mixSrc * mixDly;
    mixBuf = malloc(sizeof(int*) * mixVolO);
    if (mixBuf == NULL) err("error allocating");
    for (int i = 0; i < mixVolO; i++) {
        mixBuf[i] = malloc(sizeof(int) * mixLen);
        if (mixBuf[i] == NULL) err("error allocating");
    }
    mixVolO = vol2rng(atoi(argv[6]), 0);
    mixSel = -1;
    for (int i = mixSrc - 1 ; i >= 0 ; i--) {
        int p = (i * 5) + 7;
        rec_init(argv[2], argv[p + 0], argv[p + 1], argv[p + 2]);
        mixVolL[i] = vol2rng(atoi(argv[p + 3]), 0);
        mixVolR[i] = vol2rng(atoi(argv[p + 4]), 0);
        mixHnd[i] = recHnd;
        rec_blk(i == 0);
    }
    iou_loop();
    return 0;
}
