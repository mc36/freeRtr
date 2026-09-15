#include "io_incl.h"
#include "io_cnst.h"
#include "io_tim0.h"
#include "io_util.h"
#include "in_net.h"
#include "out_net.h"
#include "mixer.h"


int main(int argc, char**argv) {
    if (argc <= 11) err("usage: java this  <bufs> <thrs> <kind>  <group> <source> <port> <vol>  <group> <source> <port> <volL> <volR>   <group> <source> <port> <volL> <volR>  ...");
    memset(mixPosR, 0, sizeof(mixPosR));
    memset(mixPosW, 0, sizeof(mixPosW));
    memset(mixStp, 1, sizeof(mixStp));
    mixDly = atoi(argv[1]);
    mixThr = atoi(argv[2]);
    ply_init(argv[3], argv[4], argv[5], argv[6]);
    mixSrc = (argc - 8) / 5;
    mixVolO = mixSrc * mixDly;
    mixBuf = malloc(sizeof(int*) * mixVolO);
    if (mixBuf == NULL) err("error allocating");
    mixSel = sizeof(int) * (pktln / smpbt);
    for (int i = 0; i < mixVolO; i++) {
        mixBuf[i] = malloc(mixSel);
        if (mixBuf[i] == NULL) err("error allocating");
        memset(mixBuf[i], 0, mixSel);
    }
    mixVolO = vol2rng(atoi(argv[7]), 0);
    mixSel = -1;
    for (int i = mixSrc - 1 ; i >= 0 ; i--) {
        int p = (i * 5) + 8;
        rec_init(argv[3], argv[p + 0], argv[p + 1], argv[p + 2]);
        mixVolL[i] = vol2rng(atoi(argv[p + 3]), 0);
        mixVolR[i] = vol2rng(atoi(argv[p + 4]), 0);
        mixHnd[i] = recHnd;
        rec_blk(i == 0);
    }
    iou_loop();
    return 0;
}
