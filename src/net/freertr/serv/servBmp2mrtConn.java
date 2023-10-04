package net.freertr.serv;

import net.freertr.addr.addrIP;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.rtr.rtrBgpMon;
import net.freertr.util.logger;

/**
 * bmp server connection
 *
 * @author matecsaba
 */
public class servBmp2mrtConn implements Runnable {

    private final pipeSide pipe;

    private final servBmp2mrt lower;

    private final addrIP peer;

    /**
     * create instance
     *
     * @param pip pipe to use
     * @param prnt parent to use
     * @param id connection to use
     */
    public servBmp2mrtConn(pipeSide pip, servBmp2mrt prnt, prtGenConn id) {
        pipe = pip;
        lower = prnt;
        peer = id.peerAddr.copyBytes();
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void doer() {
        logger.warn("neighbor " + peer + " up");
        packHolder pck = new packHolder(true, true);
        addrIP adr = new addrIP();
        for (;;) {
            pck.clear();
            if (pck.pipeRecv(pipe, 0, servBmp2mrt.size, 144) != servBmp2mrt.size) {
                break;
            }
            if (pck.getByte(0) != 3) { // version
                break;
            }
            int len = pck.msbGetD(1) - servBmp2mrt.size; // length
            int typ = pck.getByte(5); // type
            if (len < 1) {
                continue;
            }
            pck.clear();
            if (pck.pipeRecv(pipe, 0, len, 144) != len) {
                break;
            }
            // per = pck.getByte(0); // peer type
            int flg = pck.getByte(1); // flags
            // int rd = pck.msbGetQ(2); // distinguisher
            pck.getAddr(adr, 10);
            int as = pck.msbGetD(26); // asnum
            // pck.getAddr(rtr, 30); // rtrid
            // int tim = pck.msbGetD(34); // seconds
            // int tim += pck.msbGetD(38) / 1000; // microsecs
            if ((flg & 0x80) == 0) {
                adr.fromIPv4addr(adr.toIPv4());
            }
            for (int i = 0; i < lower.relays.size(); i++) {
                lower.relays.get(i).gotMessage(as, peer, adr, typ, pck);
            }
            pck.getSkip(rtrBgpMon.size - servBmp2mrt.size);
            boolean dir = (flg & 0x10) != 0;
            switch (typ) {
                case rtrBgpMon.typMon:
                    lower.gotMessage(as, adr, peer, dir, pck.getCopy());
                    break;
                case rtrBgpMon.typPerUp:
                    pck.getSkip(20);
                    lower.gotMessage(as, adr, peer, dir, pck.getCopy());
                    lower.gotState(as, adr, peer, true);
                    break;
                case rtrBgpMon.typPerDn:
                    pck.getSkip(1);
                    lower.gotMessage(as, adr, peer, dir, pck.getCopy());
                    lower.gotState(as, adr, peer, false);
                    break;
                case rtrBgpMon.typStat:
                    pck.getSkip(4);
                    lower.gotCounts(as, adr, peer, pck);
                    break;
                default:
                    break;
            }
        }
        lower.gotState(peer, false);
        logger.error("neighbor " + peer + " down");
    }

}
