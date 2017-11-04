package clnt;

import addr.addrIP;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwd;
import ip.ipFwdIface;
import java.util.Comparator;
import rtr.rtrLdpNeigh;
import rtr.rtrLdpTrgtd;
import util.bits;
import util.logger;

/**
 * targeted ldp client
 *
 * @author matecsaba
 */
public class clntMplsTrg implements Runnable, Comparator<clntMplsTrg> {

    /**
     * target of tunnel
     */
    public addrIP target = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    private boolean working = true;

    /**
     * start connection
     */
    public void workStart() {
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        working = false;
    }

    public int compare(clntMplsTrg o1, clntMplsTrg o2) {
        return o1.target.compare(o1.target, o2.target);
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            bits.sleep(1000);
        }
    }

    private void workDoer() {
        ipFwd fwdCor = vrf.getFwd(target);
        ipFwdIface fwdIfc = srcIfc.getFwdIfc(target);
        if (fwdIfc == null) {
            return;
        }
        rtrLdpTrgtd neighT = fwdCor.ldpTargetFind(fwdIfc, target, true);
        if (neighT == null) {
            return;
        }
        if (neighT.tcp == null) {
            neighT.tcp = vrf.getTcp(target);
            neighT.udp = vrf.getUdp(target);
            neighT.workStart();
        }
        rtrLdpNeigh neighL;
        for (;;) {
            if (!working) {
                return;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, target, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(null, target, false);
            if (neighL != null) {
                break;
            }
            bits.sleep(1000);
        }
        for (;;) {
            if (!working) {
                return;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, target, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(null, target, false);
            if (neighL == null) {
                return;
            }
            bits.sleep(1000);
        }

    }

}
