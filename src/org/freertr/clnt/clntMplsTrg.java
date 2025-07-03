package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.rtr.rtrLdpIface;
import org.freertr.rtr.rtrLdpNeigh;
import org.freertr.rtr.rtrLdpTrgtd;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * targeted ldp client
 *
 * @author matecsaba
 */
public class clntMplsTrg implements Runnable, Comparable<clntMplsTrg> {

    /**
     * create instance
     */
    public clntMplsTrg() {
    }

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

    /**
     * ldp interface
     */
    public rtrLdpIface ldpIfc = null;

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

    public int compareTo(clntMplsTrg o) {
        return target.compareTo(o.target);
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
        rtrLdpTrgtd neighT = fwdCor.ldpTargetFind(fwdIfc, ldpIfc, target, true);
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
            neighT = fwdCor.ldpTargetFind(fwdIfc, ldpIfc, target, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(target, false);
            if (neighL != null) {
                break;
            }
            bits.sleep(1000);
        }
        for (;;) {
            if (!working) {
                return;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, ldpIfc, target, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(target, false);
            if (neighL == null) {
                return;
            }
            bits.sleep(1000);
        }
    }

}
