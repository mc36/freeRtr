package net.freertr.clnt;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.rtr.rtrLdpIface;
import net.freertr.rtr.rtrLdpNeigh;
import net.freertr.rtr.rtrLdpTrgtd;
import net.freertr.util.bits;
import net.freertr.util.logger;

/**
 * targeted ldp client
 *
 * @author matecsaba
 */
public class clntMplsTrg implements Runnable, Comparator<clntMplsTrg> {

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
            neighT = fwdCor.ldpTargetFind(fwdIfc, ldpIfc, target, false);
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
