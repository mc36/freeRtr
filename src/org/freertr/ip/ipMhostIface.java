package org.freertr.ip;

import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * multicast host (igmp/mld) interface
 *
 * @author matecsaba
 */
public class ipMhostIface {

    /**
     * query interval
     */
    public int queryInterval = 60000;

    /**
     * time to wait between packets
     */
    public int interPackTime = 20;

    /**
     * send join groups
     */
    public boolean sendJoins = false;

    private ipFwd fwdCore;

    /**
     * interface
     */
    protected ipFwdIface iface;

    /**
     * keepalive
     */
    protected ipMhostIfaceQuery keepTimer;

    /**
     * create new instance
     *
     * @param fwd forwarder
     * @param ifc interface
     */
    public ipMhostIface(ipFwd fwd, ipFwdIface ifc) {
        fwdCore = fwd;
        iface = ifc;
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        keepTimer = null;
        if (shutdown) {
            return;
        }
        if (queryInterval < 1) {
            return;
        }
        keepTimer = new ipMhostIfaceQuery(this);
        keepTimer.start();
    }

    /**
     * send generic query
     */
    protected void sendQuery() {
        fwdCore.mhostCore.sendQuery(iface, queryInterval / 1000, null, null);
    }

    /**
     * send one join
     *
     * @param grp group to join
     * @param need 1=join, 0=prune
     */
    public void sendJoin(ipFwdMcast grp, boolean need) {
        fwdCore.mhostCore.sendReport(iface, grp.group, grp.source, need);
    }

    /**
     * send joins
     */
    protected void sendJoins() {
        if (!sendJoins) {
            return;
        }
        for (int i = 0; i < fwdCore.groups.size(); i++) {
            ipFwdMcast grp = fwdCore.groups.get(i);
            if (grp.iface == null) {
                continue;
            }
            if (grp.iface.ifwNum != iface.ifwNum) {
                continue;
            }
            bits.sleep(interPackTime);
            sendJoin(grp, true);
        }
    }

}

class ipMhostIfaceQuery implements Runnable {

    private final ipMhostIface lower;

    public ipMhostIfaceQuery(ipMhostIface parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.sendQuery();
                lower.sendJoins();
                bits.sleep(lower.queryInterval);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
