package ip;

import java.util.Timer;
import java.util.TimerTask;
import util.bits;
import util.logger;

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

    private Timer keepTimer;

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
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (shutdown) {
            return;
        }
        if (queryInterval < 1) {
            return;
        }
        keepTimer = new Timer();
        ipMhostIfaceQuery task = new ipMhostIfaceQuery(this);
        keepTimer.schedule(task, 500, queryInterval);
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

class ipMhostIfaceQuery extends TimerTask {

    private final ipMhostIface lower;

    public ipMhostIfaceQuery(ipMhostIface parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendQuery();
            lower.sendJoins();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
