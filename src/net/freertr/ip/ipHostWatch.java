package net.freertr.ip;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgScrpt;
import net.freertr.tab.tabGen;
import net.freertr.util.bits;
import net.freertr.util.logger;

/**
 * host watch
 *
 * @author matecsaba
 */
public class ipHostWatch implements Runnable {

    private final ipIfc ifc;

    private boolean working = true;

    private tabGen<ipHostWatchEntry> hosts = new tabGen<ipHostWatchEntry>();

    /**
     * script to run on node appearance
     */
    public cfgScrpt nodeOn;

    /**
     * script to run on node change
     */
    public cfgScrpt nodeChg;

    /**
     * script to run on node disappearance
     */
    public cfgScrpt nodeOff;

    /**
     * create new interface
     *
     * @param iface interface to work on
     */
    public ipHostWatch(ipIfc iface) {
        ifc = iface;
        new Thread(this).start();
    }

    public String toString() {
        String a = "";
        if (nodeOn != null) {
            a += " appear " + nodeOn.name;
        }
        if (nodeChg != null) {
            a += " change " + nodeChg.name;
        }
        if (nodeOff != null) {
            a += " disappear " + nodeOff.name;
        }
        return a.trim();
    }

    /**
     * stop work
     */
    public void stopWork() {
        working = false;
    }

    public void run() {
        for (;;) {
            bits.sleep(60000);
            if (!working) {
                break;
            }
            try {
                doWork();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

    private void doWork() {
        tabGen<ipHostWatchEntry> fresh = new tabGen<ipHostWatchEntry>();
        for (int i = 0;; i++) {
            ipHostWatchEntry cur = new ipHostWatchEntry();
            if (ifc.getL2info(i, cur.ip, cur.mac)) {
                break;
            }
            fresh.add(cur);
        }
        for (int i = 0; i < fresh.size(); i++) {
            ipHostWatchEntry cur = fresh.get(i);
            ipHostWatchEntry old = hosts.find(cur);
            if (old == null) {
                logger.info("new host appeared " + cur);
                if (nodeOn == null) {
                    continue;
                }
                nodeOn.doRound(bits.str2lst("set remote " + cur.ip));
                continue;
            }
            if (cur.mac.compare(cur.mac, old.mac) != 0) {
                logger.info("host changed from " + old + " to " + cur);
                if (nodeChg == null) {
                    continue;
                }
                nodeChg.doRound(bits.str2lst("set remote " + cur.ip));
                continue;
            }
        }
        for (int i = 0; i < hosts.size(); i++) {
            ipHostWatchEntry old = hosts.get(i);
            if (fresh.find(old) == null) {
                logger.info("host disappeared " + old);
                if (nodeOff == null) {
                    continue;
                }
                nodeOff.doRound(bits.str2lst("set remote " + old.ip));
                continue;
            }
        }
        hosts = fresh;
    }

}

class ipHostWatchEntry implements Comparator<ipHostWatchEntry> {

    public addrIP ip = new addrIP();

    public addrMac mac = new addrMac();

    public int compare(ipHostWatchEntry o1, ipHostWatchEntry o2) {
        return ip.compare(o1.ip, o2.ip);
    }

    public String toString() {
        return ip + " " + mac;
    }

}
