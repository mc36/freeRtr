package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgScrpt;
import org.freertr.tab.tabGen;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.logger;

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
                cur.time = bits.getTime();
                logger.info("new host appeared " + cur + " on " + ifc);
                if (nodeOn == null) {
                    continue;
                }
                nodeOn.doRound(bits.str2lst("set remote " + cur.ip));
                continue;
            }
            cur.time = old.time;
            if (cur.mac.compareTo(old.mac) != 0) {
                logger.info("host changed from " + old + " to " + cur + " on " + ifc);
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
                logger.info("host disappeared " + old + " on " + ifc);
                if (nodeOff == null) {
                    continue;
                }
                nodeOff.doRound(bits.str2lst("set remote " + old.ip));
                continue;
            }
        }
        hosts = fresh;
    }

    /**
     * get show
     *
     * @return output
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "ip|mac|since|for");
        for (int i = 0; i < hosts.size(); i++) {
            ipHostWatchEntry ntry = hosts.get(i);
            res.add(ntry.ip + "|" + ntry.mac + "|" + bits.time2str(cfgAll.timeZoneName, ntry.time + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(ntry.time));
        }
        return res;
    }

}

class ipHostWatchEntry implements Comparable<ipHostWatchEntry> {

    public addrIP ip = new addrIP();

    public addrMac mac = new addrMac();

    public long time;

    public int compareTo(ipHostWatchEntry o) {
        return ip.compareTo(o.ip);
    }

    public String toString() {
        return ip + " " + mac;
    }

}
