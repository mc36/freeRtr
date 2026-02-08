package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdEcho;
import org.freertr.ip.ipFwdEchod;
import org.freertr.ip.ipFwdIface;
import org.freertr.tab.tabAverage;
import org.freertr.util.logger;

/**
 * ping client
 *
 * @author matecsaba
 */
public class clntPing implements Runnable {

    /**
     * create instance
     */
    public clntPing() {
    }

    /**
     * measurement
     */
    public tabAverage meas;

    /**
     * forwarder
     */
    public ipFwd fwd;

    /**
     * source
     */
    public ipFwdIface src;

    /**
     * target
     */
    public addrIP trg;

    /**
     * mpls mode
     */
    public int mpls = 0;

    /**
     * nexthop
     */
    public addrIP hop = null;

    /**
     * timeout
     */
    public int timeout = 1000;

    /**
     * size
     */
    public int size = 64;

    /**
     * ttl
     */
    public int tim2liv = 255;

    /**
     * sgt
     */
    public int secGrp = 0;

    /**
     * tos
     */
    public int typOsrv = 0;

    /**
     * flow label
     */
    public int flowLab = 0;

    /**
     * data pattern
     */
    public int datPat = 0;

    /**
     * do work
     */
    public void doWork() {
        logger.startThread(this);
    }

    public void run() {
        try {
            ipFwdEcho ping = fwd.echoSendReq(src.addr, trg, mpls, hop, size, false, -1, tim2liv, secGrp, typOsrv, flowLab, datPat, false);
            if (ping == null) {
                return;
            }
            if (ping.notif.totalNotifies() < 1) {
                ping.notif.sleep(timeout);
            }
            if (ping.notif.totalNotifies() < 1) {
                return;
            }
            if (ping.res.size() < 1) {
                return;
            }
            int tim = -1;
            for (int o = 0; o < ping.res.size(); o++) {
                ipFwdEchod res = ping.res.get(o);
                if (res.err != null) {
                    continue;
                }
                tim = res.tim;
                break;
            }
            if (tim < 0) {
                return;
            }
            meas.addValue(tim);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
