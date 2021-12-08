package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdEcho;
import net.freertr.ip.ipFwdEchod;
import net.freertr.ip.ipFwdIface;
import net.freertr.tab.tabAverage;
import net.freertr.util.logger;

/**
 * ping measurements
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
        new Thread(this).start();
    }

    public void run() {
        try {
            ipFwdEcho ping = fwd.echoSendReq(src.addr, trg, size, tim2liv, typOsrv, flowLab, datPat, false);
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
