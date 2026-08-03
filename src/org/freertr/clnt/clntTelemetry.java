package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgSensor;
import org.freertr.cfg.cfgTime;
import org.freertr.pack.packHolder;
import org.freertr.pack.packStreamingMdt;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servStreamingMdt;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;

/**
 * telemetry sender
 *
 * @author matecsaba
 */
public class clntTelemetry implements Runnable {

    /**
     * create instance
     */
    public clntTelemetry() {
    }

    /**
     * target
     */
    public String target;

    /**
     * interval
     */
    public int interval = 5000;

    /**
     * initial delay
     */
    public int initial = 0;

    /**
     * random time between runs
     */
    public int randInt;

    /**
     * random initial delay
     */
    public int randIni;

    /**
     * time range when allowed
     */
    public cfgTime time;

    /**
     * sensors
     */
    public tabGen<cfgSensor> sensors = new tabGen<cfgSensor>();

    /**
     * port
     */
    public int port = servStreamingMdt.port;

    /**
     * proxy
     */
    public clntProxy proxy;

    /**
     * running
     */
    public boolean need2run;

    /**
     * counter
     */
    public counter cntr = new counter();

    private pipeSide pipe;

    /**
     * stop working
     */
    public void stopWork() {
        if (!need2run) {
            return;
        }
        need2run = false;
        if (pipe != null) {
            pipe.setClose();
        }
        pipe = null;
    }

    /**
     * stop working
     */
    public void startWork() {
        if (need2run) {
            return;
        }
        need2run = true;
        logger.startThread(this);
    }

    private void doWork() {
        if (pipe != null) {
            pipe.setClose();
        }
        int del = initial;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        if (del > 0) {
            bits.sleep(del);
        }
        pipe = null;
        if (proxy == null) {
            return;
        }
        if (target == null) {
            return;
        }
        addrIP trg = clntDns.justResolv(target, proxy.prefer);
        if (trg == null) {
            return;
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, trg, port, "telemetry");
        if (pipe == null) {
            return;
        }
        pipe.setTime(120000);
        for (;;) {
            if (!need2run) {
                break;
            }
            del = interval;
            if (randInt > 0) {
                del += bits.random(1, randInt);
            }
            bits.sleep(del);
            if (time != null) {
                if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                    continue;
                }
            }
            if (pipe.isClosed() != 0) {
                break;
            }
            for (int i = 0; i < sensors.size(); i++) {
                cfgSensor ntry = sensors.get(i);
                packHolder pck = ntry.getReportKvGpb();
                if (pck == null) {
                    logger.warn("sensor " + ntry.name + " returned nothing");
                    continue;
                }
                sendReport(pck);
            }
        }
    }

    public void run() {
        try {
            for (;;) {
                if (!need2run) {
                    break;
                }
                doWork();
                bits.sleep(1000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    /**
     * send one report
     *
     * @param pck packet, header prepended
     */
    public void sendReport(packHolder pck) {
        if (pipe == null) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (pipe.isClosed() != 0) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        cntr.tx(pck);
        packStreamingMdt pckPb = new packStreamingMdt(pipe, pck);
        pckPb.typ = 1;
        pckPb.encap = 1;
        pckPb.vers = 1;
        pckPb.flags = 0;
        pckPb.sendPack();
    }

}
