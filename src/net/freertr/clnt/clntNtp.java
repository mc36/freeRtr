package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packHolder;
import net.freertr.pack.packNtp;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * network time protocol (rfc5905) client
 *
 * @author matecsaba
 */
public class clntNtp implements Runnable {

    /**
     * name of ntp server
     */
    public String serverName;

    /**
     * got time
     */
    public long tim1;

    /**
     * got diff
     */
    public long tim2;

    /**
     * new diff
     */
    public long tim3;

    private boolean need2run;

    /**
     * create new client
     *
     * @param srv server name
     */
    public clntNtp(String srv) {
        serverName = srv;
    }

    public String toString() {
        return serverName;
    }

    /**
     * start work
     */
    public void startWork() {
        need2run = true;
        new Thread(this).start();
    }

    /**
     * stop work
     */
    public void stopWork() {
        need2run = false;
    }

    /**
     * do one poll
     *
     * @return false on success, true on error
     */
    public boolean doWork() {
        addrIP serverAddr = userTerminal.justResolv(serverName, 0);
        if (serverAddr == null) {
            return true;
        }
        clntProxy prx = cfgAll.getClntPrx(cfgAll.timeProxy);
        if (prx == null) {
            return true;
        }
        pipeSide pipe = prx.doConnect(servGeneric.protoUdp, serverAddr, packNtp.port, "ntp");
        if (pipe == null) {
            return true;
        }
        packHolder pckBin = new packHolder(true, true);
        packNtp pckNtp = new packNtp();
        pckNtp.ver = 3;
        pckNtp.mode = packNtp.modClnt;
        tim1 = bits.getTime();
        pckNtp.refTime = packNtp.encode(tim1 + cfgAll.timeServerOffset);
        pckNtp.sendTime = packNtp.encode(tim1 + cfgAll.timeServerOffset);
        pckNtp.createPacket(pckBin);
        if (debugger.clntNtpTraf) {
            logger.debug("tx " + pckNtp);
        }
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
        pipe.setTime(5000);
        pckBin = pipe.readPacket(true);
        pipe.setClose();
        if (pckBin == null) {
            return true;
        }
        tim2 = bits.getTime();
        pckNtp = new packNtp();
        if (pckNtp.parsePacket(pckBin)) {
            return true;
        }
        tim1 = packNtp.decode(pckNtp.sendTime) + ((tim2 - tim1) / 2);
        tim2 = tim1 - tim2;
        tim3 = cfgAll.timeServerOffset - tim2;
        if (debugger.clntNtpTraf) {
            logger.debug("rx " + pckNtp);
            logger.debug("offsets: old=" + cfgAll.timeServerOffset + " new=" + tim2 + " diff=" + tim3);
        }
        if (tim3 < 0) {
            tim3 = -tim3;
        }
        return false;
    }

    /**
     * do one sync
     */
    public void doSync() {
        if (tim3 > 1000) {
            logger.info("setting clock to " + bits.time2str(cfgAll.timeZoneName, bits.getTime() + tim2, 3));
        }
        cfgAll.timeServerOffset = tim2;
    }

    public void run() {
        if (debugger.clntNtpTraf) {
            logger.debug("started");
        }
        for (;;) {
            bits.sleep(60000);
            if (!need2run) {
                break;
            }
            try {
                if (doWork()) {
                    continue;
                }
                doSync();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        if (debugger.clntNtpTraf) {
            logger.debug("stopped");
        }
    }

}
