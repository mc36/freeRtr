package clnt;

import addr.addrIP;
import cfg.cfgAll;
import pack.packHolder;
import pack.packNtp;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;

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
     */
    public void doWork() {
        addrIP serverAddr = userTerminal.justResolv(serverName, 0);
        if (serverAddr == null) {
            return;
        }
        pipeSide pipe = cfgAll.clntConnect(servGeneric.protoUdp, serverAddr, packNtp.port, "ntp");
        if (pipe == null) {
            return;
        }
        packHolder pckBin = new packHolder(true, true);
        packNtp pckNtp = new packNtp();
        pckNtp.ver = 3;
        pckNtp.mode = packNtp.modClnt;
        long tim1 = bits.getTime();
        pckNtp.refTime = packNtp.encode(tim1 + cfgAll.timeServerOffset);
        pckNtp.sendTime = packNtp.encode(tim1 + cfgAll.timeServerOffset);
        pckNtp.createPacket(pckBin);
        if (debugger.clntNtpTraf) {
            logger.debug("tx " + pckNtp);
        }
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
        pipe.timeout = 5000;
        pckBin = pipe.readPacket(true);
        pipe.setClose();
        if (pckBin == null) {
            return;
        }
        long tim2 = bits.getTime();
        pckNtp = new packNtp();
        if (pckNtp.parsePacket(pckBin)) {
            return;
        }
        tim1 = packNtp.decode(pckNtp.sendTime) + ((tim2 - tim1) / 2);
        tim2 = tim1 - tim2;
        if (debugger.clntNtpTraf) {
            logger.debug("rx " + pckNtp);
            logger.debug("offsets: old=" + cfgAll.timeServerOffset + " new=" + tim2 + " diff=" + (cfgAll.timeServerOffset - tim2));
        }
        tim1 = cfgAll.timeServerOffset - tim2;
        if (tim1 < 0) {
            tim1 = -tim1;
        }
        if (tim1 > 1000) {
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
                doWork();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        if (debugger.clntNtpTraf) {
            logger.debug("stopped");
        }
    }

}
