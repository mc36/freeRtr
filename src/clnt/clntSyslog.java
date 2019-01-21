package clnt;

import pipe.pipeSide;
import serv.servSyslog;
import addr.addrIP;
import cfg.cfgAll;
import serv.servGeneric;
import util.logger;

/**
 * syslog protocol (rfc3164) client
 *
 * @author matecsaba
 */
public class clntSyslog {

    private addrIP addr;

    private int fac;

    private pipeSide pipe;

    private boolean running;

    /**
     * level to severity
     *
     * @param l level
     * @return severity
     */
    public static final int level2severity(logger.logLev l) {
        switch (l) {
            case msgDebg:
                return 7;
            case msgInfo:
                return 6;
            case msgWarn:
                return 4;
            case msgEror:
                return 3;
            case msgExcp:
                return 0;
        }
        return 1;
    }

    /**
     * create logger
     *
     * @param adr address where to log
     * @param fc facility to use
     */
    public clntSyslog(addrIP adr, int fc) {
        addr = adr;
        fac = fc;
        running = addr != null;
    }

    public String toString() {
        return "" + addr;
    }

    /**
     * stop logging
     */
    public void logStop() {
        running = false;
        if (pipe != null) {
            pipe.setClose();
        }
    }

    /**
     * log one message
     *
     * @param l level
     * @param s message to log
     */
    public synchronized void logMsg(logger.logLev l, String s) {
        if (!running) {
            return;
        }
        boolean b = pipe == null;
        if (!b) {
            b = pipe.isClosed() != 0;
        }
        if (b) {
            pipe = cfgAll.clntConnect(servGeneric.protoUdp, addr, new servSyslog().srvPort(), "syslog");
        }
        if (pipe == null) {
            return;
        }
        pipe.timeout = 120000;
        if (pipe.ready2tx() < 1024) {
            return;
        }
        pipe.strPut("<" + ((fac << 3) + level2severity(l)) + ">" + cfgAll.hostName + " " + s);
    }

}
