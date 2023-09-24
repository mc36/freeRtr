package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgCheck;
import net.freertr.pack.packNrpe;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * nagios remote plugin server
 *
 * @author matecsaba
 */
public class servNrpe extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servNrpe() {
    }

    /**
     * truncate first line
     */
    public int truncState = 12288;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server nrpe .*! port " + packNrpe.portNum,
        "server nrpe .*! protocol " + proto2string(protoAllStrm),
        "server nrpe .*! truncate 12288",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "nrpe";
    }

    public int srvPort() {
        return packNrpe.portNum;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        lst.add(beg + "truncate " + truncState);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = s.equals("no");
        if (negated) {
            s = cmd.word();
        }
        if (s.equals("truncate")) {
            truncState = bits.str2num(cmd.word());
            if (negated) {
                truncState = 0;
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  truncate                     truncate first line");
        l.add(null, "2 .    <num>                      upper limit in characters");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servNrpeConn(this, pipe, id.peerAddr);
        return false;
    }

}

class servNrpeConn implements Runnable {

    private final servNrpe lower;

    private final pipeSide conn;

    private final addrIP peer;

    public servNrpeConn(servNrpe parent, pipeSide pipe, addrIP rem) {
        lower = parent;
        conn = pipe;
        peer = rem;
        new Thread(this).start();
    }

    public void run() {
        int done = 0;
        try {
            for (;;) {
                packNrpe pck = new packNrpe();
                if (pck.recvPack(conn)) {
                    break;
                }
                if (debugger.servNrpeTraf) {
                    logger.debug("rx " + pck.dump());
                }
                if (pck.typ != packNrpe.tyReq) {
                    pck.typ = packNrpe.tyRep;
                    pck.cod = packNrpe.coUnk;
                    pck.str = "UNKNOWN invalid packet type";
                    logger.error(peer + " " + pck.str);
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                cfgCheck ntry = cfgAll.checkFind(pck.str.replaceAll("!", "-"), false);
                if (ntry == null) {
                    pck.typ = packNrpe.tyRep;
                    pck.cod = packNrpe.coUnk;
                    pck.str = "UNKNOWN no such check";
                    logger.error(peer + " " + pck.str);
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                done++;
                ntry.getReportNrpe(pck);
                if (pck.str.length() > lower.truncState) {
                    pck.str = pck.str.substring(0, lower.truncState);
                }
                pck.sendPack(conn);
                if (debugger.servNrpeTraf) {
                    logger.debug("tx " + pck.dump());
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            if (done < 1) {
                packNrpe pck = new packNrpe();
                pck.typ = packNrpe.tyRep;
                pck.cod = packNrpe.coUnk;
                pck.str = "UNKNOWN nothing asked";
                logger.error(peer + " " + pck.str);
                pck.sendPack(conn);
                if (debugger.servNrpeTraf) {
                    logger.debug("tx " + pck.dump());
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            conn.setClose();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
