package serv;

import cfg.cfgAll;
import cfg.cfgCheck;
import java.util.List;
import pack.packNrpe;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * nagios remote plugin server
 *
 * @author matecsaba
 */
public class servNrpe extends servGeneric implements prtServS {

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

    public void srvShRun(String beg, List<String> lst) {
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
        l.add("1 2  truncate                     truncate first line");
        l.add("2 .    <num>                      upper limit in characters");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servNrpeConn(this, pipe);
        return false;
    }

}

class servNrpeConn implements Runnable {

    private servNrpe lower;

    private pipeSide conn;

    public servNrpeConn(servNrpe parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        new Thread(this).start();
    }

    public void run() {
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
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                ntry.getReportNrpe(pck);
                int i = pck.str.length();
                if (i > lower.truncState) {
                    i = lower.truncState;
                }
                pck.str = pck.str.substring(0, i);
                pck.sendPack(conn);
                if (debugger.servNrpeTraf) {
                    logger.debug("tx " + pck.dump());
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();

    }

}
