package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgCheck;
import org.freertr.pack.packNrpe;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

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

    private int cntrPrt;

    private int cntrOk;

    private int cntrCri;

    private int cntrUnk;

    private int cntrWrn;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server nrpe .*", cmds.tabulator + "port " + packNrpe.portNum, null),
        new userFilter("server nrpe .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server nrpe .*", cmds.tabulator + "truncate 12288", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    /**
     * update counters
     *
     * @param cod status code
     */
    protected void updateCntrs(int cod) {
        switch (cod) {
            case packNrpe.coCri:
                cntrCri++;
                break;
            case packNrpe.coOk:
                cntrOk++;
                break;
            case packNrpe.coUnk:
                cntrUnk++;
                break;
            case packNrpe.coWar:
                cntrWrn++;
                break;
            default:
                cntrPrt++;
                break;
        }
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "reply|times");
        res.add("ok|" + cntrOk);
        res.add("critical|" + cntrCri);
        res.add("unknown|" + cntrUnk);
        res.add("warning|" + cntrWrn);
        res.add("protocol|" + cntrPrt);
        return res;
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
        boolean negated = s.equals(cmds.negated);
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

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "truncate", "truncate first line");
        l.add(null, false, 2, new int[]{-1}, "<num>", "upper limit in characters");
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
                    lower.updateCntrs(-1);
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
                    lower.updateCntrs(-1);
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
                lower.updateCntrs(pck.cod);
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
                lower.updateCntrs(-1);
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
