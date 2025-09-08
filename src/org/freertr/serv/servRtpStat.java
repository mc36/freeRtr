package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.pack.packRtp;
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
import org.freertr.util.logger;

/**
 * rtp statistics
 *
 * @author matecsaba
 */
public class servRtpStat extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servRtpStat() {
    }

    /**
     * port number
     */
    public final static int port = 1;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server rtpstat .*", cmds.tabulator + "port " + port, null),
        new userFilter("server rtpstat .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server rtpstat .*", cmds.tabulator + "timeout 60000", null),
        new userFilter("server rtpstat .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    /**
     * timeout on connection
     */
    public int timeOut = 60 * 1000;

    /**
     * logging
     */
    public boolean logging = false;

    /**
     * clients
     */
    protected final tabGen<servRtpStatOne> stats = new tabGen<servRtpStatOne>();

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        cmds.cfgLine(lst, !logging, beg, "logging", "");
        lst.add(beg + "timeout " + timeOut);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("logging")) {
            logging = true;
            return false;
        }
        if (a.equals("timeout")) {
            timeOut = bits.str2num(cmd.word());
            return false;
        }
        if (!a.equals(cmds.negated)) {
            return true;
        }
        a = cmd.word();
        if (a.equals("logging")) {
            logging = false;
            return false;
        }
        return false;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "logging", "set logging");
        l.add(null, false, 1, new int[]{2}, "timeout", "set timeout on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in ms");
    }

    public String srvName() {
        return "rtpstat";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(timeOut);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servRtpStatOne(this, pipe, id);
        return false;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "peer|port|type|miss|ssrc|seq|pack|byte");
        for (int i = 0; i < stats.size(); i++) {
            res.add("" + stats.get(i));
        }
        return res;
    }

}

class servRtpStatOne implements Runnable, Comparable<servRtpStatOne> {

    private final servRtpStat lower;

    private final pipeSide conn;

    public final addrIP peer;

    public final int port;

    public int sync;

    public int typ;

    public int pak;

    public int byt;

    public int seq;

    public int mis;

    public servRtpStatOne(servRtpStat parent, pipeSide pipe, prtGenConn id) {
        lower = parent;
        conn = pipe;
        peer = id.peerAddr.copyBytes();
        port = id.portRem;
        new Thread(this).start();
    }

    public String toString() {
        return peer + "|" + port + "|" + typ + "|" + mis + "|" + sync + "|" + seq + "|" + pak + "|" + byt;
    }

    public int compareTo(servRtpStatOne o) {
        if (port < o.port) {
            return -1;
        }
        if (port > o.port) {
            return +1;
        }
        return peer.compareTo(o.peer);
    }

    public void run() {
        if (lower.logging) {
            logger.info("accepting " + peer + " " + port);
        }
        lower.stats.put(this);
        packRtp rtp = new packRtp();
        rtp.startConnect(conn, null);
        packHolder pck = new packHolder(true, true);
        try {
            for (;;) {
                if (rtp.recvPack(pck, true) < 1) {
                    break;
                }
                if (seq != rtp.packRx) {
                    mis++;
                }
                sync = rtp.syncRx;
                typ = rtp.typeRx;
                pak++;
                byt += pck.dataSize();
                seq = rtp.packRx + 1;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.stats.del(this);
        if (lower.logging) {
            logger.info("stopped " + peer + " " + port + " with " + mis + " missing packets");
        }
    }

}
