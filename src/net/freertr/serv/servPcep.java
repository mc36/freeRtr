package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.pack.packPcep;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabHop;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * path computation element protocol (rfc5440) server
 *
 * @author matecsaba
 */
public class servPcep extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servPcep() {
    }

    /**
     * logging
     */
    public boolean logging;

    /**
     * explicit
     */
    public boolean explicit;

    /**
     * exported vrf
     */
    public cfgVrf expVrf;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server pcep .*! port " + packPcep.port,
        "server pcep .*! protocol " + proto2string(protoAllStrm),
        "server pcep .*! no explicit",
        "server pcep .*! no logging"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "pcep";
    }

    public int srvPort() {
        return packPcep.port;
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

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
        cmds.cfgLine(l, !explicit, beg, "explicit", "");
        if (expVrf == null) {
            l.add(beg + "no export-vrf");
        } else {
            l.add(beg + "export-vrf " + expVrf.name);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("export-vrf")) {
            expVrf = cfgAll.vrfFind(cmd.word(), false);
            if (expVrf == null) {
                cmd.error("no such vrf");
            }
            return false;
        }
        if (s.equals("logging")) {
            logging = true;
            return false;
        }
        if (s.equals("explicit")) {
            explicit = true;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("export-vrf")) {
            expVrf = null;
            return false;
        }
        if (s.equals("logging")) {
            logging = false;
            return false;
        }
        if (s.equals("explicit")) {
            explicit = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1  .  logging                   log user communication");
        l.add(null, "1  .  explicit                  respond with explicit labels");
        l.add(null, "1  2  export-vrf                specify vrf to export");
        l.add(null, "2  .    <name:vrf>              vrf name");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCR;
        pipe.lineTx = pipeSide.modTyp.modeCR;
        new servPcepConn(this, id.peerAddr.copyBytes(), pipe);
        return false;
    }

}

class servPcepConn implements Runnable {

    public servPcep lower;

    public addrIP peer;

    public pipeSide conn;

    public servPcepConn(servPcep parent, addrIP remote, pipeSide pipe) {
        lower = parent;
        peer = remote;
        conn = pipe;
        new Thread(this).start();
    }

    public void doWork() {
        packPcep pck = new packPcep();
        pck.pipe = conn;
        pck.createOpen();
        if (debugger.servPcepTraf) {
            logger.debug("tx " + pck);
        }
        pck.sendPack();
        pck.createKeep();
        if (debugger.servPcepTraf) {
            logger.debug("tx " + pck);
        }
        pck.sendPack();
        if (pck.recvPack()) {
            return;
        }
        if (debugger.servPcepTraf) {
            logger.debug("rx " + pck);
        }
        if (pck.msgTyp != packPcep.typOpen) {
            return;
        }
        if (lower.logging) {
            logger.warn("neighbor " + peer + " up");
        }
        long lastKeep = 0;
        for (;;) {
            pck = new packPcep();
            pck.pipe = conn;
            long tim = bits.getTime();
            if ((tim - lastKeep) > 30000) {
                pck.createKeep();
                if (debugger.servPcepTraf) {
                    logger.debug("tx " + pck);
                }
                pck.sendPack();
                lastKeep = tim;
            }
            if (pck.recvPack()) {
                return;
            }
            if (debugger.servPcepTraf) {
                logger.debug("rx " + pck);
            }
            if (pck.msgTyp != packPcep.typReq) {
                continue;
            }
            if (lower.logging) {
                logger.warn("request " + peer + " type=" + pck.setupType + " " + pck.srcAddr + " -> " + pck.trgAddr);
            }
            pck.ero = null;
            pck.loose = true;
            if (lower.expVrf == null) {
                pck.createReply();
                if (debugger.servPcepTraf) {
                    logger.debug("tx " + pck);
                }
                pck.sendPack();
                continue;
            }
            ipFwd fwd;
            if (pck.isIP4) {
                fwd = lower.expVrf.fwd4;
            } else {
                fwd = lower.expVrf.fwd6;
            }
            tabRouteEntry<addrIP> ntry = fwd.actualU.route(pck.srcAddr);
            if (ntry == null) {
                pck.createReply();
                if (debugger.servPcepTraf) {
                    logger.debug("tx " + pck);
                }
                pck.sendPack();
                continue;
            }
            ntry = fwd.actualU.route(pck.trgAddr);
            if (ntry == null) {
                pck.createReply();
                if (debugger.servPcepTraf) {
                    logger.debug("tx " + pck);
                }
                pck.sendPack();
                continue;
            }
            pck.ero = new ArrayList<tabHop>();
            if (ntry.best.oldHop != null) {
                tabRouteEntry<addrIP> ntry2 = fwd.actualU.route(ntry.best.oldHop);
                if (ntry2 != null) {
                    tabHop mid = new tabHop();
                    mid.strict = false;
                    mid.adr.setAddr(ntry.best.oldHop);
                    if (pck.setupType == 1) {
                        mid.index = !lower.explicit;
                        mid.label = ntry2.best.segrouIdx;
                        if (lower.explicit) {
                            mid.label += ntry2.best.segrouOld;
                            mid.label <<= 12;
                        }
                    }
                    pck.ero.add(mid);
                }
            }
            tabHop hop = new tabHop();
            hop.strict = false;
            hop.adr.setAddr(pck.trgAddr);
            if (pck.setupType == 1) {
                hop.index = !lower.explicit;
                hop.label = ntry.best.segrouIdx;
                if (lower.explicit) {
                    hop.label += ntry.best.segrouOld;
                    hop.label <<= 12;
                }
            }
            pck.ero.add(hop);
            pck.createReply();
            if (debugger.servPcepTraf) {
                logger.debug("tx " + pck);
            }
            pck.sendPack();
        }
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (lower.logging) {
            logger.error("neighbor " + peer + " down");
        }
        conn.setClose();
    }

}
