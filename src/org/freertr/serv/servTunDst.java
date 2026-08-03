package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgScrpt;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * tunnel destination server
 *
 * @author matecsaba
 */
public class servTunDst extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servTunDst() {
    }

    /**
     * default port
     */
    public final static int portNum = 2264;

    /**
     * client interface
     */
    public cfgIfc clntIfc;

    /**
     * log hits
     */
    public boolean logging;

    /**
     * just address
     */
    public boolean justAddr;

    /**
     * script to execute
     */
    public cfgScrpt script;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server tundst .*", cmds.tabulator + "port " + portNum, null),
        new userFilter("server tundst .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server tundst .*", cmds.tabulator + cmds.negated + cmds.tabulator + "client", null),
        new userFilter("server tundst .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null),
        new userFilter("server tundst .*", cmds.tabulator + cmds.negated + cmds.tabulator + "just-address", null),
        new userFilter("server tundst .*", cmds.tabulator + cmds.negated + cmds.tabulator + "script", null),};

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (clntIfc == null) {
            l.add(beg + "no client");
        } else {
            l.add(beg + "client " + clntIfc.name);
        }
        if (script == null) {
            l.add(beg + "no script");
        } else {
            l.add(beg + "script " + script.name);
        }
        cmds.cfgLine(l, !logging, beg, "logging", "");
        cmds.cfgLine(l, !justAddr, beg, "just-address", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals(cmds.negated)) {
            negated = true;
            s = cmd.word();
        }
        if (s.equals("logging")) {
            logging = !negated;
            return false;
        }
        if (s.equals("just-address")) {
            justAddr = !negated;
            return false;
        }
        if (s.equals("client")) {
            if (negated) {
                clntIfc = null;
                return false;
            }
            cfgIfc i = cfgAll.ifcFind(cmd.word(), 0);
            if (i == null) {
                cmd.error("no such interface");
                return false;
            }
            clntIfc = i;
            return false;
        }
        if (s.equals("script")) {
            if (negated) {
                script = null;
                return false;
            }
            script = cfgAll.scrptFind(cmd.word(), false);
            if (script == null) {
                cmd.error("no such script");
                return false;
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "client", "specify interface to use");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 1, new int[]{-1}, "logging", "log translations");
        l.add(null, false, 1, new int[]{-1}, "just-address", "update only the address");
        l.add(null, false, 1, new int[]{2}, "script", "script to invoke");
        l.add(null, false, 2, new int[]{-1}, "<name:scr>", "name of script");
    }

    public String srvName() {
        return "tundst";
    }

    public int srvPort() {
        return portNum;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        return false;
    }

    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
    }

    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (logging) {
            logger.info("accepted " + id);
        }
        id.setClosing();
        if (clntIfc == null) {
            return true;
        }
        if (justAddr) {
            clntIfc.tunnelUpdateTarget(id.peerAddr, -1);
        } else {
            clntIfc.tunnelUpdateTarget(id.peerAddr, id.portRem);
        }
        if (script != null) {
            script.doRound(bits.str2lst("set remote " + id.peerAddr));
        }
        return false;
    }

    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

}
