package org.freertr.serv;

import java.util.List;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgInit;
import org.freertr.enc.encAsn1;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSnmp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.user.userScript;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * simple network management protocol (rfc1157) server
 *
 * @author matecsaba
 */
public class servSnmp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servSnmp() {
    }

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server snmp .*", cmds.tabulator + "port " + packSnmp.port, null),
        new userFilter("server snmp .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(5000);
        new servSnmpWorker(this, pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (authenticList == null) {
            l.add(beg + "no authentication");
        } else {
            l.add(beg + "authentication " + authenticList.autName);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("authentication")) {
            cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
            if (lst == null) {
                cmd.error("no such auth list");
                return false;
            }
            authenticList = lst.getAuther();
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("authentication")) {
            authenticList = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "authentication", "set authentication");
        l.add(null, false, 2, new int[]{-1}, "<name:aaa>", "name of authentication list");
    }

    public String srvName() {
        return "snmp";
    }

    public int srvPort() {
        return packSnmp.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    private String doSnmp(String cmd, String oid) {
        pipeLine pl = new pipeLine(32768, false);
        pipeSide pip = pl.getSide();
        pip.setTime(10000);
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        userFilter fltr = userFilter.findFilter(new userFilter(cmd, oid, null), cfgAll.snmpMibs);
        if (fltr == null) {
            return null;
        }
        userScript t = new userScript(pip, "");
        t.allowExec = true;
        t.allowConfig = true;
        t.addLines(fltr.listing);
        t.addLine("snmp " + cmd + " " + oid);
        pip = pl.getSide();
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCR;
        t.cmdAll();
        pl.setClose();
        String a = pip.strGet(65536);
        if (a == null) {
            return null;
        }
        a = a.replaceAll("\r", "").replaceAll("\n", "").trim();
        if (debugger.servSnmpTraf) {
            logger.debug("result " + cmd + "," + oid + "=" + a);
        }
        return a;
    }

    /**
     * got one packet
     *
     * @param pck packet got
     * @return packet to send, null if error
     */
    protected packSnmp gotPack(packSnmp pck) {
        authResult aut = authenticList.authUserPass(pck.community, "snmp");
        if (aut == null) {
            return null;
        }
        if (aut.result != authResult.authSuccessful) {
            return null;
        }
        pck.errIdx = 0;
        pck.errStat = 0;
        for (int i = 0; i < pck.res.size(); i++) {
            encAsn1 cur = pck.res.get(i);
            int typ = pck.type;
            String oid = encAsn1.oid2str(cur.oid);
            switch (typ) {
                case packSnmp.typGetNext:
                case packSnmp.typGetBulk:
                    oid = doSnmp("next", oid);
                    if (oid == null) {
                        pck.errStat = 2;
                        pck.errIdx = i + 1;
                        continue;
                    }
                    cur.oid = encAsn1.str2oid(oid);
                    typ = packSnmp.typGetReq;
                    break;
                default:
                    break;
            }
            String s = doSnmp(packSnmp.type2string(typ), oid);
            if (s == null) {
                pck.errStat = 2;
                pck.errIdx = i + 1;
                continue;
            }
            if (cur.fromString(new cmds("", s))) {
                pck.errStat = 2;
                pck.errIdx = i + 1;
                continue;
            }
        }
        pck.type = packSnmp.typResponse;
        return pck;
    }

}

class servSnmpWorker implements Runnable {

    private servSnmp lower;

    private pipeSide pipe;

    public servSnmpWorker(servSnmp parent, pipeSide conn) {
        lower = parent;
        pipe = conn;
        new Thread(this).start();
    }

    public void doer() {
        for (;;) {
            packHolder pck = pipe.readPacket(true);
            if (pck == null) {
                return;
            }
            packSnmp pckd = new packSnmp();
            if (pckd.parsePacket(pck)) {
                logger.info("got bad packet");
                return;
            }
            if (debugger.servSnmpTraf) {
                logger.debug("rx " + pckd);
            }
            pckd = lower.gotPack(pckd);
            if (pckd == null) {
                return;
            }
            if (debugger.servSnmpTraf) {
                logger.debug("tx " + pckd);
            }
            pck.clear();
            pckd.createPacket(pck);
            pck.pipeSend(pipe, 0, pck.dataSize(), 2);
        }
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
