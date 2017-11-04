package serv;

import auth.authGeneric;
import auth.authResult;
import cfg.cfgAll;
import cfg.cfgAuther;
import cfg.cfgInit;
import cry.cryAsn1;
import java.util.List;
import pack.packHolder;
import pack.packSnmp;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import user.userScript;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * simple network management protocol (rfc1157) server
 *
 * @author matecsaba
 */
public class servSnmp extends servGeneric implements prtServS {

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server snmp .*! port " + packSnmp.port,
        "server snmp .*! protocol " + proto2string(protoAllDgrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 5000;
        new servSnmpWorker(this, pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
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
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("authentication")) {
            authenticList = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  authentication               set authentication");
        l.add("2 .    <name>                     name of authentication list");
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
        pip.timeout = 10000;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        userFilter fltr = userFilter.findFilter(new userFilter(cmd, oid, null), cfgInit.snmpMibs);
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
            cryAsn1 cur = pck.res.get(i);
            int typ = pck.type;
            String oid = cryAsn1.oid2str(cur.oid);
            switch (typ) {
                case packSnmp.typGetNext:
                case packSnmp.typGetBulk:
                    oid = doSnmp("next", oid);
                    if (oid == null) {
                        pck.errStat = 2;
                        pck.errIdx = i + 1;
                        continue;
                    }
                    cur.oid = cryAsn1.str2oid(oid);
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
