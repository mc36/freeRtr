package serv;

import addr.addrEmpty;
import addr.addrType;
import auth.authLocal;
import auth.autherChap;
import cfg.cfgAll;
import cfg.cfgIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pack.packL2tp;
import pack.packL2tp2;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServP;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * layer two tunneling protocol (rfc2661) server
 *
 * @author matecsaba
 */
public class servL2tp2 extends servGeneric implements prtServP {

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * password
     */
    public String password;

    /**
     * list of connections
     */
    public tabGen<servL2tp2conn> conns = new tabGen<servL2tp2conn>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server l2tp2 .*! port " + packL2tp2.port,
        "server l2tp2 .*! protocol " + proto2string(protoAllDgrm),
        "server l2tp2 .*! no password"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    /**
     * find one connection
     *
     * @param id connection id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servL2tp2conn connFind(prtGenConn id, boolean create) {
        servL2tp2conn ntry = new servL2tp2conn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servL2tp2conn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        if (password != null) {
            ntry.chlng = new byte[16];
            for (int i = 0; i < ntry.chlng.length; i++) {
                ntry.chlng[i] = (byte) bits.randomB();
            }
        }
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servL2tp2conn connDel(prtGenConn id) {
        servL2tp2conn ntry = new servL2tp2conn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l) {
        if (clnIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + clnIfc.name);
        }
        cmds.cfgLine(l, password == null, cmds.tabulator, "password", authLocal.passwdEncode(password));
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = cfgAll.ifcFind(cmd.word(), false);
            if (clnIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (clnIfc.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not dialer interface");
                clnIfc = null;
                return false;
            }
            return false;
        }
        if (s.equals("password")) {
            password = authLocal.passwdDecode(cmd.getRemaining());
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = null;
            return false;
        }
        if (s.equals("password")) {
            password = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  clone                        set interface to clone");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  password                     set password");
        l.add("2 .    <name>                     password");
    }

    public String srvName() {
        return "l2tp2";
    }

    public int srvPort() {
        return packL2tp2.port;
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
        id.timeout = 120000;
        connFind(id, true);
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
        servL2tp2conn ntry = connDel(id);
        if (ntry == null) {
            return;
        }
        ntry.setClosed();
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        servL2tp2conn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
        ntry.doWork();
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        servL2tp2conn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        ntry.doRecv(pck);
        return false;
    }

}

class servL2tp2conn implements Comparator<servL2tp2conn> {

    public prtGenConn conn;

    public servL2tp2 lower;

    public counter cntr = new counter();

    public List<packL2tp2> queue = new ArrayList<packL2tp2>();

    public tabGen<servL2tp2sess> session = new tabGen<servL2tp2sess>();

    public int txed = 0;

    public int keep = 0;

    public int seqRx = 0;

    public int seqTx = 0;

    public int tunLoc = 0;

    public int tunRem = 0;

    public byte[] chlng = null;

    public int compare(servL2tp2conn o1, servL2tp2conn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
    }

    public servL2tp2conn(prtGenConn id, servL2tp2 parent) {
        conn = id;
        lower = parent;
    }

    public void setClosed() {
        if (debugger.servL2tp2traf) {
            logger.debug("disconnected");
        }
        for (int i = session.size(); i >= 0; i--) {
            servL2tp2sess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.connDel(conn);
        conn.setClosing();
    }

    public servL2tp2sess sesFind(int loc) {
        servL2tp2sess ses = new servL2tp2sess(this);
        ses.sesLoc = loc;
        return session.find(ses);
    }

    public servL2tp2sess sesDel(int loc) {
        servL2tp2sess ses = new servL2tp2sess(this);
        ses.sesLoc = loc;
        return session.del(ses);
    }

    public void sesAdd(servL2tp2sess ses) {
        for (;;) {
            ses.sesLoc = bits.randomW();
            if (session.add(ses) == null) {
                break;
            }
        }
    }

    public void sesData(servL2tp2sess ses, packHolder pckBin) {
        pckBin.merge2beg();
        packL2tp2 pckTx = new packL2tp2();
        pckTx.ctrl = false;
        pckTx.sesID = ses.sesRem;
        pckTx.tunID = tunRem;
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        conn.send2net(pckBin);
    }

    public void doWork() {
        packL2tp2 pckTx;
        packHolder pckBin = new packHolder(true, true);
        synchronized (queue) {
            if (queue.size() < 1) {
                keep++;
                if (keep < 5) {
                    return;
                }
                keep = 0;
                enQueue(packL2tp2.createHELLO());
                return;
            }
            pckTx = queue.get(0);
            pckTx.patchHeader(tunRem, seqRx, seqTx);
            pckTx.createTLVs(pckBin);
            pckTx.createHeader(pckBin);
            cntr.tx(pckBin);
            txed++;
        }
        conn.send2net(pckBin);
        if (debugger.servL2tp2traf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < 8) {
            return;
        }
        setClosed();
    }

    public void enQueue(packL2tp2 pck) {
        synchronized (queue) {
            queue.add(pck);
        }
    }

    public void sendAck() {
        packL2tp2 pckTx = new packL2tp2();
        pckTx.patchHeader(tunRem, seqRx, seqTx);
        packHolder pckBin = new packHolder(true, true);
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        conn.send2net(pckBin);
        if (debugger.servL2tp2traf) {
            logger.debug("tx " + pckTx.dump());
        }
    }

    public void doRecv(packHolder pckBin) {
        packL2tp2 pckRx = new packL2tp2();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        if (pckRx.tunID != tunLoc) {
            cntr.drop(pckBin, counter.reasons.badID);
            return;
        }
        keep = 0;
        if (!pckRx.ctrl) {
            servL2tp2sess ses = sesFind(pckRx.sesID);
            if (ses == null) {
                cntr.drop(pckBin, counter.reasons.badID);
                return;
            }
            ses.cntr.rx(pckBin);
            ses.upper.recvPack(pckBin);
            return;
        }
        if (pckRx.seqTx != seqRx) {
            cntr.drop(pckBin, counter.reasons.badRxSeq);
            return;
        }
        synchronized (queue) {
            if ((pckRx.seqRx == ((seqTx + 1) & 0xffff)) && (queue.size() > 0)) {
                seqTx = (seqTx + 1) & 0xffff;
                txed = 0;
                queue.remove(0);
            }
        }
        pckRx.parseTLVs(pckBin);
        cntr.rx(pckBin);
        if (debugger.servL2tp2traf) {
            logger.debug("rx " + pckRx.dump());
        }
        if (pckRx.valMsgTyp == packL2tp.typZLB) {
            return;
        }
        seqRx = (seqRx + 1) & 0xffff;
        servL2tp2sess ses;
        byte[] res = null;
        switch (pckRx.valMsgTyp) {
            case packL2tp.typSCCRQ:
                tunRem = pckRx.valTunId;
                tunLoc = bits.randomW();
                if (chlng != null) {
                    if (pckRx.valChallen == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(pckRx.valMsgTyp + 1, lower.password, pckRx.valChallen);
                }
                enQueue(packL2tp2.createSCCRP(tunLoc, cfgAll.hostName, chlng, res));
                break;
            case packL2tp.typSCCCN:
                if (chlng != null) {
                    if (pckRx.valResponse == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(pckRx.valMsgTyp, lower.password, chlng);
                    if (res.length != pckRx.valResponse.length) {
                        return;
                    }
                    if (bits.byteComp(res, 0, pckRx.valResponse, 0, res.length) != 0) {
                        return;
                    }
                }
                sendAck();
                break;
            case packL2tp.typOCRQ:
                ses = new servL2tp2sess(this);
                ses.sesRem = pckRx.valSesId;
                sesAdd(ses);
                enQueue(packL2tp2.createOCRP(ses.sesLoc, ses.sesRem));
                enQueue(packL2tp2.createOCCN(ses.sesRem));
                ses.doStartup();
                break;
            case packL2tp.typICRQ:
                ses = new servL2tp2sess(this);
                ses.sesRem = pckRx.valSesId;
                sesAdd(ses);
                enQueue(packL2tp2.createICRP(ses.sesLoc, ses.sesRem));
                break;
            case packL2tp.typICCN:
                ses = sesFind(pckRx.sesID);
                if (ses == null) {
                    break;
                }
                ses.doStartup();
                sendAck();
                break;
            case packL2tp.typCDN:
                ses = sesDel(pckRx.sesID);
                if (ses == null) {
                    break;
                }
                ses.closeDn();
                break;
            case packL2tp.typSCCNO:
                sendAck();
                setClosed();
                break;
            case packL2tp.typHELLO:
            case packL2tp.typSLI:
            case packL2tp.typWEN:
                sendAck();
                break;
        }
    }

}

class servL2tp2sess implements ifcDn, Comparator<servL2tp2sess> {

    public int sesLoc;

    public int sesRem;

    public servL2tp2conn lower;

    public ifcUp upper = new ifcNull();

    public cfgIfc ifc;

    public counter cntr = new counter();

    public servL2tp2sess(servL2tp2conn parent) {
        lower = parent;
    }

    public int compare(servL2tp2sess o1, servL2tp2sess o2) {
        if (o1.sesLoc < o2.sesLoc) {
            return -1;
        }
        if (o1.sesLoc > o2.sesLoc) {
            return +1;
        }
        return 0;
    }

    public void doStartup() {
        upper = new ifcNull();
        ifc = lower.lower.clnIfc.cloneStart(this);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        lower.enQueue(packL2tp2.createCDN(sesRem, sesLoc));
        lower.sesDel(sesLoc);
        upper.closeUp();
        if (ifc != null) {
            ifc.cloneStop();
        }
    }

    public void flapped() {
        closeDn();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1400;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.putDefaults();
        lower.sesData(this, pck);
    }

}
