package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.auth.authLocal;
import net.freertr.auth.autherChap;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.pack.packL2f;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * layer two forwarding protocol (rfc2341) server
 *
 * @author matecsaba
 */
public class servL2f extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servL2f() {
    }

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
    public tabGen<servL2fConn> conns = new tabGen<servL2fConn>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server l2f .*! port " + packL2f.port,
        "server l2f .*! protocol " + proto2string(protoAllDgrm),
        "server l2f .*! no password"
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
    public servL2fConn connFind(prtGenConn id, boolean create) {
        servL2fConn ntry = new servL2fConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servL2fConn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        if (password != null) {
            ntry.chlLoc = new byte[16];
            for (int i = 0; i < ntry.chlLoc.length; i++) {
                ntry.chlLoc[i] = (byte) bits.randomB();
            }
        }
        ntry.tunLoc = bits.randomW();
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servL2fConn connDel(prtGenConn id) {
        servL2fConn ntry = new servL2fConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (clnIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + clnIfc.name);
        }
        cmds.cfgLine(l, password == null, cmds.tabulator, "password", authLocal.passwdEncode(password, (filter & 2) != 0));
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
        l.add(null, "1 2  clone                        set interface to clone");
        l.add(null, "2 .    <name:ifc>                 name of interface");
        l.add(null, "1 2  password                     set password");
        l.add(null, "2 .    <str>                      password");
    }

    public String srvName() {
        return "l2f";
    }

    public int srvPort() {
        return packL2f.port;
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
        servL2fConn ntry = connDel(id);
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
        servL2fConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
        ntry.doWork();
    }

    /**
     * received error
     *
     * @param id connection
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return false on success, true on error
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        servL2fConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        ntry.doRecv(pck);
        return false;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "addr|port|tunloc|tunrem|sess");
        for (int i = 0; i < conns.size(); i++) {
            servL2fConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.conn.peerAddr + "|" + ntry.conn.portRem + "|" + ntry.tunLoc + "|" + ntry.tunRem + "|" + ntry.session.size());
        }
        return res;
    }

}

class servL2fConn implements Comparator<servL2fConn> {

    public prtGenConn conn;

    public servL2f lower;

    public counter cntr = new counter();

    public tabGen<servL2fSess> session = new tabGen<servL2fSess>();

    public int txed = 0;

    public int keep = 0;

    public int tunLoc = 0;

    public int tunRem = 0;

    public int keyLoc = 0;

    public int keyRem = 0;

    public byte[] chlLoc = null;

    public byte[] chlRem = null;

    public int compare(servL2fConn o1, servL2fConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
    }

    public servL2fConn(prtGenConn id, servL2f parent) {
        conn = id;
        lower = parent;
    }

    public void setClosed() {
        if (debugger.servL2fTraf) {
            logger.debug("disconnected");
        }
        for (int i = session.size(); i >= 0; i--) {
            servL2fSess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.connDel(conn);
        conn.setClosing();
    }

    public servL2fSess sesFind(int loc) {
        servL2fSess ses = new servL2fSess(this);
        ses.multi = loc;
        return session.find(ses);
    }

    public servL2fSess sesDel(int loc, boolean snd) {
        if (snd) {
            packL2f pckTx = new packL2f();
            packHolder pckBin = new packHolder(true, true);
            pckTx.createClose(pckBin, 4);
            pckTx.seq = bits.randomB();
            pckTx.client = tunRem;
            pckTx.multi = loc;
            pckTx.key = keyRem;
            pckTx.createHeader(pckBin);
            cntr.tx(pckBin);
            txed++;
            conn.send2net(pckBin);
            if (debugger.servL2fTraf) {
                logger.debug("tx " + pckTx.dump());
            }
        }
        servL2fSess ses = new servL2fSess(this);
        ses.multi = loc;
        return session.del(ses);
    }

    public void sesAdd(int loc) {
        servL2fSess ses = new servL2fSess(this);
        ses.multi = loc;
        if (session.add(ses) != null) {
            return;
        }
        ses.doStartup();
    }

    public void sesData(servL2fSess ses, packHolder pckBin) {
        pckBin.merge2beg();
        packL2f pckTx = new packL2f();
        pckTx.proto = packL2f.prtPpp;
        pckTx.client = tunRem;
        pckTx.key = keyRem;
        pckTx.multi = ses.multi;
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        conn.send2net(pckBin);
    }

    public void doWork() {
        packL2f pckTx = new packL2f();
        packHolder pckBin = new packHolder(true, true);
        keep++;
        if (keep < 5) {
            return;
        }
        pckTx.valResp = new byte[1];
        pckTx.valResp[0] = (byte) bits.randomB();
        pckTx.createEchoReq(pckBin, pckTx.valResp);
        pckTx.seq = bits.randomB();
        pckTx.client = tunRem;
        pckTx.key = keyRem;
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        txed++;
        conn.send2net(pckBin);
        if (debugger.servL2fTraf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < 10) {
            return;
        }
        setClosed();
    }

    public void doRecv(packHolder pckBin) {
        packL2f pckRx = new packL2f();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        keep = 0;
        if (pckRx.proto != packL2f.prtMgmt) {
            servL2fSess ses = sesFind(pckRx.multi);
            if (ses == null) {
                return;
            }
            ses.cntr.rx(pckBin);
            ses.upper.recvPack(pckBin);
            return;
        }
        packL2f pckTx = new packL2f();
        txed = 0;
        switch (pckRx.type) {
            case packL2f.typConf:
                if (pckRx.parseConf(pckBin)) {
                    return;
                }
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                tunRem = pckRx.valClid;
                chlRem = pckRx.valChal;
                pckBin.clear();
                pckTx.createConf(pckBin, cfgAll.hostName, chlLoc, tunLoc);
                break;
            case packL2f.typOpen:
                if (pckRx.parseOpen(pckBin)) {
                    return;
                }
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                if (pckRx.multi != 0) {
                    sesAdd(pckRx.multi);
                    pckTx.multi = pckRx.multi;
                    pckBin.clear();
                    pckTx.createOpen(pckBin, null);
                    break;
                }
                byte[] res = null;
                if (chlLoc != null) {
                    if (pckRx.valResp == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(tunLoc, lower.password, chlLoc);
                    if (res.length != pckRx.valResp.length) {
                        return;
                    }
                    if (bits.byteComp(res, 0, pckRx.valResp, 0, res.length) != 0) {
                        return;
                    }
                    keyLoc = packL2f.calcKey(res);
                    res = autherChap.calcAuthHash(tunRem, lower.password, chlRem);
                    keyRem = packL2f.calcKey(res);
                }
                pckBin.clear();
                pckTx.createOpen(pckBin, res);
                break;
            case packL2f.typClose:
                if (pckRx.parseClose(pckBin)) {
                    return;
                }
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                servL2fSess ses = sesDel(pckRx.multi, false);
                if (ses == null) {
                    return;
                }
                ses.closeDn();
                return;
            case packL2f.typEchoReq:
                pckRx.parseEcho(pckBin);
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                pckBin.clear();
                pckTx.createEchoRes(pckBin, pckRx.valResp);
                break;
            case packL2f.typEchoRes:
                pckRx.parseEcho(pckBin);
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                return;
            default:
                return;
        }
        pckTx.seq = pckRx.seq;
        pckTx.client = tunRem;
        pckTx.chksum = pckRx.chksum;
        pckTx.key = keyRem;
        pckTx.createHeader(pckBin);
        conn.send2net(pckBin);
        if (debugger.servL2fTraf) {
            logger.debug("tx " + pckTx.dump());
        }
    }

}

class servL2fSess implements ifcDn, Comparator<servL2fSess> {

    public int multi;

    public servL2fConn lower;

    public ifcUp upper = new ifcNull();

    public cfgIfc ifc;

    public counter cntr = new counter();

    public servL2fSess(servL2fConn parent) {
        lower = parent;
    }

    public int compare(servL2fSess o1, servL2fSess o2) {
        if (o1.multi < o2.multi) {
            return -1;
        }
        if (o1.multi > o2.multi) {
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
        lower.sesDel(multi, true);
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
