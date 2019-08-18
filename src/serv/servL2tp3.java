package serv;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import auth.authLocal;
import auth.autherChap;
import cfg.cfgAll;
import cfg.cfgBrdg;
import cfg.cfgIfc;
import ifc.ifcBridgeIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pack.packL2tp;
import pack.packL2tp3;
import pack.packLdpPwe;
import pipe.pipeSide;
import prt.prtGenConn;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;

/**
 * layer two tunneling protocol v3 (rfc3931) server
 *
 * @author matecsaba
 */
public class servL2tp3 extends servGeneric implements ipPrt {

    /**
     * interface to use
     */
    public cfgIfc dialIfc;

    /**
     * interface to use
     */
    public cfgBrdg brdgIfc;

    /**
     * password
     */
    public String password;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * list of connections
     */
    public tabGen<servL2tp3conn> conns = new tabGen<servL2tp3conn>();

    /**
     * counter
     */
    public counter cntr;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server l2tp3 .*! port " + packL2tp3.prot,
        "server l2tp3 .*! protocol " + proto2string(protoAllDgrm),
        "server l2tp3 .*! no password"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
        if (dialIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + dialIfc.name);
        }
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.name);
        }
        cmds.cfgLine(l, password == null, cmds.tabulator, "password", authLocal.passwdEncode(password));
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("clone")) {
            dialIfc = cfgAll.ifcFind(cmd.word(), false);
            if (dialIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (dialIfc.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not dialer interface");
                dialIfc = null;
                return false;
            }
            return false;
        }
        if (s.equals("bridge")) {
            brdgIfc = cfgAll.brdgFind(cmd.word(), false);
            if (brdgIfc == null) {
                cmd.error("no such bridge group");
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
            dialIfc = null;
            return false;
        }
        if (s.equals("bridge")) {
            brdgIfc = null;
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
        l.add("1 2  bridge                       set interface to clone");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  password                     set password");
        l.add("2 .    <name>                     password");
    }

    public String srvName() {
        return "l2tp3";
    }

    public int srvPort() {
        return packL2tp3.prot;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genRawStart(this, 0);
    }

    public boolean srvDeinit() {
        return genRawStop(this, 0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        return true;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return packL2tp3.prot;
    }

    public String toString() {
        return "l2tp3";
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        servL2tp3conn ntry = new servL2tp3conn(rxIfc, pck.IPsrc, this);
        servL2tp3conn old = conns.add(ntry);
        if (old != null) {
            old.doRecv(pck);
            return;
        }
        if (srvCheckAccept(rxIfc, pck)) {
            return;
        }
        ntry.doStartup();
        ntry.doRecv(pck);
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

}

class servL2tp3conn implements Runnable, Comparator<servL2tp3conn> {

    public servL2tp3 lower;

    public ipFwdIface iface;

    public addrIP peer;

    public boolean need2run;

    public tabGen<servL2tp3sess> session = new tabGen<servL2tp3sess>();

    public List<packL2tp3> queue = new ArrayList<packL2tp3>();

    public counter cntr = new counter();

    private notifier notif = new notifier();

    public int txed = 0;

    public int keep = 0;

    public int seqRx = 0;

    public int seqTx = 0;

    public int conLoc = 0;

    public int conRem = 0;

    public ipFwd fwdCor;

    public byte[] chlng = null;

    public servL2tp3conn(ipFwdIface ifc, addrIP adr, servL2tp3 parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
    }

    public String toString() {
        return lower + " with " + peer;
    }

    public int compare(servL2tp3conn o1, servL2tp3conn o2) {
        int i = o1.iface.compare(o1.iface, o2.iface);
        if (i != 0) {
            return i;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public void setClosed() {
        need2run = false;
        notif.wakeup();
        for (int i = session.size(); i >= 0; i--) {
            servL2tp3sess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.conns.del(this);
        fwdCor.protoDel(lower, iface, peer);
    }

    public void doStartup() {
        if (debugger.servL2tp3traf) {
            logger.debug("starting");
        }
        need2run = true;
        fwdCor = lower.srvVrf.getFwd(peer);
        conLoc = bits.randomD();
        if (lower.password != null) {
            chlng = new byte[16];
            for (int i = 0; i < chlng.length; i++) {
                chlng[i] = (byte) bits.randomB();
            }
        }
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                doWork();
                if (!need2run) {
                    break;
                }
                notif.sleep(5000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        setClosed();
        if (debugger.servL2tp3traf) {
            logger.debug("disconnected");
        }
    }

    public void enQueue(packL2tp3 pck) {
        synchronized (queue) {
            queue.add(pck);
        }
    }

    public void sendAck() {
        packL2tp3 pckTx = new packL2tp3();
        pckTx.patchHeader(conRem, seqRx, seqTx);
        packHolder pckBin = new packHolder(true, true);
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        sendProto(pckBin);
    }

    public void sendProto(packHolder pck) {
        pck.merge2beg();
        if (lower.sendingTTL >= 0) {
            pck.IPttl = lower.sendingTTL;
        }
        if (lower.sendingTOS >= 0) {
            pck.IPtos = lower.sendingTOS;
        }
        pck.IPprt = packL2tp3.prot;
        pck.IPsrc.setAddr(iface.addr);
        pck.IPtrg.setAddr(peer);
        fwdCor.protoPack(iface, pck);
    }

    public void doWork() {
        packL2tp3 pckTx;
        packHolder pckBin = new packHolder(true, true);
        synchronized (queue) {
            if (queue.size() < 1) {
                keep++;
                if (keep < 3) {
                    return;
                }
                keep = 0;
                enQueue(packL2tp3.createHELLO());
                return;
            }
            pckTx = queue.get(0);
            pckTx.patchHeader(conRem, seqRx, seqTx);
            pckTx.createTLVs(pckBin);
            pckTx.createHeader(pckBin);
            cntr.tx(pckBin);
            sendProto(pckBin);
            txed++;
        }
        if (debugger.servL2tp3traf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < 8) {
            return;
        }
        need2run = false;
    }

    public void doRecv(packHolder pckBin) {
        packL2tp3 pckRx = new packL2tp3();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        keep = 0;
        if (!pckRx.ctrl) {
            servL2tp3sess ntry = new servL2tp3sess();
            ntry.sesLoc = pckRx.sesID;
            ntry = session.find(ntry);
            if (ntry == null) {
                return;
            }
            ntry.doRecv(pckBin);
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
        if (debugger.servL2tp3traf) {
            logger.debug("rx " + pckRx.dump());
        }
        if (pckRx.valMsgTyp == packL2tp.typZLB) {
            return;
        }
        seqRx = (seqRx + 1) & 0xffff;
        byte[] res = null;
        switch (pckRx.valMsgTyp) {
            case packL2tp.typHELLO:
                sendAck();
                break;
            case packL2tp.typSCCRQ:
                conRem = pckRx.valConnId;
                if (chlng != null) {
                    if (pckRx.valChallen == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(pckRx.valMsgTyp + 1, lower.password, pckRx.valChallen);
                }
                enQueue(packL2tp3.createSCCRP(conLoc, iface.addr.toIPv4(), cfgAll.hostName, chlng, res));
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
            case packL2tp.typSCCNO:
                need2run = false;
                sendAck();
                return;
            case packL2tp.typCDN:
                servL2tp3sess ntry = new servL2tp3sess();
                ntry.sesLoc = pckRx.valRemSesId;
                ntry = session.find(ntry);
                if (ntry != null) {
                    ntry.closeDn();
                }
                sendAck();
                break;
            case packL2tp.typICCN:
                sendAck();
                break;
            case packL2tp.typICRQ:
                ntry = newSess(pckRx);
                if (ntry == null) {
                    enQueue(packL2tp3.createCDN(pckRx.valLocSesId, bits.randomW()));
                } else {
                    enQueue(packL2tp3.createICRP(ntry.sesRem, ntry.sesLoc, ntry.vcid, ntry.pwType));
                    enQueue(packL2tp3.createSLI(ntry.sesRem, ntry.sesLoc));
                }
                break;
            case packL2tp.typOCRQ:
                enQueue(packL2tp3.createCDN(pckRx.valLocSesId, bits.randomW()));
                break;
            case packL2tp.typSLI:
                sendAck();
                break;
        }
        notif.wakeup();
    }

    public servL2tp3sess newSess(packL2tp3 pck) {
        servL2tp3sess ntry = new servL2tp3sess();
        ntry.sesRem = pck.valLocSesId;
        ntry.pwType = pck.valPwTyp;
        ntry.vcid = pck.valRemEndId;
        ntry.lower = this;
        switch (ntry.pwType) {
            case packLdpPwe.pwtEthPort:
            case packLdpPwe.pwtEthVlan:
                if (lower.brdgIfc == null) {
                    return null;
                }
                ntry.brdgIfc = lower.brdgIfc.bridgeHed.newIface(false, true, false);
                ntry.setUpper(ntry.brdgIfc);
                break;
            case packLdpPwe.pwtHdlc:
            case packLdpPwe.pwtPpp:
            case packLdpPwe.pwtFrDlci:
            case packLdpPwe.pwtAtmAal5:
                if (lower.dialIfc == null) {
                    return null;
                }
                ntry.dialIfc = lower.dialIfc.cloneStart(ntry);
                break;
            default:
                return null;
        }
        for (;;) {
            ntry.sesLoc = bits.randomD();
            if (session.add(ntry) != null) {
                continue;
            }
            return ntry;
        }
    }

}

class servL2tp3sess implements ifcDn, Comparator<servL2tp3sess> {

    public int sesLoc;

    public int sesRem;

    public String vcid;

    public int pwType;

    public servL2tp3conn lower;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public ifcBridgeIfc brdgIfc;

    public cfgIfc dialIfc;

    public String toString() {
        return lower + "/" + vcid;
    }

    public int compare(servL2tp3sess o1, servL2tp3sess o2) {
        if (o1.sesLoc < o2.sesLoc) {
            return -1;
        }
        if (o1.sesLoc > o2.sesLoc) {
            return +1;
        }
        return 0;
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
        lower.enQueue(packL2tp3.createCDN(sesRem, sesLoc));
        lower.session.del(this);
        if (upper != null) {
            upper.closeUp();
            upper = null;
        }
        if (dialIfc != null) {
            dialIfc.cloneStop();
            dialIfc = null;
        }
        if (brdgIfc != null) {
            brdgIfc.closeUp();
            brdgIfc = null;
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
        pck.merge2beg();
        if (pwType == packLdpPwe.pwtPpp) {
            pck.getSkip(2);
        }
        packL2tp3 tx = new packL2tp3();
        tx.ctrl = false;
        tx.sesID = sesRem;
        tx.createHeader(pck);
        cntr.tx(pck);
        pck.putDefaults();
        lower.sendProto(pck);
    }

    public void doRecv(packHolder pck) {
        if (pwType == packLdpPwe.pwtPpp) {
            pck.msbPutW(0, 0xff03);
            pck.putSkip(2);
            pck.merge2beg();
        }
        cntr.rx(pck);
        upper.recvPack(pck);
    }

}
