package serv;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.Comparator;
import java.util.List;
import pack.packGtp;
import pack.packHolder;
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
 * gprs tunneling protocol (3gpp29060) server
 *
 * @author matecsaba
 */
public class servGtp extends servGeneric implements prtServP {

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * list of connections
     */
    public tabGen<servGtpConn> conns = new tabGen<servGtpConn>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server gtp .*! port " + packGtp.portCtrl,
        "server gtp .*! protocol " + proto2string(protoAllDgrm)
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
     * @param id peer id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servGtpConn connFind(addrIP id, boolean create) {
        servGtpConn ntry = new servGtpConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servGtpConn old = conns.add(ntry);
        if (old != null) {
            ntry = old;
        }
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id peer id
     * @return connection entry
     */
    public servGtpConn connDel(addrIP id) {
        servGtpConn ntry = new servGtpConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l) {
        if (clnIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + clnIfc.name);
        }
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
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  clone                        set interface to clone");
        l.add("2 .    <name>                     name of interface");
    }

    public String srvName() {
        return "gtp";
    }

    public int srvPort() {
        return packGtp.portCtrl;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        if (genDgrmStart(this, srvPort + (packGtp.portData - packGtp.portCtrl))) {
            return true;
        }
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        if (genericStop(srvPort + (packGtp.portData - packGtp.portCtrl))) {
            return true;
        }
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = 120000;
        boolean ctrl = id.portLoc == srvPort;
        servGtpConn ntry = connFind(id.peerAddr, ctrl);
        if (ntry == null) {
            return true;
        }
        if (ctrl) {
            if (ntry.connC != null) {
                ntry.connC.setClosing();
            }
            ntry.connC = id;
        } else {
            if (ntry.connD != null) {
                ntry.connD.setClosing();
            }
            ntry.connD = id;
        }
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
        servGtpConn ntry = connDel(id.peerAddr);
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
        servGtpConn ntry = connFind(id.peerAddr, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
        ntry.doWork(id);
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        servGtpConn ntry = connFind(id.peerAddr, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        int ctrl = -1;
        if (ntry.connC != null) {
            if (id.compare(id, ntry.connC) == 0) {
                ctrl = 1;
            }
        }
        if (ntry.connD != null) {
            if (id.compare(id, ntry.connD) == 0) {
                ctrl = 2;
            }
        }
        if (ctrl < 0) {
            id.setClosing();
            return true;
        }
        ntry.doRecv(pck, ctrl == 1);
        return false;
    }

}

class servGtpConn implements Comparator<servGtpConn> {

    public addrIP peer;

    public prtGenConn connC;

    public prtGenConn connD;

    public servGtp lower;

    public counter cntr = new counter();

    public tabGen<servGtpSess> session = new tabGen<servGtpSess>();

    public int seqCtr;

    public int keep;

    public int compare(servGtpConn o1, servGtpConn o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public servGtpConn(addrIP id, servGtp parent) {
        peer = id.copyBytes();
        lower = parent;
        seqCtr = 1;
    }

    public void setClosed() {
        if (debugger.servGtpTraf) {
            logger.debug("disconnected");
        }
        for (int i = session.size(); i >= 0; i--) {
            servGtpSess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.connDel(peer);
        if (connC != null) {
            connC.setClosing();
        }
        if (connD != null) {
            connD.setClosing();
        }
    }

    public servGtpSess sesFind(int loc) {
        servGtpSess ses = new servGtpSess(this);
        ses.teidLoc = loc;
        return session.find(ses);
    }

    public servGtpSess sesDel(int loc) {
        servGtpSess ses = new servGtpSess(this);
        ses.teidLoc = loc;
        return session.del(ses);
    }

    public void sesAdd(servGtpSess ses) {
        for (;;) {
            ses.teidLoc = bits.randomD();
            if (session.add(ses) == null) {
                break;
            }
        }
    }

    public void sesData(servGtpSess ses, packHolder pck) {
        if (connD == null) {
            return;
        }
        pck.getSkip(2);
        packGtp gtp = new packGtp();
        gtp.flags = packGtp.flgSeq;
        gtp.msgTyp = packGtp.typGPDU;
        gtp.tunId = ses.teidDat;
        gtp.seqNum = ses.seqDat++;
        gtp.createHeader(pck);
        connD.send2net(pck);
    }

    public void doWork(prtGenConn id) {
        if (connC == null) {
            setClosed();
            return;
        }
        if (connC.txBytesFree() < 0) {
            setClosed();
            return;
        }
        if (connD != null) {
            if (connD.txBytesFree() < 0) {
                setClosed();
                return;
            }
        }
        if (id.compare(id, connC) != 0) {
            return;
        }
        keep++;
        if (keep < 3) {
            return;
        }
        keep = 0;
        packGtp gtp = new packGtp();
        gtp.seqNum = seqCtr++;
        gtp.msgTyp = packGtp.typEchoReq;
        connC.send2net(gtp.createPacket());
        if (debugger.servGtpTraf) {
            logger.debug("tx " + gtp.dump());
        }
    }

    public void doRecv(packHolder pck, boolean ctrl) {
        packGtp gtp = new packGtp();
        servGtpSess ses;
        if (gtp.parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (!ctrl) {
            ses = sesFind(gtp.tunId);
            if (ses == null) {
                cntr.drop(pck, counter.reasons.noIface);
                return;
            }
            pck.msbPutW(0, 0xff03); // address + control
            pck.putSkip(2);
            pck.merge2beg();
            ses.upper.recvPack(pck);
            return;
        }
        for (;;) {
            if (gtp.parseExtHdr(pck)) {
                break;
            }
        }
        gtp.parsePacket(pck);
        if (debugger.servGtpTraf) {
            logger.debug("rx " + gtp.dump());
        }
        keep = 0;
        int i;
        switch (gtp.msgTyp) {
            case packGtp.typEchoReq:
                gtp.msgTyp = packGtp.typEchoRep;
                connC.send2net(gtp.createPacket());
                if (debugger.servGtpTraf) {
                    logger.debug("tx " + gtp.dump());
                }
                break;
            case packGtp.typDeleteReq:
                ses = sesFind(gtp.tunId);
                if (ses == null) {
                    cntr.drop(pck, counter.reasons.noIface);
                    return;
                }
                ses.closeDn();
                i = gtp.seqNum;
                gtp = new packGtp();
                gtp.msgTyp = packGtp.typDeleteRep;
                gtp.seqNum = i;
                gtp.tunId = ses.teidCtr;
                gtp.valCause = 0x80; // accepted
                connC.send2net(gtp.createPacket());
                if (debugger.servGtpTraf) {
                    logger.debug("tx " + gtp.dump());
                }
                break;
            case packGtp.typCreateReq:
                ses = new servGtpSess(this);
                if ((gtp.valTeidCp == 0) || (gtp.valTeid1 == 0)) {
                    cntr.drop(pck, counter.reasons.badID);
                    return;
                }
                ses.teidCtr = gtp.valTeidCp;
                ses.teidDat = gtp.valTeid1;
                sesAdd(ses);
                ses.doStartup();
                i = gtp.seqNum;
                gtp = new packGtp();
                gtp.msgTyp = packGtp.typCreateRep;
                gtp.seqNum = i;
                gtp.tunId = ses.teidCtr;
                gtp.valCause = 0x80; // accepted
                gtp.valReordReq = 0xff; // reordering required
                gtp.valTeid1 = ses.teidLoc; // tunnel endpoint id
                gtp.valTeidCp = ses.teidLoc; // tunnel endpoint id
                gtp.valChargID = ses.teidLoc; // charging id id
                gtp.valEndUserAddr = packGtp.adrPpp; // ppp mode
                gtp.valGSNaddr = connC.iface.addr.copyBytes(); // gsn address
                gtp.valQOSpro = 0xb921f; // best effort
                connC.send2net(gtp.createPacket());
                if (debugger.servGtpTraf) {
                    logger.debug("tx " + gtp.dump());
                }
                break;
        }
    }

}

class servGtpSess implements ifcDn, Comparator<servGtpSess> {

    public int teidLoc;

    public int teidDat;

    public int teidCtr;

    public int seqDat;

    public servGtpConn lower;

    public ifcUp upper = new ifcNull();

    public cfgIfc ifc;

    public counter cntr = new counter();

    public servGtpSess(servGtpConn parent) {
        lower = parent;
    }

    public int compare(servGtpSess o1, servGtpSess o2) {
        if (o1.teidLoc < o2.teidLoc) {
            return -1;
        }
        if (o1.teidLoc > o2.teidLoc) {
            return +1;
        }
        return 0;
    }

    public void doStartup() {
        upper = new ifcNull();
        ifc = lower.lower.clnIfc.cloneStart(this);
        seqDat = 1;
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
        lower.sesDel(teidLoc);
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
        return 4000000;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.putDefaults();
        lower.sesData(this, pck);
    }

}
