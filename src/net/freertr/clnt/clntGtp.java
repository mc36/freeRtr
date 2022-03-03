package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packGtp;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.prt.prtUdp;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * gprs tunneling protocol (3gpp29060) client
 *
 * @author matecsaba
 */
public class clntGtp implements Runnable, prtServP, ifcDn {

    /**
     * create instance
     */
    public clntGtp() {
    }

    /**
     * config class
     */
    public cfgIfc cfger;

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * preferred ip protocol version
     */
    public int prefer = 0;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * apn name
     */
    public String apn = null;

    /**
     * client isdn
     */
    public String isdn = null;

    /**
     * client imei
     */
    public String imsi = null;

    /**
     * client imei
     */
    public String imei;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private prtGenConn connC;

    private prtGenConn connD;

    /**
     * local tunnel id
     */
    public int teidLoc;

    /**
     * data tunnel id
     */
    public int teidDat;

    /**
     * control tunnel id
     */
    public int teidCtr;

    private int seqCtr;

    private int seqDat;

    private packGtp lastCtrl;

    public String toString() {
        return "gtp to " + target;
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getRemAddr() {
        if (teidDat == 0) {
            return null;
        }
        return connD.peerAddr.copyBytes();
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getLocAddr() {
        if (teidDat == 0) {
            return null;
        }
        return connD.iface.addr.copyBytes();
    }

    /**
     * get remote port
     *
     * @return address
     */
    public int getRemPort() {
        if (teidDat == 0) {
            return 0;
        }
        return connD.portRem;
    }

    /**
     * get local port
     *
     * @return address
     */
    public int getLocPort() {
        if (teidDat == 0) {
            return 0;
        }
        return connD.portLoc;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
        clearState();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1400;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 4000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        if (teidDat == 0) {
            return;
        }
        cntr.tx(pck);
        pck.getSkip(2);
        packGtp gtp = new packGtp();
        gtp.flags = packGtp.flgNothing;
        gtp.msgTyp = packGtp.typGPDU;
        gtp.tunId = teidDat;
        gtp.seqNum = seqDat++;
        gtp.createHeader(pck);
        pck.putDefaults();
        connD.send2net(pck);
    }

    /**
     * start connection
     */
    public void workStart() {
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        working = false;
        clearState();
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                clearState();
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            clearState();
            bits.sleep(1000);
        }
    }

    private void workDoer() {
        addrIP trg = userTerminal.justResolv(target, prefer);
        if (trg == null) {
            return;
        }
        prtUdp udp = vrf.getUdp(trg);
        ipFwdIface fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        connC = udp.packetConnect(this, fwdIfc, packGtp.portCtrl, trg, packGtp.portCtrl, "gtpC", null, -1);
        if (connC == null) {
            return;
        }
        connC.timeout = 120000;
        connD = udp.packetConnect(this, fwdIfc, packGtp.portData, trg, packGtp.portData, "gtpD", null, -1);
        if (connD == null) {
            connC.setClosing();
            return;
        }
        connD.timeout = 120000;
        connD.sendFLW = sendingFLW;
        connD.sendTOS = sendingTOS;
        connD.sendTTL = sendingTTL;
        packGtp gtp = new packGtp();
        gtp.seqNum = seqCtr++;
        gtp.msgTyp = packGtp.typEchoReq;
        connC.send2net(gtp.createPacket());
        if (debugger.clntGtpTraf) {
            logger.debug("tx " + gtp.dump());
        }
        for (int i = 0;; i++) {
            bits.sleep(1000);
            if (!working) {
                return;
            }
            if (lastCtrl.msgTyp == packGtp.typEchoRep) {
                break;
            }
            if (i > 8) {
                return;
            }
        }
        gtp = new packGtp();
        gtp.seqNum = seqCtr++;
        gtp.msgTyp = packGtp.typCreateReq;
        gtp.valGSNaddr = connC.iface.addr.copyBytes(); // gsn address
        gtp.valIMSI = imsi; // imsi
        gtp.valRecovery = 1; // first retry
        gtp.valSelectMode = 1; // apn provided, not verified
        gtp.valTeid1 = teidLoc | 1; // tunnel endpoint id
        gtp.valTeidCp = teidLoc | 2; // tunnel endpoint id
        gtp.valNSAPI = 0; // nsapi
        gtp.valChargChar = 0x800; // normal charging
        gtp.fillEndUserAddr(cfger, false);
        gtp.valAccessPointName = apn; // apn name
        gtp.valIMEI = imei; // imei
        gtp.valMSISDN = "19" + isdn; // msisdn
        gtp.valQOSpro = 0xb921f; // best effort
        connC.send2net(gtp.createPacket());
        if (debugger.clntGtpTraf) {
            logger.debug("tx " + gtp.dump());
        }
        for (int i = 0;; i++) {
            bits.sleep(1000);
            if (!working) {
                return;
            }
            if (lastCtrl.msgTyp == packGtp.typCreateRep) {
                break;
            }
            if (i > 8) {
                return;
            }
        }
        if (lastCtrl.valCause != 0x80) {
            return;
        }
        teidCtr = lastCtrl.valTeidCp;
        teidDat = lastCtrl.valTeid1;
        if (cfger.ppp == null) {
            if (lastCtrl.valEndUserAddr4 != null) {
                cfger.addr4changed(lastCtrl.valEndUserAddr4, cfger.mask4, null);
            }
            if (lastCtrl.valEndUserAddr6 != null) {
                cfger.addr6changed(lastCtrl.valEndUserAddr6, cfger.mask6, null);
            }
        }
        for (int i = 0;;) {
            bits.sleep(1000);
            if (!working) {
                return;
            }
            if (connC.txBytesFree() < 0) {
                return;
            }
            if (connD.txBytesFree() < 0) {
                return;
            }
            i++;
            if (i < 30) {
                continue;
            }
            i = 0;
            gtp = new packGtp();
            gtp.seqNum = seqCtr++;
            gtp.msgTyp = packGtp.typEchoReq;
            connC.send2net(gtp.createPacket());
            if (debugger.clntGtpTraf) {
                logger.debug("tx " + gtp.dump());
            }
        }
    }

    private void sendStop() {
        if (teidCtr == 0) {
            return;
        }
        packGtp gtp = new packGtp();
        gtp.seqNum = seqCtr++;
        gtp.tunId = teidCtr;
        gtp.msgTyp = packGtp.typDeleteReq;
        gtp.valNSAPI = 0;
        gtp.valTeardown = 0xff;
        connC.send2net(gtp.createPacket());
        if (debugger.clntGtpTraf) {
            logger.debug("tx " + gtp.dump());
        }
    }

    private void clearState() {
        if (connC != null) {
            sendStop();
            connC.setClosing();
        }
        if (connD != null) {
            connD.setClosing();
        }
        teidLoc = bits.randomW() << 8;
        teidCtr = 0;
        teidDat = 0;
        seqCtr = 1;
        seqDat = 1;
        lastCtrl = new packGtp();
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept connection
     *
     * @param id connection
     * @return false on success, true on error
     */
    public boolean datagramAccept(prtGenConn id) {
        return true;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * close connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
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
     * receive packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        cntr.rx(pck);
        if (connD != null) {
            if (id.compare(id, connD) == 0) {
                packGtp gtp = new packGtp();
                if (gtp.parseHeader(pck)) {
                    return false;
                }
                if (cfger.ppp != null) {
                    pck.msbPutW(0, 0xff03); // address + control
                    pck.putSkip(2);
                    pck.merge2beg();
                } else {
                    int typ = ifcEther.guessEtherType(pck);
                    if (typ < 0) {
                        logger.info("got bad protocol from " + target);
                        cntr.drop(pck, counter.reasons.badProto);
                        return false;
                    }
                    pck.msbPutW(0, typ); // ethertype
                    pck.putSkip(2);
                    pck.merge2beg();
                }
                upper.recvPack(pck);
                return false;
            }
        }
        if (connC != null) {
            if (id.compare(id, connC) == 0) {
                packGtp gtp = new packGtp();
                if (gtp.parseHeader(pck)) {
                    return false;
                }
                for (;;) {
                    if (gtp.parseExtHdr(pck)) {
                        break;
                    }
                }
                gtp.parsePacket(pck);
                if (debugger.clntGtpTraf) {
                    logger.debug("rx " + gtp.dump());
                }
                if (gtp.msgTyp == packGtp.typEchoReq) {
                    gtp.msgTyp = packGtp.typEchoRep;
                    connC.send2net(gtp.createPacket());
                    if (debugger.clntGtpTraf) {
                        logger.debug("tx " + gtp.dump());
                    }
                    return false;
                }
                lastCtrl = gtp;
                return false;
            }
        }
        id.setClosing();
        return true;
    }

}
