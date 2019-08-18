package rtr;

import ip.ipFwd;
import ip.ipFwdIface;
import pack.packHolder;
import pack.packLdp;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtTcp;
import prt.prtUdp;
import util.debugger;
import util.logger;
import addr.addrIP;
import cfg.cfgIfc;
import java.util.List;
import tab.tabListing;
import tab.tabPrfxlstN;
import util.cmds;

/**
 * label distribution protocol (rfc5036) interface
 *
 * @author matecsaba
 */
public class rtrLdpIface implements prtServP {

    /**
     * discovery hello interval
     */
    public final static int discHelloIntrvl = 5000;

    /**
     * discovery hello hold time
     */
    public final static int discHelloHldtm = 15000;

    /**
     * targeted hello interval
     */
    public final static int trgtHelloIntrvl = 10000;

    /**
     * targeted hello hold time
     */
    public final static int trgtHelloHldtm = 90000;

    /**
     * session hello interval
     */
    public final static int sessHelloIntrvl = 60000;

    /**
     * session hello hold time
     */
    public final static int sessHelloHldtm = 180000;

    /**
     * input label filter
     */
    public tabListing<tabPrfxlstN, addrIP> filterIn;

    /**
     * output label filter
     */
    public tabListing<tabPrfxlstN, addrIP> filterOut;

    private ipFwd ip;

    private prtUdp udp;

    private prtTcp tcp;

    private ipFwdIface ifc;

    private ipFwdIface src;

    private cfgIfc trn;

    private prtGenConn conn;

    /**
     * create one instance
     *
     * @param fwdr the ip forwarder
     * @param dgrm udp handler
     * @param strm tcp handler
     * @param iface the ip interface to work on
     * @param trans the ip interface to source from
     * @param trnsc the interface to source from
     */
    public rtrLdpIface(ipFwd fwdr, prtUdp dgrm, prtTcp strm, ipFwdIface iface, ipFwdIface trans, cfgIfc trnsc) {
        ip = fwdr;
        udp = dgrm;
        tcp = strm;
        ifc = iface;
        src = trans;
        trn = trnsc;
    }

    /**
     * get interface config
     *
     * @param ldp config source
     * @param ifc interface on
     * @return config string
     */
    public static String getLdpCfg(rtrLdpIface ldp, cfgIfc ifc) {
        if (ldp == null) {
            return "";
        }
        if (ifc.name.equals(ldp.trn.name)) {
            return "";
        }
        return ldp.trn.name;
    }

    /**
     * get generic config
     *
     * @param ldp config source
     * @param ver ip version
     * @param l list to append
     */
    public static void getGenCfg(rtrLdpIface ldp, int ver, List<String> l) {
        if (ldp == null) {
            return;
        }
        cmds.cfgLine(l, ldp.filterIn == null, cmds.tabulator, "mpls label" + ver + "in", "" + ldp.filterIn);
        cmds.cfgLine(l, ldp.filterOut == null, cmds.tabulator, "mpls label" + ver + "out", "" + ldp.filterOut);
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        udp.listenStop(ifc, packLdp.port, null, 0, 0);
        conn.setClosing();
    }

    /**
     * register to udp
     */
    public void register2udp() {
        addrIP adr = new addrIP();
        if (ifc.addr.isIPv4()) {
            adr.fromString("224.0.0.2");
        } else {
            adr.fromString("ff02::2");
        }
        udp.packetListen(this, ifc, packLdp.port, null, 0, 0, "ldp", null, -1);
        conn = udp.packetConnect(this, ifc, packLdp.port, adr, packLdp.port, "ldp", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 0;
        conn.workInterval = discHelloIntrvl;
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * start connection
     *
     * @param id connection
     * @return false if success, true if error
     */
    public boolean datagramAccept(prtGenConn id) {
        id.timeout = discHelloHldtm;
        id.workInterval = discHelloIntrvl;
        return false;
    }

    /**
     * stop connection
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
        if (conn == null) {
            return;
        }
        if (id.compare(id, conn) != 0) {
            return;
        }
        if (debugger.rtrLdpEvnt) {
            logger.debug("tx hello " + id);
        }
        packLdp pk = new packLdp();
        pk.lsrID = src.addr.toIPv4();
        pk.transAddr = src.addr.copyBytes();
        pk.msgTyp = packLdp.msgThello;
        pk.holdTime = discHelloHldtm / 1000;
        pk.putHelloParam();
        pk.putTransAddr();
        pk.createLDPheader();
        id.send2net(pk.pack);
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        id.setClosing();
        packLdp pk = new packLdp();
        pk.pack = pck;
        if (pk.parseLDPheader()) {
            return false;
        }
        if (pk.parseMSGheader()) {
            return false;
        }
        if (pk.getHelloParam()) {
            return false;
        }
        pk.transAddr = id.peerAddr.copyBytes();
        pk.getTransAddr();
        if (debugger.rtrLdpEvnt) {
            logger.debug("rx hello " + id);
        }
        rtrLdpNeigh ntry = ip.ldpNeighFind(src, id.peerAddr, true);
        if (ntry == null) {
            return false;
        }
        ntry.helloIfc = true;
        if (ntry.udp != null) {
            return false;
        }
        ntry.udp = udp;
        ntry.tcp = tcp;
        ntry.trans = pk.transAddr;
        ntry.lsrID = pk.lsrID;
        ntry.sessHelloHldtm = sessHelloHldtm;
        ntry.sessHelloIntrvl = sessHelloIntrvl;
        ntry.filterIn = filterIn;
        ntry.filterOut = filterOut;
        ntry.startPeer();
        return false;
    }

}
