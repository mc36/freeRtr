package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packLdp;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * label distribution protocol (rfc5036) interface
 *
 * @author matecsaba
 */
public class rtrLdpIface implements prtServP {

    /**
     * discovery hello interval
     */
    public int discHelloIntrvl = 5000;

    /**
     * discovery hello hold time
     */
    public int discHelloHldtm = 15000;

    /**
     * targeted hello interval
     */
    public int trgtHelloIntrvl = 10000;

    /**
     * targeted hello hold time
     */
    public int trgtHelloHldtm = 90000;

    /**
     * session hello interval
     */
    public int sessHelloIntrvl = 60000;

    /**
     * session hello hold time
     */
    public int sessHelloHldtm = 180000;

    /**
     * session ttl to use
     */
    public int sessionTTL = -1;

    /**
     * session tos to use
     */
    public int sessionTOS = -1;

    /**
     * input label filter
     */
    public tabListing<tabPrfxlstN, addrIP> filterIn;

    /**
     * output label filter
     */
    public tabListing<tabPrfxlstN, addrIP> filterOut;

    /**
     * listen filter
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filterPeer;

    /**
     * advertise label pop
     */
    public boolean labelPop;

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
        cmds.cfgLine(l, ldp.filterPeer == null, cmds.tabulator, "mpls label" + ver + "peer", "" + ldp.filterPeer);
        cmds.cfgLine(l, ldp.filterIn == null, cmds.tabulator, "mpls label" + ver + "in", "" + ldp.filterIn);
        cmds.cfgLine(l, ldp.filterOut == null, cmds.tabulator, "mpls label" + ver + "out", "" + ldp.filterOut);
        cmds.cfgLine(l, !ldp.labelPop, cmds.tabulator, "mpls label" + ver + "pop", "");
        l.add(cmds.tabulator + "mpls label" + ver + "sig discovery " + ldp.discHelloIntrvl + " " + ldp.discHelloHldtm);
        l.add(cmds.tabulator + "mpls label" + ver + "sig session " + ldp.sessHelloIntrvl + " " + ldp.sessHelloHldtm);
        l.add(cmds.tabulator + "mpls label" + ver + "sig target " + ldp.trgtHelloIntrvl + " " + ldp.trgtHelloHldtm);
        l.add(cmds.tabulator + "mpls label" + ver + "sig tos " + ldp.sessionTOS);
        l.add(cmds.tabulator + "mpls label" + ver + "sig ttl " + ldp.sessionTTL);
    }

    /**
     * get generic config
     *
     * @param ldp config source
     * @param beg command
     * @param cmd commands
     */
    public static void doConfig(rtrLdpIface ldp, String beg, cmds cmd) {
        if (ldp == null) {
            cmd.error("not enabled");
            return;
        }
        beg = beg.substring(6, beg.length());
        if (beg.equals("pop")) {
            ldp.labelPop = true;
            return;
        }
        if (beg.equals("peer")) {
            cfgAceslst res = cfgAll.aclsFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such access list");
                return;
            }
            ldp.filterPeer = res.aceslst;
            return;
        }
        if (beg.equals("in")) {
            cfgPrfxlst res = cfgAll.prfxFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such prefix list");
                return;
            }
            ldp.filterIn = res.prflst;
            return;
        }
        if (beg.equals("out")) {
            cfgPrfxlst res = cfgAll.prfxFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such prefix list");
                return;
            }
            ldp.filterOut = res.prflst;
            return;
        }
        if (!beg.equals("sig")) {
            cmd.badCmd();
            return;
        }
        beg = cmd.word();
        if (beg.equals("discovery")) {
            ldp.discHelloIntrvl = bits.str2num(cmd.word());
            ldp.discHelloHldtm = bits.str2num(cmd.word());
            return;
        }
        if (beg.equals("session")) {
            ldp.sessHelloIntrvl = bits.str2num(cmd.word());
            ldp.sessHelloHldtm = bits.str2num(cmd.word());
            return;
        }
        if (beg.equals("target")) {
            ldp.trgtHelloIntrvl = bits.str2num(cmd.word());
            ldp.trgtHelloHldtm = bits.str2num(cmd.word());
            return;
        }
        if (beg.equals("tos")) {
            ldp.sessionTOS = bits.str2num(cmd.word());
            return;
        }
        if (beg.equals("ttl")) {
            ldp.sessionTTL = bits.str2num(cmd.word());
            return;
        }
        cmd.badCmd();
        return;
    }

    /**
     * get generic config
     *
     * @param ldp config source
     * @param beg command
     * @param cmd commands
     */
    public static void unConfig(rtrLdpIface ldp, String beg, cmds cmd) {
        if (ldp == null) {
            cmd.error("not enabled");
            return;
        }
        beg = beg.substring(6, beg.length());
        if (beg.equals("pop")) {
            ldp.labelPop = false;
            return;
        }
        if (beg.equals("peer")) {
            ldp.filterPeer = null;
            return;
        }
        if (beg.equals("in")) {
            ldp.filterIn = null;
            return;
        }
        if (beg.equals("out")) {
            ldp.filterOut = null;
            return;
        }
        if (!beg.equals("sig")) {
            cmd.badCmd();
            return;
        }
        beg = cmd.word();
        if (beg.equals("tos")) {
            ldp.sessionTOS = -1;
            return;
        }
        if (beg.equals("ttl")) {
            ldp.sessionTTL = -1;
            return;
        }
        cmd.badCmd();
        return;
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        udp.listenStop(ifc, packLdp.port, null, 0);
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
        udp.packetListen(this, ifc, packLdp.port, null, 0, "ldp", -1, null, -1, -1);
        conn = udp.packetConnect(this, ifc, packLdp.port, adr, packLdp.port, "ldp", -1, null, -1, -1);
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
        if (id.compareTo(conn) != 0) {
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
        if (stat == state.states.up) {
            return false;
        }
        rtrLdpNeigh ntry = ip.ldpNeighFind(id.peerAddr, false);
        if (ntry == null) {
            return false;
        }
        ntry.stopPeer();
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        packLdp pk = new packLdp();
        pk.pack = pck;
        if (pk.parseLDPheader()) {
            id.setClosing();
            return false;
        }
        if (pk.parseMSGheader()) {
            id.setClosing();
            return false;
        }
        if (pk.getHelloParam()) {
            id.setClosing();
            return false;
        }
        pk.transAddr = id.peerAddr.copyBytes();
        pk.getTransAddr();
        if (debugger.rtrLdpEvnt) {
            logger.debug("rx hello " + id);
        }
        boolean trg = false;
        if (ip.connedR.route(id.peerAddr) == null) {
            if (filterPeer == null) {
                logger.warn("got from out of subnet peer " + id);
                id.setClosing();
                return false;
            }
            if (!filterPeer.matches(conn)) {
                logger.warn("got from unwanted peer " + id);
                id.setClosing();
                return false;
            }
            if (debugger.rtrLdpEvnt) {
                logger.debug("tx hello " + conn.peerAddr);
            }
            packLdp pr = new packLdp();
            pr.lsrID = ifc.addr.toIPv4();
            pr.transAddr = ifc.addr.copyBytes();
            pr.msgTyp = packLdp.msgThello;
            pr.holdTime = trgtHelloHldtm / 1000;
            pr.targeted = true;
            pr.putHelloParam();
            pr.putTransAddr();
            pr.createLDPheader();
            id.send2net(pr.pack);
            trg = true;
        }
        id.setClosing();
        rtrLdpNeigh ntry = ip.ldpNeighFind(id.peerAddr, true);
        ntry.helloIfc = !trg;
        ntry.helloTrg = trg;
        if (ntry.udp != null) {
            return false;
        }
        ntry.ifc = src;
        ntry.udp = udp;
        ntry.tcp = tcp;
        ntry.trans = pk.transAddr;
        ntry.lsrID = pk.lsrID;
        ntry.sessHelloHldtm = sessHelloHldtm;
        ntry.sessHelloIntrvl = sessHelloIntrvl;
        ntry.sessionTTL = sessionTTL;
        ntry.sessionTOS = sessionTOS;
        ntry.filterIn = filterIn;
        ntry.filterOut = filterOut;
        ntry.labelPop = labelPop;
        ntry.startPeer();
        return false;
    }

}
