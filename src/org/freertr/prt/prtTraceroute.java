package org.freertr.prt;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntDns;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdEcho;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipPrt;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.notifier;
import org.freertr.util.state;

/**
 * traceroute helper
 *
 * @author matecsaba
 */
public class prtTraceroute implements prtServP, ipPrt {

    /**
     * create instance
     */
    public prtTraceroute() {
    }

    /**
     * vrf to use
     */
    public cfgVrf vrf;

    /**
     * iface to use
     */
    public cfgIfc ifc;

    /**
     * target address
     */
    public addrIP trg;

    /**
     * target port
     */
    public int port;

    /**
     * target protocol
     */
    public int proto;

    /**
     * router process to use for lookups
     */
    public ipRtr routerPrc;

    /**
     * dns client to use for lookups
     */
    public clntDns domainCln;

    /**
     * dns servers to use for lookups
     */
    public List<addrIP> domainLst;

    /**
     * reporting router
     */
    public addrIP errRtr;

    /**
     * reported error
     */
    public counter.reasons errCod;

    /**
     * reported label
     */
    public int errLab;

    /**
     * reported time
     */
    public int errTim;

    /**
     * reported name
     */
    public String domainNam;

    /**
     * reported route
     */
    public tabRouteAttr<addrIP> routerNtry;

    private prtGenConn con;

    private ipFwdIface ifc2;

    private ipFwd fwd;

    private prtUdp udp;

    private notifier notif = new notifier();

    private int magic;

    private long started;

    private int lasTtl;

    public String toString() {
        return "traceroute to " + trg;
    }

    /**
     * register to protocol
     *
     * @return false on success, true on error
     */
    public boolean register2ip() {
        if (trg == null) {
            return true;
        }
        if (vrf == null) {
            return true;
        }
        fwd = vrf.getFwd(trg);
        udp = vrf.getUdp(trg);
        ifc2 = null;
        if (ifc != null) {
            ifc2 = ifc.getFwdIfc(trg);
        } else {
            ifc2 = ipFwdTab.findSendingIface(fwd, trg);
        }
        if (ifc2 == null) {
            return true;
        }
        if (proto > 0) {
            return fwd.protoAdd(this, ifc2, trg);
        } else {
            con = udp.packetConnect(this, ifc2, 0, trg, port, "traceroute", -1, null, -1, -1);
            return con == null;
        }
    }

    /**
     * unregister from protocol
     */
    public void unregister2ip() {
        if (proto > 0) {
            fwd.protoDel(this, ifc2, trg);
        } else {
            if (con != null) {
                con.setClosing();
            }
            con = null;
        }
    }

    /**
     * do the first round
     *
     * @param ttl hop number
     * @param tos type of service
     * @param flw flow label
     * @param tim timeout
     * @param len size
     * @return true on error, false on success
     */
    public boolean doRound1(int ttl, int tos, int flw, int tim, int len) {
        lasTtl = ttl;
        errRtr = null;
        errLab = -1;
        errCod = null;
        errTim = tim;
        if (con != null) {
            con.sendTOS = tos;
            con.sendTTL = ttl;
            con.sendFLW = flw;
        }
        packHolder pck = new packHolder(true, true);
        pck.putDefaults();
        magic = bits.randomD();
        pck.msbPutD(0, magic);
        pck.putSkip(len);
        pck.merge2beg();
        pck.IPttl = ttl;
        pck.IPtos = tos;
        pck.IPid = flw;
        started = bits.getTime();
        pck.IPprt = proto;
        pck.IPsrc.setAddr(ifc2.addr);
        pck.IPtrg.setAddr(trg);
        if (proto > 0) {
            fwd.protoPack(ifc2, null, pck);
        } else {
            con.send2net(pck);
        }
        if (tim > 0) {
            notif.misleep(tim);
        }
        return errRtr == null;
    }

    /**
     * do the second round
     *
     * @return true on error, false on success
     */
    public boolean doRound2() {
        domainNam = null;
        routerNtry = null;
        if (errRtr == null) {
            return true;
        }
        if (routerPrc != null) {
            tabRouteEntry<addrIP> ntry = routerPrc.routerComputedU.route(errRtr);
            if (ntry != null) {
                routerNtry = new tabRouteAttr<addrIP>();
                ntry.best.copyBytes(routerNtry, true);
            }
        }
        if (domainCln == null) {
            return false;
        }
        if (domainLst == null) {
            return false;
        }
        domainCln.doResolvList(domainLst, packDnsRec.generateReverse(errRtr), false, packDnsRec.typePTR);
        domainNam = domainCln.getPTR();
        return false;
    }

    /**
     * get header line of traceroute
     *
     * @return the text
     */
    public String getHeadLine() {
        domainNam = null;
        routerNtry = null;
        lasTtl = 0;
        errRtr = null;
        errLab = -1;
        errCod = null;
        errTim = 0;
        tabRouteEntry<addrIP> ntry = fwd.actualU.route(trg);
        if (ntry == null) {
            return "via nowhere";
        }
        routerNtry = new tabRouteAttr<addrIP>();
        ntry.best.copyBytes(routerNtry, true);
        return "via " + addrPrefix.ip2str(ntry.prefix) + " " + routerNtry.toShRoute().replaceAll("\\|", " ");
    }

    /**
     * get current line of traceroute
     *
     * @return the text
     */
    public String getCurrLine() {
        String a = "";
        if (errLab > 0) {
            a += ", mpls=" + errLab;
        }
        if (domainNam != null) {
            a += ", name=" + domainNam;
        }
        if (routerNtry != null) {
            a += ", path=" + routerNtry.asPathStr() + ", name=" + routerNtry.asNameStr() + ", info=" + routerNtry.asInfoStr();
        }
        return lasTtl + " " + errRtr + " time=" + errTim + a;
    }

    /**
     * closed interface
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
     * connection closed
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

    private void gotPack(packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        if (pck.msbGetD(0) != magic) {
            return;
        }
        magic++;
        errTim = (int) (bits.getTime() - started);
        errRtr = rtr.copyBytes();
        errCod = err;
        errLab = lab;
        notif.wakeup();
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
        gotPack(pck, rtr, err, lab);
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
        gotPack(pck, pck.IPsrc, null, -1);
        return false;
    }

    public int getProtoNum() {
        return proto;
    }

    public void closeUp(ipFwdIface iface) {
    }

    public void setState(ipFwdIface iface, state.states stat) {
    }

    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        gotPack(pck, pck.IPsrc, null, -1);
    }

    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return false;
    }

    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
        int lab = ipFwdEcho.getMplsExt(pck);
        gotPack(pck, rtr, err, lab);
    }

    public counter getCounter() {
        return new counter();
    }

}
