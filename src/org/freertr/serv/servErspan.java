package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgBrdg;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packErspan;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * encapsulated remote switch port analizer (erspan) server
 *
 * @author matecsaba
 */
public class servErspan extends servGeneric implements ipPrt {

    /**
     * create instance
     */
    public servErspan() {
    }

    /**
     * interface to use
     */
    public cfgBrdg brdgIfc;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * sending df value, -1 means maps out
     */
    public int sendingDFN = -1;

    /**
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * physical interface
     */
    public boolean physInt = false;

    /**
     * list of connections
     */
    public tabGen<servErspanConn> conns = new tabGen<servErspanConn>();

    /**
     * vlan id
     */
    public int vlan = 0;

    /**
     * span id
     */
    public int span = 0;

    /**
     * timeout
     */
    public int timeout = 60000;

    /**
     * counter
     */
    public counter cntr;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server erspan .*", cmds.tabulator + "port " + packErspan.prot, null),
        new userFilter("server erspan .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server erspan .*", cmds.tabulator + cmds.negated + cmds.tabulator + "physical-interface", null),
        new userFilter("server erspan .*", cmds.tabulator + "vlan 0", null),
        new userFilter("server erspan .*", cmds.tabulator + "span 0", null),
        new userFilter("server erspan .*", cmds.tabulator + "timeout 60000", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.number);
        }
        cmds.cfgLine(l, !physInt, beg, "physical-interface", "");
        l.add(beg + "vlan " + vlan);
        l.add(beg + "span " + span);
        l.add(beg + "timeout " + timeout);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("vlan")) {
            vlan = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("span")) {
            span = bits.str2num(cmd.word());
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
        if (s.equals("physical-interface")) {
            physInt = true;
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("vlan")) {
            vlan = 0;
            return false;
        }
        if (s.equals("span")) {
            span = 0;
            return false;
        }
        if (s.equals("bridge")) {
            brdgIfc = null;
            return false;
        }
        if (s.equals("physical-interface")) {
            physInt = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "bridge", "set interface to clone");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of bridge");
        l.add(null, false, 1, new int[]{2}, "timeout", "timeout of client");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds");
        l.add(null, false, 1, new int[]{-1}, "physical-interface", "adding as physical to bridge");
        l.add(null, false, 1, new int[]{2}, "vlan", "vlan of client");
        l.add(null, false, 2, new int[]{-1}, "<num>", "vlan id");
        l.add(null, false, 1, new int[]{2}, "span", "span of client");
        l.add(null, false, 2, new int[]{-1}, "<num>", "span id");
    }

    public String srvName() {
        return "erspan";
    }

    public int srvPort() {
        return packErspan.prot;
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
        return packErspan.prot;
    }

    public String toString() {
        return "erspan";
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
        servErspanConn ntry = new servErspanConn(rxIfc, pck.IPsrc, this);
        servErspanConn old = conns.add(ntry);
        if (old != null) {
            old.doRecv(pck);
            return;
        }
        if (brdgIfc == null) {
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

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "addr|iface|for|since");
        for (int i = 0; i < conns.size(); i++) {
            servErspanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.peer + "|" + ntry.brdgIfc.getIfcName() + "|" + bits.timePast(ntry.created) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.created + cfgAll.timeServerOffset, 3));
        }
        return res;
    }

    /**
     * do clear
     *
     * @param peer peer ip
     */
    public void doClear(addrIP peer) {
        for (int i = 0; i < conns.size(); i++) {
            servErspanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (peer.compareTo(ntry.peer) != 0) {
                continue;
            }
            ntry.doStop();
        }
    }

}

class servErspanConn implements Runnable, ifcDn, Comparable<servErspanConn> {

    public servErspan lower;

    public ipFwd fwdCor;

    public ipFwdIface iface;

    public addrIP peer;

    protected ifcBridgeIfc brdgIfc;

    public boolean seenPack;

    public long created;

    private counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    private int seqTx;

    public servErspanConn(ipFwdIface ifc, addrIP adr, servErspan parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
        fwdCor = lower.srvVrf.getFwd(peer);
    }

    public String toString() {
        return "erspan with " + peer;
    }

    public int compareTo(servErspanConn o) {
        int i = iface.compareTo(o.iface);
        if (i != 0) {
            return i;
        }
        return peer.compareTo(o.peer);
    }

    public void doStartup() {
        brdgIfc = lower.brdgIfc.bridgeHed.newIface(lower.physInt, true, false);
        setUpper(brdgIfc);
        created = bits.getTime();
        logger.startThread(this);
    }

    public void doRecv(packHolder pckBin) {
        seenPack = true;
        packErspan pckRx = new packErspan();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        if (pckRx.span != lower.span) {
            cntr.drop(pckBin, counter.reasons.badID);
            return;
        }
        if (pckRx.vlan != lower.vlan) {
            cntr.drop(pckBin, counter.reasons.badID);
            return;
        }
        cntr.rx(pckBin);
        upper.recvPack(pckBin);
    }

    public void doStop() {
        brdgIfc.closeUp();
        fwdCor.protoDel(lower, iface, peer);
        lower.conns.del(this);
    }

    public void run() {
        if (lower.srvCheckAcceptIp(iface, peer, lower)) {
            doStop();
            return;
        }
        for (;;) {
            bits.sleep(lower.timeout);
            if (!seenPack) {
                break;
            }
            seenPack = false;
        }
        doStop();
    }

    public void sendPack(packHolder pckBin) {
        pckBin.merge2beg();
        seqTx++;
        packErspan tx = new packErspan();
        tx.seq = seqTx;
        tx.vlan = lower.vlan;
        tx.span = lower.span;
        tx.createHeader(pckBin);
        cntr.tx(pckBin);
        pckBin.putDefaults();
        if (lower.sendingTTL >= 0) {
            pckBin.IPttl = lower.sendingTTL;
        }
        if (lower.sendingTOS >= 0) {
            pckBin.IPtos = lower.sendingTOS;
        }
        if (lower.sendingDFN >= 0) {
            pckBin.IPdf = lower.sendingDFN == 1;
        }
        if (lower.sendingFLW >= 0) {
            pckBin.IPid = lower.sendingFLW;
        }
        pckBin.IPprt = packErspan.prot;
        pckBin.IPsrc.setAddr(iface.addr);
        pckBin.IPtrg.setAddr(peer);
        fwdCor.protoPack(iface, null, pckBin);
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        doStop();
    }

    public void flapped() {
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

}
