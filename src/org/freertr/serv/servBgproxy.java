package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntDns;
import org.freertr.enc.encBase64;
import org.freertr.enc.encUrl;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtRedun;
import org.freertr.prt.prtRedunClnt;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtTcp;
import org.freertr.rtr.rtrBgp;
import org.freertr.rtr.rtrBgpAttr;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;

/**
 * bgp peer proxy
 *
 * @author matecsaba
 */
public class servBgproxy extends servGeneric implements prtServS, prtRedunClnt {

    /**
     * create instance
     */
    public servBgproxy() {
    }

    /**
     * port number
     */
    public final static int port = 17980;

    /**
     * target vrf
     */
    public cfgVrf trgVrf;

    /**
     * target interface
     */
    public cfgIfc srcIfc;

    /**
     * ha mode
     */
    public boolean haMode;

    /**
     * timeout on connection
     */
    public int timeOut = 60 * 1000;

    /**
     * timeout on connection
     */
    public int keepAlive = 30 * 1000;

    /**
     * buffer size
     */
    public int bufSiz = 65536;

    /**
     * set nexthop ingress
     */
    public boolean nextHopIn = false;

    /**
     * set nexthop egress
     */
    public boolean nextHopOut = false;

    /**
     * logging
     */
    public boolean logging = false;

    /**
     * neighbors
     */
    protected tabGen<servBgproxyNei> neighs = new tabGen<servBgproxyNei>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server bgproxy .*", cmds.tabulator + "port " + port, null),
        new userFilter("server bgproxy .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "ha-mode", null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target", null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "source", null),
        new userFilter("server bgproxy .*", cmds.tabulator + "timeout 60000", null),
        new userFilter("server bgproxy .*", cmds.tabulator + "keepalive 30000", null),
        new userFilter("server bgproxy .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "nexthop-in", null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "nexthop-out", null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "bgproxy";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        cmds.cfgLine(lst, !nextHopIn, beg, "nexthop-in", "");
        cmds.cfgLine(lst, !nextHopOut, beg, "nexthop-out", "");
        cmds.cfgLine(lst, !logging, beg, "logging", "");
        cmds.cfgLine(lst, !haMode, beg, "ha-mode", "");
        if (trgVrf == null) {
            lst.add(beg + "no target");
        } else {
            lst.add(beg + "target " + trgVrf.name);
        }
        if (srcIfc == null) {
            lst.add(beg + "no source");
        } else {
            lst.add(beg + "source " + srcIfc.name);
        }
        lst.add(beg + "timeout " + timeOut);
        lst.add(beg + "keepalive " + keepAlive);
        lst.add(beg + "buffer " + bufSiz);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals(cmds.negated);
        if (neg) {
            a = cmd.word();
        }
        if (a.equals("nexthop-in")) {
            nextHopIn = !neg;
            return false;
        }
        if (a.equals("nexthop-out")) {
            nextHopOut = !neg;
            return false;
        }
        if (a.equals("logging")) {
            logging = !neg;
            return false;
        }
        if (a.equals("ha-mode")) {
            haMode = !neg;
            if (haMode) {
                prtRedun.clientAdd(this, srvName() + " " + srvName);
            } else {
                prtRedun.clientDel(this);
            }
            return false;
        }
        if (a.equals("target")) {
            if (neg) {
                trgVrf = null;
                return false;
            }
            trgVrf = cfgAll.vrfFind(cmd.word(), false);
            if (trgVrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            return false;
        }
        if (a.equals("source")) {
            if (neg) {
                srcIfc = null;
                return false;
            }
            srcIfc = cfgAll.ifcFind(cmd.word(), 0);
            if (srcIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            return false;
        }
        if (a.equals("timeout")) {
            timeOut = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("keepalive")) {
            keepAlive = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "nexthop-in", "update nexthop inbound");
        l.add(null, false, 1, new int[]{-1}, "nexthop-out", "update nexthop outbound");
        l.add(null, false, 1, new int[]{-1}, "logging", "log the hits");
        l.add(null, false, 1, new int[]{-1}, "ha-mode", "save state");
        l.add(null, false, 1, new int[]{2}, "timeout", "set timeout on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 1, new int[]{2}, "keepalive", "set keepalive on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "keepalive in ms");
        l.add(null, false, 1, new int[]{2}, "buffer", "set buffer size on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer in bytes");
        l.add(null, false, 1, new int[]{2}, "target", "set vrf to use");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 1, new int[]{2}, "source", "set interface to use");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (logging) {
            logger.info("connection from " + id.peerAddr);
        }
        pipe.setTime(timeOut);
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.setReady();
        new servBgproxyClnt(this, pipe).startWork();
        return false;
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void redunStateGet(List<String> lst) {
        if (!haMode) {
            return;
        }
        for (int i = 0; i < neighs.size(); i++) {
            servBgproxyNei nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.stateGet(lst);
        }
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean redunStateSet(cmds cmd) {
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            return true;
        }
        servBgproxyNei nei = new servBgproxyNei(this, adr);
        int i = bits.str2num(cmd.word());
        int o = bits.str2num(cmd.word());
        cfgIfc cfg = cfgAll.ifcFind(cmd.word(), 0);
        if (cfg == null) {
            return true;
        }
        nei.ifc = cfg.getFwdIfc(adr);
        if (nei.ifc == null) {
            return true;
        }
        if (trgVrf == null) {
            return true;
        }
        nei.fwd = trgVrf.getFwd(adr);
        nei.tcp = trgVrf.getTcp(adr);
        nei.pipeRem = nei.tcp.streamResume(new pipeLine(bufSiz, false), nei.ifc, i, adr, o, "bgproxy", -1, null, -1, -1);
        if (nei.pipeRem == null) {
            return true;
        }
        nei.pipeRem.setTime(timeOut);
        nei.created = bits.str2long(cmd.word());
        nei.openLoc = encBase64.decodeBytes(cmd.word());
        if (nei.openLoc == null) {
            return true;
        }
        nei.openRem = encBase64.decodeBytes(cmd.word());
        if (nei.openRem == null) {
            return true;
        }
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            nei.afis.add(bits.str2num(a));
        }
        nei.resume = true;
        nei.startWork();
        neighs.put(nei);
        return false;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "peer|afi|loc|rem|loc|rem|tx|rx|dr|tx|rx|dr|since|ago", "2|2open|2close|3pack|3byte|2time");
        for (int i = 0; i < neighs.size(); i++) {
            res.add("" + neighs.get(i));
        }
        return res;
    }

}

class servBgproxyClnt implements Runnable {

    private final servBgproxy parent;

    private final pipeSide pipe;

    public servBgproxyClnt(servBgproxy lower, pipeSide conn) {
        parent = lower;
        pipe = conn;
    }

    public void startWork() {
        logger.startThread(this);
    }

    public void run() {
        try {
            if (doWork()) {
                return;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    public boolean doWork() {
        String a = pipe.lineGet(1);
        cmds cmd = new cmds("api", a);
        for (;;) {
            a = pipe.lineGet(1);
            if (a.length() < 1) {
                break;
            }
        }
        a = cmd.word().toLowerCase();
        if (!a.equals("connect")) {
            return false;
        }
        a = cmd.word();
        encUrl gotUrl = new encUrl();
        gotUrl.fromString("tcp://" + a);
        a = cmd.word().toLowerCase();
        if (!a.startsWith("http/")) {
            return false;
        }
        addrIP adr = clntDns.justResolv(gotUrl.server, 0);
        if (adr == null) {
            return false;
        }
        if (parent.logging) {
            logger.info("connection for " + adr);
        }
        pipe.linePut("HTTP/1.1 200 connected");
        pipe.linePut("Server: " + cfgInit.versionAgent);
        pipe.linePut("");
        servBgproxyNei nei = new servBgproxyNei(parent, adr);
        servBgproxyNei old = parent.neighs.add(nei);
        if (old != null) {
            nei = old;
        } else {
            nei.created = bits.getTime();
            nei.startWork();
        }
        nei.pipeLoc.setClose();
        packHolder pck = new packHolder(true, true);
        pck.putCopy(nei.openRem, 0, 0, nei.openRem.length);
        pck.putSkip(nei.openRem.length);
        pck.merge2beg();
        rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgOpen);
        pck.pipeSend(pipe, 0, pck.dataSize(), 2);
        for (int i = 0; i < nei.afis.size(); i++) {
            Integer o = nei.afis.get(i);
            if (o == null) {
                continue;
            }
            pck.clear();
            pck.msbPutD(0, o);
            pck.putSkip(4);
            pck.merge2beg();
            rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgRefrsh);
            pck.pipeSend(nei.pipeRem, 0, pck.dataSize(), 2);
        }
        nei.pipeLoc = pipe;
        return true;
    }

}

class servBgproxyNei implements Runnable, Comparable<servBgproxyNei> {

    private final servBgproxy parent;

    private final addrIP peer;

    protected tabGen<Integer> afis = new tabGen<Integer>();

    protected pipeSide pipeLoc;

    protected pipeSide pipeRem;

    protected counter cntr = new counter();

    protected byte[] openLoc = new byte[0];

    protected byte[] openRem = new byte[0];

    protected long keeped;

    protected long created;

    protected ipFwd fwd;

    protected prtTcp tcp;

    protected ipFwdIface ifc;

    protected boolean resume;

    public servBgproxyNei(servBgproxy lower, addrIP adr) {
        parent = lower;
        peer = adr.copyBytes();
        pipeLine pl = new pipeLine(parent.bufSiz, false);
        pipeLoc = pl.getSide();
        pipeRem = pl.getSide();
        pipeLoc.setReady();
        pipeRem.setReady();
        pipeLoc.setClose();
        pipeRem.setClose();
        pl.setClose();
    }

    public String toString() {
        return peer + "|" + afis.size() + "|" + openLoc.length + "|" + openRem.length + "|" + pipeLoc.isClosed() + "|" + pipeRem.isClosed() + "|" + cntr.getShPsum() + "|" + cntr.getShBsum() + "|" + bits.time2str(cfgAll.timeZoneName, created + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(created);
    }

    public int compareTo(servBgproxyNei o) {
        return peer.compareTo(o.peer);
    }

    public void startWork() {
        if (parent.logging) {
            logger.info("starting for " + peer);
        }
        logger.startThread(this);
    }

    public void run() {
        try {
            packHolder pck = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            if (doWorkRes(pck)) {
                pipeRem.setClose();
            }
            for (;;) {
                if (doWorkLoc(pck, hlp)) {
                    continue;
                }
                if (doWorkRem(pck, hlp)) {
                    continue;
                }
                bits.sleep(1000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    public static int doGetPack(pipeSide pip, packHolder pck) {
        if (pip.ready2rx() < rtrBgpUtil.sizeU) {
            return -1;
        }
        pck.clear();
        if (pck.pipeRecv(pip, 0, rtrBgpUtil.sizeU, 144) != rtrBgpUtil.sizeU) {
            return -1;
        }
        if (rtrBgpUtil.checkHeader(pck)) {
            return -1;
        }
        int len = pck.IPsiz;
        int typ = pck.IPprt;
        pck.clear();
        if (len > 0) {
            if (pck.pipeRecv(pip, 0, len, 144) != len) {
                return -1;
            }
        }
        return typ;
    }

    public boolean doWorkLoc(packHolder pck, packHolder hlp) {
        if (pipeLoc.isClosed() != 0) {
            if (doGetPack(pipeRem, pck) >= 0) {
                cntr.rx(pck);
            }
            long tim = bits.getTime();
            if ((tim - keeped) < parent.keepAlive) {
                return false;
            }
            keeped = tim;
            pck.clear();
            rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgKeepLiv);
            cntr.drop(pck, counter.reasons.notUp);
            pck.pipeSend(pipeRem, 0, pck.dataSize(), 2);
            return false;
        }
        int typ = doGetPack(pipeLoc, pck);
        if (typ < 0) {
            return false;
        }
        cntr.tx(pck);
        switch (typ) {
            case rtrBgpUtil.msgOpen:
                openLoc = pck.getCopy();
                return false;
            case rtrBgpUtil.msgRefrsh:
                processFrsh(pck);
                break;
            case rtrBgpUtil.msgUpdate:
                if (parent.nextHopOut) {
                    processUpdt(pck, hlp, ifc.addr);
                }
                break;
            default:
                break;
        }
        rtrBgpUtil.createHeader(pck, typ);
        pck.pipeSend(pipeRem, 0, pck.dataSize(), 2);
        return true;
    }

    public boolean doWorkRes(packHolder pck) {
        if (!resume) {
            return false;
        }
        resume = false;
        pipeRem.setReady();
        pipeRem.wait4ready(parent.timeOut);
        if (pipeRem.isReady() != 3) {
            return true;
        }
        if (rtrBgpUtil.scanForHeader(pipeRem)) {
            return true;
        }
        return false;
    }

    public boolean doWorkRem(packHolder pck, packHolder hlp) {
        if (pipeRem.isClosed() != 0) {
            if (parent.trgVrf == null) {
                return false;
            }
            if (openLoc.length < 1) {
                return false;
            }
            fwd = parent.trgVrf.getFwd(peer);
            tcp = parent.trgVrf.getTcp(peer);
            if (parent.srcIfc != null) {
                ifc = parent.srcIfc.getFwdIfc(peer);
            } else {
                ifc = ipFwdTab.findSendingIface(fwd, peer);
            }
            if (ifc == null) {
                return false;
            }
            pipeSide res = tcp.streamConnect(new pipeLine(parent.bufSiz, false), ifc, 0, peer.copyBytes(), rtrBgp.port, "bgproxy", -1, null, -1, -1);
            if (res == null) {
                return false;
            }
            res.setTime(parent.timeOut);
            res.setReady();
            res.wait4ready(parent.timeOut);
            if (res.isReady() != 3) {
                res.setClose();
                return false;
            }
            pipeRem = res;
            pipeLoc.setClose();
            pck.clear();
            pck.putCopy(openLoc, 0, 0, openLoc.length);
            pck.putSkip(openLoc.length);
            pck.merge2beg();
            rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgOpen);
            pck.pipeSend(pipeRem, 0, pck.dataSize(), 2);
            return false;
        }
        int typ = doGetPack(pipeRem, pck);
        if (typ < 0) {
            return false;
        }
        cntr.rx(pck);
        switch (typ) {
            case rtrBgpUtil.msgOpen:
                openRem = pck.getCopy();
                pipeLoc.setClose();
                return false;
            case rtrBgpUtil.msgRefrsh:
                processFrsh(pck);
                break;
            case rtrBgpUtil.msgUpdate:
                if (parent.nextHopIn) {
                    processUpdt(pck, hlp, peer);
                }
                break;
            default:
                break;
        }
        rtrBgpUtil.createHeader(pck, typ);
        pck.pipeSend(pipeLoc, 0, pck.dataSize(), 2);
        return true;
    }

    public void processFrsh(packHolder pck) {
        int i = pck.msbGetD(0);
        i &= rtrBgpUtil.frsMask;
        afis.add(i);
    }

    public void processUpdt(packHolder pck, packHolder hlp, addrIP adr) {
        pck = pck.copyBytes(false, false);
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        pck.getSkip(i);
        i = pck.msbGetW(0);
        pck.getSkip(2);
        pck.setDataSize(i);
        for (;;) {
            i = pck.dataSize();
            if (i <= 0) {
                break;
            }
            if (rtrBgpAttr.parseAttrib(pck, hlp)) {
                break;
            }
            switch (hlp.ETHtype) {
                case rtrBgpUtil.attrNextHop:
                    if (!adr.isIPv4()) {
                        break;
                    }
                    afis.add(rtrBgpUtil.safiIp4uni);
                    packHolder cur = pck.copyBytes(false, false);
                    cur.getSkip(-hlp.dataSize());
                    cur.unMergeBytes(addrIPv4.size);
                    cur.putAddr(-addrIPv4.size, adr.toIPv4());
                    cur.merge2beg();
                    break;
                case rtrBgpUtil.attrReachable:
                    cur = pck.copyBytes(false, false);
                    cur.getSkip(-hlp.dataSize());
                    i = rtrBgpUtil.triplet2safi(cur.msbGetD(0));
                    i &= rtrBgpUtil.frsMask;
                    afis.add(i);
                    i = cur.getByte(3);
                    cur.getSkip(4);
                    if (adr.isIPv4()) {
                        if (i > addrIPv4.size) {
                            break;
                        }
                        cur.unMergeBytes(addrIPv4.size);
                        cur.putAddr(-addrIPv4.size, adr.toIPv4());
                        cur.merge2beg();
                    } else {
                        if (i < addrIPv6.size) {
                            break;
                        }
                        cur.unMergeBytes(addrIPv6.size);
                        cur.putAddr(-addrIPv6.size, adr.toIPv6());
                        cur.merge2beg();
                    }
                    break;
            }
        }
    }

    public void stateGet(List<String> lst) {
        if (openRem.length < 1) {
            return;
        }
        if (openLoc.length < 1) {
            return;
        }
        if ((pipeRem.isClosed() != 0) && (pipeLoc.isClosed() != 0)) {
            return;
        }
        prtGenConn sock = tcp.findOneConn(pipeRem);
        if (sock == null) {
            return;
        }
        sock.restartable = true;
        String a = "";
        for (int i = 0; i < afis.size(); i++) {
            a += " " + afis.get(i);
        }
        lst.add(peer + " " + sock.portLoc + " " + sock.portRem + " " + sock.iface + " " + created + " " + encBase64.encodeBytes(openLoc) + " " + encBase64.encodeBytes(openRem) + a);
    }

}
