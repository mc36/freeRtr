package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
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
        userFormat res = new userFormat("|", "peer|loc|rem|loc|rem|tx|rx|dr|tx|rx|dr|since|ago", "1|2open|2close|3pack|3byte|2time");
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
        return peer + "|" + openLoc.length + "|" + openRem.length + "|" + pipeLoc.isClosed() + "|" + pipeRem.isClosed() + "|" + cntr.getShPsum() + "|" + cntr.getShBsum() + "|" + bits.time2str(cfgAll.timeZoneName, created + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(created);
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
            if (doWorkRes(pck)) {
                pipeRem.setClose();
            }
            for (;;) {
                if (doWorkLoc(pck)) {
                    continue;
                }
                if (doWorkRem(pck)) {
                    continue;
                }
                bits.sleep(1000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    public boolean doWorkLoc(packHolder pck) {
        if (pipeLoc.isClosed() != 0) {
            int i = pipeRem.ready2rx();
            if (i > 0) {
                pipeRem.nonBlockSkip(i);
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
        if (pipeLoc.ready2rx() < 1) {
            return false;
        }
        pck.clear();
        if (pck.pipeRecv(pipeLoc, 0, rtrBgpUtil.sizeU, 144) != rtrBgpUtil.sizeU) {
            return false;
        }
        if (rtrBgpUtil.checkHeader(pck)) {
            return false;
        }
        int len = pck.IPsiz;
        int typ = pck.IPprt;
        pck.clear();
        if (len > 0) {
            if (pck.pipeRecv(pipeLoc, 0, len, 144) != len) {
                return false;
            }
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

    public boolean doWorkRem(packHolder pck) {
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
        if (pipeRem.ready2rx() < 1) {
            return false;
        }
        pck.clear();
        if (pck.pipeRecv(pipeRem, 0, rtrBgpUtil.sizeU, 144) != rtrBgpUtil.sizeU) {
            return false;
        }
        if (rtrBgpUtil.checkHeader(pck)) {
            return false;
        }
        int len = pck.IPsiz;
        int typ = pck.IPprt;
        pck.clear();
        if (len > 0) {
            if (pck.pipeRecv(pipeRem, 0, len, 144) != len) {
                return false;
            }
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
