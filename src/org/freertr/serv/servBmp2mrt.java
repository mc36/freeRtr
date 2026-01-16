package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgRtr;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.rtr.rtrBgpMon;
import org.freertr.rtr.rtrBgpMrt;
import org.freertr.rtr.rtrBgpTemp;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logFil;
import org.freertr.util.logger;
import org.freertr.enc.encTlv;

/**
 * bgp monitoring (rfc7854) protocol to multi-threaded routing (rfc6396) toolkit
 *
 * @author matecsaba
 */
public class servBmp2mrt extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servBmp2mrt() {
    }

    /**
     * port number
     */
    public final static int port = 17971;

    /**
     * header size
     */
    public final static int size = 6;

    /**
     * relays
     */
    protected final tabGen<servBmp2mrtRelay> relays = new tabGen<servBmp2mrtRelay>();

    /**
     * listeners
     */
    protected final tabGen<servBmp2mrtLstn> lstns = new tabGen<servBmp2mrtLstn>();

    private boolean bulkDown;

    private final tabGen<servBmp2mrtStat> stats = new tabGen<servBmp2mrtStat>();

    private boolean local;

    private int rateInt;

    private int rateNum;

    private int listenBmp;

    private int listenRis;

    private int listenBgp;

    /**
     * as number
     */
    protected int listenAsn;

    private servBmp2mrtRate rateTim;

    private logFil fileHandle;

    private tabListing<tabAceslstN<addrIP>, addrIP> dynAcl;

    private servBmp2mrtStat dynCfg;

    private rtrBgpTemp dynTmp;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server bmp2mrt .*", cmds.tabulator + "port " + port, null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "local", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "bulk-down", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + "rate-down 0 0", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + "max-time 0", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + "max-pack 0", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + "max-byte 0", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "listen-bmp", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "listen-bgp", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "listen-ris", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "file", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "backup", null),
        new userFilter("server bmp2mrt .*", cmds.tabulator + cmds.negated + cmds.tabulator + "dyneigh", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !local, beg, "local", "");
        if (fileHandle == null) {
            l.add(beg + "no file");
        } else {
            l.add(beg + "file " + fileHandle.name());
            cmds.cfgLine(l, fileHandle.rotateN() == null, beg, "backup", fileHandle.rotateN());
            l.add(beg + "max-time " + fileHandle.rotateT());
            l.add(beg + "max-pack " + fileHandle.rotateL());
            l.add(beg + "max-byte " + fileHandle.rotateS());
        }
        cmds.cfgLine(l, listenBmp < 1, beg, "listen-bmp", "" + listenBmp);
        cmds.cfgLine(l, listenRis < 1, beg, "listen-ris", "" + listenRis);
        cmds.cfgLine(l, listenBgp < 1, beg, "listen-bgp", "" + listenBgp + " " + listenAsn);
        cmds.cfgLine(l, !bulkDown, beg, "bulk-down", "");
        l.add(beg + "rate-down " + rateInt + " " + rateNum);
        if (dynCfg != null) {
            l.add(beg + "dyneigh " + dynAcl.listName + " " + dynCfg.getCfg(false) + " " + dynTmp.tempName);
        } else {
            l.add(beg + "no dyneigh");
        }
        for (int i = 0; i < stats.size(); i++) {
            servBmp2mrtStat stat = stats.get(i);
            if (stat == null) {
                continue;
            }
            if (stat.nei == null) {
                continue;
            }
            if (stat.dyn) {
                continue;
            }
            l.add(beg + "neighbor " + stat.from + " " + stat.peer + " " + stat.getCfg(true));
        }
        for (int i = 0; i < relays.size(); i++) {
            servBmp2mrtRelay rly = relays.get(i);
            if (rly == null) {
                continue;
            }
            l.add(beg + "relay " + rly);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("relay")) {
            servBmp2mrtRelay rly = new servBmp2mrtRelay();
            if (rly.fromString(cmd)) {
                return false;
            }
            if (relays.add(rly) != null) {
                cmd.error("already exists");
                return false;
            }
            rly.startWork();
            return false;
        }
        if (s.equals("rate-down")) {
            rateInt = bits.str2num(cmd.word());
            rateNum = bits.str2num(cmd.word());
            try {
                rateTim.stopWork();
            } catch (Exception e) {
            }
            rateTim = null;
            if (rateInt < 1) {
                return false;
            }
            rateTim = new servBmp2mrtRate(this, rateInt);
            rateTim.startWork();
            return false;
        }
        if (s.equals("listen-bmp")) {
            listenBmp = bits.str2num(cmd.word());
            genStrmStart(this, new pipeLine(32768, false), listenBmp);
            return false;
        }
        if (s.equals("listen-ris")) {
            listenRis = bits.str2num(cmd.word());
            genStrmStart(this, new pipeLine(32768, false), listenRis);
            return false;
        }
        if (s.equals("listen-bgp")) {
            listenBgp = bits.str2num(cmd.word());
            listenAsn = bits.str2num(cmd.word());
            genStrmStart(this, new pipeLine(32768, false), listenBgp);
            return false;
        }
        if (s.equals("bulk-down")) {
            bulkDown = true;
            return false;
        }
        if (s.equals("dyneigh")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return false;
            }
            dynCfg = new servBmp2mrtStat();
            if (dynCfg.fromString(cmd, false)) {
                dynCfg = null;
                dynAcl = null;
                return false;
            }
            dynAcl = acl.aceslst;
            dynTmp = dynCfg.prc.findTemp(cmd.word());
            if (dynTmp == null) {
                cmd.error("no such template");
                dynCfg = null;
                dynAcl = null;
                return false;
            }
            return false;
        }
        if (s.equals("neighbor")) {
            addrIP a1 = new addrIP();
            addrIP a2 = new addrIP();
            a1.fromString(cmd.word());
            a2.fromString(cmd.word());
            servBmp2mrtStat stat = getStat(a1, a2, 2, 0);
            stat.fromString(cmd, true);
            return false;
        }
        if (s.equals("local")) {
            local = true;
            return false;
        }
        if (s.equals("max-time")) {
            if (fileHandle == null) {
                return false;
            }
            fileHandle.rotate(fileHandle.rotateN(), fileHandle.rotateS(), bits.str2num(cmd.word()), fileHandle.rotateL());
            return false;
        }
        if (s.equals("max-pack")) {
            if (fileHandle == null) {
                return false;
            }
            fileHandle.rotate(fileHandle.rotateN(), fileHandle.rotateS(), fileHandle.rotateT(), bits.str2num(cmd.word()));
            return false;
        }
        if (s.equals("max-byte")) {
            if (fileHandle == null) {
                return false;
            }
            fileHandle.rotate(fileHandle.rotateN(), bits.str2long(cmd.word()), fileHandle.rotateT(), fileHandle.rotateL());
            return false;
        }
        if (s.equals("backup")) {
            fileHandle.rotate(cmd.getRemaining(), fileHandle.rotateS(), fileHandle.rotateT(), fileHandle.rotateL());
            return false;
        }
        if (s.equals("file")) {
            try {
                fileHandle.close();
            } catch (Exception e) {
            }
            fileHandle = new logFil(cmd.getRemaining());
            fileHandle.open(false);
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("relay")) {
            servBmp2mrtRelay rly = new servBmp2mrtRelay();
            if (rly.fromString(cmd)) {
                return false;
            }
            rly = relays.del(rly);
            if (rly == null) {
                cmd.error("no such relay");
                return false;
            }
            rly.stopWork();
            return false;
        }
        if (s.equals("rate-down")) {
            rateInt = 0;
            rateNum = 0;
            try {
                rateTim.stopWork();
            } catch (Exception e) {
            }
            rateTim = null;
            return false;
        }
        if (s.equals("listen-bmp")) {
            genericStop(listenBmp);
            listenBmp = 0;
            return false;
        }
        if (s.equals("listen-ris")) {
            genericStop(listenRis);
            listenRis = 0;
            return false;
        }
        if (s.equals("listen-bgp")) {
            genericStop(listenBgp);
            listenBmp = 0;
            listenAsn = 0;
            return false;
        }
        if (s.equals("bulk-down")) {
            bulkDown = false;
            return false;
        }
        if (s.equals("dyneigh")) {
            dynCfg = null;
            dynAcl = null;
            return false;
        }
        if (s.equals("neighbor")) {
            addrIP a1 = new addrIP();
            addrIP a2 = new addrIP();
            a1.fromString(cmd.word());
            a2.fromString(cmd.word());
            servBmp2mrtStat stat = getStat(a1, a2, 0, 0);
            if (stat == null) {
                cmd.error("no such peer");
                return false;
            }
            stat.rouT = null;
            stat.rouI = 0;
            stat.rouD = false;
            stat.nei = null;
            return false;
        }
        if (s.equals("local")) {
            local = false;
            return false;
        }
        if (s.equals("max-time")) {
            if (fileHandle == null) {
                return false;
            }
            fileHandle.rotate(fileHandle.rotateN(), fileHandle.rotateS(), 0, fileHandle.rotateL());
            return false;
        }
        if (s.equals("max-pack")) {
            if (fileHandle == null) {
                return false;
            }
            fileHandle.rotate(fileHandle.rotateN(), fileHandle.rotateS(), fileHandle.rotateT(), 0);
            return false;
        }
        if (s.equals("max-byte")) {
            if (fileHandle == null) {
                return false;
            }
            fileHandle.rotate(fileHandle.rotateN(), 0, fileHandle.rotateT(), fileHandle.rotateL());
            return false;
        }
        if (s.equals("backup")) {
            fileHandle.rotate(null, 0, 0, 0);
            return false;
        }
        if (s.equals("file")) {
            try {
                fileHandle.close();
            } catch (Exception e) {
            }
            fileHandle = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "bulk-down", "down peers on speaker loss");
        l.add(null, false, 1, new int[]{2}, "rate-down", "down peers on inactivity");
        l.add(null, false, 2, new int[]{3}, "<num>", "ms between checks");
        l.add(null, false, 3, new int[]{-1}, "<num>", "packets between checks");
        l.add(null, false, 1, new int[]{2}, "file", "log to file");
        l.add(null, false, 2, new int[]{2, -1}, "<file>", "name of file");
        l.add(null, false, 1, new int[]{-1}, "local", "log to syslog");
        l.add(null, false, 1, new int[]{2}, "listen-bmp", "bmp listen port");
        l.add(null, false, 2, new int[]{-1}, "<num>", "port number");
        l.add(null, false, 1, new int[]{2}, "listen-bgp", "bgp listen port");
        l.add(null, false, 2, new int[]{3}, "<num>", "port number");
        l.add(null, false, 3, new int[]{-1}, "<num>", "as number");
        l.add(null, false, 1, new int[]{2}, "listen-ris", "ris listen port");
        l.add(null, false, 2, new int[]{-1}, "<num>", "port number");
        l.add(null, false, 1, new int[]{2}, "max-time", "maximum time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms between backups");
        l.add(null, false, 1, new int[]{2}, "max-byte", "maximum bytes");
        l.add(null, false, 2, new int[]{-1}, "<num>", "bytes between backups");
        l.add(null, false, 1, new int[]{2}, "max-pack", "maximum packets");
        l.add(null, false, 2, new int[]{-1}, "<num>", "packets between backups");
        l.add(null, false, 1, new int[]{2}, "backup", "backup to file");
        l.add(null, false, 2, new int[]{2, -1}, "<file>", "name of file");
        l.add(null, false, 1, new int[]{2}, "relay", "relay messages to bmp");
        l.add(null, false, 2, new int[]{3}, "<name:prx>", "proxy profile name");
        l.add(null, false, 3, new int[]{4}, "<addr>", "peer address");
        l.add(null, false, 4, new int[]{5, -1}, "<num>", "peer port");
        l.add(null, false, 5, new int[]{-1}, "<name:acl>", "access list name");
        l.add(null, false, 1, new int[]{2}, "dyneigh", "parse messages from dynamic neighbors");
        l.add(null, false, 2, new int[]{3}, "<name:acl>", "access list on peer name");
        l.add(null, false, 3, new int[]{4}, "rx", "process received packets");
        l.add(null, false, 3, new int[]{4}, "tx", "process transmitted packets");
        cfgRtr.getRouterList(l, 2, " to populate");
        l.add(null, false, 5, new int[]{6}, "<num:rtr>", "process number");
        l.add(null, false, 6, new int[]{-1}, "<str>", "template name");
        l.add(null, false, 1, new int[]{2}, "neighbor", "parse messages from neighbor");
        l.add(null, false, 2, new int[]{3}, "<addr>", "info source");
        l.add(null, false, 3, new int[]{4}, "<addr>", "reported address");
        l.add(null, false, 4, new int[]{5}, "rx", "process received packets");
        l.add(null, false, 4, new int[]{5}, "tx", "process transmitted packets");
        cfgRtr.getRouterList(l, 3, " to populate");
        l.add(null, false, 6, new int[]{7}, "<num:rtr>", "process number");
        l.add(null, false, 7, new int[]{-1}, "<addr>", "peer address");
    }

    public String srvName() {
        return "bmp2mrt";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        genStrmStart(this, new pipeLine(32768, false), listenBmp);
        genStrmStart(this, new pipeLine(32768, false), listenRis);
        genStrmStart(this, new pipeLine(32768, false), listenBgp);
        genStrmStart(this, new pipeLine(32768, false), 0);
        return false;
    }

    public boolean srvDeinit() {
        genericStop(listenBmp);
        genericStop(listenRis);
        genericStop(listenBgp);
        genericStop(0);
        return false;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        if (id.portLoc == listenBmp) {
            new servBmp2mrtBmp(pipe, this, id);
            return false;
        }
        if (id.portLoc == listenBgp) {
            new servBmp2mrtBgp(pipe, this, id);
            return false;
        }
        if (id.portLoc == listenRis) {
            new servBmp2mrtRis(pipe, this, id);
            return false;
        }
        new servBmp2mrtConn(pipe, this, id);
        return false;
    }

    private servBmp2mrtStat getStat(addrIP from, addrIP peer, int crt, int as) {
        servBmp2mrtStat res = new servBmp2mrtStat();
        res.from = from.copyBytes();
        res.peer = peer.copyBytes();
        res.asn = as;
        if (crt == 0) {
            return stats.find(res);
        }
        servBmp2mrtStat old = stats.add(res);
        if (old != null) {
            res = old;
            if (as != 0) {
                res.asn = as;
            }
        } else {
            res.state = crt == 1;
        }
        if (dynCfg == null) {
            return res;
        }
        if (res.nei != null) {
            return res;
        }
        packHolder pck = new packHolder(true, true);
        pck.IPsrc.setAddr(from);
        pck.IPtrg.setAddr(peer);
        pck.UDPtrg = as;
        if (!dynAcl.matches(false, false, pck)) {
            return res;
        }
        res.dyn = true;
        res.rouD = dynCfg.rouD;
        res.rouI = dynCfg.rouI;
        res.rouT = dynCfg.rouT;
        res.nei = dynCfg.prc.addListenPeer(peer, from, dynTmp);
        return res;
    }

    /**
     * got state
     *
     * @param as as number
     * @param src source of packet
     * @param spk got from speaker
     * @param st state
     */
    public void gotState(int as, addrIP src, addrIP spk, boolean st) {
        servBmp2mrtStat stat = getStat(spk, src, 1, as);
        stat.state = st;
        stat.since = bits.getTime();
        stat.change++;
    }

    /**
     * got bulk state
     *
     * @param spk got from speaker
     * @param st state
     */
    public void gotState(addrIP spk, boolean st) {
        if (!bulkDown) {
            return;
        }
        long tim = bits.getTime();
        for (int i = 0; i < stats.size(); i++) {
            servBmp2mrtStat stat = stats.get(i);
            if (stat == null) {
                continue;
            }
            if (spk.compareTo(stat.from) != 0) {
                continue;
            }
            stat.state = st;
            stat.since = tim;
            stat.change++;
        }
    }

    /**
     * got counters
     *
     * @param as as number
     * @param src source of packet
     * @param spk got from speaker
     * @param dat tlvs
     */
    public void gotCounts(int as, addrIP src, addrIP spk, packHolder dat) {
        servBmp2mrtStat stat = getStat(spk, src, 2, as);
        encTlv tlv = rtrBgpMon.getTlv();
        stat.repPack++;
        stat.repByte += dat.dataSize();
        stat.repLast = bits.getTime();
        for (;;) {
            if (tlv.getBytes(dat)) {
                break;
            }
            switch (tlv.valTyp) {
                case 0: // policy rejected
                    stat.repPolRej = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 1: // duplicate advertisements
                    stat.repDupAdv = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 2: // duplicate withdraws
                    stat.repDupWit = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 3: // cluster list loop
                    stat.repClstrL = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 4: // as path loop
                    stat.repAsPath = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 5: // originator id
                    stat.repOrgnId = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 6: // as confed loop
                    stat.repAsConf = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 11: // withdraw threated updates
                    stat.repWitUpd = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 12: // withdraw threated prefixes
                    stat.repWitPrf = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 13: // duplicate updates
                    stat.repDupUpd = bits.msbGetD(tlv.valDat, 0);
                    break;
            }
        }
    }

    /**
     * got update
     *
     * @param as as number
     * @param src source of packet
     * @param spk got from speaker
     * @param dir direction: false=rx, true=tx
     * @param hlp helper packet
     * @param pck bgp message
     */
    public void gotMessage(int as, addrIP src, addrIP spk, boolean dir, packHolder hlp, packHolder pck) {
        servBmp2mrtStat stat = getStat(spk, src, 1, as);
        stat.state = true;
        if (dir) {
            stat.packOut++;
            stat.byteOut += pck.dataSize();
        } else {
            stat.packIn++;
            stat.byteIn += pck.dataSize();
        }
        stat.packLast = bits.getTime();
        stat.packRate += pck.dataSize();
        if (local) {
            logger.info((dir ? "tx" : "rx") + " " + as + " " + src + " " + spk + " " + bits.byteDump(pck.getCopy(), 0, -1));
        }
        if ((dir == stat.rouD) && (stat.nei != null)) {
            packHolder upd = pck.copyBytes(true, true);
            upd.merge2beg();
            int typ = upd.getByte(rtrBgpUtil.sizeU - 1);
            upd.getSkip(rtrBgpUtil.sizeU);
            try {
                switch (typ) {
                    case rtrBgpUtil.msgUpdate:
                        stat.nei.conn.parseUpdate(upd, hlp);
                        break;
                    case rtrBgpUtil.msgOpen:
                        stat.nei.conn.parseOpen(upd);
                        break;
                }
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        if (fileHandle == null) {
            return;
        }
        byte[] hdr = new byte[128];
        int len = rtrBgpMrt.putMrtHeader(hdr, bits.getTime(), dir, as, 0, src, spk, pck.dataSize());
        fileHandle.add(hdr, 0, len, pck.getCopy(), 0, pck.dataSize());
    }

    /**
     * do rate calculation
     */
    public void doRates() {
        for (int i = 0; i < stats.size(); i++) {
            servBmp2mrtStat ntry = stats.get(i);
            ntry.state = ntry.packRate > rateNum;
            ntry.packRate = 0;
        }
    }

    /**
     * do clear
     */
    public void doClear() {
        for (int i = 0; i < stats.size(); i++) {
            stats.get(i).change = 1;
        }
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "from|peer|asnum|asname|state|change|ago|last");
        for (int i = 0; i < stats.size(); i++) {
            res.add("" + stats.get(i));
        }
        return res;
    }

    /**
     * get show
     *
     * @param frm from
     * @return result
     */
    public userFormat getShow(addrIP frm) {
        servBmp2mrtStat res = new servBmp2mrtStat();
        res.from = frm.copyBytes();
        for (int i = 0; i < stats.size(); i++) {
            servBmp2mrtStat cur = stats.get(i);
            if (frm.compareTo(cur.from) != 0) {
                continue;
            }
            res.addCnts(cur);
        }
        return res.getShow();
    }

    /**
     * get show
     *
     * @param frm from
     * @param per peer
     * @return result
     */
    public userFormat getShow(addrIP frm, addrIP per) {
        servBmp2mrtStat stat = getStat(frm, per, 0, 0);
        if (stat == null) {
            return null;
        }
        return stat.getShow();
    }

}
