package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgRtr;
import net.freertr.clnt.clntProxy;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.rtr.rtrBgp;
import net.freertr.rtr.rtrBgpMon;
import net.freertr.rtr.rtrBgpMrt;
import net.freertr.rtr.rtrBgpNeigh;
import net.freertr.rtr.rtrBgpSpeak;
import net.freertr.rtr.rtrBgpTemp;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabRouteAttr;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logFil;
import net.freertr.util.logger;
import net.freertr.util.typLenVal;

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
    public static final int port = 17971;

    /**
     * header size
     */
    public static final int size = 6;

    /**
     * relays
     */
    protected final tabGen<servBmp2mrtRelay> relays = new tabGen<servBmp2mrtRelay>();

    private boolean bulkDown;

    private final tabGen<servBmp2mrtStat> stats = new tabGen<servBmp2mrtStat>();

    private boolean local;

    private int rateInt;

    private int rateNum;

    private Timer rateTim;

    private logFil fileHandle;

    private tabListing<tabAceslstN<addrIP>, addrIP> dynAcl;

    private servBmp2mrtStat dynCfg;

    private rtrBgpTemp dynTmp;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server bmp2mrt .*! port " + port,
        "server bmp2mrt .*! protocol " + proto2string(protoAllStrm),
        "server bmp2mrt .*! no local",
        "server bmp2mrt .*! no bulk-down",
        "server bmp2mrt .*! rate-down 0 0",
        "server bmp2mrt .*! max-time 0",
        "server bmp2mrt .*! max-pack 0",
        "server bmp2mrt .*! max-byte 0",
        "server bmp2mrt .*! no file",
        "server bmp2mrt .*! no backup",
        "server bmp2mrt .*! no dyneigh"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
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
                rateTim.cancel();
            } catch (Exception e) {
            }
            rateTim = new Timer();
            servBmp2mrtRate task = new servBmp2mrtRate(this);
            rateTim.schedule(task, rateInt / 10, rateInt);
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
            fileHandle.rotate(fileHandle.rotateN(), bits.str2num(cmd.word()), fileHandle.rotateT(), fileHandle.rotateL());
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
        if (!s.equals("no")) {
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
                rateTim.cancel();
            } catch (Exception e) {
            }
            rateTim = null;
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

    public void srvHelp(userHelping l) {
        l.add(null, "1 .    bulk-down                 down peers on speaker loss");
        l.add(null, "1 2    rate-down                 down peers on inactivity");
        l.add(null, "2 3      <num>                   ms between checks");
        l.add(null, "3 .        <num>                 packets between checks");
        l.add(null, "1 2    file                      log to file");
        l.add(null, "2 2,.    <file>                  name of file");
        l.add(null, "1 .    local                     log to syslog");
        l.add(null, "1 2    max-time                  maximum time");
        l.add(null, "2 .      <num>                   ms between backups");
        l.add(null, "1 2    max-byte                  maximum bytes");
        l.add(null, "2 .      <num>                   bytes between backups");
        l.add(null, "1 2    max-pack                  maximum packets");
        l.add(null, "2 .      <num>                   packets between backups");
        l.add(null, "1 2    backup                    backup to file");
        l.add(null, "2 2,.    <file>                  name of file");
        l.add(null, "1 2    relay                     relay messages to bmp");
        l.add(null, "2 3      <name:prx>              proxy profile name");
        l.add(null, "3 4        <addr>                peer address");
        l.add(null, "4 5,.        <num>               peer port");
        l.add(null, "5 .            <name:acl>        access list name");
        l.add(null, "1 2    dyneigh                   parse messages from dynamic neighbors");
        l.add(null, "2 3      <name:acl>              access list on peer name");
        l.add(null, "3 4        rx                    process received packets");
        l.add(null, "3 4        tx                    process transmitted packets");
        cfgRtr.getRouterList(l, 2, " to populate");
        l.add(null, "5 6            <num>             process number");
        l.add(null, "6 .              <str>           template name");
        l.add(null, "1 2    neighbor                  parse messages from neighbor");
        l.add(null, "2 3      <addr>                  info source");
        l.add(null, "3 4        <addr>                reported address");
        l.add(null, "4 5          rx                  process received packets");
        l.add(null, "4 5          tx                  process transmitted packets");
        cfgRtr.getRouterList(l, 3, " to populate");
        l.add(null, "6 7              <num>           process number");
        l.add(null, "7 .                <addr>        peer address");
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
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servBmp2mrtConn(pipe, this, id);
        return false;
    }

    private servBmp2mrtStat getStat(addrIP from, addrIP peer, int crt, int as) {
        servBmp2mrtStat res = new servBmp2mrtStat();
        res.from = from.copyBytes();
        res.peer = peer.copyBytes();
        res.as = as;
        if (crt == 0) {
            return stats.find(res);
        }
        servBmp2mrtStat old = stats.add(res);
        if (old != null) {
            res = old;
            if (as != 0) {
                res.as = as;
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
        res.nei = dynCfg.prc.addListenPeer(peer, dynTmp);
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
            if (spk.compare(spk, stat.from) != 0) {
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
        typLenVal tlv = rtrBgpMon.getTlv();
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
     * @param dat bgp message
     */
    public void gotMessage(int as, addrIP src, addrIP spk, boolean dir, byte[] dat) {
        servBmp2mrtStat stat = getStat(spk, src, 1, as);
        stat.state = true;
        if (dir) {
            stat.packOut++;
            stat.byteOut += dat.length;
        } else {
            stat.packIn++;
            stat.byteIn += dat.length;
        }
        stat.packLast = bits.getTime();
        stat.packRate += dat.length;
        if (local) {
            logger.info((dir ? "tx" : "rx") + " " + as + " " + src + " " + spk + " " + bits.byteDump(dat, 0, -1));
        }
        if ((dir == stat.rouD) && (stat.nei != null)) {
            packHolder upd = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            upd.putCopy(dat, 0, 0, dat.length);
            upd.putSkip(dat.length);
            upd.merge2beg();
            int typ = upd.getByte(rtrBgpSpeak.sizeU - 1);
            upd.getSkip(rtrBgpSpeak.sizeU);
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
        int len = rtrBgpMrt.putMrtHeader(hdr, dir, as, 0, src, spk, dat.length);
        fileHandle.add(hdr, 0, len, dat, 0, dat.length);
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
        userFormat res = new userFormat("|", "from|peer|as|state|change|last");
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
            if (frm.compare(frm, cur.from) != 0) {
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

class servBmp2mrtStat implements Comparator<servBmp2mrtStat> {

    public addrIP from;

    public addrIP peer;

    public int as;

    public int packIn;

    public int packOut;

    public int packRate;

    public long packLast;

    public int byteIn;

    public int byteOut;

    public boolean state;

    public long since;

    public int change;

    public tabRouteAttr.routeType rouT;

    public int rouI;

    public boolean rouD;

    public rtrBgp prc;

    public rtrBgpNeigh nei;

    public boolean dyn;

    public long repLast;

    public int repPack;

    public int repByte;

    public int repPolRej;

    public int repDupAdv;

    public int repDupWit;

    public int repClstrL;

    public int repAsPath;

    public int repOrgnId;

    public int repAsConf;

    public int repWitUpd;

    public int repWitPrf;

    public int repDupUpd;

    public int compare(servBmp2mrtStat o1, servBmp2mrtStat o2) {
        int i = o1.from.compare(o1.from, o2.from);
        if (i != 0) {
            return i;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public String toString() {
        return from + "|" + peer + "|" + as + "|" + state + "|" + change + "|" + bits.timePast(since);
    }

    public boolean fromString(cmds cmd, boolean stat) {
        rouD = cmd.word().equals("tx");
        tabRouteAttr.routeType rt = cfgRtr.name2num(cmd.word());
        if (rt == null) {
            cmd.error("invalid routing protocol");
            return true;
        }
        int ri = bits.str2num(cmd.word());
        cfgRtr rp = cfgAll.rtrFind(rt, ri, false);
        if (rp == null) {
            cmd.error("bad process number");
            return true;
        }
        if (rp.bgp == null) {
            cmd.error("not a bgp process");
            return true;
        }
        prc = rp.bgp;
        if (!stat) {
            rouT = rt;
            rouI = ri;
            return false;
        }
        addrIP adr = new addrIP();
        adr.fromString(cmd.word());
        nei = rp.bgp.findPeer(adr);
        if (nei == null) {
            cmd.error("no such peer");
            return true;
        }
        rouT = rt;
        rouI = ri;
        return false;
    }

    public String getCfg(boolean stat) {
        String a = (rouD ? "tx" : "rx") + " " + cfgRtr.num2name(rouT) + " " + rouI;
        if (stat) {
            return a + " " + nei;
        } else {
            return a;
        }
    }

    public userFormat getShow() {
        userFormat res = new userFormat("|", "category|value");
        res.add("from|" + from);
        res.add("peer|" + peer);
        res.add("as|" + as);
        res.add("state|" + state);
        res.add("since|" + bits.time2str(cfgAll.timeZoneName, since + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(since) + " ago)");
        res.add("change|" + change);
        res.add("pack in|" + packIn);
        res.add("pack out|" + packOut);
        res.add("byte in|" + byteIn);
        res.add("byte out|" + byteOut);
        res.add("pack last|" + bits.time2str(cfgAll.timeZoneName, packLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(packLast) + " ago)");
        res.add("report pack|" + repPack);
        res.add("report byte|" + repByte);
        res.add("report last|" + bits.time2str(cfgAll.timeZoneName, repLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(repLast) + " ago)");
        res.add("rep policy drp|" + repPolRej);
        res.add("rep dup advert|" + repDupAdv);
        res.add("rep dup withdrw|" + repDupWit);
        res.add("rep dup update|" + repDupUpd);
        res.add("rep cluster id|" + repClstrL);
        res.add("rep as path|" + repAsPath);
        res.add("rep originator|" + repOrgnId);
        res.add("rep confed|" + repAsConf);
        res.add("rep bad updt|" + repWitUpd);
        res.add("rep bad prfx|" + repWitPrf);
        res.add("process|" + rouT + " " + rouI);
        res.add("neighbor|" + nei);
        res.add("direction|" + rouD);
        return res;
    }

    public void addCnts(servBmp2mrtStat oth) {
        if (since < oth.since) {
            since = oth.since;
        }
        if (packLast < oth.packLast) {
            packLast = oth.packLast;
        }
        if (repLast < oth.repLast) {
            repLast = oth.repLast;
        }
        change += oth.change;
        packIn += oth.packIn;
        packOut += oth.packOut;
        byteIn += oth.byteIn;
        byteOut += oth.byteOut;
        repPack += oth.repPack;
        repByte += oth.repByte;
        repPolRej += oth.repPolRej;
        repDupAdv += oth.repDupAdv;
        repDupWit += oth.repDupWit;
        repDupUpd += oth.repDupUpd;
        repClstrL += oth.repClstrL;
        repAsPath += oth.repAsPath;
        repOrgnId += oth.repOrgnId;
        repAsConf += oth.repAsConf;
        repWitUpd += oth.repWitUpd;
        repWitPrf += oth.repWitPrf;
    }

}

class servBmp2mrtRate extends TimerTask {

    private servBmp2mrt lower;

    public servBmp2mrtRate(servBmp2mrt prnt) {
        lower = prnt;
    }

    public void run() {
        lower.doRates();
    }

}

class servBmp2mrtConn implements Runnable {

    private pipeSide pipe;

    private servBmp2mrt lower;

    private addrIP peer;

    public servBmp2mrtConn(pipeSide pip, servBmp2mrt prnt, prtGenConn id) {
        pipe = pip;
        lower = prnt;
        peer = id.peerAddr.copyBytes();
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void doer() {
        logger.warn("neighbor " + peer + " up");
        packHolder pck = new packHolder(true, true);
        addrIP adr = new addrIP();
        for (;;) {
            pck.clear();
            if (pck.pipeRecv(pipe, 0, servBmp2mrt.size, 144) != servBmp2mrt.size) {
                break;
            }
            if (pck.getByte(0) != 3) { // version
                break;
            }
            int len = pck.msbGetD(1) - servBmp2mrt.size; // length
            int typ = pck.getByte(5); // type
            if (len < 1) {
                continue;
            }
            pck.clear();
            if (pck.pipeRecv(pipe, 0, len, 144) != len) {
                break;
            }
            // per = pck.getByte(0); // peer type
            int flg = pck.getByte(1); // flags
            // int rd = pck.msbGetQ(2); // distinguisher
            pck.getAddr(adr, 10);
            int as = pck.msbGetD(26); // asnum
            // pck.getAddr(rtr, 30); // rtrid
            // int tim = pck.msbGetD(34); // time
            // int nano = pck.msbGetD(38); // time
            if ((flg & 0x80) == 0) {
                adr.fromIPv4addr(adr.toIPv4());
            }
            for (int i = 0; i < lower.relays.size(); i++) {
                lower.relays.get(i).gotMessage(as, peer, adr, typ, pck);
            }
            pck.getSkip(rtrBgpMon.size - servBmp2mrt.size);
            switch (typ) {
                case rtrBgpMon.typMon:
                    break;
                case rtrBgpMon.typPerUp:
                    pck.getSkip(20);
                    lower.gotState(as, adr, peer, true);
                    break;
                case rtrBgpMon.typPerDn:
                    pck.getSkip(1);
                    lower.gotState(as, adr, peer, false);
                    break;
                case rtrBgpMon.typStat:
                    pck.getSkip(4);
                    lower.gotCounts(as, adr, peer, pck);
                    continue;
                default:
                    continue;
            }
            lower.gotMessage(as, adr, peer, (flg & 0x10) != 0, pck.getCopy());
        }
        lower.gotState(peer, false);
        logger.error("neighbor " + peer + " down");
    }

}

class servBmp2mrtRelay implements Comparator<servBmp2mrtRelay>, Runnable {

    public clntProxy proxy;

    public String server;

    public int port;

    private tabListing<tabAceslstN<addrIP>, addrIP> acl;

    private boolean need2run;

    private pipeSide pipe;

    public boolean fromString(cmds cmd) {
        cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
        if (prx == null) {
            cmd.error("no such proxy");
            return true;
        }
        proxy = prx.proxy;
        server = cmd.word();
        port = bits.str2num(cmd.word());
        String a = cmd.word();
        if (a.length() < 1) {
            return false;
        }
        cfgAceslst ac = cfgAll.aclsFind(a, false);
        if (ac == null) {
            cmd.error("no such access list");
            return true;
        }
        acl = ac.aceslst;
        return false;
    }

    public String toString() {
        String a = "";
        if (acl != null) {
            a = " " + acl.listName;
        }
        return proxy.name + " " + server + " " + port + a;
    }

    public int compare(servBmp2mrtRelay o1, servBmp2mrtRelay o2) {
        if (o1.port < o2.port) {
            return -1;
        }
        if (o1.port > o2.port) {
            return +1;
        }
        return o1.server.compareTo(o2.server);
    }

    public void startWork() {
        need2run = true;
        new Thread(this).start();
    }

    public void stopWork() {
        need2run = false;
        if (pipe == null) {
            return;
        }
        pipe.setClose();
    }

    public void run() {
        try {
            for (;;) {
                doWork();
                if (!need2run) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doWork() {
        bits.sleep(1000);
        addrIP adr = userTerminal.justResolv(server, proxy.prefer);
        if (adr == null) {
            return;
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, adr, port, "bmp");
        if (pipe == null) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, 3); // version
        pck.msbPutD(1, servBmp2mrt.size); // length
        pck.putByte(5, rtrBgpMon.typInit); // type
        pck.putSkip(servBmp2mrt.size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
        logger.warn("relay " + server + " up");
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            if (!need2run) {
                break;
            }
            bits.sleep(1000);
        }
        logger.warn("relay " + server + " down");
        pipe.setClose();
        pipe = null;
    }

    public void gotMessage(int as, addrIP from, addrIP peer, int typ, packHolder pck) {
        if (pipe == null) {
            return;
        }
        pck = pck.copyBytes(true, true);
        if (acl != null) {
            pck.IPsrc.setAddr(from);
            pck.IPtrg.setAddr(peer);
            pck.UDPtrg = as;
            if (!acl.matches(false, false, pck)) {
                return;
            }
        }
        pck.putByte(0, 3); // version
        pck.msbPutD(1, servBmp2mrt.size + pck.dataSize()); // length
        pck.putByte(5, typ); // type
        pck.putSkip(servBmp2mrt.size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
    }

}
