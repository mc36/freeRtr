package net.freertr.serv;

import java.util.List;
import java.util.Timer;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.rtr.rtrBgpMon;
import net.freertr.rtr.rtrBgpMrt;
import net.freertr.rtr.rtrBgpSpeak;
import net.freertr.rtr.rtrBgpTemp;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logFil;
import net.freertr.util.logger;
import net.freertr.enc.encTlv;

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

    private boolean bulkDown;

    private final tabGen<servBmp2mrtStat> stats = new tabGen<servBmp2mrtStat>();

    private boolean local;

    private int rateInt;

    private int rateNum;

    private servBmp2mrtRate rateTim;

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
                rateTim.stopWork();
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
        l.add(null, "5 6            <num:rtr>         process number");
        l.add(null, "6 .              <str>           template name");
        l.add(null, "1 2    neighbor                  parse messages from neighbor");
        l.add(null, "2 3      <addr>                  info source");
        l.add(null, "3 4        <addr>                reported address");
        l.add(null, "4 5          rx                  process received packets");
        l.add(null, "4 5          tx                  process transmitted packets");
        cfgRtr.getRouterList(l, 3, " to populate");
        l.add(null, "6 7              <num:rtr>       process number");
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
        userFormat res = new userFormat("|", "from|peer|asnum|asname|state|change|last|ago");
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
