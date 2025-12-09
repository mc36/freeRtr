package org.freertr.serv;

import org.freertr.util.keyword;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdMcast;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packEsp;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGen;
import org.freertr.sec.secTransform;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabLabelBierN;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabNatTraN;
import org.freertr.tab.tabPbrN;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabSession;
import org.freertr.tab.tabSessionEntry;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * p4lang utilities
 *
 * @author matecsaba
 */
public class servP4langUtil {

    private servP4langUtil() {
    }

    /**
     * convert forwarder id to interface
     *
     * @param p parent
     * @param i id
     * @return interface
     */
    protected final static servP4langIfc forwarder2iface(servP4lang p, int i) {
        return new servP4langIfc(p, -1 - i);
    }

    /**
     * convert a packet to a packet out message
     *
     * @param nei neighbor out
     * @param pck packet to convert
     * @param cnt counter to use
     * @param prt port to use
     * @param port port to use
     * @return converted packet
     */
    protected final static String packet2packout(boolean nei, packHolder pck, int cnt, int prt, int port) {
        String a;
        if (nei) {
            a = "neighout_add ";
        } else {
            a = "packout_add ";
        }
        a += cnt + " " + (pck.dataSize() + addrMac.sizeX2) + " " + prt + " " + port + " " + pck.SGTid + " " + ((pck.UDPsrc ^ pck.UDPtrg) & 15) + " ";
        byte[] buf = pck.ETHtrg.getBytes();
        a += bits.toHex(buf);
        buf = pck.ETHsrc.getBytes();
        a += bits.toHex(buf);
        buf = new byte[pck.dataSize()];
        pck.getCopy(buf, 0, 0, buf.length);
        return a + bits.toHex(buf);
    }

    /**
     * convert to number
     *
     * @param src source list
     * @param num number or name to find
     * @param inv invalid value
     * @return id, inv if error
     */
    protected final static int toNum(tabGen<servP4langMgcN> src, String num, int inv) {
        int i = bits.str2num(num);
        if (num.equals("" + i)) {
            return i;
        }
        servP4langMgcN ntry = new servP4langMgcN(inv, num);
        ntry = src.find(ntry);
        if (ntry == null) {
            return inv;
        }
        return ntry.id;
    }

    /**
     * convert to show
     *
     * @param beg beginning
     * @param src source list
     * @param trg target list
     */
    protected final static void toShow(String beg, tabGen<servP4langMgcN> src, userFormat trg) {
        for (int i = 0; i < src.size(); i++) {
            servP4langMgcN ntry = src.get(i);
            trg.add(beg + ntry.id + "|" + ntry.nam);
        }
    }

    /**
     * convert to help
     *
     * @param src source list
     * @return converted text
     */
    protected final static List<String> toHelp(tabGen<servP4langMgcN> src) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < src.size(); i++) {
            res.add(src.get(i).nam);
        }
        return res;
    }

    /**
     * convert id
     *
     * @param id identifier
     * @param src source list
     * @return converted string
     */
    protected final static String convId(int id, tabGen<servP4langMgcI> src) {
        servP4langMgcI ntry = new servP4langMgcI(id, null);
        ntry = src.find(ntry);
        if (ntry == null) {
            return "" + id;
        }
        return ntry.nam;
    }

    /**
     * convert table
     *
     * @param src source
     * @param ned needed
     * @return converted
     */
    protected final static tabGen<servP4langMgcI> convTab(tabGen<servP4langMgcN> src, boolean ned) {
        tabGen<servP4langMgcI> res = new tabGen<servP4langMgcI>();
        if (!ned) {
            return res;
        }
        for (int i = 0; i < src.size(); i++) {
            servP4langMgcN ntry = src.get(i);
            res.add(new servP4langMgcI(ntry.id, ntry.nam));
        }
        return res;
    }

    /**
     * update nat translation
     *
     * @param cmd commands
     * @param fwd forwarder
     */
    protected final static void updateNatTrans(cmds cmd, ipFwd fwd) {
        tabNatTraN ntry = new tabNatTraN();
        ntry.protocol = bits.str2num(cmd.word());
        addrIP adr = new addrIP();
        adr.fromString(cmd.word());
        ntry.origSrcAddr = adr;
        adr = new addrIP();
        adr.fromString(cmd.word());
        ntry.origTrgAddr = adr;
        ntry.origSrcPort = bits.str2num(cmd.word());
        ntry.origTrgPort = bits.str2num(cmd.word());
        ntry = fwd.natTrns.find(ntry);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        if (old == null) {
            return;
        }
        if (old.compareTo(ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastUsed = bits.getTime();
        ntry.reverse.lastUsed = ntry.lastUsed;
    }

    /**
     * update inspection
     *
     * @param cmd commands
     * @param insp sessions
     */
    protected final static void updateInsp(cmds cmd, tabSession insp) {
        if (insp == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        tabSessionEntry ntry = new tabSessionEntry(false);
        ntry.ipPrt = bits.str2num(cmd.word());
        ntry.srcAdr = new addrIP();
        ntry.srcAdr.fromString(cmd.word());
        ntry.trgAdr = new addrIP();
        ntry.trgAdr.fromString(cmd.word());
        ntry.srcPrt = bits.str2num(cmd.word());
        ntry.trgPrt = bits.str2num(cmd.word());
        tabSessionEntry res = insp.connects.find(ntry);
        if (res == null) {
            ntry = ntry.reverseDirection();
            res = insp.connects.find(ntry);
            if (res == null) {
                if (debugger.servP4langErr) {
                    logger.debug("got unneeded report: " + cmd.getOriginal());
                }
                return;
            }
        }
        ntry = res;
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        ntry.hwCntr.packTx = bits.str2long(cmd.word());
        ntry.hwCntr.byteTx = bits.str2long(cmd.word());
        if (old == null) {
            return;
        }
        if (old.compareTo(ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastTime = bits.getTime();
    }

    /**
     * update multicast routes
     *
     * @param cmd commands
     * @param fwd forwarder
     */
    protected final static void updateMroute(cmds cmd, ipFwd fwd) {
        addrIP src = new addrIP();
        src.fromString(cmd.word());
        addrIP grp = new addrIP();
        grp.fromString(cmd.word());
        ipFwdMcast ntry = new ipFwdMcast(grp, src);
        ntry = fwd.groups.find(ntry);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        ntry.hwCntr = new counter();
        ntry.hwCntr.packTx = bits.str2long(cmd.word());
        ntry.hwCntr.byteTx = bits.str2long(cmd.word());
    }

    /**
     * update route counters
     *
     * @param cmd commands
     * @param fwd forwarder
     * @param prf prefix
     */
    protected final static void updateRoute(cmds cmd, ipFwd fwd, addrPrefix<addrIP> prf) {
        tabRouteEntry<addrIP> ntry = fwd.actualU.find(prf);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        ntry.hwCntr = new counter();
        ntry.hwCntr.packTx = bits.str2long(cmd.word());
        ntry.hwCntr.byteTx = bits.str2long(cmd.word());
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
    }

    /**
     * update policy routing
     *
     * @param cmd commands
     * @param pbr pbr config
     */
    protected final static void updatePbr(cmds cmd, tabListing<tabPbrN, addrIP> pbr) {
        int seq = bits.str2num(cmd.word());
        tabPbrN rul = null;
        for (int i = 0; i < pbr.size(); i++) {
            rul = pbr.get(i);
            if (seq < rul.matcher.size()) {
                break;
            }
            seq -= rul.matcher.size();
        }
        if (rul == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        tabAceslstN<addrIP> ntry = rul.matcher.get(seq);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        if (ntry.rolledFrom != null) {
            ntry = ntry.rolledFrom;
        }
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        if (old == null) {
            return;
        }
        if (old.compareTo(ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastMatch = bits.getTime();
    }

    /**
     * update access list
     *
     * @param cmd commands
     * @param acl access list
     */
    protected final static void updateAcl(cmds cmd, tabListing<tabAceslstN<addrIP>, addrIP> acl) {
        tabAceslstN<addrIP> ntry = acl.get(bits.str2num(cmd.word()));
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        if (ntry.rolledFrom != null) {
            ntry = ntry.rolledFrom;
        }
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        if (old == null) {
            return;
        }
        if (old.compareTo(ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastMatch = bits.getTime();
    }

    /**
     * update tunnel counters
     *
     * @param cmd commands
     * @param fwd forwarder
     * @param prt protocol
     */
    protected final static void updateTunn(cmds cmd, ipFwd fwd, prtGen prt) {
        addrIP sa = new addrIP();
        sa.fromString(cmd.word());
        addrIP da = new addrIP();
        da.fromString(cmd.word());
        int sp = bits.str2num(cmd.word());
        int dp = bits.str2num(cmd.word());
        tabRouteEntry<addrIP> ntry = fwd.actualU.route(da);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        if (ntry.best.iface == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        counter cntr = new counter();
        cntr.packTx = bits.str2long(cmd.word());
        cntr.byteTx = bits.str2long(cmd.word());
        prt.counterUpdate((ipFwdIface) ntry.best.iface, sa, sp, dp, cntr);
    }

    /**
     * calculate verify source
     *
     * @param ifc interface
     * @return config
     */
    protected final static int getVerifySource(ipFwdIface ifc) {
        if (ifc == null) {
            return 0;
        }
        if (!ifc.verifySource) {
            return 0;
        }
        if (ifc.verifyStricht) {
            return 2;
        } else {
            return 1;
        }
    }

    private final static String getIpsecParam(packEsp esp) {
        return " " + esp.spi + " " + bits.toHex(esp.keyEncr) + " " + bits.toHex(esp.keyHash);
    }

    /**
     * calculate ipsec parameters
     *
     * @param rx receiving esp
     * @param tx transmitting esp
     * @param ts transform set
     * @return config
     */
    protected final static String getIpsecParam(packEsp rx, packEsp tx, secTransform ts) {
        if (rx == null) {
            return "";
        }
        if (tx == null) {
            return "";
        }
        if (ts == null) {
            return "";
        }
        if (rx.keyHash == null) {
            return "";
        }
        if (tx.keyHash == null) {
            return "";
        }
        String a = ts.hash2str();
        if (ts.isAead()) {
            a = "none";
        }
        return " " + rx.encrSize + " " + rx.tagSize + " " + rx.hashSize + " " + ts.encr2str() + " " + a + servP4langUtil.getIpsecParam(rx) + servP4langUtil.getIpsecParam(tx);
    }

    /**
     * nat translation config
     *
     * @param ntry entry to convert
     * @return config
     */
    protected final static String natTrns2str(tabNatTraN ntry) {
        return ntry.protocol + " " + ntry.origSrcAddr + " " + ntry.origSrcPort + " " + ntry.origTrgAddr + " " + ntry.origTrgPort + " " + ntry.newSrcAddr + " " + ntry.newSrcPort + " " + ntry.newTrgAddr + " " + ntry.newTrgPort;
    }

    /**
     * get matcher parameter
     *
     * @param mat matcher
     * @param max maximum value
     * @return config
     */
    protected final static String numat2str(tabIntMatcher mat, int max) {
        switch (mat.action) {
            case xact:
                return (mat.rangeMin & max) + " " + max;
            case range:
                return mat.rangeMin + " " + (max - mat.rangeMax + mat.rangeMin);
            case mask:
                return mat.rangeMin + " " + mat.rangeMax;
            default:
                return "0 0";
        }
    }

    /**
     * address to string
     *
     * @param ipv4 true of ipv4, false for ipv6
     * @param adr address to convert
     * @return config
     */
    protected final static String ip2str(boolean ipv4, addrIP adr) {
        if (ipv4) {
            return "" + adr.toIPv4();
        } else {
            return "" + adr.toIPv6();
        }
    }

    /**
     * convert access list entry to string
     *
     * @param seq sequence number
     * @param ipv4 protocol selector
     * @param ace entry to convert
     * @param check check protocol version
     * @param negate negate action
     * @return config
     */
    protected final static String ace2str(int seq, boolean ipv4, tabAceslstN<addrIP> ace, boolean check, boolean negate) {
        if (check) {
            if (!ace.srcMask.isEmpty()) {
                if (ace.srcMask.isIPv4() != ipv4) {
                    return null;
                }
            }
            if (!ace.trgMask.isEmpty()) {
                if (ace.trgMask.isIPv4() != ipv4) {
                    return null;
                }
            }
        }
        String cmd;
        if (negate ^ (ace.action == tabListingEntry.actionType.actPermit)) {
            cmd = tabListingEntry.action2string(tabListingEntry.actionType.actPermit);
        } else {
            cmd = tabListingEntry.action2string(tabListingEntry.actionType.actDeny);
        }
        if (ace.reflectFwd != null) {
            cmd = "punt";
        }
        return seq + " " + cmd + " " + servP4langUtil.numat2str(ace.proto, 255) + " " + servP4langUtil.ip2str(ipv4, ace.srcAddr) + " " + servP4langUtil.ip2str(ipv4, ace.srcMask) + " " + servP4langUtil.ip2str(ipv4, ace.trgAddr) + " " + servP4langUtil.ip2str(ipv4, ace.trgMask) + " " + servP4langUtil.numat2str(ace.srcPort, 65535) + " " + servP4langUtil.numat2str(ace.trgPort, 65535) + " " + servP4langUtil.numat2str(ace.tos, 255) + " " + servP4langUtil.numat2str(ace.flow, ipv4 ? 65535 : 1048575) + " " + servP4langUtil.numat2str(ace.sgt, 65535);
    }

    /**
     * check if acl update needed
     *
     * @param done1 already sent
     * @param need1 need to be sent
     * @param done2 already sent
     * @param need2 need to be sent
     * @param sess inspect table
     * @param sent already sent out
     * @return true if yes, false if not
     */
    protected final static boolean needAcl(tabListing<tabAceslstN<addrIP>, addrIP> done1, tabListing<tabAceslstN<addrIP>, addrIP> need1, tabListing<tabAceslstN<addrIP>, addrIP> done2, tabListing<tabAceslstN<addrIP>, addrIP> need2, tabSession sess, tabListing<tabAceslstN<addrIP>, addrIP> sent) {
        if (need1 != done1) {
            return true;
        }
        if (need2 != done2) {
            return true;
        }
        int i = 0;
        if (need1 != null) {
            i += tabAceslstN.sizeofAcl(need1);
        }
        if (need2 != null) {
            i += tabAceslstN.sizeofAcl(need2);
        }
        if (sess != null) {
            i++;
        }
        return tabAceslstN.sizeofAcl(sent) != i;
    }

    /**
     * convert session entry
     *
     * @param ntry inspect entry
     * @return config
     */
    protected final static String sess2str(tabSessionEntry ntry) {
        if (ntry.dir) {
            return ntry.ipPrt + " " + ntry.trgAdr + " " + ntry.trgPrt + " " + ntry.srcAdr + " " + ntry.srcPrt;
        } else {
            return ntry.ipPrt + " " + ntry.srcAdr + " " + ntry.srcPrt + " " + ntry.trgAdr + " " + ntry.trgPrt;
        }
    }

    /**
     * get null label
     *
     * @param ntry route entry
     * @return label
     */
    protected final static int getNullLabel(tabRouteEntry<addrIP> ntry) {
        if (ntry.prefix.network.isIPv4()) {
            return ipMpls.labelExp4;
        } else {
            return ipMpls.labelExp6;
        }
    }

    /**
     * get first label
     *
     * @param labs labels
     * @return label
     */
    protected final static int get1stLabel(List<Integer> labs) {
        if (labs == null) {
            return -1;
        }
        if (labs.size() < 1) {
            return -1;
        }
        int i = labs.get(0);
        if (i != ipMpls.labelImp) {
            return i;
        }
        if (labs.size() < 2) {
            return ipMpls.labelExp4;
        }
        i = labs.get(1);
        if (i != ipMpls.labelImp) {
            return i;
        }
        return ipMpls.labelExp4;
    }

    /**
     * get second label
     *
     * @param labs labels
     * @return label
     */
    protected final static int get2ndLabel(List<Integer> labs) {
        if (labs == null) {
            return -1;
        }
        if (labs.size() < 2) {
            return -1;
        }
        int i = labs.get(0);
        if (i != ipMpls.labelImp) {
            i = labs.get(1);
            if (i == ipMpls.labelImp) {
                return -1;
            }
            return i;
        }
        if (labs.size() < 3) {
            return -1;
        }
        i = labs.get(1);
        if (i == ipMpls.labelImp) {
            return -1;
        }
        i = labs.get(2);
        if (i == ipMpls.labelImp) {
            return -1;
        }
        return i;
    }

    /**
     * get label
     *
     * @param ntry route entry
     * @return label
     */
    protected final static int getLabel(tabRouteEntry<addrIP> ntry) {
        if (ntry.best.labelRem == null) {
            return servP4langUtil.getNullLabel(ntry);
        }
        if (ntry.best.labelRem.size() < 1) {
            return servP4langUtil.getNullLabel(ntry);
        }
        int i = ntry.best.labelRem.get(0);
        if (i != ipMpls.labelImp) {
            return i;
        }
        if (ntry.best.labelRem.size() < 2) {
            return servP4langUtil.getNullLabel(ntry);
        }
        i = ntry.best.labelRem.get(1);
        if (i != ipMpls.labelImp) {
            return i;
        }
        return servP4langUtil.getNullLabel(ntry);
    }

    /**
     * get bier label
     *
     * @param ntry route entry
     * @param full bitmap
     * @param sis shift
     * @return label
     */
    protected final static String getBierLabs(tabLabelBierN ntry, byte[] full, int sis) {
        byte[] res = ntry.getAndShr(full, sis);
        if (res == null) {
            return " 0 0 0 0 0 0 0 0";
        }
        if (res.length < 1) {
            return " 0 0 0 0 0 0 0 0";
        }
        String a = "";
        for (int i = 0; i < res.length; i += 4) {
            a += " " + bits.msbGetD(res, i);
        }
        return a;
    }

    /**
     * negate dataplane command
     *
     * @param s string to negate
     * @return negated string
     */
    protected final static String negateOneCommand(String s) {
        s = s.replaceAll("_add ", "_del ");
        s = s.replaceAll("_mod ", "_del ");
        return s;
    }

}
