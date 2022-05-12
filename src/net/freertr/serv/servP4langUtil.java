package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdMcast;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packEsp;
import net.freertr.prt.prtGen;
import net.freertr.sec.secTransform;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabLabelBierN;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabListingEntry;
import net.freertr.tab.tabNatTraN;
import net.freertr.tab.tabPbrN;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabSession;
import net.freertr.tab.tabSessionEntry;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * p4lang utilities
 *
 * @author matecsaba
 */
public class servP4langUtil {

    private servP4langUtil() {
    }

    /**
     * update nat translation
     *
     * @param cmd commands
     * @param fwd forwarder
     */
    protected static void updateNatTrans(cmds cmd, ipFwd fwd) {
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
            old = new counter();
        }
        if (old.compare(old, ntry.hwCntr) >= 0) {
            ntry.hwCntr = old;
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
    protected static void updateInsp(cmds cmd, tabSession insp) {
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
            old = new counter();
        }
        if (old.compare(old, ntry.hwCntr) >= 0) {
            ntry.hwCntr = old;
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
    protected static void updateMroute(cmds cmd, ipFwd fwd) {
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
    protected static void updateRoute(cmds cmd, ipFwd fwd, addrPrefix<addrIP> prf) {
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
    protected static void updatePbr(cmds cmd, tabListing<tabPbrN, addrIP> pbr) {
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
            old = new counter();
        }
        if (old.compare(old, ntry.hwCntr) >= 0) {
            ntry.hwCntr = old;
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
    protected static void updateAcl(cmds cmd, tabListing<tabAceslstN<addrIP>, addrIP> acl) {
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
            old = new counter();
        }
        if (old.compare(old, ntry.hwCntr) >= 0) {
            ntry.hwCntr = old;
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
    protected static void updateTunn(cmds cmd, ipFwd fwd, prtGen prt) {
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
    protected static int getVerifySource(ipFwdIface ifc) {
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

    private static String getIpsecParam(packEsp esp) {
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
    protected static String getIpsecParam(packEsp rx, packEsp tx, secTransform ts) {
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
        return " " + rx.encrSize + " " + rx.hashSize + " " + ts.encr2str() + " " + ts.hash2str() + servP4langUtil.getIpsecParam(rx) + servP4langUtil.getIpsecParam(tx);
    }

    /**
     * nat translation config
     *
     * @param ntry entry to convert
     * @return config
     */
    protected static String natTrns2str(tabNatTraN ntry) {
        return ntry.protocol + " " + ntry.origSrcAddr + " " + ntry.origSrcPort + " " + ntry.origTrgAddr + " " + ntry.origTrgPort + " " + ntry.newSrcAddr + " " + ntry.newSrcPort + " " + ntry.newTrgAddr + " " + ntry.newTrgPort;
    }

    /**
     * get matcher parameter
     *
     * @param mat matcher
     * @param max maximum value
     * @return config
     */
    protected static String numat2str(tabIntMatcher mat, int max) {
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
    protected static String ip2str(boolean ipv4, addrIP adr) {
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
    protected static String ace2str(int seq, boolean ipv4, tabAceslstN<addrIP> ace, boolean check, boolean negate) {
        if (check) {
            if (!ace.srcMask.isFilled(0)) {
                if (ace.srcMask.isIPv4() != ipv4) {
                    return null;
                }
            }
            if (!ace.trgMask.isFilled(0)) {
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
    protected static boolean needAcl(tabListing<tabAceslstN<addrIP>, addrIP> done1, tabListing<tabAceslstN<addrIP>, addrIP> need1, tabListing<tabAceslstN<addrIP>, addrIP> done2, tabListing<tabAceslstN<addrIP>, addrIP> need2, tabSession sess, tabListing<tabAceslstN<addrIP>, addrIP> sent) {
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
    protected static String sess2str(tabSessionEntry ntry) {
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
    public static int getNullLabel(tabRouteEntry<addrIP> ntry) {
        if (ntry.prefix.network.isIPv4()) {
            return ipMpls.labelExp4;
        } else {
            return ipMpls.labelExp6;
        }
    }

    /**
     * get label
     *
     * @param labs labels
     * @return label
     */
    public static int getLabel(List<Integer> labs) {
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
     * get label
     *
     * @param ntry route entry
     * @return label
     */
    public static int getLabel(tabRouteEntry<addrIP> ntry) {
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
    public static String getBierLabs(tabLabelBierN ntry, byte[] full, int sis) {
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
    public static String negateOneCommand(String s) {
        s = s.replaceAll("_add ", "_del ");
        s = s.replaceAll("_mod ", "_del ");
        return s;
    }

    /**
     * dump api statistics
     *
     * @param tab table to dump
     * @return dump
     */
    public static userFormat dumpApiStats(tabGen<servP4langMsg> tab) {
        if (tab == null) {
            return null;
        }
        userFormat res = new userFormat("|", "message|count|last|ago");
        for (int i = 0; i < tab.size(); i++) {
            res.add("" + tab.get(i));
        }
        return res;
    }

    /**
     * update api statistics
     *
     * @param l table to dump
     * @param a message
     */
    public static void updateApiStats(tabGen<servP4langMsg> l, String a) {
        servP4langMsg m = new servP4langMsg(a);
        servP4langMsg o = l.add(m);
        if (o != null) {
            m = o;
        }
        m.cnt++;
        m.lst = bits.getTime();
    }

}
