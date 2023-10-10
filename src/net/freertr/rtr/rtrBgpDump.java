package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.enc.enc7bit;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.counter;

/**
 * bgp message dumper
 *
 * @author matecsaba
 */
public class rtrBgpDump {

    private rtrBgpDump() {
    }

    /**
     * counters to statistics
     *
     * @param c counter
     * @return statistics
     */
    public static String counter2stats(counter c) {
        return c.packTx + "|" + c.packRx + "|" + c.byteTx + "|" + c.byteRx
                + "|" + bits.timePast(c.lastTx) + "|" + bits.timePast(c.lastRx)
                + "|" + bits.time2str(cfgAll.timeZoneName, c.lastTx + cfgAll.timeServerOffset, 3)
                + "|" + bits.time2str(cfgAll.timeZoneName, c.lastRx + cfgAll.timeServerOffset, 3);
    }

    /**
     * summarize unknown attributes
     *
     * @param c counters
     * @return summary of unknowns
     */
    public static counter sumUnkAttrs(counter c[]) {
        counter res = new counter();
        for (int i = 0; i < c.length; i++) {
            if (!rtrBgpUtil.isUnknownAttr(i)) {
                continue;
            }
            res.plus(c[i]);
        }
        return res;
    }

    /**
     * summarize unknown attributes
     *
     * @param c counters
     * @return summary of unknowns
     */
    public static counter sumUnkMsgs(counter c[]) {
        counter res = new counter();
        for (int i = 0; i < c.length; i++) {
            if (!rtrBgpUtil.isUnknownMsg(i)) {
                continue;
            }
            res.plus(c[i]);
        }
        return res;
    }

    /**
     * get unknown summary
     *
     * @param l list to update
     * @param t true to attributes, false to messages
     * @param c counters
     * @param sr rx separator
     * @param st tx separator
     */
    public static void getUnknwSum(userFormat l, boolean t, counter c[], String sr, String st) {
        counter r;
        String a;
        if (t) {
            r = sumUnkAttrs(c);
            a = "attributes";
        } else {
            r = sumUnkMsgs(c);
            a = "messages";
        }
        l.add("unknown " + a + sr + r.packRx + st + r.packTx);
    }

    /**
     * get message statistics
     *
     * @param l list to append
     * @param t message type
     * @param c counters
     * @param sr rx separator
     * @param st tx separator
     */
    public static void getMsgStats(userFormat l, int t, counter c[], String st, String sr) {
        l.add(rtrBgpUtil.msgType2string(t) + " message" + st + c[t].packTx + sr + c[t].packRx);
    }

    /**
     * get message statistics
     *
     * @param s statistics
     * @return list of statistics
     */
    public static userFormat getMsgStats(counter s[]) {
        userFormat l = new userFormat("|", "typ|name|tx|rx|tx|rx|tx|rx|tx|rx", "2|2pack|2byte|2ago|2last");
        for (int i = 0; i < s.length; i++) {
            counter c = s[i];
            l.add(i + "|" + rtrBgpUtil.msgType2string(i) + "|" + counter2stats(c));
        }
        return l;
    }

    /**
     * get message statistics
     *
     * @param s statistics
     * @return list of statistics
     */
    public static userFormat getAttrStats(counter s[]) {
        userFormat l = new userFormat("|", "typ|name|tx|rx|tx|rx|tx|rx|tx|rx", "2|2pack|2byte|2ago|2last");
        for (int i = 0; i < s.length; i++) {
            counter c = s[i];
            l.add(i + "|" + rtrBgpUtil.attrType2string(i) + "|" + counter2stats(c));
        }
        return l;
    }

    /**
     * get reachable statistics
     *
     * @param l list to append
     * @param cr reachable statistics
     * @param cu unreachable statistics
     * @param sr rx separator
     * @param st tx separator
     */
    public static void getUnReachStats(userFormat l, counter cr, counter cu, String sr, String st) {
        l.add("reachable messages" + sr + cr.packRx + st + cr.packTx);
        l.add("unreachable messages" + sr + cu.packRx + st + cu.packTx);
    }

    /**
     * convert hexdump log to packet
     *
     * @param s string to convert
     * @return converted packet
     */
    public static packHolder log2pck(String s) {
        if (s == null) {
            return null;
        }
        String a = s.replaceAll("\\|", "");
        int i = a.indexOf("ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff");
        if (i < 1) {
            return null;
        }
        s = a.substring(i, a.length());
        a = a.substring(0, i);
        s = s.replaceAll(" ", "");
        packHolder pck = new packHolder(true, true);
        pck.INTtime = bits.str2time(cfgAll.timeZoneName, a);
        int o = s.length() & 0xfffffe;
        for (i = 0; i < o; i += 2) {
            pck.putByte(0, bits.fromHex(s.substring(i, i + 2)));
            pck.putSkip(1);
            pck.merge2end();
        }
        i = a.indexOf("->");
        if (i < 0) {
            return pck;
        }
        s = a.substring(i + 2, a.length()).trim();
        a = a.substring(0, i).trim();
        i = a.lastIndexOf(" ");
        if (i > 0) {
            a = a.substring(i + 1, a.length());
        }
        i = s.indexOf(" ");
        if (i > 0) {
            s = s.substring(0, i);
        }
        pck.IPsrc.fromString(a);
        pck.IPtrg.fromString(s);
        return pck;
    }

    /**
     * decode bgp dumps
     *
     * @param txt text to read
     * @return list of packets
     */
    public static List<packHolder> logs2pcks(List<String> txt) {
        List<packHolder> res = new ArrayList<packHolder>();
        if (txt == null) {
            return res;
        }
        for (int i = 0; i < txt.size(); i++) {
            String a = txt.get(i);
            packHolder p = log2pck(a);
            if (p == null) {
                continue;
            }
            res.add(p);
        }
        return res;
    }

    /**
     * dump one packet
     *
     * @param pck packet to dump
     * @return text dump of the packet
     */
    public static List<String> dumpPacket(packHolder pck) {
        pck = pck.copyBytes(true, true);
        pck.merge2end();
        List<String> res = new ArrayList<String>();
        res.add(bits.time2str(cfgAll.timeZoneName, pck.INTtime + cfgAll.timeServerOffset, 3) + " " + pck.IPsrc + " -> " + pck.IPtrg);
        enc7bit.buf2hex(res, pck.getCopy(), 0);
        if (rtrBgpUtil.checkHeader(pck)) {
            return res;
        }
        pck.getSkip(rtrBgpUtil.sizeU);
        packHolder hlp = new packHolder(true, true);
        res.add("len=" + pck.IPsiz + " typ=" + pck.IPprt + " " + rtrBgpUtil.msgType2string(pck.IPprt));
        int prt = pck.msbGetW(0);
        pck.getSkip(2);
        res.add("withdraw len=" + prt);
        prt = pck.dataSize() - prt;
        tabRouteEntry<addrIP> ntry;
        for (;;) {
            if (pck.dataSize() <= prt) {
                break;
            }
            ntry = rtrBgpUtil.readPrefix(rtrBgpUtil.safiIp4uni, true, pck);
            if (res == null) {
                continue;
            }
            res.add("withdrawn " + addrPrefix.ip2str(ntry.prefix));
        }
        pck.setBytesLeft(prt);
        prt = pck.msbGetW(0);
        pck.getSkip(2);
        prt = pck.dataSize() - prt;
        res.add("attrib len=" + prt);
        ntry = new tabRouteEntry<addrIP>();
        for (;;) {
            if (pck.dataSize() <= prt) {
                break;
            }
            rtrBgpUtil.parseAttrib(pck, hlp);
            res.add("  attrib typ " + hlp.ETHtype + " " + rtrBgpUtil.attrType2string(hlp.ETHtype));
            res.add("   " + bits.byteDump(hlp.getCopy(), 0, -1));
            rtrBgpUtil.interpretAttribute(null, ntry, hlp);
        }
        res.add("reachable len=" + pck.dataSize());
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            ntry = rtrBgpUtil.readPrefix(rtrBgpUtil.safiIp4uni, true, pck);
            if (res == null) {
                continue;
            }
            res.add("  reachable " + addrPrefix.ip2str(ntry.prefix));
        }
        //////////////////
        return res;
    }

}
