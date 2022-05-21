package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgObjnet;
import net.freertr.cfg.cfgObjprt;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * represents one access list entry (source/addr/port, target/addr/port)
 *
 * @param <T> type of address
 * @author matecsaba
 */
public class tabAceslstN<T extends addrType> extends tabListingEntry<T> {

    /**
     * protocol
     */
    public tabIntMatcher proto;

    /**
     * source address
     */
    public T srcAddr;

    /**
     * source mask
     */
    public T srcMask;

    /**
     * source port
     */
    public tabIntMatcher srcPort;

    /**
     * target address
     */
    public T trgAddr;

    /**
     * target mask
     */
    public T trgMask;

    /**
     * target port
     */
    public tabIntMatcher trgPort;

    /**
     * source network
     */
    public tabListing<tabObjnetN<T>, T> srcOGnet;

    /**
     * source port
     */
    public tabListing<tabObjprtN<T>, T> srcOGprt;

    /**
     * target network
     */
    public tabListing<tabObjnetN<T>, T> trgOGnet;

    /**
     * target port
     */
    public tabListing<tabObjprtN<T>, T> trgOGprt;

    /**
     * flow
     */
    public tabIntMatcher flow;

    /**
     * tos
     */
    public tabIntMatcher tos;

    /**
     * dscp
     */
    public tabIntMatcher dscp;

    /**
     * prec
     */
    public tabIntMatcher prec;

    /**
     * sgt
     */
    public tabIntMatcher sgt;

    /**
     * ttl
     */
    public tabIntMatcher ttl;

    /**
     * length
     */
    public tabIntMatcher len;

    /**
     * flag
     */
    public tabIntMatcher flag;

    /**
     * fragment
     */
    public boolean frag;

    /**
     * list to evaluate
     */
    public tabListing<tabAceslstN<T>, T> evaluate;

    /**
     * list to forward reflect
     */
    public tabListing<tabAceslstN<T>, T> reflectFwd;

    /**
     * list to reverse reflect
     */
    public tabListing<tabAceslstN<T>, T> reflectRev;

    /**
     * timeout to reflect
     */
    public int reflectTim;

    /**
     * original entry
     */
    public tabAceslstN<T> rolledFrom;

    /**
     * create new access list entry
     *
     * @param adr empty address to use
     */
    @SuppressWarnings("unchecked")
    public tabAceslstN(T adr) {
        srcAddr = (T) adr.copyBytes();
        srcMask = (T) adr.copyBytes();
        trgAddr = (T) adr.copyBytes();
        trgMask = (T) adr.copyBytes();
        proto = new tabIntMatcher();
        tos = new tabIntMatcher();
        flow = new tabIntMatcher();
        dscp = new tabIntMatcher();
        prec = new tabIntMatcher();
        sgt = new tabIntMatcher();
        ttl = new tabIntMatcher();
        len = new tabIntMatcher();
        flag = new tabIntMatcher();
        frag = false;
        srcPort = new tabIntMatcher();
        trgPort = new tabIntMatcher();
    }

    /**
     * copy the matcher
     *
     * @return a copy
     */
    @SuppressWarnings("unchecked")
    public tabAceslstN<T> copyBytes() {
        tabAceslstN<T> r = new tabAceslstN<T>(srcAddr);
        r.srcAddr = (T) srcAddr.copyBytes();
        r.srcMask = (T) srcMask.copyBytes();
        r.trgAddr = (T) trgAddr.copyBytes();
        r.trgMask = (T) trgMask.copyBytes();
        r.proto = proto.copyBytes();
        r.tos = tos.copyBytes();
        r.flow = flow.copyBytes();
        r.dscp = dscp.copyBytes();
        r.prec = prec.copyBytes();
        r.sgt = sgt.copyBytes();
        r.ttl = ttl.copyBytes();
        r.len = len.copyBytes();
        r.flag = flag.copyBytes();
        r.frag = frag;
        r.srcPort = srcPort.copyBytes();
        r.trgPort = trgPort.copyBytes();
        r.srcOGnet = srcOGnet;
        r.srcOGprt = srcOGprt;
        r.trgOGnet = trgOGnet;
        r.trgOGprt = trgOGprt;
        return r;
    }

    private String convPart(T addr, T mask, tabIntMatcher port, tabListing<tabObjnetN<T>, T> ogNet, tabListing<tabObjprtN<T>, T> ogPrt) {
        String a = addr + " " + mask;
        if (mask.isFilled(0) && addr.isFilled(0)) {
            a = "any";
        }
        if (ogNet != null) {
            a = "obj " + ogNet.listName;
        }
        if (ogPrt == null) {
            return a + " " + port;
        } else {
            return a + " obj " + ogPrt.listName;
        }
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        String a = proto + " " + convPart(srcAddr, srcMask, srcPort, srcOGnet, srcOGprt) + " " + convPart(trgAddr, trgMask, trgPort, trgOGnet, trgOGprt);
        if (flow.action != tabIntMatcher.actionType.always) {
            a += " flow " + flow;
        }
        if (tos.action != tabIntMatcher.actionType.always) {
            a += " tos " + tos;
        }
        if (dscp.action != tabIntMatcher.actionType.always) {
            a += " dscp " + dscp;
        }
        if (prec.action != tabIntMatcher.actionType.always) {
            a += " prec " + prec;
        }
        if (sgt.action != tabIntMatcher.actionType.always) {
            a += " sgt " + sgt;
        }
        if (ttl.action != tabIntMatcher.actionType.always) {
            a += " ttl " + ttl;
        }
        if (len.action != tabIntMatcher.actionType.always) {
            a += " len " + len;
        }
        if (flag.action != tabIntMatcher.actionType.always) {
            a += " flag " + flag;
        }
        if (frag) {
            a += " frag";
        }
        if (logMatch) {
            a += " log";
        }
        return a;
    }

    private static boolean parsePart(tabAceslstN<addrIP> ntry, cmds cmd, boolean src) {
        addrIP addr;
        addrIP mask;
        tabIntMatcher port;
        if (src) {
            addr = ntry.srcAddr;
            mask = ntry.srcMask;
            port = ntry.srcPort;
        } else {
            addr = ntry.trgAddr;
            mask = ntry.trgMask;
            port = ntry.trgPort;
        }
        String a = cmd.word();
        if (a.equals("obj")) {
            cfgObjnet og = cfgAll.objnetFind(cmd.word(), false);
            if (og == null) {
                return true;
            }
            if (src) {
                ntry.srcOGnet = og.objgrp;
            } else {
                ntry.trgOGnet = og.objgrp;
            }
        } else if (a.equals("any")) {
            addr.fromNetmask(0);
            mask.fromNetmask(0);
        } else if (a.equals("host")) {
            if (addr.fromString(cmd.word())) {
                return true;
            }
            mask.setAddr(getMaxMask(addr));
        } else {
            if (addr.fromString(a)) {
                return true;
            }
            a = cmd.word();
            if (a.startsWith("/")) {
                if (addr.isIPv4()) {
                    addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(addr.toIPv4(), bits.str2num(a.substring(1, a.length())));
                    mask.fromIPv4addr(prf.mask);
                } else {
                    addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(addr.toIPv6(), bits.str2num(a.substring(1, a.length())));
                    mask.fromIPv6addr(prf.mask);
                }
            } else if (mask.fromString(a)) {
                return true;
            }
        }
        addr.setAnd(addr, mask);
        a = cmd.word();
        if (a.equals("obj")) {
            cfgObjprt og = cfgAll.objprtFind(cmd.word(), false);
            if (og == null) {
                return true;
            }
            if (src) {
                ntry.srcOGprt = og.objgrp;
            } else {
                ntry.trgOGprt = og.objgrp;
            }
        } else {
            if (port.fromString(a)) {
                return true;
            }
        }
        return false;
    }

    /**
     * convert string to ace
     *
     * @param ntry ace to update
     * @param cmd string to convert
     * @return true if error happened
     */
    public static boolean fromString(tabAceslstN<addrIP> ntry, cmds cmd) {
        if (ntry.proto.fromString(cmd.word())) {
            return true;
        }
        if (parsePart(ntry, cmd, true)) {
            return true;
        }
        if (parsePart(ntry, cmd, false)) {
            return true;
        }
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                return false;
            }
            if (a.equals("log")) {
                ntry.logMatch = true;
                continue;
            }
            if (a.equals("flow")) {
                if (ntry.flow.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            if (a.equals("tos")) {
                if (ntry.tos.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            if (a.equals("dscp")) {
                if (ntry.dscp.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            if (a.equals("prec")) {
                if (ntry.prec.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            if (a.equals("sgt")) {
                if (ntry.sgt.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            if (a.equals("ttl")) {
                if (ntry.ttl.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            if (a.equals("len")) {
                if (ntry.len.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            if (a.equals("frag")) {
                ntry.frag = true;
                continue;
            }
            if (a.equals("flag")) {
                if (ntry.flag.fromString(cmd.word())) {
                    return true;
                }
                continue;
            }
            return true;
        }
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
        String a;
        if (evaluate != null) {
            a = "evaluate " + tabListingEntry.action2string(action) + " " + evaluate.listName;
        } else {
            a = tabListingEntry.action2string(action) + " " + this;
        }
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + a);
        if (reflectFwd != null) {
            l.add(beg + "sequence " + sequence + " reflect " + reflectFwd.listName + " " + reflectRev.listName + " " + reflectTim);
        }
        return l;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, addrPrefix<T> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, tabRouteEntry<T> net) {
        return false;
    }

    /**
     * update entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     */
    public void update(int afi, int asn, tabRouteEntry<T> net) {
    }

    /**
     * test if matches
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean matches(packHolder pck) {
        if (evaluate != null) {
            tabAceslstN<T> res = evaluate.find(pck);
            if (res == null) {
                return false;
            }
            if (!res.matches(pck)) {
                return false;
            }
            doReflect(pck);
            return true;
        }
        if (!proto.matches(pck.IPprt)) {
            return false;
        }
        if (!flow.matches(pck.IPid)) {
            return false;
        }
        if (!tos.matches(pck.IPtos)) {
            return false;
        }
        if (!dscp.matches(pck.IPtos >>> 2)) {
            return false;
        }
        if (!prec.matches(pck.IPtos >>> 5)) {
            return false;
        }
        if (!sgt.matches(pck.SGTid)) {
            return false;
        }
        if (!ttl.matches(pck.IPttl)) {
            return false;
        }
        if (!len.matches(pck.dataSize())) {
            return false;
        }
        if (frag) {
            if (!pck.IPmf && (pck.IPfrg < 1)) {
                return false;
            }
        }
        if (!flag.matches(pck.TCPflg)) {
            return false;
        }
        if (srcOGprt != null) {
            if (!srcOGprt.matches(false, false, pck)) {
                return false;
            }
        } else {
            if (!srcPort.matches(pck.UDPsrc)) {
                return false;
            }
        }
        if (trgOGprt != null) {
            int old = pck.UDPsrc;
            pck.UDPsrc = pck.UDPtrg;
            boolean b = trgOGprt.matches(false, false, pck);
            pck.UDPsrc = old;
            if (!b) {
                return false;
            }
        } else {
            if (!trgPort.matches(pck.UDPtrg)) {
                return false;
            }
        }
        if (srcOGnet != null) {
            if (!srcOGnet.matches(false, false, pck)) {
                return false;
            }
        } else {
            if (!pck.IPsrc.isMatches(srcAddr, srcMask)) {
                return false;
            }
        }
        if (trgOGnet != null) {
            addrIP old = pck.IPsrc.copyBytes();
            pck.IPsrc.setAddr(pck.IPtrg);
            boolean b = trgOGnet.matches(false, false, pck);
            pck.IPsrc.setAddr(old);
            if (!b) {
                return false;
            }
        } else {
            if (!pck.IPtrg.isMatches(trgAddr, trgMask)) {
                return false;
            }
        }
        doReflect(pck);
        return true;
    }

    private void doReflect(packHolder pck) {
        if (reflectFwd == null) {
            return;
        }
        tabAceslstN<T> ntry = pack2ace(pck);
        ntry.sequence = reflectFwd.nextseq();
        ntry.action = action;
        ntry.timeout = reflectTim;
        reflectFwd.add(ntry);
        ntry = pack2ace(pck);
        ntry.reverseAce();
        ntry.sequence = reflectRev.nextseq();
        ntry.action = action;
        ntry.timeout = reflectTim;
        reflectRev.add(ntry);
    }

    /**
     * reverse this ace
     */
    public void reverseAce() {
        tabIntMatcher im = srcPort;
        srcPort = trgPort;
        trgPort = im;
        T a = srcAddr;
        srcAddr = trgAddr;
        trgAddr = a;
        a = srcMask;
        srcMask = trgMask;
        trgMask = a;
        tabListing<tabObjnetN<T>, T> on = srcOGnet;
        srcOGnet = trgOGnet;
        trgOGnet = on;
        tabListing<tabObjprtN<T>, T> op = srcOGprt;
        srcOGprt = trgOGprt;
        trgOGprt = op;
    }

    /**
     * create ace from packet
     *
     * @param pck packet
     * @return ace
     */
    public tabAceslstN<T> pack2ace(packHolder pck) {
        tabAceslstN<T> ntry = new tabAceslstN<T>(trgAddr);
        ntry.proto.setExact(pck.IPprt);
        ntry.srcPort.setExact(pck.UDPsrc);
        ntry.trgPort.setExact(pck.UDPtrg);
        ntry.srcAddr.setAddr(pck.IPsrc);
        ntry.trgAddr.setAddr(pck.IPtrg);
        ntry.srcMask.setAddr(getMaxMask(pck.IPsrc));
        ntry.trgMask.setAddr(getMaxMask(pck.IPtrg));
        ntry.lastMatch = bits.getTime();
        return ntry;
    }

    /**
     * get host mask for address
     *
     * @param adr address
     * @return netmask
     */
    public static addrIP getMaxMask(addrIP adr) {
        addrIP res = new addrIP();
        if (adr.isIPv4()) {
            addrIPv4 msk = new addrIPv4();
            msk.fromNetmask(msk.maxBits());
            res.fromIPv4addr(msk);
        } else {
            addrIPv6 msk = new addrIPv6();
            msk.fromNetmask(msk.maxBits());
            res.fromIPv6addr(msk);
        }
        return res;
    }

    /**
     * unroll this ace into an other list
     *
     * @param trg target list
     * @param ace source ace
     * @param neg negate ace
     */
    public static void unrollAce(tabListing<tabAceslstN<addrIP>, addrIP> trg, tabAceslstN<addrIP> ace, boolean neg) {
        if (ace.evaluate != null) {
            unrollAcl(trg, ace.evaluate, ace.action != actionType.actPermit);
            return;
        }
        actionType act;
        if (neg) {
            act = tabListingEntry.negateAction(ace.action);
        } else {
            act = ace.action;
        }
        tabAceslstN<addrIP> ntry;
        int sn = 0;
        int sp = 0;
        int dn = 0;
        int dp = 0;
        for (;;) {
            ntry = ace.copyBytes();
            ntry.srcOGnet = null;
            ntry.srcOGprt = null;
            ntry.trgOGnet = null;
            ntry.trgOGprt = null;
            if (ace.srcOGnet != null) {
                tabObjnetN<addrIP> obj = ace.srcOGnet.get(sn);
                ntry.srcAddr = obj.addr;
                ntry.srcMask = obj.mask;
            }
            if (ace.srcOGprt != null) {
                ntry.srcPort = ace.srcOGprt.get(sp).port;
            }
            if (ace.trgOGnet != null) {
                tabObjnetN<addrIP> obj = ace.trgOGnet.get(dn);
                ntry.trgAddr = obj.addr;
                ntry.trgMask = obj.mask;
            }
            if (ace.trgOGprt != null) {
                ntry.trgPort = ace.trgOGprt.get(dp).port;
            }
            ntry.sequence = trg.nextseq();
            ntry.action = act;
            ntry.rolledFrom = ace;
            ntry.reflectFwd = ace.reflectFwd;
            ntry.reflectRev = ace.reflectRev;
            ntry.reflectTim = ace.reflectTim;
            trg.add(ntry);
            boolean incr = true;
            if (incr && (ace.trgOGprt != null)) {
                dp++;
                incr = dp >= ace.trgOGprt.size();
                if (incr) {
                    dp = 0;
                }
            }
            if (incr && (ace.trgOGnet != null)) {
                dn++;
                incr = dn >= ace.trgOGnet.size();
                if (incr) {
                    dn = 0;
                }
            }
            if (incr && (ace.srcOGprt != null)) {
                sp++;
                incr = sp >= ace.srcOGprt.size();
                if (incr) {
                    sp = 0;
                }
            }
            if (incr && (ace.srcOGnet != null)) {
                sn++;
                incr = sn >= ace.srcOGnet.size();
                if (incr) {
                    sn = 0;
                }
            }
            if (incr) {
                break;
            }
        }
    }

    /**
     * unroll one list
     *
     * @param trg target list
     * @param src source list
     * @param neg negate aces
     */
    public static void unrollAcl(tabListing<tabAceslstN<addrIP>, addrIP> trg, tabListing<tabAceslstN<addrIP>, addrIP> src, boolean neg) {
        for (int i = 0; i < src.size(); i++) {
            unrollAce(trg, src.get(i), neg);
        }
    }

    /**
     * unroll one list
     *
     * @param src source list
     * @return result
     */
    public static tabListing<tabAceslstN<addrIP>, addrIP> unrollAcl(tabListing<tabAceslstN<addrIP>, addrIP> src) {
        tabListing<tabAceslstN<addrIP>, addrIP> res = new tabListing<tabAceslstN<addrIP>, addrIP>();
        res.copyCores(src);
        res.listName = "unroll of " + src.listName;
        unrollAcl(res, src, false);
        return res;
    }

    /**
     * size of one list
     *
     * @param src source list
     * @return number of aces
     */
    public static int sizeofAcl(tabListing<tabAceslstN<addrIP>, addrIP> src) {
        int o = 0;
        for (int i = 0; i < src.size(); i++) {
            tabAceslstN<addrIP> ace = src.get(i);
            if (ace.evaluate != null) {
                o += sizeofAcl(ace.evaluate);
                continue;
            }
            int p = 1;
            if (ace.srcOGnet != null) {
                p *= ace.srcOGnet.size();
            }
            if (ace.srcOGprt != null) {
                p *= ace.srcOGprt.size();
            }
            if (ace.trgOGnet != null) {
                p *= ace.trgOGnet.size();
            }
            if (ace.trgOGprt != null) {
                p *= ace.trgOGprt.size();
            }
            o += p;
        }
        return o;
    }

}
