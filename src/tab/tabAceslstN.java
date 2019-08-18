package tab;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import java.util.ArrayList;
import java.util.List;
import cfg.cfgAll;
import cfg.cfgObjnet;
import cfg.cfgObjprt;
import pack.packHolder;
import addr.addrPrefix;
import addr.addrType;
import util.bits;
import util.cmds;

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
        dscp = new tabIntMatcher();
        prec = new tabIntMatcher();
        ttl = new tabIntMatcher();
        len = new tabIntMatcher();
        flag = new tabIntMatcher();
        srcPort = new tabIntMatcher();
        trgPort = new tabIntMatcher();
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
        if (tos.action != tabIntMatcher.actionType.always) {
            a += " tos " + tos;
        }
        if (dscp.action != tabIntMatcher.actionType.always) {
            a += " dscp " + dscp;
        }
        if (prec.action != tabIntMatcher.actionType.always) {
            a += " prec " + prec;
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
            mask.fromNetmask(mask.maxBits());
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
     * @return string
     */
    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + tabListingEntry.action2string(action) + " " + this);
        return l;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, addrPrefix<T> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, tabRouteEntry<T> net) {
        return false;
    }

    /**
     * update entry
     *
     * @param afi address family
     * @param net network
     */
    public void update(int afi, tabRouteEntry<T> net) {
    }

    /**
     * test if matches
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean matches(packHolder pck) {
        if (!proto.matches(pck.IPprt)) {
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
        if (!ttl.matches(pck.IPttl)) {
            return false;
        }
        if (!len.matches(pck.dataSize())) {
            return false;
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
        return true;
    }

}
