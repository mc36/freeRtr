package tab;

import java.util.ArrayList;
import java.util.List;

import pack.packHolder;
import addr.addrPrefix;
import addr.addrType;

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

    private String parsePart(String s, T addr, T mask, tabIntMatcher port) {
        s = s.trim() + " ";
        int i = s.indexOf(" ");
        String a = s.substring(0, i).trim();
        s = s.substring(i, s.length()).trim() + " ";
        i = s.indexOf(" ");
        String b = s.substring(0, i).trim();
        s = s.substring(i, s.length()).trim() + " ";
        String c = a.toLowerCase();
        if (c.equals("any")) {
            addr.fromNetmask(0);
            mask.fromNetmask(0);
            s = b + " " + s;
        } else if (c.equals("host")) {
            if (addr.fromString(b)) {
                return null;
            }
            mask.fromNetmask(mask.maxBits());
        } else {
            if (addr.fromString(a)) {
                return null;
            }
            if (mask.fromString(b)) {
                return null;
            }
        }
        addr.setAnd(addr, mask);
        i = s.indexOf(" ");
        a = s.substring(0, i).trim();
        s = s.substring(i, s.length()).trim() + " ";
        if (port.fromString(a)) {
            return null;
        }
        return s;
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public boolean fromString(String s) {
        s = s.trim() + " ";
        int i = s.indexOf(" ");
        String a = s.substring(0, i).trim();
        s = s.substring(i, s.length()).trim();
        if (proto.fromString(a)) {
            return true;
        }
        s = parsePart(s, srcAddr, srcMask, srcPort);
        if (s == null) {
            return true;
        }
        s = parsePart(s, trgAddr, trgMask, trgPort);
        if (s == null) {
            return true;
        }
        String b;
        for (;;) {
            s = s.trim();
            if (s.length() < 1) {
                return false;
            }
            i = s.indexOf(" ");
            if (i < 0) {
                a = s;
                s = "";
            } else {
                a = s.substring(0, i).trim().toLowerCase();
                s = s.substring(i, s.length()).trim();
            }
            if (a.equals("log")) {
                logMatch = true;
                continue;
            }
            i = s.indexOf(" ");
            if (i < 0) {
                b = s;
                s = "";
            } else {
                b = s.substring(0, i).trim();
                s = s.substring(i, s.length()).trim();
            }
            if (a.equals("tos")) {
                if (tos.fromString(b)) {
                    return true;
                }
                continue;
            }
            if (a.equals("dscp")) {
                if (dscp.fromString(b)) {
                    return true;
                }
                continue;
            }
            if (a.equals("prec")) {
                if (prec.fromString(b)) {
                    return true;
                }
                continue;
            }
            if (a.equals("ttl")) {
                if (ttl.fromString(b)) {
                    return true;
                }
                continue;
            }
            if (a.equals("len")) {
                if (len.fromString(b)) {
                    return true;
                }
                continue;
            }
            if (a.equals("flag")) {
                if (flag.fromString(b)) {
                    return true;
                }
                continue;
            }
            return true;
        }
    }

    private String convPart(T addr, T mask, tabIntMatcher port) {
        String a = addr + " " + mask;
        if (mask.isFilled(0) && addr.isFilled(0)) {
            a = "any";
        }
        return a + " " + port;
    }

    public String toString() {
        String a = proto + " " + convPart(srcAddr, srcMask, srcPort) + " " + convPart(trgAddr, trgMask, trgPort);
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

    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + tabListingEntry.action2string(action) + " " + this);
        return l;
    }

    public boolean matches(int afi, addrPrefix<T> net) {
        return false;
    }

    public boolean matches(int afi, tabRouteEntry<T> net) {
        return false;
    }

    public void update(int afi, tabRouteEntry<T> net) {
    }

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
        if (!srcPort.matches(pck.UDPsrc)) {
            return false;
        }
        if (!trgPort.matches(pck.UDPtrg)) {
            return false;
        }
        if (!pck.IPsrc.isMatches(srcAddr, srcMask)) {
            return false;
        }
        if (!pck.IPtrg.isMatches(trgAddr, trgMask)) {
            return false;
        }
        return true;
    }

}
