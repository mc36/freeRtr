package tab;

import addr.addrIP;
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
