package addr;

import java.util.Comparator;

/**
 * represents one prefix (address/netmask)
 *
 * @param <T> class of address
 * @author matecsaba
 */
public class addrPrefix<T extends addrType> implements Comparator<addrPrefix<T>> {

    /**
     * network address
     */
    public T network;

    /**
     * the netmask
     */
    public T mask;

    /**
     * wildcard mask
     */
    public T wildcard;

    /**
     * broadcast address
     */
    public T broadcast;

    /**
     * netmask length
     */
    public int maskLen;

    private addrPrefix() {
    }

    /**
     * create new prefix
     *
     * @param adr address in the prefix
     * @param msk netmask size in bits
     */
    @SuppressWarnings("unchecked")
    public addrPrefix(T adr, int msk) {
        maskLen = rangeCheckMask(adr, msk);
        network = (T) adr.copyBytes();
        mask = (T) adr.copyBytes();
        wildcard = (T) adr.copyBytes();
        broadcast = (T) adr.copyBytes();
        update();
    }

    /**
     * clone this prefix
     *
     * @return new instance containing same data
     */
    @SuppressWarnings("unchecked")
    public addrPrefix<T> copyBytes() {
        addrPrefix<T> n = new addrPrefix<T>();
        n.network = (T) network.copyBytes();
        n.mask = (T) mask.copyBytes();
        n.wildcard = (T) wildcard.copyBytes();
        n.broadcast = (T) broadcast.copyBytes();
        n.maskLen = maskLen;
        return n;
    }

    /**
     * compare two instances
     *
     * @param o1 first
     * @param o2 second
     * @return as usual
     */
    public int compare(addrPrefix<T> o1, addrPrefix<T> o2) {
        int i = network.compare(o1.network, o2.network);
        if (i != 0) {
            return i;
        }
        return -broadcast.compare(o1.broadcast, o2.broadcast);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return network + "/" + maskLen;
    }

    private int rangeCheckMask(T adr, int msk) {
        if (msk > adr.maxBits()) {
            msk = adr.maxBits();
        }
        if (msk < 0) {
            msk = 0;
        }
        return msk;
    }

    private void update() {
        mask.fromNetmask(maskLen);
        network.setAnd(network, mask);
        wildcard.setNot(mask);
        broadcast.setOr(network, wildcard);
    }

    /**
     * change just mask
     *
     * @param msk netmask size
     */
    public void setMask(int msk) {
        maskLen = rangeCheckMask(network, msk);
        update();
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public boolean fromString(String s) {
        s = s.trim();
        int i = s.indexOf("/");
        if (i < 0) {
            return true;
        }
        if (network.fromString(s.substring(0, i).trim())) {
            return true;
        }
        try {
            maskLen = Integer.parseInt(s.substring(i + 1, s.length()).trim());
        } catch (Exception e) {
            return true;
        }
        maskLen = rangeCheckMask(network, maskLen);
        update();
        return false;
    }

    /**
     * test if this prefix contains an address
     *
     * @param adr address to test
     * @return true if matches, false if not matches
     */
    public boolean matches(T adr) {
        if (adr.compare(adr, network) < 0) {
            return false;
        }
        if (adr.compare(adr, broadcast) > 0) {
            return false;
        }
        return true;
    }

    /**
     * test if this prefix's network is address
     *
     * @param adr address to test
     * @return true if this is the network address
     */
    public boolean isNetwork(T adr) {
        return (adr.compare(adr, network) == 0);
    }

    /**
     * test if this prefix's broadcast is address
     *
     * @param adr address to test
     * @return true if this is the broadcast address
     */
    public boolean isBroadcast(T adr) {
        return (adr.compare(adr, broadcast) == 0);
    }

    /**
     * test if this prefix contains this unicast address
     *
     * @param adr address to test
     * @return true if this is the broadcast address
     */
    public boolean isUnicast(T adr) {
        boolean b = matches(adr);
        b &= !isNetwork(adr);
        b &= !isBroadcast(adr);
        return b;
    }

    /**
     * test if this prefix contains an other
     *
     * @param other other to test
     * @param equals set true to allow equals to contain
     * @return true if other is subnet of this, false otherwise
     */
    public boolean supernet(addrPrefix<T> other, boolean equals) {
        if (!matches(other.network)) {
            return false;
        }
        if (!matches(other.broadcast)) {
            return false;
        }
        if (equals) {
            return true;
        }
        if (network.compare(network, other.network) != 0) {
            return true;
        }
        if (broadcast.compare(broadcast, other.broadcast) != 0) {
            return true;
        }
        return false;
    }

    /**
     * convert ipv4 prefix to ip
     *
     * @param old old prefix
     * @return new prefix
     */
    public static addrPrefix<addrIP> ip4toIP(addrPrefix<addrIPv4> old) {
        addrIPv4 a4 = addrIPv4.getBroadcast();
        a4.fromNetmask(old.maskLen);
        addrIP a1 = new addrIP();
        addrIP a2 = new addrIP();
        a1.fromIPv4mask(a4);
        a2.fromIPv4addr(old.network);
        return new addrPrefix<addrIP>(a2, a1.toNetmask());
    }

    /**
     * convert ipv6 prefix to ip
     *
     * @param old old prefix
     * @return new prefix
     */
    public static addrPrefix<addrIP> ip6toIP(addrPrefix<addrIPv6> old) {
        addrIPv6 a6 = addrIPv6.getAllNodes();
        a6.fromNetmask(old.maskLen);
        addrIP a1 = new addrIP();
        addrIP a2 = new addrIP();
        a1.fromIPv6mask(a6);
        a2.fromIPv6addr(old.network);
        return new addrPrefix<addrIP>(a2, a1.toNetmask());
    }

    /**
     * convert prefix to ipv4
     *
     * @param old old prefix
     * @return new prefix
     */
    public static addrPrefix<addrIPv4> ip2ip4(addrPrefix<addrIP> old) {
        addrIPv4 a1 = old.network.toIPv4();
        addrIPv4 a2 = old.mask.toIPv4();
        return new addrPrefix<addrIPv4>(a1, a2.toNetmask());
    }

    /**
     * convert prefix to ipv6
     *
     * @param old old prefix
     * @return new prefix
     */
    public static addrPrefix<addrIPv6> ip2ip6(addrPrefix<addrIP> old) {
        addrIPv6 a1 = old.network.toIPv6();
        addrIPv6 a2 = old.mask.toIPv6();
        return new addrPrefix<addrIPv6>(a1, a2.toNetmask());
    }

    /**
     * convert string to ip prefix
     *
     * @param s string to convert
     * @return null if failed, prefix if succeeded
     */
    public static addrPrefix<addrIP> str2ip(String s) {
        addrPrefix<addrIPv4> p4 = new addrPrefix<addrIPv4>(addrIPv4.getBroadcast(), 0);
        if (!p4.fromString(s)) {
            return ip4toIP(p4);
        }
        addrPrefix<addrIPv6> p6 = new addrPrefix<addrIPv6>(addrIPv6.getAllNodes(), 0);
        if (!p6.fromString(s)) {
            return ip6toIP(p6);
        }
        int i = s.indexOf("#");
        if (i < 0) {
            return null;
        }
        addrPrefix<addrIP> p = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 128);
        if (p.network.fromString(s.substring(0, i))) {
            return null;
        }
        if (p.broadcast.fromString(s.substring(i + 1, s.length()))) {
            return null;
        }
        return p;
    }

    /**
     * convert prefix to string
     *
     * @param old prefix to convert
     * @return string representation
     */
    public static String ip2str(addrPrefix<addrIP> old) {
        if (old.network.isIPv4()) {
            return "" + ip2ip4(old);
        } else {
            return "" + ip2ip6(old);
        }
    }

    /**
     * convert prefix to string
     *
     * @param old prefix to convert
     * @return string representation
     */
    public static String ip2evpn(addrPrefix<addrIP> old) {
        return old.network + "#" + old.broadcast;
    }

    /**
     * get default route
     *
     * @return default route
     */
    public static addrPrefix<addrIPv4> defaultRoute4() {
        return new addrPrefix<addrIPv4>(new addrIPv4(), 0);
    }

    /**
     * get default route
     *
     * @return default route
     */
    public static addrPrefix<addrIPv6> defaultRoute6() {
        return new addrPrefix<addrIPv6>(new addrIPv6(), 0);
    }

    /**
     * get default route
     *
     * @param ver version number
     * @return default route
     */
    public static addrPrefix<addrIP> defaultRoute(int ver) {
        if (ver == 4) {
            return ip4toIP(defaultRoute4());
        } else {
            return ip6toIP(defaultRoute6());
        }
    }

    /**
     * substract on displaying
     *
     * @param old prefix to test
     * @return substract value
     */
    public static int dispSub(addrPrefix<addrIP> old) {
        int i;
        if (old.network.isIPv4()) {
            i = addrIPv4.size;
        } else {
            i = addrIPv6.size;
        }
        return (addrIP.size - i) * 8;
    }

}
