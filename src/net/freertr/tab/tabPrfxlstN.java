package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.pack.packHolder;
import net.freertr.rtr.rtrBgpUtil;

/**
 * represents one prefix list entry (network and a range)
 *
 * @author matecsaba
 */
public class tabPrfxlstN extends tabListingEntry<addrIP> {

    /**
     * create instance
     */
    public tabPrfxlstN() {
    }

    /**
     * prefix that should matched
     */
    private addrIP network;

    /**
     * prefix default length
     */
    private int lenDef;

    /**
     * minimum length of prefix
     */
    private int lenMin;

    /**
     * maximum length of prefix
     */
    private int lenMax;

    /**
     * prefix of the net/def
     */
    private addrPrefix<addrIP> prefix;

    /**
     * list to evaluate
     */
    public tabListing<tabPrfxlstN, addrIP> evaluate;

    private int ranger(int i) {
        int o = network.maxBits();
        if (i < 0) {
            i = 0;
        }
        if (i > o) {
            i = o;
        }
        return i;
    }

    /**
     * create one prefix list entry
     *
     * @param net network to check
     * @param def default length
     * @param min minimal length
     * @param max maximal length
     */
    public void setNetwork(addrIP net, int def, int min, int max) {
        network = net.copyBytes();
        def = ranger(def);
        min = ranger(min);
        max = ranger(max);
        if (min < def) {
            min = def;
        }
        if (max < min) {
            max = min;
        }
        addrIP tmp = net.copyBytes();
        tmp.fromNetmask(def);
        network.setAnd(network, tmp);
        lenDef = def;
        lenMin = min;
        lenMax = max;
        prefix = new addrPrefix<addrIP>(network, lenDef);
    }

    /**
     * get current prefix
     *
     * @return current prefix
     */
    public addrPrefix<addrIP> getPrefix() {
        return new addrPrefix<addrIP>(network, lenDef);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        String a = "";
        if (logMatch) {
            a = " log";
        }
        int i = addrPrefix.dispSub(prefix);
        return tabListingEntry.action2string(action) + " " + network + "/" + (lenDef - i) + " ge " + (lenMin - i) + " le " + (lenMax - i) + a;
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @return string
     */
    public List<String> usrString(String beg) {
        String a;
        if (evaluate != null) {
            a = "evaluate " + tabListingEntry.action2string(action) + " " + evaluate.listName;
        } else {
            a = "" + this;
        }
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + a);
        return l;
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public boolean fromString(String s) {
        int min = 0;
        int max = 0;
        s += " ";
        int i = s.indexOf(" ");
        addrPrefix<addrIP> prf = addrPrefix.str2ip(s.substring(0, i).trim());
        if (prf == null) {
            return true;
        }
        s = s.substring(i, s.length()).trim() + " ";
        for (;;) {
            s = s.trim() + " ";
            if (s.length() <= 1) {
                break;
            }
            i = s.indexOf(" ");
            String b = s.substring(0, i).trim().toLowerCase();
            s = s.substring(i + 1, s.length());
            if (b.equals("log")) {
                logMatch = true;
                continue;
            }
            i = s.indexOf(" ");
            int o;
            try {
                o = Integer.parseInt(s.substring(0, i).trim());
            } catch (Exception e) {
                return true;
            }
            s = s.substring(i + 1, s.length());
            if (b.equals("ge")) {
                min = o;
                continue;
            }
            if (b.equals("le")) {
                max = o;
                continue;
            }
            return true;
        }
        i = addrPrefix.dispSub(prf);
        setNetwork(prf.network, prf.maskLen, min + i, max + i);
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
    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        if (evaluate != null) {
            tabPrfxlstN res = evaluate.find(afi, asn, net);
            if (res == null) {
                return false;
            }
            return res.matches(afi, asn, net);
        }
        return matches(afi, asn, net.prefix);
    }

    /**
     * test if matches
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean matches(packHolder pck) {
        if (evaluate != null) {
            tabPrfxlstN res = evaluate.find(pck);
            if (res == null) {
                return false;
            }
            return res.matches(pck);
        }
        return matches(rtrBgpUtil.sfiUnicast, 0, new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    /**
     * update entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     */
    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        if (evaluate != null) {
            tabPrfxlstN res = evaluate.find(afi, asn, net);
            if (res == null) {
                return false;
            }
            return res.matches(afi, asn, net);
        }
        if (net.maskLen < lenMin) {
            return false;
        }
        if (net.maskLen > lenMax) {
            return false;
        }
        return prefix.matches(net.network);
    }

}
