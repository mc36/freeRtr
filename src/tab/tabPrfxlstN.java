package tab;

import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import addr.addrIP;
import addr.addrPrefix;

/**
 * represents one prefix list entry (network and a range)
 *
 * @author matecsaba
 */
public class tabPrfxlstN extends tabListingEntry<addrIP> {

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

    public String toString() {
        int i = addrPrefix.dispSub(prefix);
        return network + "/" + (lenDef - i) + " ge " + (lenMin - i) + " le " + (lenMax - i);
    }

    public List<String> usrString(String beg) {
        String a = "";
        if (logMatch) {
            a = " log";
        }
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + tabListingEntry.action2string(action) + " " + this + a);
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

    public boolean matches(tabRouteEntry<addrIP> net) {
        return matches(net.prefix);
    }

    public boolean matches(packHolder pck) {
        return matches(new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    public void update(tabRouteEntry<addrIP> net) {
    }

    public boolean matches(addrPrefix<addrIP> net) {
        if (net.maskLen < lenMin) {
            return false;
        }
        if (net.maskLen > lenMax) {
            return false;
        }
        return prefix.matches(net.network);
    }

}
