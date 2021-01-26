package ip;

import addr.addrIP;
import java.util.Comparator;
import tab.tabLabelNtry;

/**
 * stores one multipoint neighbor
 *
 * @author matecsaba
 */
public class ipFwdMpNe implements Comparator<ipFwdMpNe> {

    /**
     * peer address
     */
    public final addrIP addr;

    /**
     * interface to use
     */
    public ipFwdIface iface;

    /**
     * remote label
     */
    public int labelR;

    /**
     * local label
     */
    public tabLabelNtry labelL;

    /**
     * create instance
     *
     * @param per peer address
     */
    public ipFwdMpNe(addrIP per) {
        addr = per.copyBytes();
    }

    public int compare(ipFwdMpNe o1, ipFwdMpNe o2) {
        return o1.addr.compare(o1.addr, o2.addr);
    }

}
