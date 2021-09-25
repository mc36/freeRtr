package ip;

import tab.tabConnectLower;

/**
 * stores one ip protocol
 *
 * @author matecsaba
 */
public class ipFwdProto implements tabConnectLower {

    /**
     * protocol number
     */
    public int proto = -1;

    /**
     * interface number
     */
    public ipFwdIface iface = null;

    /**
     * upper protocol
     */
    public ipPrt upper;

    public String toString() {
        return "" + upper;
    }

    /**
     * dump out
     *
     * @return string
     */
    public String dumper() {
        return "n/a";
    }

}
