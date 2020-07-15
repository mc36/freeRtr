package tab;

import addr.addrType;
import ip.ipFwd;
import ip.ipFwdTab;
import java.util.Comparator;

/**
 * one connection entry
 *
 * @param <Ta> address type
 * @param <Td> data type
 * @author matecsaba
 */
public class tabConnectEntry<Ta extends addrType, Td extends tabConnectLower> implements Comparator<tabConnectEntry<Ta, Td>> {

    /**
     * data to store
     */
    public Td data;

    /**
     * interface number
     */
    public int iface;

    /**
     * peer address
     */
    public Ta peer;

    /**
     * local port
     */
    public int local;

    /**
     * remote port
     */
    public int remote;

    /**
     * connection name
     */
    public String name;

    /**
     * connection password
     */
    public String passwd;

    public int compare(tabConnectEntry<Ta, Td> o1, tabConnectEntry<Ta, Td> o2) {
        if (o1.local < o2.local) {
            return -1;
        }
        if (o1.local > o2.local) {
            return +1;
        }
        if (o1.iface < o2.iface) {
            return -1;
        }
        if (o1.iface > o2.iface) {
            return +1;
        }
        if (o1.remote < o2.remote) {
            return -1;
        }
        if (o1.remote > o2.remote) {
            return +1;
        }
        if (o1.peer == null) {
            if (o2.peer != null) {
                return +1;
            }
            return 0;
        }
        if (o2.peer == null) {
            return -1;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public String toString() {
        String a;
        if (data == null) {
            a = "null";
        } else {
            a = data.dumper();
        }
        return name + " #" + iface + " " + local + " " + remote + " " + peer + " " + a;
    }

    /**
     * dump out connection
     *
     * @param f forwarder
     * @return dump data
     */
    public String dump(ipFwd f) {
        String a;
        if (data == null) {
            a = "null";
        } else {
            a = data.dumper();
        }
        return name + "|" + a + "|" + ipFwdTab.iface2name(f, iface) + "|" + local + "|" + remote + "|" + peer;
    }

}
