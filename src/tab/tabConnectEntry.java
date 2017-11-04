package tab;

import addr.addrPrefix;
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
    public addrPrefix<Ta> peer;

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

    private static int cmpNum(int a, int b) {
        if (a < b) {
            return -1;
        }
        if (a > b) {
            return +1;
        }
        return 0;
    }

    private static int cmpWild(int a, int b) { // 0=continue, 1=greater, 2=less
        if (b == 0) {
            if (a == 0) {
                return 0;
            }
            return 2;
        }
        if (a == 0) {
            return 1;
        }
        if (a != b) {
            return 2;
        }
        return 0;
    }

    /**
     * check if other greater
     *
     * @param other other to compare
     * @return true if greater
     */
    public boolean greaterLIR(tabConnectEntry<Ta, Td> other) {
        int i;
        i = cmpWild(local, other.local);
        if (i != 0) {
            return i == 1;
        }
        i = cmpWild(iface, other.iface);
        if (i != 0) {
            return i == 1;
        }
        i = cmpWild(remote, other.remote);
        if (i != 0) {
            return i == 1;
        }
        if (!peer.supernet(other.peer, false)) {
            return false;
        }
        return true;
    }

    /**
     * check if other greater
     *
     * @param other other to compare
     * @return true if greater
     */
    public boolean greaterILR(tabConnectEntry<Ta, Td> other) {
        int i;
        i = cmpWild(iface, other.iface);
        if (i != 0) {
            return i == 1;
        }
        i = cmpWild(local, other.local);
        if (i != 0) {
            return i == 1;
        }
        i = cmpWild(remote, other.remote);
        if (i != 0) {
            return i == 1;
        }
        if (!peer.supernet(other.peer, false)) {
            return false;
        }
        return true;
    }

    public int compare(tabConnectEntry<Ta, Td> o1, tabConnectEntry<Ta, Td> o2) {
        if (o1.greaterLIR(o2)) {
            return +1;
        }
        if (o2.greaterLIR(o1)) {
            return -1;
        }
        int i;
        i = cmpNum(o1.local, o2.local);
        if (i != 0) {
            return i;
        }
        i = cmpNum(o1.iface, o2.iface);
        if (i != 0) {
            return i;
        }
        i = cmpNum(o1.remote, o2.remote);
        if (i != 0) {
            return i;
        }
        return o1.peer.compare(o1.peer, o2.peer);
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
