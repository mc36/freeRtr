package org.freertr.tab;

import java.util.Comparator;
import org.freertr.addr.addrType;

/**
 * one connection entry
 *
 * @param <Ta> address type
 * @param <Td> data type
 * @author matecsaba
 */
public class tabConnectEntry<Ta extends addrType, Td extends tabConnectLower> implements Comparator<tabConnectEntry<Ta, Td>> {

    /**
     * create instance
     */
    public tabConnectEntry() {
    }

    /**
     * data to store
     */
    public Td data;

    /**
     * interface
     */
    public tabRouteIface iface;

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
     * hit count
     */
    public int hits;

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
        if (o1.iface == null) {
            if (o2.iface != null) {
                return -1;
            }
        } else {
            if (o2.iface == null) {
                return +1;
            }
            int i = o1.iface.compare(o1.iface, o2.iface);
            if (i != 0) {
                return i;
            }
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
     * @return dump data
     */
    public String dump() {
        String a;
        if (data == null) {
            a = "null";
        } else {
            a = data.dumper();
        }
        return name + "|" + a + "|" + iface + "|" + local + "|" + remote + "|" + peer + "|" + hits;
    }

}
