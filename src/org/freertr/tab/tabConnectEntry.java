package org.freertr.tab;


import org.freertr.addr.addrType;

/**
 * one connection entry
 *
 * @param <Ta> address type
 * @param <Td> data type
 * @author matecsaba
 */
public class tabConnectEntry<Ta extends addrType, Td extends tabConnectLower> implements Comparable<tabConnectEntry<Ta, Td>> {

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

    public int compareTo(tabConnectEntry<Ta, Td> o) {
        if (local < o.local) {
            return -1;
        }
        if (local > o.local) {
            return +1;
        }
        if (iface == null) {
            if (o.iface != null) {
                return -1;
            }
        } else {
            if (o.iface == null) {
                return +1;
            }
            int i = iface.compareTo(o.iface);
            if (i != 0) {
                return i;
            }
        }
        if (remote < o.remote) {
            return -1;
        }
        if (remote > o.remote) {
            return +1;
        }
        if (peer == null) {
            if (o.peer != null) {
                return +1;
            }
            return 0;
        }
        if (o.peer == null) {
            return -1;
        }
        return peer.compareTo(o.peer);
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
