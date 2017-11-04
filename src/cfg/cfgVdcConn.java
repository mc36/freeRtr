package cfg;

import java.util.Comparator;

/**
 * vdc connection
 *
 * @author matecsaba
 */
public class cfgVdcConn implements Comparator<cfgVdcConn> {

    /**
     * name of interface
     */
    public final String name;

    /**
     * peer vdc
     */
    public cfgVdc peer;

    /**
     * peer vdc connection
     */
    public cfgVdcConn conn;

    /**
     * port number
     */
    public int port;

    /**
     * create new connection
     *
     * @param nam name of interface
     * @param per peer vdc
     */
    public cfgVdcConn(String nam, cfgVdc per) {
        name = nam;
        peer = per;
    }

    public String toString() {
        return name + " " + peer.name;
    }

    public int compare(cfgVdcConn o1, cfgVdcConn o2) {
        return o1.name.compareTo(o2.name);
    }

}
