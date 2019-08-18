package prt;

import ip.ipFwdIface;
import pipe.pipeLine;
import tab.tabConnectLower;

/**
 * one server descriptor
 *
 * @author matecsaba
 */
public class prtGenServ implements tabConnectLower {

    /**
     * stream mode
     */
    public boolean stream;

    /**
     * packet handler
     */
    public prtServP serverP;

    /**
     * stream handler
     */
    public prtServS serverS;

    /**
     * interface
     */
    public ipFwdIface iface;

    /**
     * pipeline
     */
    public pipeLine sample;

    /**
     * port number
     */
    public int locP;

    /**
     * connection name
     */
    public String name;

    /**
     * password to use
     */
    public String passwd;

    /**
     * ttl to use
     */
    public int ttl;

    public String toString() {
        return name + " " + iface + " " + locP;
    }

    /**
     * dump
     *
     * @return string
     */
    public String dumper() {
        return "n/a";
    }

}
