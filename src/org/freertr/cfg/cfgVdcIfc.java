package org.freertr.cfg;

/**
 * vdc resource interface
 *
 * @author matecsaba
 */
public class cfgVdcIfc implements Comparable<cfgVdcIfc> {

    /**
     * name of interface
     */
    public final String name;

    /**
     * config line of interface
     */
    public String line;

    /**
     * local port number
     */
    public int portL;

    /**
     * remote port number
     */
    public int portR;

    /**
     * remote address
     */
    public String peer;

    /**
     * redundancy interface
     */
    public boolean redundancy = false;

    /**
     * create new interface
     *
     * @param nam name of it
     * @param lin line to store
     */
    public cfgVdcIfc(String nam, String lin) {
        name = nam;
        line = lin;
    }

    public String toString() {
        String a = "";
        if (redundancy) {
            a = " redundancy";
        }
        return name + a;
    }

    public int compareTo(cfgVdcIfc o) {
        return name.compareTo(o.name);
    }

}
