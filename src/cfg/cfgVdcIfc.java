package cfg;

import java.util.Comparator;

/**
 * vdc resource interface
 *
 * @author matecsaba
 */
public class cfgVdcIfc implements Comparator<cfgVdcIfc> {

    /**
     * name of interface
     */
    public final String name;

    /**
     * config line of interface
     */
    public String line;

    /**
     * port number
     */
    public int port;

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

    public int compare(cfgVdcIfc o1, cfgVdcIfc o2) {
        return o1.name.compareTo(o2.name);
    }

}
