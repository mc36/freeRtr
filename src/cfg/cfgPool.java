package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import user.userHelping;
import util.cmds;
import addr.addrPool;
import addr.addrType;

/**
 * address pool configuration
 *
 * @param <T> address type of pool
 * @author matecsaba
 */
public class cfgPool<T extends addrType> implements Comparator<cfgPool<T>>, cfgGeneric {

    /**
     * name of pool
     */
    public String name;

    /**
     * address pool
     */
    public addrPool<T> pool;

    /**
     * beginning address
     */
    public T begin;

    /**
     * increments
     */
    public T increment;

    /**
     * number of addresses
     */
    public int number;

    /**
     * ip version
     */
    public int version;

    public int compare(cfgPool<T> o1, cfgPool<T> o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return name;
    }

    public userHelping getHelp() {
        return null;
    }

    public String getPrompt() {
        return "pool";
    }

    public List<String> getShRun(boolean filter) {
        List<String> lst = new ArrayList<String>();
        lst.add("ipv" + version + " pool " + name + " " + begin + " " + increment + " " + number);
        lst.add(cmds.comment);
        return lst;
    }

    public void doCfgStr(cmds cmd) {
    }

    /**
     * setup address pool
     *
     * @param ver version
     * @param beg beginning address
     * @param inc increment
     * @param num number of addresses
     */
    public void setup(int ver, T beg, T inc, int num) {
        version = ver;
        begin = beg;
        increment = inc;
        number = num;
        pool = new addrPool<T>(beg, inc, num);
    }

}
