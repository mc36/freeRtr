package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrPool;
import org.freertr.addr.addrType;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * address pool configuration
 *
 * @param <T> address type of pool
 * @author matecsaba
 */
public class cfgPool<T extends addrType> implements Comparable<cfgPool<T>>, cfgGeneric {

    /**
     * create instance
     *
     * @param s name
     */
    public cfgPool(String s) {
        name = s;
    }

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

    public int compareTo(cfgPool<T> o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String toString() {
        return name;
    }

    public void getHelp(userHelp l) {
    }

    public String getPrompt() {
        return "pool";
    }

    public List<String> getShRun(int filter) {
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
