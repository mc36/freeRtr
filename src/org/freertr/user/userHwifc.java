package org.freertr.user;

import java.util.List;

/**
 * one interface
 *
 * @author matecsaba
 */
public class userHwifc implements Comparable<userHwifc> {

    /**
     * create instance
     */
    public userHwifc() {
    }

    /**
     * name of interface
     */
    public String name = "";

    /**
     * mac address
     */
    public String mac = "";

    public int compareTo(userHwifc o) {
        return name.compareTo(o.name);
    }

    public String toString() {
        return name + " " + mac;
    }

    /**
     * from raw text
     *
     * @param rd text
     * @param ln position
     * @return null on error, entry on success
     */
    public static userHwifc fromRaw(List<String> rd, int ln) {
        String a = rd.get(ln).trim();
        int i = a.indexOf(" mtu ");
        if (i < 0) {
            return null;
        }
        i = a.indexOf(": ");
        if (i < 0) {
            return null;
        }
        if (i > 4) {
            return null;
        }
        a = a.substring(i + 2, a.length());
        i = a.indexOf(": ");
        if (i < 0) {
            return null;
        }
        userHwifc ntry = new userHwifc();
        ntry.name = a.substring(0, i).trim();
        a = rd.get(ln + 1).trim();
        if (!a.startsWith("link/ether")) {
            return null;
        }
        ntry.mac = a.substring(11, 28);
        a = ntry.name;
        i = a.indexOf("@");
        if (i >= 0) {
            a = a.substring(0, i);
        }
        ntry.name = a;
        return ntry;
    }

    /**
     * from own text
     *
     * @param s text
     * @return null on error, entry on success
     */
    public static userHwifc fromOwn(String s) {
        if (!s.startsWith("# ")) {
            return null;
        }
        if (!s.endsWith(" #")) {
            return null;
        }
        s = s.substring(2, s.length() - 2).trim();
        int i = s.indexOf(" ");
        if (i < 0) {
            return null;
        }
        userHwifc ntry = new userHwifc();
        ntry.name = s.substring(0, i).trim();
        ntry.mac = s.substring(i, s.length()).trim();
        return ntry;
    }

}
