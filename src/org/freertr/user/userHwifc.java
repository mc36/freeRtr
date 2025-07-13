package org.freertr.user;

import java.util.List;
import org.freertr.addr.addrMac;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
        String a = rd.get(ln).replaceAll("\t", " ");
        if (a.length() < 1) {
            return null;
        }
        if (a.startsWith(" ")) {
            return null;
        }
        userHwifc ntry = new userHwifc();
        addrMac adr = new addrMac();
        cmds cmd = new cmds("ifc", a.trim());
        a = cmd.word();
        if (a.endsWith(":")) {
            a = a.substring(0, a.length() - 1);
        }
        if (a.equals("" + bits.str2num(a))) {
            a = cmd.word();
        }
        if (a.endsWith(":")) {
            a = a.substring(0, a.length() - 1);
        }
        int i = a.indexOf("@");
        if (i >= 0) {
            a = a.substring(0, i).trim();
        }
        ntry.name = a;
        for (;;) {
            a = cmd.word();
            if (a.length() == 17) {
                if (adr.fromString(a)) {
                    continue;
                }
                if (adr.isFilled(0)) {
                    continue;
                }
                if (adr.isBroadcast()) {
                    continue;
                }
                ntry.mac = a.toLowerCase();
                return ntry;
            }
            if (a.length() > 0) {
                continue;
            }
            ln++;
            if (ln >= rd.size()) {
                return null;
            }
            a = rd.get(ln).replaceAll("\t", " ");
            if (!a.startsWith(" ")) {
                return null;
            }
            cmd = new cmds("ifc", a.trim());
        }
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
