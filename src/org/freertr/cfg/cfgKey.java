package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authLocal;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * cryptographic key configuration
 *
 * @param <T> type of key
 * @author matecsaba
 */
public class cfgKey<T extends cryKeyGeneric> implements Comparable<cfgKey<T>>, cfgGeneric {

    /**
     * create instance
     *
     * @param s name
     */
    public cfgKey(String s) {
        name = s;
    }

    /**
     * name of key
     */
    public String name;

    /**
     * name of file
     */
    public String filNam;

    /**
     * the key
     */
    public T key;

    public String toString() {
        return name;
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {};

    public int compareTo(cfgKey<T> o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public void getHelp(userHelp l) {
    }

    public String getPrompt() {
        return "key";
    }

    public void doCfgStr(cmds cmd) {
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (filNam == null) {
            l.add("crypto " + key.algName() + "key " + name + " import " + authLocal.passwdEncode(key.pemWriteStr(false), (filter & 2) != 0));
        } else {
            l.add("crypto " + key.algName() + "key " + name + " external " + filNam);
        }
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

}
