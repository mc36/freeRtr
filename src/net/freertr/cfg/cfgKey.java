package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.auth.authLocal;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * cryptographic key configuration
 *
 * @param <T> type of key
 * @author matecsaba
 */
public class cfgKey<T extends cryKeyGeneric> implements Comparator<cfgKey<T>>, cfgGeneric {

    /**
     * create instance
     */
    public cfgKey() {
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
    public final static String[] defaultL = {};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgKey<T> o1, cfgKey<T> o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public void getHelp(userHelping l) {
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
