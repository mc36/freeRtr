package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.auth.authLocal;
import net.freertr.cry.cryCertificate;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * cryptographic certificate configuration
 *
 * @author matecsaba
 */
public class cfgCert implements Comparator<cfgCert>, cfgGeneric {

    /**
     * name of key
     */
    public String name;

    /**
     * name of file
     */
    public String filNam;

    /**
     * certificate
     */
    public cryCertificate cert;

    /**
     * key
     */
    public cryKeyGeneric key;

    /**
     * defaults text
     */
    public final static String[] defaultL = {};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgCert o1, cfgCert o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    /**
     * create new certificate
     *
     * @param nam name of interface
     */
    public cfgCert(String nam) {
        name = nam.trim();
    }

    public String toString() {
        return name;
    }

    public void getHelp(userHelping l) {
    }

    public String getPrompt() {
        return "cert";
    }

    public void doCfgStr(cmds cmd) {
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (filNam == null) {
            l.add("crypto certificate " + name + " import " + key.algName() + " " + key.keyName + " " + authLocal.passwdEncode(cert.pemWriteStr(), (filter & 2) != 0));
        } else {
            l.add("crypto certificate " + name + " external " + key.algName() + " " + key.keyName + " " + filNam);
        }
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

}
