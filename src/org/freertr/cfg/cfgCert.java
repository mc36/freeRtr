package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authLocal;
import org.freertr.cry.cryCertificate;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * cryptographic certificate configuration
 *
 * @author matecsaba
 */
public class cfgCert implements Comparable<cfgCert>, cfgGeneric {

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
    public final static userFilter[] defaultF = {};

    public int compareTo(cfgCert o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
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

    public void getHelp(userHelp l) {
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
