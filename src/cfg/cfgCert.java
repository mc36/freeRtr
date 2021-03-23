package cfg;

import auth.authLocal;
import cry.cryCertificate;
import cry.cryKeyGeneric;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import user.userHelping;
import util.cmds;

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
     * certificate
     */
    public cryCertificate cert;

    /**
     * key
     */
    public cryKeyGeneric key;

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
        List<String> lst = new ArrayList<String>();
        lst.add("crypto certificate " + name + " import " + key.algName() + " " + key.keyName + " " + authLocal.passwdEncode(cert.pemWriteStr(), (filter & 2) != 0));
        lst.add(cmds.comment);
        return lst;
    }

}
