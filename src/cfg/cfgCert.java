package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import user.userHelping;
import util.cmds;
import cry.cryCertificate;
import cry.cryKeyGeneric;

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

    public userHelping getHelp() {
        return null;
    }

    public String getPrompt() {
        return "cert";
    }

    public void doCfgStr(cmds cmd) {
    }

    public List<String> getShRun(boolean filter) {
        List<String> lst = new ArrayList<String>();
        lst.add("crypto certificate " + name + " import " + key.algName() + " " + key.keyName + " " + cert.pemWriteStr());
        lst.add(cmds.comment);
        return lst;
    }

}
