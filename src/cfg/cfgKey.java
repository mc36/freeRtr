package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import user.userHelping;
import util.cmds;
import cry.cryKeyGeneric;

/**
 * cryptographic key configuration
 *
 * @param <T> type of key
 * @author matecsaba
 */
public class cfgKey<T extends cryKeyGeneric> implements Comparator<cfgKey<T>>, cfgGeneric {

    /**
     * name of key
     */
    public String name;

    /**
     * the key
     */
    public T key;

    public String toString() {
        return name;
    }

    public int compare(cfgKey<T> o1, cfgKey<T> o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public userHelping getHelp() {
        return null;
    }

    public String getPrompt() {
        return "key";
    }

    public void doCfgStr(cmds cmd) {
    }

    public List<String> getShRun(boolean filter) {
        List<String> lst = new ArrayList<String>();
        lst.add("crypto " + key.algName() + "key " + name + " import " + key.pemWriteStr(false));
        lst.add(cmds.comment);
        return lst;
    }

}
