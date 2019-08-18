package cfg;

import ifc.ifcBundle;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * one bundle configuration
 *
 * @author matecsaba
 */
public class cfgBndl implements Comparator<cfgBndl>, cfgGeneric {

    /**
     * name of this bundle
     */
    public final String name;

    /**
     * bundle handler
     */
    public ifcBundle bundleHed;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "bundle .*! ethernet",
        "bundle .*! no backup",
        "bundle .*! no logging",
        "bundle .*! no loadbalance",
        "bundle .*! no replicate",
        "bundle .*! no reporter",
        "bundle .*! no dynamic",
        "bundle .*! no sequence",
        "bundle .*! no dejitter",
        "bundle .*! no peering"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgBndl o1, cfgBndl o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "bndl " + name;
    }

    /**
     * create new bundle instance
     *
     * @param nam name of bridge
     */
    public cfgBndl(String nam) {
        name = "" + bits.str2num(nam);
    }

    /**
     * close this bundle
     */
    public void closeUp() {
    }

    /**
     * get name of interface represents this bundle
     *
     * @return interface name
     */
    public String getIntName() {
        return "bundle" + name;
    }

    /**
     * get config
     *
     * @param filter filter
     * @return config
     */
    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("bundle " + name);
        bundleHed.getConfig(l, cmds.tabulator);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    /**
     * get help text
     *
     * @return help
     */
    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        ifcBundle.getHelp(l);
        return l;
    }

    /**
     * do config string
     *
     * @param cmd config
     */
    public void doCfgStr(cmds cmd) {
        bundleHed.doConfig(cmd);
    }

    /**
     * get prompt
     *
     * @return prompt
     */
    public String getPrompt() {
        return "bndl";
    }

}
