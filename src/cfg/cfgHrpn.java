package cfg;

import ifc.ifcHairpin;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * one hairpin configuration
 *
 * @author matecsaba
 */
public class cfgHrpn implements Comparator<cfgHrpn>, cfgGeneric {

    /**
     * name of this hairpin
     */
    public final String name;

    /**
     * hairpin handler
     */
    public ifcHairpin hairpinHed;

    public int compare(cfgHrpn o1, cfgHrpn o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "hrpn " + name;
    }

    /**
     * create new hairpin instance
     *
     * @param nam name of bridge
     */
    public cfgHrpn(String nam) {
        name = "" + bits.str2num(nam);
    }

    /**
     * close this hairpin
     */
    public void closeUp() {
    }

    /**
     * get name of interface represents this hairpin
     *
     * @param side side to initialize
     * @return interface name
     */
    public String getIntName(boolean side) {
        return "hairpin" + name + (side ? "1" : "2");
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("hairpin " + name);
        hairpinHed.getConfig(l, cmds.tabulator);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        return l;
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        ifcHairpin.getHelp(l);
        return l;
    }

    public void doCfgStr(cmds cmd) {
        hairpinHed.doConfig(cmd);
    }

    public String getPrompt() {
        return "hrpn";
    }

}
