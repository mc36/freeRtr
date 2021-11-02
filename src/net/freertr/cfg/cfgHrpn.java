package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.ifc.ifcHairpin;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

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
     * stop this hairpin
     */
    public void stopWork() {
        hairpinHed.stopWork();
    }

    /**
     * start this hairpin
     */
    public void startWork() {
        hairpinHed.startWork();
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

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("hairpin " + name);
        hairpinHed.getConfig(l, cmds.tabulator);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        return l;
    }

    public void getHelp(userHelping l) {
        ifcHairpin.getHelp(l);
    }

    public void doCfgStr(cmds cmd) {
        hairpinHed.doConfig(cmd);
    }

    public String getPrompt() {
        return "hrpn";
    }

}
