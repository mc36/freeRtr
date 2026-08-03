package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.ifc.ifcHairpin;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * one hairpin configuration
 *
 * @author matecsaba
 */
public class cfgHrpn implements Comparable<cfgHrpn>, cfgGeneric {

    /**
     * name of this hairpin
     */
    public final int number;

    /**
     * hairpin handler
     */
    public ifcHairpin hairpinHed;

    public int compareTo(cfgHrpn o) {
        if (number < o.number) {
            return -1;
        }
        if (number > o.number) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "hrpn " + number;
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("hairpin .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("hairpin .*", cmds.tabulator + "ethernet", null),
        new userFilter("hairpin .*", cmds.tabulator + "random12drop 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random12burst 0 0 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random12duplicate 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random12reorder 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random12delay 0 0 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random12corrupt 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random21drop 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random21burst 0 0 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random21duplicate 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random21reorder 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random21delay 0 0 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "random21corrupt 0", null),
        new userFilter("hairpin .*", cmds.tabulator + "buffer 65536", null)
    };

    /**
     * create new hairpin instance
     *
     * @param nam name of bridge
     */
    public cfgHrpn(String nam) {
        number = bits.str2num(nam);
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
        return "hairpin" + number + (side ? "1" : "2");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("hairpin " + number);
        hairpinHed.getConfig(l, cmds.tabulator);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
        ifcHairpin.getHelp(l);
    }

    public void doCfgStr(cmds cmd) {
        hairpinHed.doConfig(cmd);
    }

    public String getPrompt() {
        return "hrpn";
    }

}
