package net.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userLine;
import net.freertr.util.cmds;

/**
 * default console parameters
 *
 * @author matecsaba
 */
public class cfgCons implements cfgGeneric {

    /**
     * line handler
     */
    public userLine line = new userLine();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "console0! exec timeout 0"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public String toString() {
        return "con0";
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("console0");
        line.getShRun(cmds.tabulator, l);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelping l) {
        line.getHelp(l);
    }

    public void doCfgStr(cmds cmd) {
        if (line.doCfgStr(cmd.copyBytes(true))) {
            cmd.badCmd();
            return;
        }
    }

    public String getPrompt() {
        return "con0";
    }

}
