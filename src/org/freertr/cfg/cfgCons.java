package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.user.userLine;
import org.freertr.user.userRead;
import org.freertr.util.cmds;

/**
 * default console parameters
 *
 * @author matecsaba
 */
public class cfgCons implements cfgGeneric {

    /**
     * create instance
     */
    public cfgCons() {
    }

    /**
     * line handler
     */
    public userLine line = new userLine();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("console0", " exec timeout 0", null)
    };

    public String toString() {
        return "con0";
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("console0");
        line.getShRun(cmds.tabulator, l, filter);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
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
