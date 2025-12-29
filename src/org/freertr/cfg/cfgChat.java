package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.pipe.pipeChat;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * chat script configuration
 *
 * @author matecsaba
 */
public class cfgChat implements Comparable<cfgChat>, cfgGeneric {

    /**
     * name of script
     */
    public String name;

    /**
     * the script
     */
    public pipeChat script;

    public int compareTo(cfgChat o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String toString() {
        return "chat script " + name;
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {};

    /**
     * create new profile
     *
     * @param nam name of interface
     */
    public cfgChat(String nam) {
        name = nam.trim();
        script = new pipeChat();
        script.scrName = name;
    }

    public void getHelp(userHelp l) {
        script.getHelp(l);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("chat-script " + name);
        l.addAll(script.getCfg(cmds.tabulator, filter));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean neg = s.equals(cmds.negated);
        if (!neg) {
            cmd = cmd.copyBytes(true);
        }
        if (s.equals("rename")) {
            s = cmd.word();
            cfgChat v = cfgAll.chatFind(s, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            script.scrName = s;
            name = s;
            return;
        }
        if (s.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            script.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        if (script.doCfg(cmd, neg)) {
            cmd.badCmd();
        }
    }

    public String getPrompt() {
        return "chat";
    }

}
