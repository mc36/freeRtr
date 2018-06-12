package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import line.lineScript;
import user.userHelping;
import util.cmds;

/**
 * chat script configuration
 *
 * @author matecsaba
 */
public class cfgChat implements Comparator<cfgChat>, cfgGeneric {

    /**
     * name of script
     */
    public String name;

    /**
     * the script
     */
    public lineScript script;

    public int compare(cfgChat o1, cfgChat o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "chat script " + name;
    }

    /**
     * create new profile
     *
     * @param nam name of interface
     */
    public cfgChat(String nam) {
        name = nam.trim();
        script = new lineScript();
        script.scrName = name;
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        script.getHelp(l);
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("chat-script " + name);
        l.addAll(script.getCfg(cmds.tabulator));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        return l;
    }

    public void doCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean neg = s.equals("no");
        if (!neg) {
            cmd = cmd.copyBytes(true);
        }
        if (script.doCfg(cmd, neg)) {
            cmd.badCmd();
        }
    }

    public String getPrompt() {
        return "chat";
    }

}
