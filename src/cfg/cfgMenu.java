package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import user.userFilter;
import user.userHelping;
import tab.tabGen;
import util.cmds;

/**
 * one menu configuration
 *
 * @author matecsaba
 */
public class cfgMenu implements Comparator<cfgMenu>, cfgGeneric {

    /**
     * defaults text
     */
    public final static String defaultL[] = {};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * name of menu
     */
    public String name;

    /**
     * letters of menu
     */
    public tabGen<cfgMenuEntry> letter = new tabGen<cfgMenuEntry>();

    /**
     * create new instance
     *
     * @param s name of menu
     */
    public cfgMenu(String s) {
        name = s;
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2  letter                         set letter to configure");
        l.add("2 3,.  <name>                       menu item");
        l.add("3 4      command                    command to do");
        l.add("4 4,.      <name>                   command");
        l.add("3 4      text                       text to display");
        l.add("4 4,.      <name>                   text");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("menu " + name);
        for (int i = 0; i < letter.size(); i++) {
            cfgMenuEntry ntry = letter.get(i);
            l.add(cmds.tabulator + "letter " + ntry.name + " command "
                    + ntry.command);
            l.add(cmds.tabulator + "letter " + ntry.name + " text " + ntry.text);
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);

    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean negated = a.equals("no");
        if (negated) {
            a = cmd.word();
        }
        if (!a.equals("letter")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        cfgMenuEntry ntry = letter.find(new cfgMenuEntry(a));
        if (ntry == null) {
            ntry = new cfgMenuEntry(a);
            letter.put(ntry);
        }
        a = cmd.word();
        if (a.equals("command")) {
            ntry.command = cmd.getRemaining();
            if (negated) {
                letter.del(ntry);
            }
            return;
        }
        if (a.equals("text")) {
            ntry.text = cmd.getRemaining();
            if (negated) {
                ntry.text = "";
            }
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "menu";
    }

    /**
     * find one key
     *
     * @param s key to find
     * @return menu entry, null if not found
     */
    public String findKey(String s) {
        for (int i = 0; i < letter.size(); i++) {
            cfgMenuEntry ntry = letter.get(i);
            if (ntry.name.compareTo(s) == 0) {
                return ntry.command;
            }
        }
        return null;
    }

    public int compare(cfgMenu o1, cfgMenu o2) {
        return o1.name.compareTo(o2.name);
    }

}

class cfgMenuEntry implements Comparator<cfgMenuEntry> {

    public String name;

    public String command;

    public String text;

    public cfgMenuEntry(String s) {
        name = s;
        command = "";
        text = "";
    }

    public String toString() {
        return name + " - " + text;
    }

    public int compare(cfgMenuEntry o1, cfgMenuEntry o2) {
        return o1.name.compareTo(o2.name);
    }

}
