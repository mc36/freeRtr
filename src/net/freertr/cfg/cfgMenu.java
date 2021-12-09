package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * one menu configuration
 *
 * @author matecsaba
 */
public class cfgMenu implements Comparator<cfgMenu>, cfgGeneric {

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "menu .*! no description",
        "menu .*! no ignore-case"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * name of menu
     */
    public String name;

    /**
     * description
     */
    public String description;

    /**
     * ignore case
     */
    public boolean ignoreCase;

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

    public void getHelp(userHelping l) {
        l.add(null, "1 3,. description                   specify description");
        l.add(null, "3 3,.   <str>                       text");
        l.add(null, "1 2   rename                        rename this menu");
        l.add(null, "2 .     <name>                      set new name");
        l.add(null, "1 .  ignore-case                    ignore case on matching");
        l.add(null, "1 2  letter                         set letter to configure");
        l.add(null, "2 3,.  <name>                       menu item");
        l.add(null, "3 4      command                    command to do");
        l.add(null, "4 4,.      <name>                   command");
        l.add(null, "3 4      text                       text to display");
        l.add(null, "4 4,.      <name>                   text");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("menu " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !ignoreCase, cmds.tabulator, "ignore-case", "");
        for (int i = 0; i < letter.size(); i++) {
            cfgMenuEntry ntry = letter.get(i);
            l.add(cmds.tabulator + "letter " + ntry.name + " command " + ntry.command);
            l.add(cmds.tabulator + "letter " + ntry.name + " text " + ntry.text);
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
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
        if (a.equals("ignore-case")) {
            ignoreCase = !negated;
            return;
        }
        if (a.equals("description")) {
            if (negated) {
                description = null;
            } else {
                description = cmd.getRemaining();
            }
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgMenu v = cfgAll.menuFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            return;
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
     * put menu
     *
     * @param p pipe to use
     */
    public void putMenu(pipeSide p) {
        p.linePut("menu " + name + ":");
        for (int i = 0; i < letter.size(); i++) {
            p.linePut(letter.get(i) + "");
        }
    }

    /**
     * get keys
     *
     * @return keys
     */
    public String getKeys() {
        String s = "";
        for (int i = 0; i < letter.size(); i++) {
            cfgMenuEntry ntry = letter.get(i);
            String a = ntry.name;
            if (ignoreCase) {
                s += a.toLowerCase() + a.toUpperCase();
            } else {
                s += a;
            }
        }
        return s;
    }

    /**
     * find one key
     *
     * @param s key to find
     * @return menu entry, null if not found
     */
    public String findKey(String s) {
        if (ignoreCase) {
            s = s.toLowerCase();
        }
        for (int i = 0; i < letter.size(); i++) {
            cfgMenuEntry ntry = letter.get(i);
            String a = ntry.name;
            if (ignoreCase) {
                a = a.toLowerCase();
            }
            if (a.compareTo(s) == 0) {
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
