package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * one key based menu configuration
 *
 * @author matecsaba
 */
public class cfgMenuK implements Comparable<cfgMenuK>, cfgGeneric {

    /**
     * defaults text
     */
    public final static userFilter[] defaultL = {
        new userFilter("menu key .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("menu key .*", cmds.tabulator + cmds.negated + cmds.tabulator + "ignore-case", null)
    };

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
    public tabGen<cfgMenuKentry> letter = new tabGen<cfgMenuKentry>();

    /**
     * create new instance
     *
     * @param s name of menu
     */
    public cfgMenuK(String s) {
        name = s;
    }

    public int compareTo(cfgMenuK o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this menu");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{-1}, "ignore-case", "ignore case on matching");
        l.add(null, false, 1, new int[]{2}, "letter", "set letter to configure");
        l.add(null, false, 2, new int[]{3, -1}, "<str>", "menu item");
        l.add(null, false, 3, new int[]{4}, "command", "command to do");
        l.add(null, false, 4, new int[]{4, -1}, "<str>", "command");
        l.add(null, false, 3, new int[]{4}, "text", "text to display");
        l.add(null, false, 4, new int[]{4, -1}, "<str>", "text");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("menu key " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !ignoreCase, cmds.tabulator, "ignore-case", "");
        for (int i = 0; i < letter.size(); i++) {
            cfgMenuKentry ntry = letter.get(i);
            l.add(cmds.tabulator + "letter " + ntry.name + " command " + ntry.command);
            l.add(cmds.tabulator + "letter " + ntry.name + " text " + ntry.text);
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultL);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean negated = a.equals(cmds.negated);
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
            cfgMenuK v = cfgAll.menuKfind(a, false);
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
        cfgMenuKentry ntry = letter.find(new cfgMenuKentry(a));
        if (ntry == null) {
            ntry = new cfgMenuKentry(a);
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
        return "menuk";
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
     * do menu
     *
     * @param pipe pipe to use
     * @return key pressed, null if nothing
     */
    public String doMenu(pipeSide pipe) {
        putMenu(pipe);
        String k = getKeys();
        String a = pipe.strChr("choose:", k);
        String s = findKey(a);
        if (s.length() < 1) {
            return null;
        }
        return s;
    }

    /**
     * get keys
     *
     * @return keys
     */
    public String getKeys() {
        String s = "";
        for (int i = 0; i < letter.size(); i++) {
            cfgMenuKentry ntry = letter.get(i);
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
            cfgMenuKentry ntry = letter.get(i);
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

}

class cfgMenuKentry implements Comparable<cfgMenuKentry> {

    public String name;

    public String command;

    public String text;

    public cfgMenuKentry(String s) {
        name = s;
        command = "";
        text = "";
    }

    public String toString() {
        return name + " - " + text;
    }

    public int compareTo(cfgMenuKentry o) {
        return name.compareTo(o.name);
    }

}
