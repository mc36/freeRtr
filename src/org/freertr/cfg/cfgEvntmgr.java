package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabEvntmgrN;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.user.userScript;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * event manager configuration
 *
 * @author matecsaba
 */
public class cfgEvntmgr implements Comparable<cfgEvntmgr>, cfgGeneric {

    /**
     * name of event manager
     */
    public String name;

    /**
     * description
     */
    public String description;

    /**
     * log to match
     */
    public String event;

    /**
     * suppress log
     */
    public boolean suppress;

    /**
     * list of statements
     */
    public tabListing<tabEvntmgrN, addrIP> script;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("event-manager .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("event-manager .*", cmds.tabulator + cmds.negated + cmds.tabulator + "suppress", null),
        new userFilter("event-manager .*", cmds.tabulator + cmds.negated + cmds.tabulator + "event", null)
    };

    /**
     * create new event manager
     *
     * @param s name
     */
    public cfgEvntmgr(String s) {
        script = new tabListing<tabEvntmgrN, addrIP>();
        name = s;
        script.listName = s;
    }

    /**
     * add one table entry with preset values
     *
     * @param seq sequence number
     * @param s string represeting proto src/mask/port trg/mask/port
     * @return true if error happened, false if not
     */
    public synchronized boolean add(int seq, String s) {
        tabEvntmgrN ntry = new tabEvntmgrN();
        if (ntry.fromString(s)) {
            return true;
        }
        ntry.sequence = seq;
        script.add(ntry);
        return false;
    }

    /**
     * delete one entry
     *
     * @param seq sequence number
     * @return true if deleted, false if not
     */
    public boolean del(int seq) {
        tabEvntmgrN ntry = new tabEvntmgrN();
        ntry.sequence = seq;
        return script.del(ntry);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("event-manager " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, event == null, cmds.tabulator, "event", event);
        cmds.cfgLine(l, !suppress, cmds.tabulator, "suppress", "");
        l.addAll(script.dump(cmds.tabulator, filter));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "sequence number of an entry");
        l.add(null, false, 2, new int[]{1}, "<num>", "sequence number");
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{3, -1}, "event", "specify log event to match");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{3, -1}, "tcl", "specify tcl command to execute");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{-1}, "suppress", "suppress the log");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex event manager");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals(cmds.negated)) {
            a = cmd.word();
            if (a.equals("sequence")) {
                if (del(bits.str2num(cmd.word()))) {
                    cmd.error("invalid sequence");
                    return;
                }
                return;
            }
            if (a.equals("description")) {
                description = null;
                return;
            }
            if (a.equals("event")) {
                event = null;
                return;
            }
            if (a.equals("suppress")) {
                suppress = false;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            script.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("event")) {
            event = cmd.getRemaining();
            return;
        }
        if (a.equals("suppress")) {
            suppress = true;
            return;
        }
        int seq = script.nextseq();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
        }
        if (!a.equals("tcl")) {
            cmd.badCmd();
            return;
        }
        if (add(seq, cmd.getRemaining())) {
            cmd.error("invalid network");
            return;
        }
    }

    public int compareTo(cfgEvntmgr o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "eem";
    }

    /**
     * process log event
     *
     * @param s string to process
     * @return true if suppression needed
     */
    public static boolean processEvent(String s) {
        boolean sup = false;
        for (int o = 0; o < cfgAll.eventmgrs.size(); o++) {
            cfgEvntmgr ntry = cfgAll.eventmgrs.get(o);
            if (ntry == null) {
                continue;
            }
            if (ntry.event == null) {
                continue;
            }
            if (!s.matches(ntry.event)) {
                continue;
            }
            sup |= ntry.suppress;
            if (ntry.script.size() < 1) {
                continue;
            }
            pipeLine pl = new pipeLine(32768, false);
            pipeSide pip = pl.getSide();
            pip.setTime(10000);
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            userScript t = new userScript(pip, "");
            t.allowExec = true;
            t.allowConfig = true;
            t.addLine("set msg \"" + s + "\"");
            for (int i = 0; i < ntry.script.size(); i++) {
                t.addLine(ntry.script.get(i).tcl);
            }
            pip = pl.getSide();
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            pip.lineTx = pipeSide.modTyp.modeCR;
            t.cmdAll();
            pl.setClose();
        }
        return sup;
    }

}
