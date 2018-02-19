package cfg;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pipe.pipeLine;
import pipe.pipeSide;
import tab.tabEvntmgrN;
import tab.tabGen;
import tab.tabListing;
import user.userFilter;
import user.userHelping;
import user.userScript;
import util.bits;
import util.cmds;

/**
 * event manager configuration
 *
 * @author matecsaba
 */
public class cfgEvntmgr implements Comparator<cfgEvntmgr>, cfgGeneric {

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
    public final static String defaultL[] = {
        "event-manager .*! no description",
        "event-manager .*! no suppress",
        "event-manager .*! no event"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * create new access list
     */
    public cfgEvntmgr() {
        script = new tabListing<tabEvntmgrN, addrIP>();
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

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("event-manager " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, event == null, cmds.tabulator, "event", event);
        cmds.cfgLine(l, !suppress, cmds.tabulator, "suppress", "");
        l.addAll(script.dump(cmds.tabulator));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2   sequence              sequence number of an entry");
        l.add("2  1     <num>               sequence number");
        l.add("1  3,. description           specify description");
        l.add("3  3,.   <str>               text");
        l.add("1  3,. event                 specify log event to match");
        l.add("3  3,.   <str>               text");
        l.add("1  3,. tcl                   specify tcl command to execute");
        l.add("3  3,.   <str>               text");
        l.add("1  .   suppress              suppress the log");
        l.add("1  2,. reindex               reindex access list");
        l.add("2  3,.   [num]               initial number to start with");
        l.add("3  4,.     [num]             increment number");
        return l;
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("no")) {
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

    public int compare(cfgEvntmgr o1, cfgEvntmgr o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
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
            pip.timeout = 10000;
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
