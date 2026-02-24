package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.clnt.clntMtrack;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * mtracker configuration
 *
 * @author matecsaba
 */
public class cfgMtrack implements Comparable<cfgMtrack>, cfgGeneric {

    /**
     * create instance
     *
     * @param s name
     */
    public cfgMtrack(String s) {
        name = s;
        worker.name = s;
    }

    /**
     * name of tracker
     */
    public String name;

    /**
     * description
     */
    public String description;

    /**
     * worker
     */
    public final clntMtrack worker = new clntMtrack();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("mtracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("mtracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "vrf", null),
        new userFilter("mtracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "source", null),
        new userFilter("mtracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "group", null),
        new userFilter("mtracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null),
        new userFilter("mtracker .*", cmds.tabulator + "interval 30000", null),
        new userFilter("mtracker .*", cmds.tabulator + "timeout 10", null),
        new userFilter("mtracker .*", cmds.tabulator + "packet 10", null),
        new userFilter("mtracker .*", cmds.tabulator + "port " + clntMtrack.defPort, null),
        new userFilter("mtracker .*", cmds.tabulator + "tos 0", null),
        new userFilter("mtracker .*", cmds.tabulator + "ttl 255", null),
        new userFilter("mtracker .*", cmds.tabulator + "flow 0", null)
    };

    public int compareTo(cfgMtrack o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this mtracker");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "group", "specify group address");
        l.add(null, false, 2, new int[]{2, -1}, "<addr>", "address of group");
        l.add(null, false, 1, new int[]{2}, "target", "specify target address");
        l.add(null, false, 2, new int[]{2, -1}, "<addr>", "address of target");
        l.add(null, false, 1, new int[]{2}, "vrf", "specify vrf of test");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "vrf to use");
        l.add(null, false, 1, new int[]{2}, "source", "specify source of test");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "interface to use");
        l.add(null, false, 1, new int[]{2}, "interval", "specify time between runs");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "timeout", "timeout of peer");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "tos", "specify tos");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "ttl", "specify ttl");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "flow", "specify flow");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "packet", "inter packet gap");
        l.add(null, false, 2, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 1, new int[]{2}, "port", "specify port");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{-1}, "logging", "log events");
        l.add(null, false, 1, new int[]{-1}, "stop", "stop working");
        l.add(null, false, 1, new int[]{-1}, "start", "start working");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("mtracker " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !worker.logging, cmds.tabulator, "logging", "");
        cmds.cfgLine(l, worker.cfgGrp == null, cmds.tabulator, "group", "" + worker.cfgGrp);
        for (int i = 0; i < worker.cfgTrg.size(); i++) {
            l.add(cmds.tabulator + "target " + worker.cfgTrg.get(i));
        }
        if (worker.vrf != null) {
            l.add(cmds.tabulator + "vrf " + worker.vrf.name);
        } else {
            l.add(cmds.tabulator + "no vrf");
        }
        if (worker.srcIfc != null) {
            l.add(cmds.tabulator + "source " + worker.srcIfc.name);
        } else {
            l.add(cmds.tabulator + "no source");
        }
        if (worker.cfgGrp != null) {
            l.add(cmds.tabulator + "interval " + worker.interval);
            l.add(cmds.tabulator + "timeout " + worker.timeout);
        }
        l.add(cmds.tabulator + "port " + worker.port);
        l.add(cmds.tabulator + "tos " + worker.typOsrv);
        l.add(cmds.tabulator + "ttl " + worker.tim2liv);
        l.add(cmds.tabulator + "flow " + worker.flwLab);
        l.add(cmds.tabulator + "packet " + worker.packTim);
        if (worker.getWorking()) {
            l.add(cmds.tabulator + "start");
        } else {
            l.add(cmds.tabulator + "stop");
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    private boolean doConfStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return false;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgMtrack v = cfgAll.mtrackFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return true;
            }
            name = a;
            return false;
        }
        if (a.equals("logging")) {
            worker.logging = true;
            return false;
        }
        if (a.equals("group")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return false;
            }
            worker.cfgGrp = adr;
            return false;
        }
        if (a.equals("target")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return false;
            }
            worker.cfgTrg.add(adr);
            return false;
        }
        if (a.equals("vrf")) {
            worker.vrf = cfgAll.vrfFind(cmd.word(), false);
            if (worker.vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            return false;
        }
        if (a.equals("source")) {
            worker.srcIfc = cfgAll.ifcFind(cmd.word(), 0);
            if (worker.srcIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            return false;
        }
        if (a.equals("interval")) {
            worker.interval = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("timeout")) {
            worker.timeout = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("tos")) {
            worker.typOsrv = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("ttl")) {
            worker.tim2liv = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("flow")) {
            worker.flwLab = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("packet")) {
            worker.packTim = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("port")) {
            worker.port = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("stop")) {
            worker.stopNow();
            return true;
        }
        if (a.equals("start")) {
            worker.startNow();
            return true;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return false;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = null;
            return false;
        }
        if (a.equals("logging")) {
            worker.logging = false;
            return false;
        }
        if (a.equals("vrf")) {
            worker.vrf = null;
            return false;
        }
        if (a.equals("group")) {
            worker.cfgGrp = null;
            return false;
        }
        if (a.equals("target")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return false;
            }
            worker.cfgTrg.del(adr);
            return false;
        }
        if (a.equals("source")) {
            worker.srcIfc = null;
            return false;
        }
        if (a.equals("port")) {
            worker.port = clntMtrack.defPort;
            return false;
        }
        if (a.equals("stop")) {
            worker.startNow();
            return true;
        }
        if (a.equals("start")) {
            worker.stopNow();
            return true;
        }
        cmd.badCmd();
        return false;
    }

    public void doCfgStr(cmds cmd) {
        boolean old = worker.getWorking();
        if (old) {
            worker.stopNow();
        }
        if (doConfStr(cmd)) {
            return;
        }
        if (old) {
            worker.startNow();
        }
    }

    public String getPrompt() {
        return "mtrck";
    }

}
