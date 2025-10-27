package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authLocal;
import org.freertr.clnt.clntTrack;
import org.freertr.enc.encBase64;
import org.freertr.serv.servGeneric;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * tracker configuration
 *
 * @author matecsaba
 */
public class cfgTrack implements Comparable<cfgTrack>, cfgGeneric {

    /**
     * create instance
     *
     * @param s name
     */
    public cfgTrack(String s) {
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
    public final clntTrack worker = new clntTrack();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("tracker .*", cmds.tabulator + "force normal", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "script", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "hidden", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "wake-vrf", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec-up", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec-down", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "chat-script", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "pubkey", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "vrf", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "source", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log", null),
        new userFilter("tracker .*", cmds.tabulator + "random-interval 0", null),
        new userFilter("tracker .*", cmds.tabulator + "random-initial 0", null),
        new userFilter("tracker .*", cmds.tabulator + "interval 0", null),
        new userFilter("tracker .*", cmds.tabulator + "timeout 0", null),
        new userFilter("tracker .*", cmds.tabulator + "sgt 0", null),
        new userFilter("tracker .*", cmds.tabulator + "tos 0", null),
        new userFilter("tracker .*", cmds.tabulator + "flow 0", null),
        new userFilter("tracker .*", cmds.tabulator + "ttl 255", null),
        new userFilter("tracker .*", cmds.tabulator + "size 80", null),
        new userFilter("tracker .*", cmds.tabulator + "delay-start 0", null),
        new userFilter("tracker .*", cmds.tabulator + "delay-up 0", null),
        new userFilter("tracker .*", cmds.tabulator + "delay-down 0", null),
        new userFilter("tracker .*", cmds.tabulator + cmds.negated + cmds.tabulator + "range", null)
    };

    public int compareTo(cfgTrack o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    /**
     * get prompt
     *
     * @return prompt
     */
    public String getPrompt() {
        return "trck";
    }

    public String toString() {
        return name;
    }

    /**
     * get help text
     *
     * @param l help text
     */
    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this tracker");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "mode", "specify mode of runs");
        l.add(null, false, 2, new int[]{-1}, "icmp", "icmp echo request");
        l.add(null, false, 2, new int[]{-1}, "mpls", "mpls echo request");
        l.add(null, false, 2, new int[]{-1}, "bier", "bier echo request");
        l.add(null, false, 2, new int[]{-1}, "bck-icmp", "backward addressed icmp echo request");
        l.add(null, false, 2, new int[]{-1}, "bck-mpls", "backward addressed mpls echo request");
        l.add(null, false, 2, new int[]{-1}, "bck-bier", "backward addressed bier echo request");
        l.add(null, false, 2, new int[]{-1}, "nrpe", "nrpe remote check");
        l.add(null, false, 2, new int[]{-1}, "other", "other tracker");
        l.add(null, false, 2, new int[]{-1}, "check", "local check");
        l.add(null, false, 2, new int[]{-1}, "tcp", "tcp connection");
        l.add(null, false, 2, new int[]{-1}, "udp", "udp connection");
        l.add(null, false, 2, new int[]{-1}, "twamp", "twamp connection");
        l.add(null, false, 2, new int[]{-1}, "bfd", "bidirectional forwarding detection");
        l.add(null, false, 2, new int[]{-1}, "interface", "interface state");
        l.add(null, false, 2, new int[]{-1}, "route", "any route table entry for address");
        l.add(null, false, 2, new int[]{-1}, "prefix", "exact route table entry for prefix");
        l.add(null, false, 2, new int[]{-1}, "script", "tcl script");
        l.add(null, false, 1, new int[]{2, -1}, "script", "modify result with script");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "script");
        l.add(null, false, 1, new int[]{2}, "force", "specify result of runs");
        l.add(null, false, 2, new int[]{-1}, "up", "always up");
        l.add(null, false, 2, new int[]{-1}, "down", "always down");
        l.add(null, false, 2, new int[]{-1}, "negate", "negate result");
        l.add(null, false, 2, new int[]{-1}, "normal", "leave result");
        l.add(null, false, 1, new int[]{2}, "target", "specify address of test");
        l.add(null, false, 2, new int[]{2, -1}, "<addr>", "address of target");
        l.add(null, false, 1, new int[]{2}, "vrf", "specify vrf of test");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "vrf to use");
        l.add(null, false, 1, new int[]{2}, "pubkey", "specify public key");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "public key");
        l.add(null, false, 1, new int[]{2}, "security", "select security protocol");
        servGeneric.getSecProts(l, 2, new int[]{-1});
        l.add(null, false, 1, new int[]{2}, "chat-script", "specify script to use");
        l.add(null, false, 2, new int[]{-1}, "<name:cht>", "chatter to use");
        l.add(null, false, 1, new int[]{2}, "source", "specify source of test");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "interface to use");
        l.add(null, false, 1, new int[]{2}, "random-interval", "specify random time between runs");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "random-initial", "specify random time before run");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "range", "specify time range");
        l.add(null, false, 2, new int[]{-1}, "<name:tm>", "name of time map");
        l.add(null, false, 1, new int[]{2}, "interval", "specify time between runs");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "timeout", "specify timeout value");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in milliseconds");
        l.add(null, false, 1, new int[]{2}, "sgt", "specify sgt");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "tos", "specify tos");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "ttl", "specify ttl");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "size", "size of payload");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "delay-start", "time before start");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 1, new int[]{2}, "delay-up", "number of successes before up");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "delay-down", "number of failures before down");
        l.add(null, false, 2, new int[]{-1}, "<num>", "value");
        l.add(null, false, 1, new int[]{2}, "wake-vrf", "wake vrf on state change");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 1, new int[]{2}, "exec-up", "exec command to execute on up");
        l.add(null, false, 2, new int[]{2, -1}, "<cmd>", "value");
        l.add(null, false, 1, new int[]{2}, "exec-down", "exec command to execute on down");
        l.add(null, false, 2, new int[]{2, -1}, "<cmd>", "value");
        l.add(null, false, 1, new int[]{-1}, "stop", "stop working");
        l.add(null, false, 1, new int[]{-1}, "start", "start working");
        l.add(null, false, 1, new int[]{-1}, "runnow", "run one round now");
        l.add(null, false, 1, new int[]{-1}, "hidden", "hide exec commands");
        l.add(null, false, 1, new int[]{-1}, "log", "log actions");
    }

    /**
     * get config
     *
     * @param filter filter
     * @return config
     */
    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("tracker " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        worker.getConfig(l, filter);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    /**
     * do config string
     *
     * @param cmd config
     */
    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgTrack v = cfgAll.trackFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            worker.name = a;
            return;
        }
        if (a.equals("script")) {
            worker.script = cmd.getRemaining();
            return;
        }
        if (a.equals("hidden")) {
            worker.hidden = true;
            return;
        }
        if (a.equals("log")) {
            worker.logging = true;
            return;
        }
        if (a.equals("mode")) {
            worker.mode = clntTrack.string2mode(cmd.word());
            return;
        }
        if (a.equals("force")) {
            worker.force = clntTrack.string2force(cmd.word());
            return;
        }
        if (a.equals("target")) {
            worker.target = cmd.getRemaining();
            return;
        }
        if (a.equals("exec-up")) {
            worker.execUp = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (a.equals("exec-down")) {
            worker.execDn = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (a.equals("pubkey")) {
            worker.pubkey = encBase64.decodeBytes(cmd.getRemaining());
            return;
        }
        if (a.equals("security")) {
            worker.secProto = servGeneric.string2proto(cmd.word());
            return;
        }
        if (a.equals("chat-script")) {
            cfgChat cht = cfgAll.chatFind(cmd.word(), false);
            if (cht == null) {
                cmd.error("no such script");
                return;
            }
            worker.chats = cht.script;
            return;
        }
        if (a.equals("vrf")) {
            worker.vrf = cfgAll.vrfFind(cmd.word(), false);
            if (worker.vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            return;
        }
        if (a.equals("wake-vrf")) {
            worker.wakeVrf = cfgAll.vrfFind(cmd.word(), false);
            if (worker.wakeVrf == null) {
                cmd.error("no such vrf");
                return;
            }
            return;
        }
        if (a.equals("source")) {
            worker.srcIfc = cfgAll.ifcFind(cmd.word(), 0);
            if (worker.srcIfc == null) {
                cmd.error("no such interface");
                return;
            }
            return;
        }
        if (a.equals("random-interval")) {
            worker.randInt = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("random-initial")) {
            worker.randIni = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("interval")) {
            worker.interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("timeout")) {
            worker.timeout = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("range")) {
            worker.time = cfgAll.timeFind(cmd.word(), false);
            return;
        }
        if (a.equals("sgt")) {
            worker.secGrp = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("tos")) {
            worker.typOsrv = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("flow")) {
            worker.flowLab = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ttl")) {
            worker.tim2liv = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("size")) {
            worker.size = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("delay-start")) {
            worker.delaySt = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("delay-up")) {
            worker.delayUp = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("delay-down")) {
            worker.delayDn = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("stop")) {
            worker.stopNow();
            return;
        }
        if (a.equals("start")) {
            worker.startNow();
            return;
        }
        if (a.equals("runnow")) {
            worker.doRound();
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = null;
            return;
        }
        if (a.equals("script")) {
            worker.script = null;
            return;
        }
        if (a.equals("hidden")) {
            worker.hidden = false;
            return;
        }
        if (a.equals("log")) {
            worker.logging = false;
            return;
        }
        if (a.equals("start")) {
            worker.stopNow();
            return;
        }
        if (a.equals("stop")) {
            worker.startNow();
            return;
        }
        if (a.equals("mode")) {
            worker.mode = null;
            return;
        }
        if (a.equals("target")) {
            worker.target = null;
            return;
        }
        if (a.equals("exec-up")) {
            worker.execUp = null;
            return;
        }
        if (a.equals("exec-down")) {
            worker.execDn = null;
            return;
        }
        if (a.equals("pubkey")) {
            worker.pubkey = null;
            return;
        }
        if (a.equals("security")) {
            worker.secProto = 0;
            return;
        }
        if (a.equals("chat-script")) {
            worker.chats = null;
            return;
        }
        if (a.equals("vrf")) {
            worker.vrf = null;
            return;
        }
        if (a.equals("wake-vrf")) {
            worker.wakeVrf = null;
            return;
        }
        if (a.equals("source")) {
            worker.srcIfc = null;
            return;
        }
        if (a.equals("random-interval")) {
            worker.randInt = 0;
            return;
        }
        if (a.equals("random-initial")) {
            worker.randIni = 0;
            return;
        }
        if (a.equals("interval")) {
            worker.interval = 0;
            return;
        }
        if (a.equals("timeout")) {
            worker.timeout = 0;
            return;
        }
        if (a.equals("range")) {
            worker.time = null;
            return;
        }
        if (a.equals("delay-start")) {
            worker.delaySt = 0;
            return;
        }
        if (a.equals("delay-up")) {
            worker.delayUp = 0;
            return;
        }
        if (a.equals("delay-down")) {
            worker.delayDn = 0;
            return;
        }
        if (a.equals("force")) {
            worker.force = clntTrack.forMode.norm;
            return;
        }
        cmd.badCmd();
    }
}
