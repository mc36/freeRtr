package cfg;

import auth.authLocal;
import clnt.clntTrack;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import serv.servGeneric;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * tracker configuration
 *
 * @author matecsaba
 */
public class cfgTrack implements Comparator<cfgTrack>, cfgGeneric {

    /**
     * name of tracker
     */
    public String name;

    /**
     * worker
     */
    public final clntTrack worker = new clntTrack();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "tracker .*! force normal",
        "tracker .*! no hidden",
        "tracker .*! no target",
        "tracker .*! no exec-up",
        "tracker .*! no exec-down",
        "tracker .*! no chat-script",
        "tracker .*! no security",
        "tracker .*! no vrf",
        "tracker .*! no source",
        "tracker .*! no log",
        "tracker .*! random-interval 0",
        "tracker .*! random-initial 0",
        "tracker .*! interval 0",
        "tracker .*! timeout 0",
        "tracker .*! tos 0",
        "tracker .*! ttl 255",
        "tracker .*! size 80",
        "tracker .*! delay-up 0",
        "tracker .*! delay-down 0"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgTrack o1, cfgTrack o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
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
     * @return help
     */
    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2      mode                       specify mode of runs");
        l.add("2  .        icmp                     icmp echo request");
        l.add("2  .        nrpe                     nrpe remote check");
        l.add("2  .        other                    other tracker");
        l.add("2  .        tcp                      tcp connection");
        l.add("2  .        bfd                      bidirectional forwarding detection");
        l.add("2  .        interface                interface state");
        l.add("2  .        route                    any route table entry for address");
        l.add("2  .        prefix                   exact route table entry for prefix");
        l.add("2  .        script                   tcl script");
        l.add("1  2      force                      specify result of runs");
        l.add("2  .        up                       always up");
        l.add("2  .        down                     always down");
        l.add("2  .        negate                   negate result");
        l.add("2  .        normal                   leave result");
        l.add("1  2      target                     specify address of test");
        l.add("2  2,.      <addr>                   address of target");
        l.add("1  2      vrf                        specify vrf of test");
        l.add("2  .        <name>                   vrf to use");
        l.add("1  2      security                   select security protocol");
        l.add("2  .        ssh                      use secure shell");
        l.add("2  .        tls                      use transport layer security");
        l.add("2  .        dtls                     use datagram transport layer security");
        l.add("2  .        telnet                   use telnet protocol");
        l.add("1  2      chat-script                specify script to use");
        l.add("2  .        <name>                   chatter to use");
        l.add("1  2      source                     specify source of test");
        l.add("2  .        <name>                   interface to use");
        l.add("1  2      random-interval            specify random time between runs");
        l.add("2  .        <num>                    milliseconds between runs");
        l.add("1  2      random-initial             specify random time before run");
        l.add("2  .        <num>                    milliseconds between runs");
        l.add("1  2      interval                   specify time between runs");
        l.add("2  .        <num>                    milliseconds between runs");
        l.add("1  2      timeout                    specify timeout value");
        l.add("2  .        <num>                    timeout in milliseconds");
        l.add("1  2      tos                        specify tos");
        l.add("2  .        <num>                    value");
        l.add("1  2      ttl                        specify ttl");
        l.add("2  .        <num>                    value");
        l.add("1  2      size                       size of payload");
        l.add("2  .        <num>                    value");
        l.add("1  2      delay-up                   number of successes before up");
        l.add("2  .        <num>                    value");
        l.add("1  2      delay-down                 number of failures before down");
        l.add("2  .        <num>                    value");
        l.add("1  2      exec-up                    exec command to execute on up");
        l.add("2  2,.      <cmd>                    value");
        l.add("1  2      exec-down                  exec command to execute on down");
        l.add("2  2,.      <cmd>                    value");
        l.add("1  .      stop                       stop working");
        l.add("1  .      start                      start working");
        l.add("1  .      runnow                     run one round now");
        l.add("1  .      hidden                     hide exec commands");
        l.add("1  .      log                        log actions");
        return l;
    }

    /**
     * get config
     *
     * @param filter filter
     * @return config
     */
    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("tracker " + name);
        cmds.cfgLine(l, !worker.hidden, cmds.tabulator, "hidden", "");
        cmds.cfgLine(l, !worker.logging, cmds.tabulator, "log", "");
        l.add(cmds.tabulator + "mode " + clntTrack.mode2string(worker.mode));
        l.add(cmds.tabulator + "force " + clntTrack.force2string(worker.force));
        cmds.cfgLine(l, worker.target == null, cmds.tabulator, "target", worker.target);
        if (worker.hidden) {
            cmds.cfgLine(l, worker.execUp == null, cmds.tabulator, "exec-up", authLocal.passwdEncode(worker.execUp));
            cmds.cfgLine(l, worker.execDn == null, cmds.tabulator, "exec-down", authLocal.passwdEncode(worker.execDn));
        } else {
            cmds.cfgLine(l, worker.execUp == null, cmds.tabulator, "exec-up", worker.execUp);
            cmds.cfgLine(l, worker.execDn == null, cmds.tabulator, "exec-down", worker.execDn);
        }
        cmds.cfgLine(l, worker.secProto == 0, cmds.tabulator, "security", servGeneric.proto2string(worker.secProto));
        if (worker.chats != null) {
            l.add(cmds.tabulator + "chat-script " + worker.chats.scrName);
        } else {
            l.add(cmds.tabulator + "no chat-script");
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
        l.add(cmds.tabulator + "random-interval " + worker.randInt);
        l.add(cmds.tabulator + "random-initial " + worker.randIni);
        l.add(cmds.tabulator + "interval " + worker.interval);
        l.add(cmds.tabulator + "timeout " + worker.timeout);
        l.add(cmds.tabulator + "tos " + worker.typOsrv);
        l.add(cmds.tabulator + "ttl " + worker.tim2liv);
        l.add(cmds.tabulator + "size " + worker.size);
        l.add(cmds.tabulator + "delay-up " + worker.delayUp);
        l.add(cmds.tabulator + "delay-down " + worker.delayDn);
        if (worker.working) {
            l.add(cmds.tabulator + "start");
        } else {
            l.add(cmds.tabulator + "stop");
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
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
        if (a.equals("hidden")) {
            worker.hidden = true;
            return;
        }
        if (a.equals("log")) {
            worker.logging = true;
            return;
        }
        if (a.equals("mode")) {
            a = cmd.word();
            worker.mode = null;
            if (a.equals("icmp")) {
                worker.mode = clntTrack.operMod.icmp;
                return;
            }
            if (a.equals("tcp")) {
                worker.mode = clntTrack.operMod.tcp;
                return;
            }
            if (a.equals("bfd")) {
                worker.mode = clntTrack.operMod.bfd;
                return;
            }
            if (a.equals("interface")) {
                worker.mode = clntTrack.operMod.iface;
                return;
            }
            if (a.equals("route")) {
                worker.mode = clntTrack.operMod.route;
                return;
            }
            if (a.equals("prefix")) {
                worker.mode = clntTrack.operMod.prefix;
                return;
            }
            if (a.equals("script")) {
                worker.mode = clntTrack.operMod.script;
                return;
            }
            if (a.equals("nrpe")) {
                worker.mode = clntTrack.operMod.nrpe;
                return;
            }
            if (a.equals("other")) {
                worker.mode = clntTrack.operMod.other;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("force")) {
            a = cmd.word();
            worker.force = clntTrack.forMode.norm;
            if (a.equals("up")) {
                worker.force = clntTrack.forMode.up;
                return;
            }
            if (a.equals("down")) {
                worker.force = clntTrack.forMode.down;
                return;
            }
            if (a.equals("negate")) {
                worker.force = clntTrack.forMode.neg;
                return;
            }
            if (a.equals("normal")) {
                worker.force = clntTrack.forMode.norm;
                return;
            }
            cmd.badCmd();
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
        if (a.equals("source")) {
            worker.srcIfc = cfgAll.ifcFind(cmd.word(), false);
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
        if (a.equals("tos")) {
            worker.typOsrv = bits.str2num(cmd.word());
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
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
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
