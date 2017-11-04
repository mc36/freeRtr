package cfg;

import addr.addrIP;
import clnt.clntMtrack;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packMtrack;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * mtracker configuration
 *
 * @author matecsaba
 */
public class cfgMtrack implements Comparator<cfgMtrack>, cfgGeneric {

    /**
     * name of tracker
     */
    public String name;

    /**
     * worker
     */
    public final clntMtrack worker = new clntMtrack();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "mtracker .*! no vrf",
        "mtracker .*! no source",
        "mtracker .*! no group",
        "mtracker .*! no logging",
        "mtracker .*! interval 30000",
        "mtracker .*! timeout 10",
        "mtracker .*! packet 10",
        "mtracker .*! port " + packMtrack.port,
        "mtracker .*! tos 0",
        "mtracker .*! ttl 255"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgMtrack o1, cfgMtrack o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2      group                      specify group address");
        l.add("2  2,.      <addr>                   address of group");
        l.add("1  2      target                     specify target address");
        l.add("2  2,.      <addr>                   address of target");
        l.add("1  2      vrf                        specify vrf of test");
        l.add("2  .        <name>                   vrf to use");
        l.add("1  2      source                     specify source of test");
        l.add("2  .        <name>                   interface to use");
        l.add("1  2      interval                   specify time between runs");
        l.add("2  .        <num>                    milliseconds between runs");
        l.add("1  2      timeout                    timeout of peer");
        l.add("2  .        <num>                    value");
        l.add("1  2      tos                        specify tos");
        l.add("2  .        <num>                    value");
        l.add("1  2      ttl                        specify ttl");
        l.add("2  .        <num>                    value");
        l.add("1  2      packet                     inter packet gap");
        l.add("2  .        <num>                    time in ms");
        l.add("1  2      port                       specify port");
        l.add("2  .        <num>                    value");
        l.add("1  .      logging                    log events");
        l.add("1  .      stop                       stop working");
        l.add("1  .      start                      start working");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("mtracker " + name);
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
        l.add(cmds.tabulator + "packet " + worker.packTim);
        if (worker.getWorking()) {
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

    private boolean doConfStr(cmds cmd) {
        String a = cmd.word();
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
            worker.srcIfc = cfgAll.ifcFind(cmd.word(), false);
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
        if (!a.equals("no")) {
            cmd.badCmd();
            return false;
        }
        a = cmd.word();
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
            worker.port = packMtrack.port;
            return false;
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
