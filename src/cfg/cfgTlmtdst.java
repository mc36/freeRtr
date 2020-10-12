package cfg;

import clnt.clntTelemetry;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import serv.servStreamingMdt;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * telemetry destination
 *
 * @author matecsaba
 */
public class cfgTlmtdst implements Comparator<cfgTlmtdst>, cfgGeneric {

    /**
     * name of telemetry export
     */
    public String name;

    /**
     * description
     */
    public String description;

    /**
     * worker
     */
    public clntTelemetry worker;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "telemetry destination .*! port " + servStreamingMdt.port,
        "telemetry destination .*! no description",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * create new telemetry export
     */
    public cfgTlmtdst() {
        worker = new clntTelemetry();
    }

    public int compare(cfgTlmtdst o1, cfgTlmtdst o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "tlmtdst";
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2,.    description              specify description");
        l.add("2  2,.      <str>                  text");
        l.add("1  2      target                   specify target address");
        l.add("2  2,.      <str>                  name");
        l.add("1  2      port                     specify target port");
        l.add("2  .        <num>                  lines to skip");
        l.add("1  2      proxy                    specify proxy profile to use");
        l.add("2  .        <str>                  name of profile");
        l.add("1  .      start                    start exporting");
        l.add("1  .      stop                     stop exporting");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("telemetry destination " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, worker.target == null, cmds.tabulator, "target", worker.target);
        l.add(cmds.tabulator + "port " + worker.port);
        cmds.cfgLine(l, worker.proxy == null, cmds.tabulator, "proxy", "" + worker.proxy);
        if (worker.need2run) {
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

    public void doCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = s.equals("no");
        if (negated) {
            s = cmd.word();
        }
        if (s.equals("description")) {
            description = cmd.getRemaining();
            if (negated) {
                description = null;
            }
            return;
        }
        if (s.equals("target")) {
            if (negated) {
                worker.target = null;
                return;
            }
            worker.target = cmd.getRemaining();
            return;
        }
        if (s.equals("port")) {
            worker.port = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("proxy")) {
            if (negated) {
                worker.proxy = null;
                return;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            worker.proxy = prx.proxy;
            return;
        }
        if (s.equals("start")) {
            if (negated) {
                worker.stopWork();
            } else {
                worker.startWork();
            }
            return;
        }
        if (s.equals("stop")) {
            if (negated) {
                worker.startWork();
            } else {
                worker.stopWork();
            }
            return;
        }
        cmd.badCmd();
    }

}
