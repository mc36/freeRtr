package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.clnt.clntTelemetry;
import net.freertr.serv.servStreamingMdt;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * telemetry destination
 *
 * @author matecsaba
 */
public class cfgTlmtry implements Comparator<cfgTlmtry>, cfgGeneric {

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
    public final static String[] defaultL = {
        "telemetry .*! port " + servStreamingMdt.port,
        "telemetry .*! interval 5000",
        "telemetry .*! no description",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * create new telemetry export
     */
    public cfgTlmtry() {
        worker = new clntTelemetry();
    }

    public int compare(cfgTlmtry o1, cfgTlmtry o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "tlmtdst";
    }

    public void getHelp(userHelping l) {
        l.add(null, "1  2,.    description              specify description");
        l.add(null, "2  2,.      <str>                  text");
        l.add(null, "1  2      rename                   rename this exporter");
        l.add(null, "2  .        <name>                 set new name");
        l.add(null, "1  2      target                   specify target address");
        l.add(null, "2  2,.      <str>                  name");
        l.add(null, "1  2      port                     specify target port");
        l.add(null, "2  .        <num>                  lines to skip");
        l.add(null, "1  2      sensor                   specify sensor to export");
        l.add(null, "2  .        <name:sns>             name of sensor");
        l.add(null, "1  2      interval                 specify interval");
        l.add(null, "2  .        <num>                  time in ms");
        l.add(null, "1  2      proxy                    specify proxy profile to use");
        l.add(null, "2  .        <name:prx>             name of profile");
        l.add(null, "1  .      start                    start exporting");
        l.add(null, "1  .      stop                     stop exporting");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("telemetry " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, worker.target == null, cmds.tabulator, "target", worker.target);
        l.add(cmds.tabulator + "port " + worker.port);
        l.add(cmds.tabulator + "interval " + worker.interval);
        cmds.cfgLine(l, worker.proxy == null, cmds.tabulator, "proxy", "" + worker.proxy);
        for (int i = 0; i < worker.sensors.size(); i++) {
            cfgSensor ntry = worker.sensors.get(i);
            l.add(cmds.tabulator + "sensor " + ntry.name);
        }
        if (worker.need2run) {
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
        if (s.equals("rename")) {
            s = cmd.word();
            cfgTlmtry v = cfgAll.tlmdsFind(s, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = s;
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
        if (s.equals("interval")) {
            worker.interval = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("sensor")) {
            cfgSensor ntry = cfgAll.sensorFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such sensor");
                return;
            }
            if (negated) {
                worker.sensors.del(ntry);
                return;
            }
            worker.sensors.put(ntry);
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
