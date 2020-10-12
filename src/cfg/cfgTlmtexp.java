package cfg;

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
 * telemetry exporter
 *
 * @author matecsaba
 */
public class cfgTlmtexp implements Comparator<cfgTlmtexp>, cfgGeneric {

    /**
     * name of telemetry export
     */
    public String name;

    /**
     * description
     */
    public String description;

    /**
     * command
     */
    public String command;

    /**
     * path
     */
    public String path;

    /**
     * skip
     */
    public int skip;

    /**
     * name column
     */
    public int col;

    /**
     * name key
     */
    public String key;

    /**
     * columns
     */
    public tabGen<cfgTlmtexpCol> cols;

    /**
     * replacers
     */
    public tabGen<cfgTlmtexpRep> reps;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "telemetry exporter .*! no description",
        "telemetry exporter .*! name 0 null",
        "telemetry exporter .*! path null",
        "telemetry exporter .*! command null",
        "telemetry exporter .*! skip 1",
        "telemetry exporter .*! column .* name null",
        "telemetry exporter .*! column .* type sint64",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * create new telemetry export
     */
    public cfgTlmtexp() {
        cols = new tabGen<cfgTlmtexpCol>();
        reps = new tabGen<cfgTlmtexpRep>();
        skip = 1;
    }

    public int compare(cfgTlmtexp o1, cfgTlmtexp o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "tlmtexp";
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2,.    description              specify description");
        l.add("2  2,.      <str>                  text");
        l.add("1  2      command                  specify command to execute");
        l.add("2  2,.      <str>                  command");
        l.add("1  2      path                     specify sensor path");
        l.add("2  2,.      <str>                  command");
        l.add("1  2      name                     name column number");
        l.add("2  3        <num>                  column number");
        l.add("3  .          <str>                name of key");
        l.add("1  2      skip                     rows to skip");
        l.add("2  .        <num>                  lines to skip");
        l.add("1  2      replace                  define replaces in name");
        l.add("2  3        <str>                  string to replace");
        l.add("3  .          <str>                replacement string");
        l.add("1  2      column                   define column to export");
        l.add("2  3        <num>                  number");
        l.add("3  4          name                 set metric name");
        l.add("4  .            <str>              metric name");
        l.add("3  4          type                 set metric type");
        l.add("4  .            bytes              bytes");
        l.add("4  .            string             string");
        l.add("4  .            bool               boolean");
        l.add("4  .            uint32             unsigned 32bit integer");
        l.add("4  .            uint64             unsigned 64bit integer");
        l.add("4  .            sint32             signed 32bit integer");
        l.add("4  .            sint64             signed 64bit integer");
        l.add("4  .            float              32bit floating point number");
        l.add("4  .            double             64bit floating point number");
        l.add("3  4          replace              define replaces in value");
        l.add("4  5            <str>              string to replace");
        l.add("5  .              <str>            replacement string");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("telemetry exporter " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        l.add(cmds.tabulator + "command " + command);
        l.add(cmds.tabulator + "path " + path);
        l.add(cmds.tabulator + "skip " + skip);
        l.add(cmds.tabulator + "name " + col + " " + key);
        for (int i = 0; i < reps.size(); i++) {
            cfgTlmtexpRep rep = reps.get(i);
            l.add(cmds.tabulator + "replace " + rep.src + " " + rep.trg);
        }
        for (int o = 0; o < cols.size(); o++) {
            cfgTlmtexpCol col = cols.get(o);
            String cn = cmds.tabulator + "column " + col.num;
            l.add(cn + " name " + col.nam);
            l.add(cn + " type " + servStreamingMdt.type2string(col.typ));
            for (int i = 0; i < col.reps.size(); i++) {
                cfgTlmtexpRep rep = col.reps.get(i);
                l.add(cn + " replace " + rep.src + " " + rep.trg);
            }
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
        if (s.equals("command")) {
            if (negated) {
                command = null;
                return;
            }
            command = cmd.getRemaining();
            return;
        }
        if (s.equals("path")) {
            if (negated) {
                path = null;
                return;
            }
            path = cmd.getRemaining();
            return;
        }
        if (s.equals("skip")) {
            skip = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("name")) {
            col = bits.str2num(cmd.word());
            key = cmd.word();
            return;
        }
        if (s.equals("replace")) {
            cfgTlmtexpRep rep = new cfgTlmtexpRep(cmd.word());
            rep.trg = cmd.word();
            if (negated) {
                reps.del(rep);
            } else {
                reps.add(rep);
            }
            return;
        }
        if (!s.equals("column")) {
            cmd.badCmd();
            return;
        }
        cfgTlmtexpCol col = new cfgTlmtexpCol(bits.str2num(cmd.word()));
        cfgTlmtexpCol oldc = cols.add(col);
        if (oldc != null) {
            col = oldc;
        }
        s = cmd.word();
        if (s.equals("name")) {
            if (negated) {
                cols.del(col);
                return;
            }
            col.nam = cmd.word();
            return;
        }
        if (s.equals("type")) {
            col.typ = servStreamingMdt.string2type(cmd.word());
            return;
        }
        if (s.equals("replace")) {
            cfgTlmtexpRep rep = new cfgTlmtexpRep(cmd.word());
            rep.trg = cmd.word();
            if (negated) {
                col.reps.del(rep);
            } else {
                col.reps.add(rep);
            }
            return;
        }
        cmd.badCmd();
    }

}

class cfgTlmtexpRep implements Comparator<cfgTlmtexpRep> {

    public final String src;

    public String trg;

    public cfgTlmtexpRep(String n) {
        src = n;
    }

    public int compare(cfgTlmtexpRep o1, cfgTlmtexpRep o2) {
        return o1.src.compareTo(o2.src);
    }

}

class cfgTlmtexpCol implements Comparator<cfgTlmtexpCol> {

    public final int num;

    public String nam;

    public int typ = servStreamingMdt.fnSint64;

    public tabGen<cfgTlmtexpRep> reps = new tabGen<cfgTlmtexpRep>();

    public cfgTlmtexpCol(int n) {
        num = n;
    }

    public int compare(cfgTlmtexpCol o1, cfgTlmtexpCol o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

}
