package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import serv.servStreamingMdt;
import tab.tabGen;
import user.userExec;
import user.userFilter;
import user.userFormat;
import user.userHelping;
import user.userReader;
import util.bits;
import util.cmds;
import util.protoBuf;
import util.protoBufEntry;

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
     * last reported
     */
    public long last;

    /**
     * time elapsed
     */
    public int time;

    /**
     * reports generated
     */
    public int cnt;

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

    /**
     * get result
     *
     * @return result
     */
    public List<String> getResult() {
        if (command == null) {
            return null;
        }
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, 1023);
        rdr.tabMod = userFormat.tableMode.raw;
        rdr.height = 0;
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.timeout = 120000;
        String a = exe.repairCommand(command);
        exe.executeCommand(a);
        pip = pl.getSide();
        pl.setClose();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRtryLF;
        List<String> lst = new ArrayList<String>();
        for (;;) {
            if (pip.ready2rx() < 1) {
                break;
            }
            a = pip.lineGet(1);
            if (a.length() < 1) {
                continue;
            }
            lst.add(a);
        }
        return lst;
    }

    private packHolder doLine(String a) {
        cmds cm = new cmds("prom", a);
        List<String> cl = new ArrayList<String>();
        for (;;) {
            a = cm.word(";");
            if (a.length() < 1) {
                break;
            }
            cl.add(a);
        }
        int cls = cl.size();
        if (col >= cls) {
            return null;
        }
        protoBuf pb = new protoBuf();
        a = cl.get(col);
        for (int i = 0; i < reps.size(); i++) {
            cfgTlmtexpRep rep = reps.get(i);
            a = a.replaceAll(rep.src, rep.trg);
        }
        packHolder pck1 = new packHolder(true, true);
        packHolder pck2 = new packHolder(true, true);
        packHolder pck3 = new packHolder(true, true);
        pb.putField(servStreamingMdt.fnName, protoBufEntry.tpBuf, key.getBytes());
        pb.putField(servStreamingMdt.fnString, protoBufEntry.tpBuf, a.getBytes());
        pb.toPacket(pck1);
        pb.clear();
        pb.putField(servStreamingMdt.fnName, protoBufEntry.tpBuf, servStreamingMdt.nmDat.getBytes());
        pb.toPacket(pck2);
        pb.clear();
        for (int o = 0; o < cols.size(); o++) {
            cfgTlmtexpCol cc = cols.get(o);
            if (cl.size() <= cc.num) {
                continue;
            }
            a = cl.get(cc.num);
            for (int i = 0; i < cc.reps.size(); i++) {
                cfgTlmtexpRep rep = cc.reps.get(i);
                a = a.replaceAll(rep.src, rep.trg);
            }
            protoBuf pb2 = new protoBuf();
            pb2.putField(servStreamingMdt.fnName, protoBufEntry.tpBuf, cc.nam.getBytes());
            switch (cc.typ) {
                case servStreamingMdt.fnByte:
                    pb2.putField(cc.typ, protoBufEntry.tpBuf, a.getBytes());
                    break;
                case servStreamingMdt.fnString:
                    pb2.putField(cc.typ, protoBufEntry.tpBuf, a.getBytes());
                    break;
                case servStreamingMdt.fnBool:
                    pb2.putField(cc.typ, protoBufEntry.tpInt, bits.str2num(a));
                    break;
                case servStreamingMdt.fnUint32:
                case servStreamingMdt.fnUint64:
                    pb2.putField(cc.typ, protoBufEntry.tpInt, bits.str2long(a));
                    break;
                case servStreamingMdt.fnSint32:
                case servStreamingMdt.fnSint64:
                    pb2.putField(cc.typ, protoBufEntry.tpInt, protoBuf.toZigzag(bits.str2long(a)));
                    break;
                case servStreamingMdt.fnDouble:
                    Double d;
                    try {
                        d = Double.parseDouble(a);
                    } catch (Exception e) {
                        continue;
                    }
                    pb2.putField(cc.typ, protoBufEntry.tpInt, Double.doubleToLongBits(d));
                    break;
                case servStreamingMdt.fnFloat:
                    Float f;
                    try {
                        f = Float.parseFloat(a);
                    } catch (Exception e) {
                        continue;
                    }
                    pb2.putField(cc.typ, protoBufEntry.tpInt, Float.floatToIntBits(f));
                    break;
                default:
                    continue;
            }
            pck3.clear();
            pb2.toPacket(pck3);
            pb2.clear();
            pb2.putField(servStreamingMdt.fnFields, protoBufEntry.tpBuf, pck3.getCopy());
            pb2.toPacket(pck2);
            pb2.clear();
        }
        protoBuf pb2 = new protoBuf();
        pb2.putField(servStreamingMdt.fnName, protoBufEntry.tpBuf, servStreamingMdt.nmKey.getBytes());
        pb2.putField(servStreamingMdt.fnFields, protoBufEntry.tpBuf, pck1.getCopy());
        pck3.clear();
        pb2.toPacket(pck3);
        pb2.clear();
        pb.putField(servStreamingMdt.fnFields, protoBufEntry.tpBuf, pck3.getCopy());
        pb.putField(servStreamingMdt.fnFields, protoBufEntry.tpBuf, pck2.getCopy());
        pck3.clear();
        pb.toPacket(pck3);
        return pck3;
    }

    /**
     * generate report
     *
     * @return report, null on error
     */
    public packHolder getReport() {
        last = bits.getTime();
        List<String> res = getResult();
        for (int i = 0; i < skip; i++) {
            if (res.size() < 1) {
                break;
            }
            res.remove(0);
        }
        packHolder pck = new packHolder(true, true);
        protoBuf pb = new protoBuf();
        pb.putField(servStreamingMdt.rpStart, protoBufEntry.tpInt, last);
        pb.putField(servStreamingMdt.rpNodeStr, protoBufEntry.tpBuf, cfgAll.hostName.getBytes());
        pb.putField(servStreamingMdt.rpSubsStr, protoBufEntry.tpBuf, name.getBytes());
        pb.putField(servStreamingMdt.rpEnc, protoBufEntry.tpBuf, path.getBytes());
        pb.toPacket(pck);
        pb.clear();
        for (int i = 0; i < res.size(); i++) {
            packHolder ln = doLine(res.get(i));
            if (ln == null) {
                continue;
            }
            pb.putField(servStreamingMdt.rpKvgpb, protoBufEntry.tpBuf, ln.getCopy());
            pb.toPacket(pck);
            pb.clear();
        }
        long tim = bits.getTime();
        pb.putField(servStreamingMdt.rpStop, protoBufEntry.tpInt, tim);
        pb.toPacket(pck);
        time = (int) (tim - last);
        return pck;
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShow() {
        List<String> res = new ArrayList<String>();
        res.add("command=" + command);
        res.add("asked=" + cnt + " times");
        res.add("reply=" + time + " ms");
        res.add("output:");
        res.addAll(getResult());
        res.add("result:" + getReport().dump());
        return res;
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
