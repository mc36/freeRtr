package net.freertr.cfg;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.zip.Deflater;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servStreamingMdt;
import net.freertr.tab.tabGen;
import net.freertr.user.userExec;
import net.freertr.user.userFilter;
import net.freertr.user.userFlash;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.user.userReader;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.extMrkLng;
import net.freertr.util.extMrkLngEntry;
import net.freertr.util.history;
import net.freertr.util.logger;
import net.freertr.util.protoBuf;
import net.freertr.util.protoBufEntry;
import net.freertr.util.verCore;

/**
 * telemetry exporter
 *
 * @author matecsaba
 */
public class cfgSensor implements Runnable, Comparator<cfgSensor>, cfgGeneric {

    /**
     * name of sensor
     */
    public String name;

    /**
     * description of this dialpeer
     */
    public String description = null;

    /**
     * hidden sensor
     */
    public boolean hidden;

    /**
     * command
     */
    public String command;

    /**
     * prefix
     */
    public String prefix;

    /**
     * prepend
     */
    public String prepend;

    /**
     * path
     */
    public String path;

    /**
     * skip
     */
    public int skip;

    /**
     * key name
     */
    public String keyN;

    /**
     * key path
     */
    public String keyP;

    /**
     * name column
     */
    public int namC;

    /**
     * name label
     */
    public String namL;

    /**
     * static labels
     */
    public String namS;

    /**
     * additional column
     */
    public int acol = -1;

    /**
     * additional separator
     */
    public String asep;

    /**
     * additional label
     */
    public String alab;

    /**
     * columns
     */
    public tabGen<cfgSensorCol> cols;

    /**
     * replacers
     */
    public tabGen<cfgSensorRep> reps;

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
     * collection interval
     */
    public int locInt;

    /**
     * collection memory
     */
    public tabGen<cfgSensorMem> locMem;

    /**
     * collection file
     */
    public String locFil;

    /**
     * collection backup
     */
    public String locBak;

    /**
     * collection bytes
     */
    public int locByt;

    /**
     * collection time
     */
    public int locTim;

    /**
     * collection start
     */
    public long locStr;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "sensor .*! no command",
        "sensor .*! no description",
        "sensor .*! name 0",
        "sensor .*! no labels",
        "sensor .*! addname -1 null",
        "sensor .*! skip 1",
        "sensor .*! no excluded",
        "sensor .*! column .* style gauge",
        "sensor .*! column .* type uint64",
        "sensor .*! column .* split null null null",
        "sensor .*! column .* help null",
        "sensor .*! no local interval",
        "sensor .*! no local memory",
        "sensor .*! no local file",
        "sensor .*! no local backup",
        "sensor .*! no local max-byte",
        "sensor .*! no local max-time",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * create new sensor
     *
     * @param n name
     */
    public cfgSensor(String n) {
        cols = new tabGen<cfgSensorCol>();
        reps = new tabGen<cfgSensorRep>();
        skip = 1;
        name = n;
        path = n + "/" + n;
        keyN = n;
        keyP = n + "/" + n;
        prefix = n;
        prepend = n;
    }

    public String toString() {
        return name;
    }

    public int compare(cfgSensor o1, cfgSensor o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "sensor";
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 2      rename                   rename this sensor");
        l.add(null, "2 .        <str>                  set new name");
        l.add(null, "1 2      description              specify description");
        l.add(null, "2 2,.      <str>                  description");
        l.add(null, "1 2      command                  specify command to execute");
        l.add(null, "2 2,.      <str>                  command");
        l.add(null, "1 2      prepend                  specify prepend");
        l.add(null, "2 .        <str>                  name");
        l.add(null, "1 2      prefix                   specify prefix");
        l.add(null, "2 .        <str>                  name");
        l.add(null, "1 2      path                     specify prefix");
        l.add(null, "2 .        <str>                  name");
        l.add(null, "1 2      labels                   static labels");
        l.add(null, "2 .        <str>                  name");
        l.add(null, "1 2      key                      key column number");
        l.add(null, "2 3        <str>                  name");
        l.add(null, "3 .          <str>                path");
        l.add(null, "1 2      name                     name column number");
        l.add(null, "2 3,.      <num>                  column number");
        l.add(null, "3 .          <str>                label");
        l.add(null, "1 2      addname                  add name column number");
        l.add(null, "2 3        <num>                  column number");
        l.add(null, "3 4,.        <str>                separator, * means empty");
        l.add(null, "4 .            <str>              label");
        l.add(null, "1 2      skip                     rows to skip");
        l.add(null, "2 .        <num>                  lines to skip");
        l.add(null, "1 2      replace                  define replaces in name");
        l.add(null, "2 3        <str>                  string to replace");
        l.add(null, "3 .          <str>                replacement string");
        l.add(null, "1 2      column                   define column to export");
        l.add(null, "2 3        <num>                  number");
        l.add(null, "3 4,.        name                 set name");
        l.add(null, "4 5,.          <str>              name, * means empty");
        l.add(null, "5 .              <str>            label");
        l.add(null, "3 4          type                 set type");
        l.add(null, "4 .            bytes              bytes");
        l.add(null, "4 .            string             string");
        l.add(null, "4 .            bool               boolean");
        l.add(null, "4 .            uint32             unsigned 32bit integer");
        l.add(null, "4 .            uint64             unsigned 64bit integer");
        l.add(null, "4 .            sint32             signed 32bit integer");
        l.add(null, "4 .            sint64             signed 64bit integer");
        l.add(null, "4 .            float              32bit floating point number");
        l.add(null, "4 .            double             64bit floating point number");
        l.add(null, "3 4          style                set style");
        l.add(null, "4 .            gauge              gauge");
        l.add(null, "4 .            counter            counter");
        l.add(null, "3 4          help                 set help");
        l.add(null, "4 4,.          <str>              help");
        l.add(null, "3 4          replace              define replaces in value");
        l.add(null, "4 5            <str>              string to replace");
        l.add(null, "5 .              <str>            replacement string");
        l.add(null, "3 4          split                define split of value");
        l.add(null, "4 5            <str>              delimiter");
        l.add(null, "5 6              <str>            first label");
        l.add(null, "6 .                <str>          second label");
        l.add(null, "1 2      local                    local collection options");
        l.add(null, "2 3        interval               collection interval");
        l.add(null, "3 .          <num>                time in ms");
        l.add(null, "2 .        memory                 collect to memory");
        l.add(null, "2 3        file                   collect to file");
        l.add(null, "3 .          <str>                file name");
        l.add(null, "2 3        max-byte               maximum bytes");
        l.add(null, "3 .          <num>                bytes");
        l.add(null, "2 3        max-time               maximum time");
        l.add(null, "3 .          <num>                time in ms");
        l.add(null, "2 3        backup                 backup to file");
        l.add(null, "3 .          <str>                file name");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (hidden) {
            return l;
        }
        l.add("sensor " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        l.add(cmds.tabulator + "path " + path);
        l.add(cmds.tabulator + "prefix " + prefix);
        l.add(cmds.tabulator + "prepend " + prepend);
        cmds.cfgLine(l, command == null, cmds.tabulator, "command", "" + command);
        String a = "";
        if (namL != null) {
            a = " " + namL;
        }
        l.add(cmds.tabulator + "name " + namC + a);
        l.add(cmds.tabulator + "key " + keyN + " " + keyP);
        if (namS != null) {
            l.add(cmds.tabulator + "labels " + namS);
        } else {
            l.add(cmds.tabulator + "no labels");
        }
        a = "";
        if (alab != null) {
            a = " " + alab;
        }
        l.add(cmds.tabulator + "addname " + acol + " " + asep + a);
        l.add(cmds.tabulator + "skip " + skip);
        for (int i = 0; i < reps.size(); i++) {
            cfgSensorRep rep = reps.get(i);
            l.add(cmds.tabulator + "replace " + rep.src + " " + rep.trg);
        }
        for (int o = 0; o < cols.size(); o++) {
            cfgSensorCol col = cols.get(o);
            String cn = cmds.tabulator + "column " + col.num;
            a = "";
            if (col.lab != null) {
                a = " " + col.lab;
            }
            l.add(cn + " name " + col.nam + a);
            l.add(cn + " style " + col.sty);
            l.add(cn + " type " + servStreamingMdt.type2string(col.typ));
            l.add(cn + " help " + col.hlp);
            l.add(cn + " split " + col.splS + " " + col.splL + " " + col.splR);
            for (int i = 0; i < col.reps.size(); i++) {
                cfgSensorRep rep = col.reps.get(i);
                l.add(cn + " replace " + rep.src + " " + rep.trg);
            }
        }
        cmds.cfgLine(l, locInt < 1, cmds.tabulator, "local interval", "" + locInt);
        cmds.cfgLine(l, locMem == null, cmds.tabulator, "local memory", "");
        cmds.cfgLine(l, locFil == null, cmds.tabulator, "local file", "" + locFil);
        cmds.cfgLine(l, locBak == null, cmds.tabulator, "local backup", "" + locBak);
        cmds.cfgLine(l, locByt < 1, cmds.tabulator, "local max-byte", "" + locByt);
        cmds.cfgLine(l, locTim < 1, cmds.tabulator, "local max-time", "" + locTim);
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
        if (s.equals("rename")) {
            s = cmd.word();
            cfgSensor v = cfgAll.sensorFind(s, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = s;
            return;
        }
        if (s.equals("description")) {
            description = cmd.getRemaining();
            if (negated) {
                description = null;
            }
            return;
        }
        if (s.equals("local")) {
            s = cmd.word();
            if (s.equals("interval")) {
                if (negated) {
                    locInt = 0;
                    return;
                }
                locInt = bits.str2num(cmd.word());
                new Thread(this).start();
                return;
            }
            if (s.equals("memory")) {
                if (negated) {
                    locMem = null;
                    return;
                }
                locMem = new tabGen<cfgSensorMem>();
                return;
            }
            if (s.equals("file")) {
                locStr = bits.getTime();
                locFil = cmd.word();
                if (negated) {
                    locFil = null;
                    return;
                }
                return;
            }
            if (s.equals("backup")) {
                locBak = cmd.word();
                if (negated) {
                    locBak = null;
                    return;
                }
                return;
            }
            if (s.equals("max-byte")) {
                locByt = bits.str2num(cmd.word());
                if (negated) {
                    locByt = 0;
                    return;
                }
                return;
            }
            if (s.equals("max-time")) {
                locTim = bits.str2num(cmd.word());
                if (negated) {
                    locTim = 0;
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (s.equals("command")) {
            command = cmd.getRemaining();
            if (negated) {
                command = null;
            }
            return;
        }
        if (s.equals("path")) {
            path = cmd.getRemaining();
            return;
        }
        if (s.equals("prefix")) {
            prefix = cmd.getRemaining();
            return;
        }
        if (s.equals("prepend")) {
            prepend = cmd.word();
            return;
        }
        if (s.equals("key")) {
            keyN = cmd.word();
            keyP = cmd.word();
            return;
        }
        if (s.equals("name")) {
            namC = bits.str2num(cmd.word());
            if (cmd.size() < 1) {
                namL = null;
            } else {
                namL = cmd.word();
            }
            return;
        }
        if (s.equals("labels")) {
            if (negated) {
                namS = null;
            } else {
                namS = cmd.word();
            }
            return;
        }
        if (s.equals("addname")) {
            if (negated) {
                acol = -1;
                asep = null;
                alab = null;
                return;
            }
            acol = bits.str2num(cmd.word());
            asep = cmd.word();
            if (cmd.size() < 1) {
                alab = null;
            } else {
                alab = cmd.word();
            }
            return;
        }
        if (s.equals("skip")) {
            skip = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("replace")) {
            cfgSensorRep rep = new cfgSensorRep(cmd.word());
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
        cfgSensorCol col = new cfgSensorCol(bits.str2num(cmd.word()));
        cfgSensorCol oldc = cols.add(col);
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
            if (cmd.size() < 1) {
                col.lab = null;
            } else {
                col.lab = cmd.word();
            }
            return;
        }
        if (s.equals("help")) {
            if (negated) {
                col.hlp = null;
            } else {
                col.hlp = cmd.getRemaining();
            }
            return;
        }
        if (s.equals("type")) {
            col.typ = servStreamingMdt.string2type(cmd.word());
            return;
        }
        if (s.equals("style")) {
            col.sty = cmd.word();
            return;
        }
        if (s.equals("split")) {
            if (negated) {
                col.splS = null;
                col.splL = null;
                col.splR = null;
            } else {
                col.splS = cmd.word();
                col.splL = cmd.word();
                col.splR = cmd.word();
            }
            return;
        }
        if (s.equals("replace")) {
            cfgSensorRep rep = new cfgSensorRep(cmd.word());
            rep.trg = cmd.word();
            if (negated) {
                col.reps.del(rep);
            } else {
                col.reps.add(rep);
            }
            return;
        }
    }

    /**
     * get result
     *
     * @return result
     */
    public List<String> getResult() {
        if (command == null) {
            return new ArrayList<String>();
        }
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, null);
        pip.settingsPut(pipeSetting.tabMod, userFormat.tableMode.raw);
        pip.settingsPut(pipeSetting.height, 0);
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.setTime(120000);
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

    private void doMetricKvGpb(packHolder pck2, packHolder pck3, int typ, String nam, String val) {
        protoBuf pb2 = new protoBuf();
        pb2.putField(servStreamingMdt.fnName, protoBufEntry.tpBuf, nam.getBytes());
        switch (typ) {
            case servStreamingMdt.fnByte:
                pb2.putField(typ, protoBufEntry.tpBuf, val.getBytes());
                break;
            case servStreamingMdt.fnString:
                pb2.putField(typ, protoBufEntry.tpBuf, val.getBytes());
                break;
            case servStreamingMdt.fnBool:
                pb2.putField(typ, protoBufEntry.tpInt, bits.str2num(val));
                break;
            case servStreamingMdt.fnUint32:
            case servStreamingMdt.fnUint64:
                pb2.putField(typ, protoBufEntry.tpInt, bits.str2long(val));
                break;
            case servStreamingMdt.fnSint32:
            case servStreamingMdt.fnSint64:
                pb2.putField(typ, protoBufEntry.tpInt, protoBuf.toZigzag(bits.str2long(val)));
                break;
            case servStreamingMdt.fnDouble:
                double d;
                try {
                    d = Double.parseDouble(val);
                } catch (Exception e) {
                    return;
                }
                pb2.putField(typ, protoBufEntry.tpInt, Double.doubleToLongBits(d));
                break;
            case servStreamingMdt.fnFloat:
                float f;
                try {
                    f = Float.parseFloat(val);
                } catch (Exception e) {
                    return;
                }
                pb2.putField(typ, protoBufEntry.tpInt, Float.floatToIntBits(f));
                break;
            default:
                return;
        }
        pck3.clear();
        pb2.toPacket(pck3);
        pb2.clear();
        pb2.putField(servStreamingMdt.fnFields, protoBufEntry.tpBuf, pck3.getCopy());
        pb2.toPacket(pck2);
        pb2.clear();
    }

    private void doMetricNetConf(extMrkLng res, String nam, String val) {
        res.data.add(new extMrkLngEntry(null, nam, "", val));
    }

    private List<String> doSplitLine(String a) {
        cmds cm = new cmds("tele", a);
        List<String> cl = new ArrayList<String>();
        for (;;) {
            a = cm.word(";");
            if (a.length() < 1) {
                break;
            }
            cl.add(a);
        }
        return cl;
    }

    private void doMetricProm(List<String> lst, String nb, String labs, String val) {
        if (labs.length() > 0) {
            labs = "{" + labs.substring(1, labs.length()) + "}";
        }
        lst.add(nb + labs + " " + val);
    }

    private static String doReplaces(String a, tabGen<cfgSensorRep> reps) {
        for (int i = 0; i < reps.size(); i++) {
            cfgSensorRep rep = reps.get(i);
            a = a.replaceAll(rep.src, rep.trg);
        }
        return a;
    }

    private packHolder doLineKvGpb(String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (namC >= cls) {
            return null;
        }
        protoBuf pb = new protoBuf();
        a = cl.get(namC);
        if ((acol >= 0) && (acol < cls)) {
            a = asep;
            if (asep.equals("*")) {
                a = "";
            }
            a = a + cl.get(acol);
        }
        a = doReplaces(a, reps);
        packHolder pck1 = new packHolder(true, true);
        packHolder pck2 = new packHolder(true, true);
        packHolder pck3 = new packHolder(true, true);
        pb.putField(servStreamingMdt.fnName, protoBufEntry.tpBuf, keyN.getBytes());
        pb.putField(servStreamingMdt.fnString, protoBufEntry.tpBuf, a.getBytes());
        pb.toPacket(pck1);
        pb.clear();
        pb.putField(servStreamingMdt.fnName, protoBufEntry.tpBuf, servStreamingMdt.nmDat.getBytes());
        pb.toPacket(pck2);
        pb.clear();
        for (int o = 0; o < cols.size(); o++) {
            cfgSensorCol cc = cols.get(o);
            if (cl.size() <= cc.num) {
                continue;
            }
            a = doReplaces(cl.get(cc.num), cc.reps);
            if (cc.splS == null) {
                doMetricKvGpb(pck2, pck3, cc.typ, cc.nam, a);
                continue;
            }
            int i = a.indexOf(cc.splS);
            if (i < 0) {
                doMetricKvGpb(pck2, pck3, cc.typ, cc.nam, a);
                continue;
            }
            doMetricKvGpb(pck2, pck3, cc.typ, cc.nam + cc.splL, a.substring(0, i));
            doMetricKvGpb(pck2, pck3, cc.typ, cc.nam + cc.splR, a.substring(i + cc.splS.length(), a.length()));
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

    private String doLineCsv(String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (namC >= cls) {
            return null;
        }
        a = cl.get(namC);
        if ((acol >= 0) && (acol < cls)) {
            a = asep;
            if (asep.equals("*")) {
                a = "";
            }
            a = a + cl.get(acol);
        }
        String beg = prepend + doReplaces(a, reps);
        for (int o = 0; o < cols.size(); o++) {
            cfgSensorCol cc = cols.get(o);
            if (cl.size() <= cc.num) {
                continue;
            }
            a = doReplaces(cl.get(cc.num), cc.reps);
            if (cc.splS == null) {
                beg += ";" + a;
                continue;
            }
            int i = a.indexOf(cc.splS);
            if (i < 0) {
                beg += ";" + a;
                continue;
            }
            beg += ";" + a.substring(0, i);
            beg += ";" + a.substring(i + cc.splS.length(), a.length());
        }
        return beg;
    }

    private void doLineMem(tabGen<cfgSensorMem> tab, String beg, int col, String val) {
        cfgSensorMem ntry = new cfgSensorMem(beg, col);
        cfgSensorMem old = tab.add(ntry);
        if (old != null) {
            ntry = old;
        } else {
            ntry.hist = new history();
            ntry.cntr = new counter();
        }
        counter cntr = new counter();
        cntr.packRx = bits.str2long(val);
        cntr.packTx = bits.str2long(val);
        cntr = cntr.plus(ntry.cntr);
        ntry.hist.update(cntr);
        ntry.cntr = cntr;
    }

    private void doLineMem(tabGen<cfgSensorMem> tab, String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (namC >= cls) {
            return;
        }
        a = cl.get(namC);
        if ((acol >= 0) && (acol < cls)) {
            a = asep;
            if (asep.equals("*")) {
                a = "";
            }
            a = a + cl.get(acol);
        }
        String beg = doReplaces(a, reps);
        for (int o = 0; o < cols.size(); o++) {
            cfgSensorCol cc = cols.get(o);
            if (cl.size() <= cc.num) {
                continue;
            }
            a = doReplaces(cl.get(cc.num), cc.reps);
            if (cc.splS == null) {
                doLineMem(tab, beg, cc.num, a);
                continue;
            }
            int i = a.indexOf(cc.splS);
            if (i < 0) {
                doLineMem(tab, beg, cc.num, a);
                continue;
            }
            doLineMem(tab, beg, cc.num, a.substring(0, i));
            doLineMem(tab, beg, cc.num, a.substring(i + cc.splS.length(), a.length()));
        }
    }

    private void doLineNetConf(extMrkLng res, String beg, String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (namC >= cls) {
            return;
        }
        a = cl.get(namC);
        if ((acol >= 0) && (acol < cls)) {
            a = asep;
            if (asep.equals("*")) {
                a = "";
            }
            a = a + cl.get(acol);
        }
        a = doReplaces(a, reps);
        res.data.add(new extMrkLngEntry(null, beg + keyP + "/" + keyN, "", a));
        for (int o = 0; o < cols.size(); o++) {
            cfgSensorCol cc = cols.get(o);
            if (cl.size() <= cc.num) {
                continue;
            }
            a = doReplaces(cl.get(cc.num), cc.reps);
            if (cc.splS == null) {
                doMetricNetConf(res, beg + path + "/" + cc.nam, a);
                continue;
            }
            int i = a.indexOf(cc.splS);
            if (i < 0) {
                doMetricNetConf(res, beg + path + "/" + cc.nam, a);
                continue;
            }
            doMetricNetConf(res, beg + path + "/" + cc.nam + cc.splL, a.substring(0, i));
            doMetricNetConf(res, beg + path + "/" + cc.nam + cc.splR, a.substring(i + cc.splS.length(), a.length()));
        }
        int i = keyP.lastIndexOf("/");
        if (i < 0) {
            i = keyP.length();
        }
        res.data.add(new extMrkLngEntry(null, beg + keyP.substring(0, i), "", ""));
    }

    private void doLineProm(List<String> lst, List<String> smt, String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (namC >= cls) {
            return;
        }
        String na = prepend;
        String nc = cl.get(namC);
        String nd = "";
        if ((acol >= 0) && (acol < cls)) {
            a = asep;
            if (asep.equals("*")) {
                a = "";
            }
            nd = a + cl.get(acol);
        }
        na = doReplaces(na, reps);
        nc = doReplaces(nc, reps);
        nd = doReplaces(nd, reps);
        if (namL == null) {
            na += nc;
            na += nd;
        }
        for (int o = 0; o < cols.size(); o++) {
            cfgSensorCol cc = cols.get(o);
            if (cl.size() <= cc.num) {
                continue;
            }
            String nb = na;
            if (!cc.nam.equals("*")) {
                nb += cc.nam;
            }
            String labs = "";
            if (namS != null) {
                labs += "," + namS;
            }
            if (namL != null) {
                labs += "," + namL + "\"" + nc + "\"";
            }
            if (alab != null) {
                labs += "," + alab + "\"" + nd + "\"";
            }
            if (cc.lab != null) {
                labs += "," + cc.lab;
            }
            if (smt.indexOf(nb) < 0) {
                String h;
                if (cc.hlp == null) {
                    h = " column " + cc.num + " of " + command;
                } else {
                    h = " " + cc.hlp;
                }
                lst.add("# HELP " + nb + h);
                lst.add("# TYPE " + nb + " " + cc.sty);
                smt.add(nb);
            }
            a = doReplaces(cl.get(cc.num), cc.reps);
            if (cc.splS == null) {
                doMetricProm(lst, nb, labs, a);
                continue;
            }
            int i = a.indexOf(cc.splS);
            if (i < 0) {
                doMetricProm(lst, nb, labs, a);
                continue;
            }
            doMetricProm(lst, nb, labs + "," + cc.splL, a.substring(0, i));
            doMetricProm(lst, nb, labs + "," + cc.splR, a.substring(i + cc.splS.length(), a.length()));
        }
    }

    /**
     * generate report
     *
     * @return report, null on error
     */
    public packHolder getReportKvGpb() {
        last = bits.getTime();
        cnt++;
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
        pb.putField(servStreamingMdt.rpTime, protoBufEntry.tpInt, last);
        pb.putField(servStreamingMdt.rpNodeStr, protoBufEntry.tpBuf, cfgAll.hostName.getBytes());
        pb.putField(servStreamingMdt.rpSubsStr, protoBufEntry.tpBuf, name.getBytes());
        pb.putField(servStreamingMdt.rpEnc, protoBufEntry.tpBuf, (prefix + ":" + path).getBytes());
        pb.toPacket(pck);
        pb.clear();
        for (int i = 0; i < res.size(); i++) {
            packHolder ln = doLineKvGpb(res.get(i));
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
     * generate report
     *
     * @param rep report
     * @param beg beginning
     */
    public void getReportNetConf(extMrkLng rep, String beg) {
        last = bits.getTime();
        cnt++;
        List<String> res = getResult();
        for (int i = 0; i < skip; i++) {
            if (res.size() < 1) {
                break;
            }
            res.remove(0);
        }
        for (int i = 0; i < res.size(); i++) {
            doLineNetConf(rep, beg, res.get(i));
        }
        time = (int) (bits.getTime() - last);
    }

    /**
     * generate report
     *
     * @return report
     */
    public List<String> getReportProm() {
        last = bits.getTime();
        cnt++;
        List<String> lst = new ArrayList<String>();
        List<String> res = getResult();
        for (int i = 0; i < skip; i++) {
            if (res.size() < 1) {
                break;
            }
            res.remove(0);
        }
        List<String> smt = new ArrayList<String>();
        for (int p = 0; p < res.size(); p++) {
            doLineProm(lst, smt, res.get(p));
        }
        time = (int) (bits.getTime() - last);
        return lst;
    }

    /**
     * generate report
     *
     * @return report
     */
    public List<String> getReportCsv() {
        last = bits.getTime();
        cnt++;
        List<String> lst = new ArrayList<String>();
        List<String> res = getResult();
        for (int i = 0; i < skip; i++) {
            if (res.size() < 1) {
                break;
            }
            res.remove(0);
        }
        for (int p = 0; p < res.size(); p++) {
            String a = doLineCsv(res.get(p));
            if (a == null) {
                continue;
            }
            lst.add(last + ";" + a);
        }
        time = (int) (bits.getTime() - last);
        return lst;
    }

    /**
     * generate report
     *
     * @param tab table to update
     */
    public void getReportMem(tabGen<cfgSensorMem> tab) {
        if (tab == null) {
            return;
        }
        last = bits.getTime();
        cnt++;
        List<String> res = getResult();
        for (int i = 0; i < skip; i++) {
            if (res.size() < 1) {
                break;
            }
            res.remove(0);
        }
        for (int p = 0; p < res.size(); p++) {
            doLineMem(tab, res.get(p));
        }
        time = (int) (bits.getTime() - last);
    }

    /**
     * generate report
     *
     * @param tab table to update
     * @return result
     */
    public List<String> showReportMem(tabGen<cfgSensorMem> tab) {
        List<String> lst = new ArrayList<String>();
        if (tab == null) {
            return lst;
        }
        for (int i = 0; i < tab.size(); i++) {
            tab.get(i).dump(lst);
        }
        return lst;
    }

    /**
     * get yang
     *
     * @return result
     */
    public List<String> getYang() {
        List<String> res = new ArrayList<String>();
        res.add("module " + prefix + " {");
        res.add("  namespace \"" + verCore.homeUrl + "yang/" + prefix + "\";");
        res.add("  prefix \"" + prefix + "\";");
        cmds cp = new cmds("ya", path);
        cmds ck = new cmds("ya", keyP);
        String id = "  ";
        boolean key = false;
        for (;;) {
            if (cp.size() < 1) {
                break;
            }
            if (key) {
                res.add(id + "key \"" + keyN + "\";");
                res.add(id + "leaf " + keyN + " {");
                res.add(id + "  type string;");
                res.add(id + "}");
                key = false;
            }
            String a = cp.word("/");
            String s = ck.word("/");
            String m = "container ";
            if ((s.length() > 0) && (ck.size() < 1)) {
                m = "list ";
                key = true;
            }
            res.add(id + m + a + " {");
            id += "  ";
        }
        for (int i = 0; i < cols.size(); i++) {
            cfgSensorCol col = cols.get(i);
            if (col.splS == null) {
                res.add(id + "leaf " + col.nam + " {");
                res.add(id + "  type " + servStreamingMdt.type2string(col.typ) + ";");
                if (col.hlp != null) {
                    res.add(id + "  description \"" + col.hlp + "\";");
                }
                res.add(id + "}");
                continue;
            }
            res.add(id + "leaf " + col.nam + col.splL + " {");
            res.add(id + "  type " + servStreamingMdt.type2string(col.typ) + ";");
            if (col.hlp != null) {
                res.add(id + "  description \"" + col.hlp + "\";");
            }
            res.add(id + "}");
            res.add(id + "leaf " + col.nam + col.splR + " {");
            res.add(id + "  type " + servStreamingMdt.type2string(col.typ) + ";");
            if (col.hlp != null) {
                res.add(id + "  description \"" + col.hlp + "\";");
            }
            res.add(id + "}");
        }
        for (; id.length() > 0;) {
            id = id.substring(0, id.length() - 2);
            res.add(id + "}");
        }
        return res;
    }

    private byte[] compressReply(List<String> lst) {
        byte[] buf = bits.lst2str(lst, "\r").getBytes();
        Deflater cmp = new Deflater();
        cmp.setInput(buf);
        cmp.finish();
        int i = cmp.deflate(buf);
        byte[] res = new byte[i];
        bits.byteCopy(buf, 0, res, 0, res.length);
        return res;
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShow() {
        List<String> res = new ArrayList<String>();
        res.add("command=" + command);
        res.add("path=" + path);
        res.add("prefix=" + prefix);
        res.add("asked=" + cnt + " times");
        res.add("reply=" + time + " ms");
        res.add("output:");
        res.addAll(getResult());
        res.add("yang:");
        res.addAll(getYang());
        res.add("prometheus:");
        List<String> lst = getReportProm();
        res.addAll(lst);
        res.add("promwire:" + bits.byteDump(compressReply(lst), 0, -1));
        res.add("csv:");
        lst = getReportCsv();
        res.addAll(lst);
        res.add("csvwire:" + bits.byteDump(compressReply(lst), 0, -1));
        res.add("netconf:");
        extMrkLng xml = new extMrkLng();
        getReportNetConf(xml, "/");
        lst = xml.show();
        res.addAll(lst);
        res.add("xml:");
        lst = xml.toXMLlst();
        res.addAll(lst);
        res.add("xmlwire:" + bits.byteDump(compressReply(lst), 0, -1));
        res.add("kvgpb:" + getReportKvGpb().dump());
        res.add("memory:");
        res.addAll(showReportMem(locMem));
        return res;
    }

    /**
     * stop collection
     */
    public void stopWork() {
        locInt = -1;
    }

    private void doLocalCollect() {
        getReportMem(locMem);
        if (locFil == null) {
            return;
        }
        bits.buf2txt(false, getReportCsv(), locFil);
        if (locBak == null) {
            return;
        }
        boolean ned = false;
        long tim = bits.getTime();
        if (locTim > 0) {
            ned |= (tim - locStr) > locTim;
        }
        if (locByt > 0) {
            ned |= new File(locFil).length() > locByt;
        }
        if (!ned) {
            return;
        }
        userFlash.rename(locFil, locBak, true, false);
        locStr = tim;
    }

    public void run() {
        for (;;) {
            if (locInt < 1) {
                break;
            }
            bits.sleep(locInt);
            try {
                doLocalCollect();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }

    }

    class cfgSensorRep implements Comparator<cfgSensorRep> {

        public final String src;

        public String trg;

        public cfgSensorRep(String n) {
            src = n;
        }

        public int compare(cfgSensorRep o1, cfgSensorRep o2) {
            return o1.src.compareTo(o2.src);
        }

    }

    class cfgSensorCol implements Comparator<cfgSensorCol> {

        public final int num;

        public String nam;

        public String hlp;

        public String lab;

        public String splS;

        public String splL;

        public String splR;

        public int typ = servStreamingMdt.fnUint64;

        public String sty = "gauge";

        public tabGen<cfgSensorRep> reps = new tabGen<cfgSensorRep>();

        public cfgSensorCol(int n) {
            num = n;
        }

        public int compare(cfgSensorCol o1, cfgSensorCol o2) {
            if (o1.num < o2.num) {
                return -1;
            }
            if (o1.num > o2.num) {
                return +1;
            }
            return 0;
        }

    }
}

class cfgSensorMem implements Comparator<cfgSensorMem> {

    public final String key;

    public final int col;

    public counter cntr;

    public history hist;

    public cfgSensorMem(String k, int c) {
        key = k;
        col = c;
    }

    public int compare(cfgSensorMem o1, cfgSensorMem o2) {
        if (o1.col < o2.col) {
            return -1;
        }
        if (o1.col > o2.col) {
            return +1;
        }
        return o1.key.compareTo(o2.key);
    }

    public void dump(List<String> lst) {
        lst.add(key + " column " + col + ":");
        lst.addAll(hist.show(6));
    }

}
