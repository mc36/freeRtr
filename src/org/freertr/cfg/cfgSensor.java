package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import org.freertr.enc.enc7bit;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servStreamingMdt;
import org.freertr.tab.tabGen;
import org.freertr.user.userExec;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.user.userRead;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.enc.encXml;
import org.freertr.enc.encXmlEntry;
import org.freertr.util.history;
import org.freertr.util.logFil;
import org.freertr.util.logger;
import org.freertr.enc.encPrtbuf;
import org.freertr.enc.encPrtbufEntry;
import org.freertr.user.userScreen;
import org.freertr.util.version;

/**
 * telemetry exporter
 *
 * @author matecsaba
 */
public class cfgSensor implements Runnable, Comparable<cfgSensor>, cfgGeneric {

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
     * collection delay
     */
    public int locDel;

    /**
     * collection memory
     */
    public tabGen<cfgSensorMem> locMem;

    /**
     * collection file
     */
    public logFil locFil;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "command", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("sensor .*", cmds.tabulator + "name 0", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "labels", null),
        new userFilter("sensor .*", cmds.tabulator + "addname -1 null", null),
        new userFilter("sensor .*", cmds.tabulator + "skip 1", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "excluded", null),
        new userFilter("sensor .*", cmds.tabulator + "column .* style gauge", null),
        new userFilter("sensor .*", cmds.tabulator + "column .* type uint64", null),
        new userFilter("sensor .*", cmds.tabulator + "column .* split null null null", null),
        new userFilter("sensor .*", cmds.tabulator + "column .* help null", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "local interval", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "local delay", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "local memory", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "local file", null),
        new userFilter("sensor .*", cmds.tabulator + cmds.negated + cmds.tabulator + "local backup", null),
        new userFilter("sensor .*", cmds.tabulator + "local max-byte 0", null),
        new userFilter("sensor .*", cmds.tabulator + "local max-pack 0", null),
        new userFilter("sensor .*", cmds.tabulator + "local max-time 0", null)
    };

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

    public int compareTo(cfgSensor o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "sensor";
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "rename", "rename this sensor");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "description", "specify description");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "description");
        l.add(null, false, 1, new int[]{2}, "command", "specify command to execute");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "command");
        l.add(null, false, 1, new int[]{2}, "prepend", "specify prepend");
        l.add(null, false, 2, new int[]{-1}, "<str>", "name");
        l.add(null, false, 1, new int[]{2}, "prefix", "specify prefix");
        l.add(null, false, 2, new int[]{-1}, "<str>", "name");
        l.add(null, false, 1, new int[]{2}, "path", "specify prefix");
        l.add(null, false, 2, new int[]{-1}, "<str>", "name");
        l.add(null, false, 1, new int[]{2}, "labels", "static labels");
        l.add(null, false, 2, new int[]{-1}, "<str>", "name");
        l.add(null, false, 1, new int[]{2}, "key", "key column number");
        l.add(null, false, 2, new int[]{3}, "<str>", "name");
        l.add(null, false, 3, new int[]{-1}, "<str>", "path");
        l.add(null, false, 1, new int[]{2}, "name", "name column number");
        l.add(null, false, 2, new int[]{3, -1}, "<num>", "column number");
        l.add(null, false, 3, new int[]{-1}, "<str>", "label");
        l.add(null, false, 1, new int[]{2}, "addname", "add name column number");
        l.add(null, false, 2, new int[]{3}, "<num>", "column number");
        l.add(null, false, 3, new int[]{4, -1}, "<str>", "separator, * means empty");
        l.add(null, false, 4, new int[]{-1}, "<str>", "label");
        l.add(null, false, 1, new int[]{2}, "skip", "rows to skip");
        l.add(null, false, 2, new int[]{-1}, "<num>", "lines to skip");
        l.add(null, false, 1, new int[]{2}, "replace", "define replaces in name");
        l.add(null, false, 2, new int[]{3}, "<str>", "string to replace");
        l.add(null, false, 3, new int[]{-1}, "<str>", "replacement string");
        l.add(null, false, 1, new int[]{2}, "column", "define column to export");
        l.add(null, false, 2, new int[]{3}, "<num>", "number");
        l.add(null, false, 3, new int[]{4, -1}, "name", "set name");
        l.add(null, false, 4, new int[]{5, -1}, "<str>", "name, * means empty");
        l.add(null, false, 5, new int[]{-1}, "<str>", "label");
        l.add(null, false, 3, new int[]{4}, "type", "set type");
        l.add(null, false, 4, new int[]{-1}, "bytes", "bytes");
        l.add(null, false, 4, new int[]{-1}, "string", "string");
        l.add(null, false, 4, new int[]{-1}, "bool", "boolean");
        l.add(null, false, 4, new int[]{-1}, "uint32", "unsigned 32bit integer");
        l.add(null, false, 4, new int[]{-1}, "uint64", "unsigned 64bit integer");
        l.add(null, false, 4, new int[]{-1}, "sint32", "signed 32bit integer");
        l.add(null, false, 4, new int[]{-1}, "sint64", "signed 64bit integer");
        l.add(null, false, 4, new int[]{-1}, "float", "32bit floating point number");
        l.add(null, false, 4, new int[]{-1}, "double", "64bit floating point number");
        l.add(null, false, 3, new int[]{4}, "style", "set style");
        l.add(null, false, 4, new int[]{-1}, "gauge", "gauge");
        l.add(null, false, 4, new int[]{-1}, "counter", "counter");
        l.add(null, false, 3, new int[]{4}, "help", "set help");
        l.add(null, false, 4, new int[]{4, -1}, "<str>", "help");
        l.add(null, false, 3, new int[]{4}, "replace", "define replaces in value");
        l.add(null, false, 4, new int[]{5}, "<str>", "string to replace");
        l.add(null, false, 5, new int[]{-1}, "<str>", "replacement string");
        l.add(null, false, 3, new int[]{4}, "split", "define split of value");
        l.add(null, false, 4, new int[]{5}, "<str>", "delimiter");
        l.add(null, false, 5, new int[]{6}, "<str>", "first label");
        l.add(null, false, 6, new int[]{-1}, "<str>", "second label");
        l.add(null, false, 1, new int[]{2}, "local", "local collection options");
        l.add(null, false, 2, new int[]{3}, "interval", "collection interval");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "delay", "collection delay");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{-1}, "memory", "collect to memory");
        l.add(null, false, 2, new int[]{3}, "file", "collect to file");
        l.add(null, false, 3, new int[]{-1}, "<str>", "file name");
        l.add(null, false, 2, new int[]{3}, "max-pack", "maximum packets");
        l.add(null, false, 3, new int[]{-1}, "<num>", "packets between backups");
        l.add(null, false, 2, new int[]{3}, "max-byte", "maximum bytes");
        l.add(null, false, 3, new int[]{-1}, "<num>", "bytes");
        l.add(null, false, 2, new int[]{3}, "max-time", "maximum time");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "backup", "backup to file");
        l.add(null, false, 3, new int[]{-1}, "<str>", "file name");
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
        cmds.cfgLine(l, locDel < 1, cmds.tabulator, "local delay", "" + locDel);
        cmds.cfgLine(l, locMem == null, cmds.tabulator, "local memory", "");
        if (locFil == null) {
            l.add(cmds.tabulator + "no local file");
        } else {
            l.add(cmds.tabulator + "local file " + locFil.name());
            cmds.cfgLine(l, locFil.rotateN() == null, cmds.tabulator, "local backup", locFil.rotateN());
            l.add(cmds.tabulator + "local max-time " + locFil.rotateT());
            l.add(cmds.tabulator + "local max-pack " + locFil.rotateL());
            l.add(cmds.tabulator + "local max-byte " + locFil.rotateS());
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
        boolean negated = s.equals(cmds.negated);
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
            if (s.equals("delay")) {
                if (negated) {
                    locDel = 0;
                    return;
                }
                locDel = bits.str2num(cmd.word());
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
                try {
                    locFil.close();
                } catch (Exception e) {
                }
                if (negated) {
                    locFil = null;
                    return;
                }
                locFil = new logFil(cmd.getRemaining());
                locFil.open(false);
                return;
            }
            if (locFil == null) {
                cmd.error("not enabled");
                return;
            }
            if (s.equals("backup")) {
                if (negated) {
                    locFil.rotate(null, 0, 0, 0);
                    return;
                }
                locFil.rotate(cmd.getRemaining(), locFil.rotateS(), locFil.rotateT(), locFil.rotateL());
                return;
            }
            if (s.equals("max-pack")) {
                if (negated) {
                    locFil.rotate(locFil.rotateN(), locFil.rotateS(), locFil.rotateT(), 0);
                    return;
                }
                locFil.rotate(locFil.rotateN(), locFil.rotateS(), locFil.rotateT(), bits.str2num(cmd.word()));
                return;
            }
            if (s.equals("max-byte")) {
                if (negated) {
                    locFil.rotate(locFil.rotateN(), 0, locFil.rotateT(), locFil.rotateL());
                    return;
                }
                locFil.rotate(locFil.rotateN(), bits.str2num(cmd.word()), locFil.rotateT(), locFil.rotateL());
                return;
            }
            if (s.equals("max-time")) {
                if (negated) {
                    locFil.rotate(locFil.rotateN(), locFil.rotateS(), 0, locFil.rotateL());
                    return;
                }
                locFil.rotate(locFil.rotateN(), locFil.rotateS(), bits.str2num(cmd.word()), locFil.rotateL());
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
        userRead rdr = new userRead(pip, null);
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
        encPrtbuf pb2 = new encPrtbuf();
        pb2.putField(servStreamingMdt.fnName, encPrtbufEntry.tpBuf, nam.getBytes());
        switch (typ) {
            case servStreamingMdt.fnByte:
                pb2.putField(typ, encPrtbufEntry.tpBuf, val.getBytes());
                break;
            case servStreamingMdt.fnString:
                pb2.putField(typ, encPrtbufEntry.tpBuf, val.getBytes());
                break;
            case servStreamingMdt.fnBool:
                pb2.putField(typ, encPrtbufEntry.tpInt, bits.str2num(val));
                break;
            case servStreamingMdt.fnUint32:
            case servStreamingMdt.fnUint64:
                pb2.putField(typ, encPrtbufEntry.tpInt, bits.str2long(val));
                break;
            case servStreamingMdt.fnSint32:
            case servStreamingMdt.fnSint64:
                pb2.putField(typ, encPrtbufEntry.tpInt, encPrtbuf.toZigzag(bits.str2long(val)));
                break;
            case servStreamingMdt.fnDouble:
                double d;
                try {
                    d = Double.parseDouble(val);
                } catch (Exception e) {
                    return;
                }
                pb2.putField(typ, encPrtbufEntry.tpInt, Double.doubleToLongBits(d));
                break;
            case servStreamingMdt.fnFloat:
                float f;
                try {
                    f = Float.parseFloat(val);
                } catch (Exception e) {
                    return;
                }
                pb2.putField(typ, encPrtbufEntry.tpInt, Float.floatToIntBits(f));
                break;
            default:
                return;
        }
        pck3.clear();
        pb2.toPacket(pck3);
        pb2.clear();
        pb2.putField(servStreamingMdt.fnFields, encPrtbufEntry.tpBuf, pck3.getCopy());
        pb2.toPacket(pck2);
        pb2.clear();
    }

    private void doMetricNetConf(encXml res, String nam, String val) {
        res.data.add(new encXmlEntry(null, nam, "", val));
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
        encPrtbuf pb = new encPrtbuf();
        a = cl.get(namC);
        if ((acol >= 0) && (acol < cls)) {
            if (!asep.equals("*")) {
                a += asep;
            }
            a += cl.get(acol);
        }
        a = doReplaces(a, reps);
        packHolder pck1 = new packHolder(true, true);
        packHolder pck2 = new packHolder(true, true);
        packHolder pck3 = new packHolder(true, true);
        pb.putField(servStreamingMdt.fnName, encPrtbufEntry.tpBuf, keyN.getBytes());
        pb.putField(servStreamingMdt.fnString, encPrtbufEntry.tpBuf, a.getBytes());
        pb.toPacket(pck1);
        pb.clear();
        pb.putField(servStreamingMdt.fnName, encPrtbufEntry.tpBuf, servStreamingMdt.nmDat.getBytes());
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
        encPrtbuf pb2 = new encPrtbuf();
        pb2.putField(servStreamingMdt.fnName, encPrtbufEntry.tpBuf, servStreamingMdt.nmKey.getBytes());
        pb2.putField(servStreamingMdt.fnFields, encPrtbufEntry.tpBuf, pck1.getCopy());
        pck3.clear();
        pb2.toPacket(pck3);
        pb2.clear();
        pb.putField(servStreamingMdt.fnFields, encPrtbufEntry.tpBuf, pck3.getCopy());
        pb.putField(servStreamingMdt.fnFields, encPrtbufEntry.tpBuf, pck2.getCopy());
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
            a += ";" + cl.get(acol);
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

    private void doLineMem(String beg, int col, long mul, String val) {
        cfgSensorMem ntry = new cfgSensorMem(beg, col);
        cfgSensorMem old = locMem.add(ntry);
        if (old != null) {
            ntry = old;
        } else {
            ntry.hist = new history();
        }
        counter cntr = new counter();
        cntr.packRx = bits.str2long(val) * mul;
        cntr.packTx = cntr.packRx;
        ntry.hist.update(cntr, false);
    }

    private void doLineMem(String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (namC >= cls) {
            return;
        }
        a = cl.get(namC);
        if ((acol >= 0) && (acol < cls)) {
            if (!asep.equals("*")) {
                a += asep;
            }
            a += cl.get(acol);
        }
        String beg = doReplaces(a, reps);
        long mul = locInt / 1000;
        if (mul < 1) {
            mul = 1;
        }
        for (int o = 0; o < cols.size(); o++) {
            cfgSensorCol cc = cols.get(o);
            if (cl.size() <= cc.num) {
                continue;
            }
            a = doReplaces(cl.get(cc.num), cc.reps);
            if (cc.splS == null) {
                doLineMem(beg, cc.num, mul, a);
                continue;
            }
            int i = a.indexOf(cc.splS);
            if (i < 0) {
                doLineMem(beg, cc.num, mul, a);
                continue;
            }
            doLineMem(beg, cc.num, mul, a.substring(0, i));
            doLineMem(beg, cc.num, mul, a.substring(i + cc.splS.length(), a.length()));
        }
    }

    private void doLineNetConf(encXml res, String beg, String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (namC >= cls) {
            return;
        }
        a = cl.get(namC);
        if ((acol >= 0) && (acol < cls)) {
            if (!asep.equals("*")) {
                a += asep;
            }
            a += cl.get(acol);
        }
        a = doReplaces(a, reps);
        res.data.add(new encXmlEntry(null, beg + keyP + "/" + keyN, "", a));
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
        res.data.add(new encXmlEntry(null, beg + keyP.substring(0, i), "", ""));
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
        encPrtbuf pb = new encPrtbuf();
        pb.putField(servStreamingMdt.rpStart, encPrtbufEntry.tpInt, last);
        pb.putField(servStreamingMdt.rpTime, encPrtbufEntry.tpInt, last);
        pb.putField(servStreamingMdt.rpNodeStr, encPrtbufEntry.tpBuf, cfgAll.hostName.getBytes());
        pb.putField(servStreamingMdt.rpSubsStr, encPrtbufEntry.tpBuf, name.getBytes());
        pb.putField(servStreamingMdt.rpEnc, encPrtbufEntry.tpBuf, (prefix + ":" + path).getBytes());
        pb.toPacket(pck);
        pb.clear();
        for (int i = 0; i < res.size(); i++) {
            packHolder ln = doLineKvGpb(res.get(i));
            if (ln == null) {
                continue;
            }
            pb.putField(servStreamingMdt.rpKvgpb, encPrtbufEntry.tpBuf, ln.getCopy());
            pb.toPacket(pck);
            pb.clear();
        }
        long tim = bits.getTime();
        pb.putField(servStreamingMdt.rpStop, encPrtbufEntry.tpInt, tim);
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
    public void getReportNetConf(encXml rep, String beg) {
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
     * get yang
     *
     * @return result
     */
    public List<String> getYang() {
        List<String> res = new ArrayList<String>();
        res.add("module " + prefix + " {");
        res.add("  namespace \"" + version.homeUrl + "yang/" + prefix + "\";");
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
    public userFormat getShowDetail() {
        userFormat res = new userFormat("|", "category|value");
        res.add("name|" + name);
        res.add("command|" + command);
        res.add("path|" + path);
        res.add("prefix|" + prefix);
        res.add("reply|" + time + " ms");
        res.add("asked|" + cnt + " times");
        res.add("last|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3));
        res.add("ago|" + bits.timePast(last));
        return res;
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShowNetconf() {
        encXml xml = new encXml();
        getReportNetConf(xml, "/");
        return xml.show();
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShowXml() {
        encXml xml = new encXml();
        getReportNetConf(xml, "/");
        return xml.toXMLlst();
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShowKvgpb() {
        byte[] buf = getReportKvGpb().getCopy();
        List<String> res = new ArrayList<String>();
        enc7bit.buf2hex(res, buf, 0, "");
        return res;
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShowMemory() {
        List<String> lst = new ArrayList<String>();
        if (locMem == null) {
            return lst;
        }
        for (int i = 0; i < locMem.size(); i++) {
            locMem.get(i).dump(lst);
        }
        return lst;
    }

    /**
     * get show
     *
     * @param col column
     * @param scr screen
     */
    public void getShowGraph(int col, userScreen scr) {
        if (locFil == null) {
            return;
        }
        getShowGraph(locFil.name(), col, scr);
    }

    /**
     * get show
     *
     * @param col column
     * @param scr screen
     */
    public void getShowOldGraph(int col, userScreen scr) {
        if (locFil == null) {
            return;
        }
        getShowGraph(locFil.rotateN(), col, scr);
    }

    private void getShowGraph(String a, int col, userScreen scr) {
        if (a == null) {
            return;
        }
        List<String> res = bits.txt2buf(a);
        if (res == null) {
            return;
        }
        if (res.size() < scr.sizX) {
            return;
        }
        int sizX = scr.sizX - 6;
        int sizY = scr.sizY - 3;
        cmds cmd = new cmds("ts", res.get(0));
        long beg = bits.str2long(cmd.word(";"));
        cmd = new cmds("ts", res.get(res.size() - 1));
        long end = bits.str2long(cmd.word(";"));
        end -= beg;
        end /= sizX;
        long[] avg = new long[sizX];
        long[] min = new long[sizX];
        long[] max = new long[sizX];
        int pos = 0;
        for (int i = 0; i < avg.length; i++) {
            long sum = 0;
            long vMin = Long.MAX_VALUE;
            long vMax = Long.MIN_VALUE;
            int ok = 0;
            for (;;) {
                cmd = new cmds("ts", res.get(pos));
                long cur = bits.str2long(cmd.word(";"));
                cur -= beg;
                cur /= end;
                if (cur > i) {
                    break;
                }
                pos++;
                for (int o = 0; o < col; o++) {
                    cmd.word(";");
                }
                cur = bits.str2long(cmd.word(";"));
                if (cur < vMin) {
                    vMin = cur;
                }
                if (cur > vMax) {
                    vMax = cur;
                }
                sum += cur;
                ok++;
            }
            if (ok < 1) {
                return;
            }
            avg[i] = sum / ok;
            min[i] = vMin;
            max[i] = vMax;
        }
        long cMin = min[0];
        long cMax = max[0];
        for (int i = 1; i < avg.length; i++) {
            if (min[i] < cMin) {
                cMin = min[i];
            }
            if (max[i] > cMax) {
                cMax = max[i];
            }
        }
        cMax -= cMin;
        cMax /= sizY;
        if (cMax < 1) {
            cMax = 1;
        }
        for (int i = 1; i < avg.length; i++) {
            getShowGraph(scr, sizY, i, min[i], cMin, cMax, "-");
            getShowGraph(scr, sizY, i, max[i], cMin, cMax, "+");
            getShowGraph(scr, sizY, i, avg[i], cMin, cMax, "*");
        }
        for (int i = 0; i < sizY; i++) {
            scr.putStr(sizX, i, userScreen.colBlack, userScreen.colWhite, false, "|" + bits.toUser(cMin + (cMax * (sizY - i))));
        }
        scr.putStr(0, sizY, userScreen.colBlack, userScreen.colWhite, false, bits.padEnd("", sizX, "-") + "/");
        a = "";
        for (int i = 0; i <= sizX; i += 15) {
            scr.putStr(i, sizY + 1, userScreen.colBlack, userScreen.colWhite, false, bits.time2str(cfgAll.timeZoneName, beg + (i * end), 1));
        }
    }

    private void getShowGraph(userScreen scr, int max, int i, long v, long cMin, long cMax, String ch) {
        v -= cMin;
        v /= cMax;
        int ln = max - (int) v;
        if (ln < 0) {
            ln = 0;
        }
        if (ln >= max) {
            ln = max - 1;
        }
        scr.putStr(i, ln, userScreen.colBlack, userScreen.colWhite, false, ch);
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShowHistory() {
        if (locFil == null) {
            return null;
        }
        return bits.txt2buf(locFil.name());
    }

    /**
     * stop collection
     */
    public void stopWork() {
        locInt = -1;
    }

    private void doLocalCollect() {
        if (locMem != null) {
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
                doLineMem(res.get(p));
            }
            time = (int) (bits.getTime() - last);
        }
        if (locFil == null) {
            return;
        }
        List<String> res = getReportCsv();
        for (int i = 0; i < res.size(); i++) {
            locFil.add(res.get(i));
        }
    }

    public void run() {
        if (locDel > 0) {
            bits.sleep(locDel);
        }
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

}

class cfgSensorRep implements Comparable<cfgSensorRep> {

    public final String src;

    public String trg;

    public cfgSensorRep(String n) {
        src = n;
    }

    public int compareTo(cfgSensorRep o) {
        return src.compareTo(o.src);
    }

}

class cfgSensorCol implements Comparable<cfgSensorCol> {

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

    public int compareTo(cfgSensorCol o) {
        if (num < o.num) {
            return -1;
        }
        if (num > o.num) {
            return +1;
        }
        return 0;
    }

}

class cfgSensorMem implements Comparable<cfgSensorMem> {

    public final String key;

    public final int col;

    public history hist;

    public cfgSensorMem(String k, int c) {
        key = k;
        col = c;
    }

    public int compareTo(cfgSensorMem o) {
        if (col < o.col) {
            return -1;
        }
        if (col > o.col) {
            return +1;
        }
        return key.compareTo(o.key);
    }

    public void dump(List<String> lst) {
        lst.add(key + " column " + col + ":");
        lst.addAll(hist.show(6));
    }

}
