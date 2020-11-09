package user;

import cfg.cfgAll;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import serv.servStreamingMdt;
import tab.tabGen;
import util.bits;
import util.cmds;
import util.extMrkLng;
import util.extMrkLngEntry;
import util.protoBuf;
import util.protoBufEntry;
import util.verCore;

/**
 * telemetry exporter
 *
 * @author matecsaba
 */
public class userSensor implements Comparator<userSensor> {

    /**
     * name of telemetry export
     */
    public String name;

    /**
     * command
     */
    public String command;

    /**
     * prefix
     */
    public String prefix;

    /**
     * path
     */
    public String path;

    /**
     * skip
     */
    public int skip;

    /**
     * key column
     */
    public int keyC;

    /**
     * key name
     */
    public String keyN;

    /**
     * key path
     */
    public String keyP;

    /**
     * columns
     */
    public tabGen<userSensorCol> cols;

    /**
     * replacers
     */
    public tabGen<userSensorRep> reps;

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
     * create new telemetry export
     */
    public userSensor() {
        cols = new tabGen<userSensorCol>();
        reps = new tabGen<userSensorRep>();
        skip = 1;
    }

    public int compare(userSensor o1, userSensor o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    /**
     * do config line
     *
     * @param cmd line
     */
    public void doCfgLine(cmds cmd) {
        String s = cmd.word();
        if (s.equals("command")) {
            command = cmd.getRemaining();
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
        if (s.equals("skip")) {
            skip = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("key")) {
            keyC = bits.str2num(cmd.word());
            keyN = cmd.word();
            keyP = cmd.word();
            return;
        }
        if (s.equals("replace")) {
            userSensorRep rep = new userSensorRep(cmd.word());
            rep.trg = cmd.word();
            reps.add(rep);
            return;
        }
        if (!s.equals("column")) {
            cmd.badCmd();
            return;
        }
        userSensorCol col = new userSensorCol(bits.str2num(cmd.word()));
        userSensorCol oldc = cols.add(col);
        if (oldc != null) {
            col = oldc;
        }
        s = cmd.word();
        if (s.equals("name")) {
            col.nam = cmd.word();
            return;
        }
        if (s.equals("help")) {
            col.hlp = cmd.word();
            return;
        }
        if (s.equals("type")) {
            col.typ = servStreamingMdt.string2type(cmd.word());
            return;
        }
        if (s.equals("split")) {
            col.splS = cmd.word();
            col.splL = cmd.word();
            col.splR = cmd.word();
            return;
        }
        if (s.equals("replace")) {
            userSensorRep rep = new userSensorRep(cmd.word());
            rep.trg = cmd.word();
            col.reps.add(rep);
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
            return null;
        }
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, null);
        rdr.tabMod = userFormat.tableMode.raw;
        rdr.height = 0;
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
        res.data.add(new extMrkLngEntry(nam, "", val));
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

    private static String doReplaces(String a, tabGen<userSensorRep> reps) {
        for (int i = 0; i < reps.size(); i++) {
            userSensorRep rep = reps.get(i);
            a = a.replaceAll(rep.src, rep.trg);
        }
        return a;
    }

    private packHolder doLineKvGpb(String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (keyC >= cls) {
            return null;
        }
        protoBuf pb = new protoBuf();
        a = doReplaces(cl.get(keyC), reps);
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
            userSensorCol cc = cols.get(o);
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

    private void doLineNetConf(extMrkLng res, String beg, String a) {
        List<String> cl = doSplitLine(a);
        int cls = cl.size();
        if (keyC >= cls) {
            return;
        }
        a = doReplaces(cl.get(keyC), reps);
        res.data.add(new extMrkLngEntry(beg + keyP + "/" + keyN, "", a));
        for (int o = 0; o < cols.size(); o++) {
            userSensorCol cc = cols.get(o);
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
        res.data.add(new extMrkLngEntry(beg + keyP.substring(0, i), "", ""));
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
            userSensorCol col = cols.get(i);
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
        res.add("netconf:");
        extMrkLng x = new extMrkLng();
        getReportNetConf(x, "/");
        res.addAll(x.show());
        res.add("kvgpb:" + getReportKvGpb().dump());
        return res;
    }

}

class userSensorRep implements Comparator<userSensorRep> {

    public final String src;

    public String trg;

    public userSensorRep(String n) {
        src = n;
    }

    public int compare(userSensorRep o1, userSensorRep o2) {
        return o1.src.compareTo(o2.src);
    }

}

class userSensorCol implements Comparator<userSensorCol> {

    public final int num;

    public String nam;

    public String hlp;

    public String splS;

    public String splL;

    public String splR;

    public int typ = servStreamingMdt.fnUint64;

    public tabGen<userSensorRep> reps = new tabGen<userSensorRep>();

    public userSensorCol(int n) {
        num = n;
    }

    public int compare(userSensorCol o1, userSensorCol o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

}
