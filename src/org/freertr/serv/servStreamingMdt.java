package org.freertr.serv;

import java.util.ArrayList;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.pack.packStreamingMdt;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.enc.encPrtbuf;
import org.freertr.enc.encPrtbufEntry;

/**
 * streaming telemetry server
 *
 * @author matecsaba
 */
public class servStreamingMdt extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servStreamingMdt() {
    }

    /**
     * default port
     */
    public final static int port = 9002;

    /**
     * report node string - string
     */
    public final static int rpNodeStr = 1;

    /**
     * report node uuid - bytes
     */
    public final static int rpNodeUuid = 2;

    /**
     * report subscription string - string
     */
    public final static int rpSubsStr = 3;

    /**
     * report subscription id - uint32
     */
    public final static int rpSubsId = 4;

    /**
     * report sensor path - string
     */
    public final static int rpSens = 5;

    /**
     * report encoding path - string
     */
    public final static int rpEnc = 6;

    /**
     * report model version - string
     */
    public final static int rpModl = 7;

    /**
     * report collection id - uint64
     */
    public final static int rpColl = 8;

    /**
     * report collection start - uint64
     */
    public final static int rpStart = 9;

    /**
     * report message timestamp - uint64
     */
    public final static int rpTime = 10;

    /**
     * report kv gpb - repeated entry
     */
    public final static int rpKvgpb = 11;

    /**
     * report gpb - gpb table
     */
    public final static int rpGpb = 12;

    /**
     * report collection stop - uint64
     */
    public final static int rpStop = 13;

    /**
     * field timestamp
     */
    public final static int fnTime = 1;

    /**
     * field name
     */
    public final static int fnName = 2;

    /**
     * field bytes
     */
    public final static int fnByte = 4;

    /**
     * field string
     */
    public final static int fnString = 5;

    /**
     * field boolean
     */
    public final static int fnBool = 6;

    /**
     * field uint32
     */
    public final static int fnUint32 = 7;

    /**
     * field uint64
     */
    public final static int fnUint64 = 8;

    /**
     * field sint32
     */
    public final static int fnSint32 = 9;

    /**
     * field sint64
     */
    public final static int fnSint64 = 10;

    /**
     * field double
     */
    public final static int fnDouble = 11;

    /**
     * field float
     */
    public final static int fnFloat = 12;

    /**
     * field subfields
     */
    public final static int fnFields = 15;

    /**
     * name of key
     */
    public final static String nmKey = "keys";

    /**
     * name of data
     */
    public final static String nmDat = "content";

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case fnByte:
                return "bytes";
            case fnString:
                return "string";
            case fnBool:
                return "boolean";
            case fnUint32:
                return "uint32";
            case fnUint64:
                return "uint64";
            case fnSint32:
                return "sint32";
            case fnSint64:
                return "sint64";
            case fnDouble:
                return "double";
            case fnFloat:
                return "float";
            default:
                return "unknown";
        }
    }

    /**
     * convert string to type
     *
     * @param a string
     * @return type, -1 on error
     */
    public static int string2type(String a) {
        if (a.equals("bytes")) {
            return fnByte;
        }
        if (a.equals("string")) {
            return fnString;
        }
        if (a.equals("boolean")) {
            return fnBool;
        }
        if (a.equals("uint32")) {
            return fnUint32;
        }
        if (a.equals("uint64")) {
            return fnUint64;
        }
        if (a.equals("sint32")) {
            return fnSint32;
        }
        if (a.equals("sint64")) {
            return fnSint64;
        }
        if (a.equals("double")) {
            return fnDouble;
        }
        if (a.equals("float")) {
            return fnFloat;
        }
        return -1;
    }

    /**
     * clients
     */
    protected tabGen<servTelemetryConn> conns = new tabGen<servTelemetryConn>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server streamingmdt .*", cmds.tabulator + "port " + port, null),
        new userFilter("server streamingmdt .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "streamingmdt";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(65536, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelp l) {
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        servTelemetryConn ntry = new servTelemetryConn(this, pipe, id.peerAddr.copyBytes());
        ntry.start();
        return false;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "peer|node|last|upd|sens");
        for (int i = 0; i < conns.size(); i++) {
            servTelemetryConn ntry = conns.get(i);
            res.add(ntry.peer + "|" + ntry.node + "|" + bits.timePast(ntry.tim) + "|" + ntry.num + "|" + ntry.sens.size());
        }
        return res;
    }

    /**
     * get show
     *
     * @param per peer ip
     * @return result
     */
    public userFormat getShow(addrIP per) {
        userFormat res = new userFormat("|", "path|key|last|upd");
        servTelemetryConn ntry = new servTelemetryConn(this, null, per);
        ntry = conns.find(ntry);
        if (ntry == null) {
            return res;
        }
        for (int i = 0; i < ntry.sens.size(); i++) {
            servTelemetrySens sens = ntry.sens.get(i);
            res.add(sens.path + "|" + sens.key + "|" + bits.timePast(sens.tim) + "|" + sens.num);
        }
        return res;
    }

    /**
     * get show
     *
     * @param per peer ip
     * @param pat path
     * @param key key
     * @return result
     */
    public userFormat getShow(addrIP per, String pat, String key) {
        userFormat res = new userFormat("|", "name|value");
        servTelemetryConn ntry = new servTelemetryConn(this, null, per);
        ntry = conns.find(ntry);
        if (ntry == null) {
            return res;
        }
        servTelemetrySens sens = new servTelemetrySens();
        sens.path = pat;
        sens.key = key;
        sens = ntry.sens.find(sens);
        if (sens == null) {
            return res;
        }
        for (int i = 0; i < sens.meas.size(); i++) {
            servTelemetryMeas mea = sens.meas.get(i);
            res.add(mea.nam + "|" + mea.val);
        }
        return res;
    }

}

class servTelemetryMeas {

    public String nam;

    public String val;

}

class servTelemetrySens implements Comparable<servTelemetrySens> {

    public String path;

    public String key;

    public long tim;

    public int num;

    public List<servTelemetryMeas> meas = new ArrayList<servTelemetryMeas>();

    public int compareTo(servTelemetrySens o) {
        int i = path.compareTo(o.path);
        if (i != 0) {
            return i;
        }
        return key.compareTo(o.key);
    }

}

class servTelemetryConn implements Comparable<servTelemetryConn>, Runnable {

    public addrIP peer;

    public pipeSide conn;

    public servStreamingMdt lower;

    public String node;

    public tabGen<servTelemetrySens> sens = new tabGen<servTelemetrySens>();

    public long tim;

    public int num;

    public servTelemetryConn(servStreamingMdt parent, pipeSide pipe, addrIP addr) {
        lower = parent;
        conn = pipe;
        peer = addr;
    }

    public int compareTo(servTelemetryConn o) {
        return peer.compareTo(o.peer);
    }

    private encPrtbufEntry getSubfld(encPrtbuf pb, int seq) {
        return pb.getField(servStreamingMdt.fnFields, seq); // telemetry field
    }

    private String getName(encPrtbuf pb) {
        encPrtbufEntry res = pb.getField(servStreamingMdt.fnName, 0); // name
        if (res == null) {
            return null;
        }
        return res.getString();
    }

    private String getValue(encPrtbuf pb) {
        encPrtbufEntry res = pb.getField(servStreamingMdt.fnByte, 0); // bytes
        if (res != null) {
            return bits.byteDump(res.dat, 0, -1);
        }
        res = pb.getField(servStreamingMdt.fnString, 0); // string
        if (res != null) {
            return res.getString();
        }
        res = pb.getField(servStreamingMdt.fnBool, 0); // boolean
        if (res != null) {
            return "" + (res.val != 0);
        }
        res = pb.getField(servStreamingMdt.fnUint32, 0); // uint32
        if (res != null) {
            return "" + res.val;
        }
        res = pb.getField(servStreamingMdt.fnUint64, 0); // uint64
        if (res != null) {
            return "" + res.val;
        }
        res = pb.getField(servStreamingMdt.fnSint32, 0); // sint32
        if (res != null) {
            return "" + encPrtbuf.fromZigzag(res.val);
        }
        res = pb.getField(servStreamingMdt.fnSint64, 0); // sint64
        if (res != null) {
            return "" + encPrtbuf.fromZigzag(res.val);
        }
        res = pb.getField(servStreamingMdt.fnDouble, 0); // double
        if (res != null) {
            return "" + Double.longBitsToDouble(res.val);
        }
        res = pb.getField(servStreamingMdt.fnFloat, 0); // float
        if (res != null) {
            return "" + Float.intBitsToFloat((int) res.val);
        }
        return null;
    }

    private String parseKeys(packHolder pck, encPrtbuf pb) {
        encPrtbufEntry res = getSubfld(pb, 0);
        pck.clear();
        res.getPacket(pck);
        encPrtbuf pb2 = new encPrtbuf();
        if (pb2.fromPacket(pck)) {
            return null;
        }
        return getName(pb2) + "=" + getValue(pb2);
    }

    private void parseContent(packHolder pck, encPrtbuf pb, List<servTelemetryMeas> meas) {
        for (int i = 0;; i++) {
            encPrtbufEntry res = getSubfld(pb, i);
            if (res == null) {
                break;
            }
            pck.clear();
            res.getPacket(pck);
            encPrtbuf pb2 = new encPrtbuf();
            if (pb2.fromPacket(pck)) {
                continue;
            }
            String nam = getName(pb2);
            if (nam == null) {
                continue;
            }
            String val = getValue(pb2);
            if (val == null) {
                continue;
            }
            servTelemetryMeas mea = new servTelemetryMeas();
            mea.nam = nam;
            mea.val = val;
            meas.add(mea);
        }
    }

    private servTelemetrySens parseEntry(String path, packHolder pck, encPrtbufEntry res) {
        pck.clear();
        res.getPacket(pck);
        encPrtbuf pb = new encPrtbuf();
        if (pb.fromPacket(pck)) {
            return null;
        }
        String key = null;
        encPrtbuf cnt = null;
        for (int i = 0;; i++) {
            res = getSubfld(pb, i);
            if (res == null) {
                break;
            }
            pck.clear();
            res.getPacket(pck);
            encPrtbuf pb2 = new encPrtbuf();
            if (pb2.fromPacket(pck)) {
                continue;
            }
            String a = getName(pb2);
            if (a == null) {
                continue;
            }
            if (a.equals(servStreamingMdt.nmKey)) {
                key = parseKeys(pck, pb2);
                continue;
            }
            if (a.equals(servStreamingMdt.nmDat)) {
                cnt = pb2;
                continue;
            }
        }
        if (key == null) {
            return null;
        }
        if (cnt == null) {
            return null;
        }
        servTelemetrySens sen = new servTelemetrySens();
        sen.path = path;
        sen.key = key;
        parseContent(pck, cnt, sen.meas);
        if (sen.meas.size() < 1) {
            return null;
        }
        return sen;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        lower.conns.add(this);
        try {
            packHolder pck = new packHolder(true, true);
            packStreamingMdt pckPb = new packStreamingMdt(conn, pck);
            encPrtbuf pb = new encPrtbuf();
            encPrtbufEntry res;
            for (;;) {
                if (pckPb.recvPack()) {
                    break;
                }
                pb.clear();
                if (pb.fromPacket(pck)) {
                    break;
                }
                res = pb.getField(servStreamingMdt.rpNodeStr, 0); // node id
                if (res == null) {
                    break;
                }
                node = res.getString();
                tim = bits.getTime();
                num++;
                res = pb.getField(servStreamingMdt.rpEnc, 0); // encoding path
                if (res == null) {
                    break;
                }
                String path = res.getString();
                for (int i = 0;; i++) {
                    res = pb.getField(servStreamingMdt.rpKvgpb, i); // telemetry field
                    if (res == null) {
                        break;
                    }
                    servTelemetrySens sen = parseEntry(path, pck, res);
                    if (sen == null) {
                        continue;
                    }
                    sen.tim = tim;
                    servTelemetrySens old = sens.put(sen);
                    if (old == null) {
                        continue;
                    }
                    sen.num = old.num + 1;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();
        lower.conns.del(this);
    }

}
