package serv;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pack.packStreamingMdt;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.protoBuf;
import util.logger;
import util.protoBufEntry;

/**
 * streaming telemetry server
 *
 * @author matecsaba
 */
public class servStreamingMdt extends servGeneric implements prtServS {

    /**
     * default port
     */
    public final static int port = 9002;
    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server streamingmdt .*! port " + port,
        "server streamingmdt .*! protocol " + proto2string(protoAllStrm),};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * clients
     */
    protected tabGen<servTelemetryConn> conns = new tabGen<servTelemetryConn>();

    public tabGen<userFilter> srvDefFlt() {
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

    public void srvShRun(String beg, List<String> lst) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelping l) {
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        servTelemetryConn ntry = new servTelemetryConn(this, pipe, id.peerAddr.copyBytes());
        new Thread(ntry).start();
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

class servTelemetrySens implements Comparator<servTelemetrySens> {

    public String path;

    public String key;

    public long tim;

    public int num;

    public List<servTelemetryMeas> meas = new ArrayList<servTelemetryMeas>();

    public int compare(servTelemetrySens o1, servTelemetrySens o2) {
        int i = o1.path.compareTo(o2.path);
        if (i != 0) {
            return i;
        }
        return o1.key.compareTo(o2.key);
    }

}

class servTelemetryConn implements Comparator<servTelemetryConn>, Runnable {

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

    public int compare(servTelemetryConn o1, servTelemetryConn o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    private protoBufEntry getSubfld(protoBuf pb, int seq) {
        return pb.getField(15, seq); // telemetry field
    }

    private String getName(protoBuf pb) {
        protoBufEntry res = pb.getField(2, 0); // name
        if (res == null) {
            return null;
        }
        return res.getString();
    }

    private String getValue(protoBuf pb) {
        protoBufEntry res = pb.getField(4, 0); // bytes
        if (res != null) {
            return bits.byteDump(res.dat, 0, -1);
        }
        res = pb.getField(5, 0); // string
        if (res != null) {
            return res.getString();
        }
        res = pb.getField(6, 0); // boolean
        if (res != null) {
            return "" + (res.val != 0);
        }
        res = pb.getField(7, 0); // uint32
        if (res != null) {
            return "" + res.val;
        }
        res = pb.getField(8, 0); // uint64
        if (res != null) {
            return "" + res.val;
        }
        res = pb.getField(9, 0); // sint32
        if (res != null) {
            return "" + protoBuf.fromZigzag(res.val);
        }
        res = pb.getField(10, 0); // sint64
        if (res != null) {
            return "" + protoBuf.fromZigzag(res.val);
        }
        res = pb.getField(11, 0); // double
        if (res != null) {
            return "" + Double.longBitsToDouble(res.val);
        }
        res = pb.getField(12, 0); // float
        if (res != null) {
            return "" + Float.intBitsToFloat((int) res.val);
        }
        return null;
    }

    private String parseKeys(packHolder pck, protoBuf pb) {
        protoBufEntry res = getSubfld(pb, 0);
        pck.clear();
        res.getPacket(pck);
        protoBuf pb2 = new protoBuf();
        if (pb2.fromPacket(pck)) {
            return null;
        }
        return getName(pb2) + "=" + getValue(pb2);
    }

    private void parseContent(packHolder pck, protoBuf pb, List<servTelemetryMeas> meas) {
        for (int i = 0;; i++) {
            protoBufEntry res = getSubfld(pb, i);
            if (res == null) {
                break;
            }
            pck.clear();
            res.getPacket(pck);
            protoBuf pb2 = new protoBuf();
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

    private servTelemetrySens parseEntry(String path, packHolder pck, protoBufEntry res) {
        pck.clear();
        res.getPacket(pck);
        protoBuf pb = new protoBuf();
        if (pb.fromPacket(pck)) {
            return null;
        }
        String key = null;
        protoBuf cnt = null;
        for (int i = 0;; i++) {
            res = getSubfld(pb, i);
            if (res == null) {
                break;
            }
            pck.clear();
            res.getPacket(pck);
            protoBuf pb2 = new protoBuf();
            if (pb2.fromPacket(pck)) {
                continue;
            }
            String a = getName(pb2);
            if (a.equals("keys")) {
                key = parseKeys(pck, pb2);
                continue;
            }
            if (a.equals("content")) {
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

    public void run() {
        lower.conns.add(this);
        try {
            packHolder pck = new packHolder(true, true);
            packStreamingMdt pckPb = new packStreamingMdt(conn, pck);
            protoBuf pb = new protoBuf();
            protoBufEntry res;
            for (;;) {
                if (pckPb.recvPack()) {
                    break;
                }
                pb.clear();
                if (pb.fromPacket(pck)) {
                    break;
                }
                res = pb.getField(1, 0); // node id
                if (res == null) {
                    break;
                }
                node = res.getString();
                tim = bits.getTime();
                num++;
                res = pb.getField(6, 0); // encoding path
                if (res == null) {
                    break;
                }
                String path = res.getString();
                for (int i = 0;; i++) {
                    res = pb.getField(11, i); // telemetry field
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
