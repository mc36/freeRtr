package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import java.util.Comparator;
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import util.bits;
import util.cmds;
import util.shrtPthFrst;

/**
 * lsrp data
 *
 * @author matecsaba
 */
public class rtrLsrpData implements Comparator<rtrLsrpData> {

    /**
     * full dump
     */
    public static final int dmpFull = -1;

    /**
     * compare dump
     */
    public static final int dmpComp = dmpFull - 0x800 - 0x100 - 0x80 - 0x8 - 0x4;

    /**
     * router id
     */
    public addrIPv4 rtrId;

    /**
     * topology summary
     */
    public int topoSum;

    /**
     * router name
     */
    public String hostname;

    /**
     * software
     */
    public String software;

    /**
     * hardware
     */
    public String hardware;

    /**
     * middleware
     */
    public String middleware;

    /**
     * kernel
     */
    public String kernel;

    /**
     * segment routing index
     */
    public int segrouIdx;

    /**
     * segment routing maximum
     */
    public int segrouMax;

    /**
     * segment routing base
     */
    public int segrouBeg;

    /**
     * bier index
     */
    public int bierIdx;

    /**
     * bier length
     */
    public int bierLen;

    /**
     * bier maximum
     */
    public int bierMax;

    /**
     * bier base
     */
    public int bierBeg;

    /**
     * sequence number
     */
    public int sequence;

    /**
     * time
     */
    public long time;

    /**
     * uptime
     */
    public long uptime;

    /**
     * number of changes
     */
    public int changesNum;

    /**
     * last changed
     */
    public long changesTim;

    /**
     * advertised routes
     */
    public tabRoute<addrIP> network;

    /**
     * advertised addresses
     */
    public tabGen<addrIP> address;

    /**
     * advertised neighbors
     */
    public tabGen<rtrLsrpDataNeigh> neighbor;

    public int compare(rtrLsrpData o1, rtrLsrpData o2) {
        return o1.rtrId.compare(o1.rtrId, o2.rtrId);
    }

    /**
     * clone this data
     *
     * @return copied data object
     */
    public rtrLsrpData copyHead() {
        rtrLsrpData n = new rtrLsrpData();
        n.rtrId = rtrId.copyBytes();
        n.sequence = sequence;
        n.uptime = uptime;
        return n;
    }

    /**
     * test if differs from other
     *
     * @param other data to compare
     * @return true if differs, false if not
     */
    public boolean differs(rtrLsrpData other) {
        if (other == null) {
            return true;
        }
        return (other.sequence != sequence) || (other.uptime != uptime);
    }

    /**
     * test if better from other
     *
     * @param other data to compare
     * @return true if better, false if not
     */
    public boolean better(rtrLsrpData other) {
        if (other == null) {
            return true;
        }
        if (other.sequence < sequence) {
            return true;
        }
        if (other.sequence > sequence) {
            return false;
        }
        return other.uptime > uptime;
    }

    /**
     * dump out this data
     *
     * @param typ type to use: 0x1=id, 0x2=nam, 0x4=seq, 0x8=time, 0x10=neighs,
     * 0x20=nets, 0x40=sr, 0x80=uptime 0x100=change 0x200=version, 0x400=bier
     * 0x800=toposum
     * @return dumped data
     */
    public String dump(int typ) {
        String s = "";
        if ((typ & 0x1) != 0) {
            s += " rtrid=" + rtrId;
        }
        if ((typ & 0x2) != 0) {
            s += " hostname=" + hostname;
        }
        if ((typ & 0x40) != 0) {
            s += " segroubeg=" + segrouBeg;
            s += " segroumax=" + segrouMax;
            s += " segrouidx=" + segrouIdx;
        }
        if ((typ & 0x400) != 0) {
            s += " bierbeg=" + bierBeg;
            s += " biermax=" + bierMax;
            s += " bieridx=" + bierIdx;
            s += " bierlen=" + bierLen;
        }
        int metric = 0;
        int bndwdt = 0;
        int affinity = 0;
        int srlg = 0;
        int segrou = 0;
        int tag = 0;
        if ((typ & 0x10) != 0) {
            for (int i = 0; i < neighbor.size(); i++) {
                rtrLsrpDataNeigh ntry = neighbor.get(i);
                if (ntry.metric != metric) {
                    s += " metric=" + ntry.metric;
                    metric = ntry.metric;
                }
                if (ntry.bndwdt != bndwdt) {
                    s += " bandwidth=" + ntry.bndwdt;
                    bndwdt = ntry.bndwdt;
                }
                if (ntry.srlg != srlg) {
                    s += " srlg=" + ntry.srlg;
                    srlg = ntry.srlg;
                }
                if (ntry.affnty != affinity) {
                    s += " affinity=" + ntry.affnty;
                    affinity = ntry.affnty;
                }
                if (ntry.segrou != segrou) {
                    s += " segrouadj=" + ntry.segrou;
                    segrou = ntry.segrou;
                }
                s += " neighbor=" + ntry.rtrid;
            }
        }
        if ((typ & 0x20) != 0) {
            for (int i = 0; i < network.size(); i++) {
                tabRouteEntry<addrIP> ntry = network.get(i);
                if (ntry.metric != metric) {
                    s += " metric=" + ntry.metric;
                    metric = ntry.metric;
                }
                if (ntry.tag != tag) {
                    s += " tag=" + ntry.tag;
                    tag = ntry.tag;
                }
                s += " network=" + addrPrefix.ip2str(ntry.prefix);
            }
        }
        if ((typ & 0x200) != 0) {
            s += " software=" + software;
            s += " hardware=" + hardware;
            s += " middleware=" + middleware;
            s += " kernel=" + kernel;
        }
        if ((typ & 0x4) != 0) {
            s += " sequence=" + sequence;
        }
        if ((typ & 0x100) != 0) {
            s += " changenum=" + changesNum;
            s += " changetim=" + changesTim;
        }
        if ((typ & 0x800) != 0) {
            s += " toposum=" + topoSum;
        }
        if ((typ & 0x1000) != 0) {
            for (int i = 0; i < address.size(); i++) {
                s += " address=" + address.get(i);
            }
        }
        if ((typ & 0x80) != 0) {
            s += " uptime=" + uptime;
        }
        if ((typ & 0x8) != 0) {
            s += " time=" + (time - bits.getTime());
        }
        return s.trim() + " ending=true";
    }

    /**
     * read up data
     *
     * @param cmd where from read
     * @return false on success, true on error
     */
    public boolean fromString(cmds cmd) {
        rtrId = new addrIPv4();
        hostname = "";
        software = "";
        hardware = "";
        middleware = "";
        kernel = "";
        topoSum = 0;
        sequence = 0;
        segrouBeg = 0;
        segrouIdx = 0;
        segrouMax = 0;
        bierBeg = 0;
        bierMax = 0;
        bierIdx = 0;
        bierLen = 0;
        time = 0;
        uptime = 0;
        changesNum = 0;
        changesTim = 0;
        int segrouAdj = 0;
        int metric = 0;
        int bndwdt = 0;
        int affinity = 0;
        int srlg = 0;
        int tag = 0;
        address = new tabGen<addrIP>();
        network = new tabRoute<addrIP>("net");
        neighbor = new tabGen<rtrLsrpDataNeigh>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            int i = a.indexOf("=");
            if (i < 0) {
                return true;
            }
            String s = a.substring(i + 1, a.length()).trim();
            a = a.substring(0, i).trim().toLowerCase();
            if (a.equals("ending")) {
                return false;
            }
            if (a.equals("rtrid")) {
                if (rtrId.fromString(s)) {
                    return true;
                }
                continue;
            }
            if (a.equals("toposum")) {
                topoSum = bits.str2num(s);
                continue;
            }
            if (a.equals("hostname")) {
                hostname = s;
                continue;
            }
            if (a.equals("software")) {
                software = s;
                continue;
            }
            if (a.equals("hardware")) {
                hardware = s;
                continue;
            }
            if (a.equals("middleware")) {
                middleware = s;
                continue;
            }
            if (a.equals("kernel")) {
                kernel = s;
                continue;
            }
            if (a.equals("segroubeg")) {
                segrouBeg = bits.str2num(s);
                continue;
            }
            if (a.equals("segroumax")) {
                segrouMax = bits.str2num(s);
                continue;
            }
            if (a.equals("segrouidx")) {
                segrouIdx = bits.str2num(s);
                continue;
            }
            if (a.equals("segrouadj")) {
                segrouAdj = bits.str2num(s);
                continue;
            }
            if (a.equals("bierbeg")) {
                bierBeg = bits.str2num(s);
                continue;
            }
            if (a.equals("biermax")) {
                bierMax = bits.str2num(s);
                continue;
            }
            if (a.equals("bieridx")) {
                bierIdx = bits.str2num(s);
                continue;
            }
            if (a.equals("bierlen")) {
                bierLen = bits.str2num(s);
                continue;
            }
            if (a.equals("sequence")) {
                sequence = bits.str2num(s);
                continue;
            }
            if (a.equals("time")) {
                time = bits.str2num(s) + bits.getTime();
                continue;
            }
            if (a.equals("uptime")) {
                uptime = bits.str2num(s);
                continue;
            }
            if (a.equals("changenum")) {
                changesNum = bits.str2num(s);
                continue;
            }
            if (a.equals("changetim")) {
                changesTim = bits.str2num(s);
                continue;
            }
            if (a.equals("metric")) {
                metric = bits.str2num(s);
                continue;
            }
            if (a.equals("bandwidth")) {
                bndwdt = bits.str2num(s);
                continue;
            }
            if (a.equals("affinity")) {
                affinity = bits.str2num(s);
                continue;
            }
            if (a.equals("srlg")) {
                srlg = bits.str2num(s);
                continue;
            }
            if (a.equals("tag")) {
                tag = bits.str2num(s);
                continue;
            }
            if (a.equals("address")) {
                addrIP adr = new addrIP();
                if (adr.fromString(s)) {
                    return true;
                }
                address.add(adr);
                continue;
            }
            if (a.equals("network")) {
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = addrPrefix.str2ip(s);
                if (ntry.prefix == null) {
                    return true;
                }
                ntry.metric = metric;
                ntry.tag = tag;
                network.add(tabRoute.addType.better, ntry, true, true);
                continue;
            }
            if (a.equals("neighbor")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(s)) {
                    return true;
                }
                addNeigh(adr, metric, bndwdt, affinity, srlg, segrouAdj);
                continue;
            }
        }
        return (time < 1);
    }

    /**
     * add neighbor
     *
     * @param nei router id
     * @param met metric
     * @param bw bandwidth
     * @param aff affinity
     * @param srl srlg
     * @param adj segrout adjacency
     */
    protected void addNeigh(addrIPv4 nei, int met, int bw, int aff, int srl, int adj) {
        rtrLsrpDataNeigh ntry = new rtrLsrpDataNeigh();
        ntry.rtrid = nei.copyBytes();
        ntry.metric = met;
        ntry.bndwdt = bw;
        ntry.affnty = aff;
        ntry.srlg = srl;
        ntry.segrou = adj;
        rtrLsrpDataNeigh old = neighbor.find(ntry);
        if (old != null) {
            if (old.metric < met) {
                return;
            }
        }
        neighbor.put(ntry);
    }

    /**
     * add neighbors
     *
     * @param spf calcer
     */
    protected void putNeighs(shrtPthFrst<addrIPv4> spf) {
        for (int i = 0; i < neighbor.size(); i++) {
            rtrLsrpDataNeigh ntry = neighbor.get(i);
            spf.addConn(rtrId, ntry.rtrid, ntry.metric, true);
        }
    }

}

class rtrLsrpDataNeigh implements Comparator<rtrLsrpDataNeigh> {

    /**
     * router id
     */
    public addrIPv4 rtrid;

    /**
     * metric
     */
    public int metric;

    /**
     * affinity
     */
    public int affnty;

    /**
     * srlg
     */
    public int srlg;

    /**
     * bandwidth
     */
    public int bndwdt;

    /**
     * segment routing
     */
    public int segrou;

    public int compare(rtrLsrpDataNeigh o1, rtrLsrpDataNeigh o2) {
        return o1.rtrid.compare(o1.rtrid, o2.rtrid);
    }

}
