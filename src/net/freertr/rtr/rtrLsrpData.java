package net.freertr.rtr;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userUpgrade;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.shrtPthFrst;

/**
 * lsrp data
 *
 * @author matecsaba
 */
public class rtrLsrpData implements Comparator<rtrLsrpData> {

    /**
     * create instance
     */
    public rtrLsrpData() {
    }

    /**
     * full dump
     */
    public static final int dmpFull = -1;

    /**
     * compare dump
     */
    public static final int dmpComp = dmpFull - 0x800 - 0x200 - 0x100 - 0x80 - 0x8 - 0x4;

    /**
     * checksum dump
     */
    public static final int dmpCsum = dmpComp - 0x4000;

    /**
     * router id
     */
    public addrIPv4 rtrId;

    /**
     * management address
     */
    public addrIP mgmtIp;

    /**
     * topology summary
     */
    public int topoSum;

    /**
     * checksum
     */
    public String password;

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
     * segment routing maximum
     */
    public int segrouMax;

    /**
     * segment routing base
     */
    public int segrouBeg;

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
     * first time
     */
    public long since;

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
    public tabGen<rtrLsrpDataAddr> address;

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
     * 0x800=toposum, 0x1000=addrs, 0x2000=mgmtip, 0x4000=password, 0x8000=since
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
        }
        if ((typ & 0x400) != 0) {
            s += " bierbeg=" + bierBeg;
            s += " biermax=" + bierMax;
            s += " bierlen=" + bierLen;
        }
        int metric = 0;
        boolean stub = false;
        long bndwdt = 0;
        int affinity = 0;
        int srlg = 0;
        int mtu = 0;
        int segrouAdj = 0;
        int segrouIdx = 0;
        boolean segrouPop = false;
        boolean external = false;
        int bierIdx = 0;
        int tag = 0;
        String iface = "";
        if ((typ & 0x10) != 0) {
            for (int i = 0; i < neighbor.size(); i++) {
                rtrLsrpDataNeigh ntry = neighbor.get(i);
                if (!iface.equals(ntry.iface)) {
                    s += " interface=" + ntry.iface;
                    iface = ntry.iface;
                }
                if (ntry.metric != metric) {
                    s += " metric=" + ntry.metric;
                    metric = ntry.metric;
                }
                if (ntry.stub != stub) {
                    s += " stub=" + ntry.stub;
                    stub = ntry.stub;
                }
                if (ntry.bndwdt != bndwdt) {
                    s += " bandwidth=" + ntry.bndwdt;
                    bndwdt = ntry.bndwdt;
                }
                if (ntry.srlg != srlg) {
                    s += " srlg=" + ntry.srlg;
                    srlg = ntry.srlg;
                }
                if (ntry.mtu != mtu) {
                    s += " mtu=" + ntry.mtu;
                    mtu = ntry.mtu;
                }
                if (ntry.affnty != affinity) {
                    s += " affinity=" + ntry.affnty;
                    affinity = ntry.affnty;
                }
                if (ntry.segrou != segrouAdj) {
                    s += " segrouadj=" + ntry.segrou;
                    segrouAdj = ntry.segrou;
                }
                s += " peeraddr=" + ntry.peer;
                s += " peeriface=" + ntry.perif;
                s += " neighbor=" + ntry.rtrid;
            }
        }
        if ((typ & 0x20) != 0) {
            for (int i = 0; i < network.size(); i++) {
                tabRouteEntry<addrIP> ntry = network.get(i);
                if (ntry.best.metric != metric) {
                    s += " metric=" + ntry.best.metric;
                    metric = ntry.best.metric;
                }
                if (ntry.best.tag != tag) {
                    s += " tag=" + ntry.best.tag;
                    tag = ntry.best.tag;
                }
                boolean ext = (ntry.best.rouSrc & 1) != 0;
                if (ext != external) {
                    s += " external=" + ext;
                    external = ext;
                }
                boolean srp = (ntry.best.rouSrc & 16) != 0;
                if (srp != segrouPop) {
                    s += " segroupop=" + srp;
                    segrouPop = srp;
                }
                if (ntry.best.segrouIdx != segrouIdx) {
                    s += " segrouidx=" + ntry.best.segrouIdx;
                    segrouIdx = ntry.best.segrouIdx;
                }
                if (ntry.best.bierIdx != bierIdx) {
                    s += " bieridx=" + ntry.best.bierIdx;
                    bierIdx = ntry.best.bierIdx;
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
                rtrLsrpDataAddr ntry = address.get(i);
                if (!iface.equals(ntry.iface)) {
                    s += " interface=" + ntry.iface;
                    iface = ntry.iface;
                }
                s += " address=" + ntry.addr;
            }
        }
        if ((typ & 0x2000) != 0) {
            s += " mgmtaddr=" + mgmtIp;
        }
        if ((typ & 0x4000) != 0) {
            s += " password=" + password;
        }
        if ((typ & 0x8000) != 0) {
            s += " since=" + since;
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
        password = "none";
        kernel = "";
        topoSum = 0;
        sequence = 0;
        segrouBeg = 0;
        int segrouIdx = 0;
        segrouMax = 0;
        boolean segrouPop = false;
        bierBeg = 0;
        bierMax = 0;
        int bierIdx = 0;
        bierLen = 0;
        time = 0;
        uptime = 0;
        changesNum = 0;
        changesTim = 0;
        since = bits.getTime();
        addrIP peerAddr = new addrIP();
        String peerIf = "unknown";
        mgmtIp = new addrIP();
        String iface = "unknown";
        boolean stub = false;
        int segrouAdj = 0;
        int metric = 0;
        long bndwdt = 0;
        int affinity = 0;
        int srlg = 0;
        int mtu = 0;
        boolean external = false;
        int tag = 0;
        address = new tabGen<rtrLsrpDataAddr>();
        network = new tabRoute<addrIP>("net");
        neighbor = new tabGen<rtrLsrpDataNeigh>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                return true;
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
            if (a.equals("password")) {
                password = s;
                continue;
            }
            if (a.equals("external")) {
                external = s.toLowerCase().equals("true");
                continue;
            }
            if (a.equals("stub")) {
                stub = s.toLowerCase().equals("true");
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
            if (a.equals("segroupop")) {
                segrouPop = s.toLowerCase().equals("true");
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
                time = bits.str2long(s) + bits.getTime();
                continue;
            }
            if (a.equals("uptime")) {
                uptime = bits.str2long(s);
                continue;
            }
            if (a.equals("since")) {
                since = bits.str2long(s);
                continue;
            }
            if (a.equals("changenum")) {
                changesNum = bits.str2num(s);
                continue;
            }
            if (a.equals("changetim")) {
                changesTim = bits.str2long(s);
                continue;
            }
            if (a.equals("metric")) {
                metric = bits.str2num(s);
                continue;
            }
            if (a.equals("bandwidth")) {
                bndwdt = bits.str2long(s);
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
            if (a.equals("mtu")) {
                mtu = bits.str2num(s);
                continue;
            }
            if (a.equals("tag")) {
                tag = bits.str2num(s);
                continue;
            }
            if (a.equals("mgmtaddr")) {
                if (mgmtIp.fromString(s)) {
                    return true;
                }
                continue;
            }
            if (a.equals("address")) {
                rtrLsrpDataAddr ntry = new rtrLsrpDataAddr();
                if (ntry.addr.fromString(s)) {
                    return true;
                }
                ntry.iface = iface;
                address.add(ntry);
                continue;
            }
            if (a.equals("interface")) {
                iface = s;
                continue;
            }
            if (a.equals("peeriface")) {
                peerIf = s;
                continue;
            }
            if (a.equals("peeraddr")) {
                if (peerAddr.fromString(s)) {
                    return true;
                }
                continue;
            }
            if (a.equals("network")) {
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = addrPrefix.str2ip(s);
                if (ntry.prefix == null) {
                    return true;
                }
                ntry.best.metric = metric;
                ntry.best.tag = tag;
                ntry.best.bierIdx = bierIdx;
                ntry.best.segrouIdx = segrouIdx;
                ntry.best.rouSrc = segrouPop ? 16 : 0;
                if (external) {
                    ntry.best.rouSrc |= 1;
                }
                network.add(tabRoute.addType.better, ntry, true, true);
                continue;
            }
            if (a.equals("neighbor")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(s)) {
                    return true;
                }
                addNeigh(adr, iface, metric, stub, bndwdt, affinity, srlg, mtu, segrouAdj, peerAddr, peerIf);
                continue;
            }
        }
    }

    /**
     * add address
     *
     * @param ifc interface
     * @param adr interface address
     */
    protected void addAddr(String ifc, addrIP adr) {
        rtrLsrpDataAddr ntry = new rtrLsrpDataAddr();
        ntry.addr.setAddr(adr);
        ntry.iface = ifc;
        address.add(ntry);
    }

    /**
     * add neighbor
     *
     * @param nei router id
     * @param ifc interface
     * @param met metric
     * @param stb stub flag
     * @param bw bandwidth
     * @param aff affinity
     * @param srl srlg
     * @param mtu mtu
     * @param adj segrout adjacency
     * @param adr adjacency address
     * @param pif peer interface
     */
    protected void addNeigh(addrIPv4 nei, String ifc, int met, boolean stb, long bw, int aff, int srl, int mtu, int adj, addrIP adr, String pif) {
        rtrLsrpDataNeigh ntry = new rtrLsrpDataNeigh();
        ntry.rtrid = nei.copyBytes();
        ntry.metric = met;
        ntry.stub = stb;
        ntry.iface = ifc;
        ntry.bndwdt = bw;
        ntry.affnty = aff;
        ntry.srlg = srl;
        ntry.mtu = mtu;
        ntry.segrou = adj;
        ntry.peer = adr.copyBytes();
        ntry.perif = pif;
        rtrLsrpDataNeigh old = neighbor.find(ntry);
        if (old != null) {
            if (old.metric < met) {
                return;
            }
        }
        neighbor.put(ntry);
    }

    /**
     * add to spf
     *
     * @param spf calcer
     * @param dist distance
     */
    protected void put2spf(shrtPthFrst<addrIPv4> spf, int dist) {
        for (int i = 0; i < neighbor.size(); i++) {
            rtrLsrpDataNeigh ntry = neighbor.get(i);
            String a;
            if (ntry.iface == null) {
                a = "" + ntry.peer;
            } else {
                a = ntry.iface;
            }
            spf.addConn(rtrId, ntry.rtrid, ntry.metric, true, ntry.stub, a);
        }
        spf.addIdent(rtrId, hostname);
        spf.addSegRouB(rtrId, segrouBeg);
        spf.addBierB(rtrId, bierBeg);
        int brh = tabLabelBier.num2bsl(bierLen);
        for (int i = 0; i < network.size(); i++) {
            tabRouteEntry<addrIP> rou = network.get(i).copyBytes(tabRoute.addType.notyet);
            rou.best.bierHdr = brh;
            rou.best.distance = dist;
            spf.addPref(rtrId, rou, false);
            spf.addSegRouI(rtrId, rou.best.segrouIdx);
            spf.addBierI(rtrId, rou.best.bierIdx);
        }
    }

    /**
     * calculate password
     *
     * @param pwd password
     * @return value
     */
    protected String calcPass(String pwd) {
        List<String> lst = new ArrayList<String>();
        lst.add(pwd);
        lst.add(dump(dmpCsum));
        lst.add(pwd);
        return userUpgrade.calcTextHash(lst);
    }

}

class rtrLsrpDataAddr implements Comparator<rtrLsrpDataAddr> {

    /**
     * address
     */
    public addrIP addr = new addrIP();

    /**
     * interface
     */
    public String iface;

    public int compare(rtrLsrpDataAddr o1, rtrLsrpDataAddr o2) {
        return o1.addr.compare(o1.addr, o2.addr);
    }

}

class rtrLsrpDataNeigh implements Comparator<rtrLsrpDataNeigh> {

    /**
     * router id
     */
    public addrIPv4 rtrid;

    /**
     * local interface
     */
    public String iface;

    /**
     * adjacency address
     */
    public addrIP peer;

    /**
     * stub flag
     */
    public boolean stub;

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
     * mtu
     */
    public int mtu;

    /**
     * bandwidth
     */
    public long bndwdt;

    /**
     * segment routing
     */
    public int segrou;

    /**
     * peer interface
     */
    public String perif;

    public int compare(rtrLsrpDataNeigh o1, rtrLsrpDataNeigh o2) {
        int i = o1.rtrid.compare(o1.rtrid, o2.rtrid);
        if (i != 0) {
            return i;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

}
