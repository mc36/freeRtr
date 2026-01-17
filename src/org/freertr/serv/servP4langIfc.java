package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabRouteIface;
import org.freertr.tab.tabSession;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * one p4lang interface
 *
 * @author matecsaba
 */
public class servP4langIfc implements ifcDn, Comparable<servP4langIfc> {

    /**
     * dynamically created
     */
    protected boolean dynamic;

    /**
     * hidden from exports
     */
    protected boolean hidden;

    /**
     * config
     */
    protected final servP4lang lower;

    /**
     * id
     */
    protected int id;

    /**
     * reinit parameters
     */
    protected String reinit;

    /**
     * send packets through api
     */
    protected boolean apiPack;

    /**
     * via neighbor
     */
    protected servP4langNei viaN;

    /**
     * speed
     */
    protected String speed;

    /**
     * speed converted
     */
    protected int spdNum;

    /**
     * error correction
     */
    protected int errCorr;

    /**
     * autoneg
     */
    protected int autoNeg;

    /**
     * flow control
     */
    protected int flowCtrl;

    /**
     * sent vrf
     */
    protected int sentVrf;

    /**
     * sent monitoring
     */
    protected int sentMon;

    /**
     * sent rate-limit in
     */
    protected int sentRatBin;

    /**
     * sent rate-limit in
     */
    protected int sentRatTin;

    /**
     * sent rate-limit out
     */
    protected int sentRatBout;

    /**
     * sent rate-limit out
     */
    protected int sentRatTout;

    /**
     * sent vlan
     */
    protected int sentVlan;

    /**
     * sent bundle
     */
    protected int sentBundle;

    /**
     * sent hairpin
     */
    protected int sentHairpin;

    /**
     * sent mtu
     */
    protected int sentMtu;

    /**
     * sent pppoe
     */
    protected int sentPppoe;

    /**
     * sent label
     */
    protected int sentLabel;

    /**
     * sent polka
     */
    protected int sentPolka;

    /**
     * sent sgt taq
     */
    protected int sentSgtTag;

    /**
     * sent sgt set
     */
    protected int sentSgtSet;

    /**
     * sent pmtud
     */
    protected int sentPmtud4in;

    /**
     * sent pmtud
     */
    protected int sentPmtud6in;

    /**
     * sent pmtud
     */
    protected int sentPmtud4out;

    /**
     * sent pmtud
     */
    protected int sentPmtud6out;

    /**
     * sent mss
     */
    protected int sentMss4in;

    /**
     * sent mss
     */
    protected int sentMss4out;

    /**
     * sent mss
     */
    protected int sentMss6in;

    /**
     * sent mss
     */
    protected int sentMss6out;

    /**
     * sent ttl
     */
    protected int sentTtl4in;

    /**
     * sent ttl
     */
    protected int sentTtl4out;

    /**
     * sent ttl
     */
    protected int sentTtl6in;

    /**
     * sent ttl
     */
    protected int sentTtl6out;

    /**
     * sent verify
     */
    protected int sentVerify4;

    /**
     * sent verify
     */
    protected int sentVerify6;

    /**
     * sent propagate ipv4
     */
    protected int sentPropagate4;

    /**
     * sent propagate ipv6
     */
    protected int sentPropagate6;

    /**
     * sent flowspec ipv4
     */
    protected int sentFlowDis4;

    /**
     * sent flowspec ipv6
     */
    protected int sentFlowDis6;

    /**
     * sent mpls
     */
    protected int sentMpls;

    /**
     * sent nsh
     */
    protected int sentNsh;

    /**
     * sent macsec
     */
    protected String sentMacsec;

    /**
     * sent interface state
     */
    protected state.states sentState = state.states.close;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4in1;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4out1;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6in1;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6out1;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4in2;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4out2;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6in2;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6out2;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4inF;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4outF;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6inF;

    /**
     * sent acl
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6outF;

    /**
     * sent sessions
     */
    protected tabSession sentInsp4;

    /**
     * sent sessions
     */
    protected tabSession sentInsp6;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos4in;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos4out;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos6in;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos6out;

    /**
     * sent qos
     */
    protected List<Long> sentQos4inB;

    /**
     * sent qos
     */
    protected List<Long> sentQos4outB;

    /**
     * sent qos
     */
    protected List<Long> sentQos6inB;

    /**
     * sent qos
     */
    protected List<Long> sentQos6outB;

    /**
     * sent qos
     */
    protected List<Integer> sentQos4inI;

    /**
     * sent qos
     */
    protected List<Integer> sentQos4outI;

    /**
     * sent qos
     */
    protected List<Integer> sentQos6inI;

    /**
     * sent qos
     */
    protected List<Integer> sentQos6outI;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos4inF;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos4outF;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos6inF;

    /**
     * sent qos
     */
    protected List<tabListing<tabAceslstN<addrIP>, addrIP>> sentQos6outF;

    /**
     * sent bridge encapsulation
     */
    protected String sentBrTun;

    /**
     * controlling interface
     */
    protected servP4langIfc master;

    /**
     * member interfaces
     */
    protected List<servP4langIfc> members;

    /**
     * pppoe headend
     */
    protected servP4langIfc pppoe;

    /**
     * cloned from
     */
    protected servP4langIfc cloned;

    /**
     * intreface config
     */
    protected cfgIfc ifc;

    /**
     * bridge interface
     */
    protected ifcBridgeIfc brif;

    /**
     * upper layer
     */
    protected ifcUp upper = new ifcNull();

    /**
     * counters
     */
    protected counter cntr = new counter();

    /**
     * last state
     */
    protected state.states lastState = state.states.down;

    /**
     * create instance
     *
     * @param p parent
     * @param i id
     */
    protected servP4langIfc(servP4lang p, int i) {
        id = i;
        lower = p;
    }

    public int compareTo(servP4langIfc o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        return 0;
    }

    /**
     * check if state needed
     *
     * @return true if suppression needed
     */
    protected boolean suppressState() {
        if (ifc == null) {
            return true;
        }
        return (master != null) || (ifc.type == tabRouteIface.ifaceType.bundle) || (ifc.type == tabRouteIface.ifaceType.bridge) || (ifc.type == tabRouteIface.ifaceType.dialer) || (ifc.type == tabRouteIface.ifaceType.hairpin) || (ifc.type == tabRouteIface.ifaceType.tunnel) || (ifc.type == tabRouteIface.ifaceType.virtppp) || (ifc.type == tabRouteIface.ifaceType.pweth);
    }

    /**
     * clear down interface
     */
    protected void tearDown() {
        if (ifc == null) {
            return;
        }
        if (sentMpls != 0) {
            lower.sendLine("mplspack_del " + id + " " + sentMpls);
        }
        if (sentNsh != 0) {
            lower.sendLine("nshpack_del " + id + " " + sentNsh);
        }
        if (sentVrf != 0) {
            lower.sendLine("portvrf_del " + id + " " + sentVrf);
        }
        if ((master != null) && (sentVlan != 0)) {
            if (master.master == null) {
                lower.sendLine("portvlan_del " + id + " " + master.id + " " + ifc.vlanNum);
            } else {
                lower.sendLine("portqinq_del " + id + " " + master.master.id + " " + master.id + " " + master.ifc.vlanNum + " " + ifc.vlanNum);
            }
        }
        if (!suppressState()) {
            lower.sendLine("state " + id + " 0 " + getStateEnding());
            lower.sendLine("ports_del " + id + " " + getStateEnding());
        }
        if ((ifc.type == tabRouteIface.ifaceType.sdn) && (ifc.vlanNum == 0)) {
            ifcNull nul = new ifcNull();
            nul.setUpper(ifc.ethtyp);
        }
    }

    public String toString() {
        return "p4lang port " + id;
    }

    /**
     * get unicast interface
     *
     * @return interface
     */
    protected servP4langIfc getUcast() {
        if (master == null) {
            return this;
        }
        if (master.master == null) {
            return master;
        }
        return master.master;
    }

    /**
     * get multicast interface
     *
     * @param gid group id
     * @param hop nexthop
     * @return interface
     */
    protected servP4langIfc getMcast(int gid, servP4langNei hop) {
        servP4langIfc i;
        if (master == null) {
            i = this;
        } else {
            if (master.master == null) {
                i = master;
            } else {
                i = master.master;
            }
        }
        if (i.members == null) {
            return i;
        }
        if (i.members.size() < 1) {
            return i;
        }
        if (hop != null) {
            gid ^= hop.id;
        }
        return i.members.get(gid % i.members.size());
    }

    /**
     * get source address
     *
     * @return mac address
     */
    protected addrMac getMac() {
        addrType adr = ifc.ethtyp.getHwAddr();
        if (adr.getSize() != addrMac.size) {
            return new addrMac();
        }
        return (addrMac) adr;
    }

    /**
     * encode state message
     *
     * @return encoding
     */
    protected String getStateEnding() {
        return speed + " " + errCorr + " " + autoNeg + " " + flowCtrl;
    }

    /**
     * clear sent state
     */
    protected void doClear() {
        viaN = null;
        if (suppressState()) {
            lastState = state.states.up;
        } else {
            lastState = state.states.down;
        }
        upper.setState(lastState);
        sentVlan = 0;
        sentBundle = 0;
        sentHairpin = 0;
        sentPppoe = -1;
        sentMacsec = null;
        sentVrf = 0;
        sentMon = -1;
        sentRatBin = -1;
        sentRatTin = -1;
        sentRatBout = -1;
        sentRatTout = -1;
        sentState = state.states.close;
        sentMtu = 0;
        sentLabel = -1;
        sentPolka = -1;
        sentSgtTag = 0;
        sentSgtSet = -1;
        sentMss4in = 0;
        sentMss4out = 0;
        sentMss6in = 0;
        sentMss6out = 0;
        sentTtl4in = 0;
        sentTtl4out = 0;
        sentTtl6in = 0;
        sentTtl6out = 0;
        sentPmtud4in = 0;
        sentPmtud6in = 0;
        sentPmtud4out = 0;
        sentPmtud6out = 0;
        sentVerify4 = 0;
        sentVerify6 = 0;
        sentPropagate4 = 0;
        sentPropagate6 = 0;
        sentFlowDis4 = 0;
        sentFlowDis6 = 0;
        sentMpls = 0;
        sentNsh = 0;
        sentAcl4in1 = null;
        sentAcl4in2 = null;
        sentAcl4inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentAcl4out1 = null;
        sentAcl4out2 = null;
        sentAcl4outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentAcl6in1 = null;
        sentAcl6in2 = null;
        sentAcl6inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentAcl6out1 = null;
        sentAcl6out2 = null;
        sentAcl6outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos4in = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos4inF = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos4out = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos4outF = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos6in = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos6inF = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos6out = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos6outF = new ArrayList<tabListing<tabAceslstN<addrIP>, addrIP>>();
        sentQos4inB = new ArrayList<Long>();
        sentQos4outB = new ArrayList<Long>();
        sentQos6inB = new ArrayList<Long>();
        sentQos6outB = new ArrayList<Long>();
        sentQos4inI = new ArrayList<Integer>();
        sentQos4outI = new ArrayList<Integer>();
        sentQos6inI = new ArrayList<Integer>();
        sentQos6outI = new ArrayList<Integer>();
        sentInsp4 = null;
        sentInsp6 = null;
        sentBrTun = null;
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return lastState;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

    /**
     * send one packet through api
     *
     * @param cnt counter to use
     * @param pck packet to send
     */
    protected void apiSendPack(int cnt, packHolder pck) {
        if (debugger.servP4langTraf) {
            logger.debug("sending on #" + id + " " + pck.dataOffset());
        }
        if (viaN == null) {
            lower.sendLine(servP4langUtil.packet2packout(false, pck, cnt, id, id));
        } else {
            lower.sendLine(servP4langUtil.packet2packout(true, pck, cnt, viaN.id, id));
        }
    }

    /**
     * setup for api packet usage
     */
    protected void setup2apiPack() {
        if (ifc == null) {
            return;
        }
        boolean ned = false;
        if (speed != null) {
            ned = spdNum == -2;
        }
        servP4langIfc ntry = lower.findIfc(ifc.cloned);
        if (ntry != null) {
            ned |= ntry.apiPack;
        }
        ntry = lower.findIfc(ifc.parent);
        if (ntry != null) {
            ned |= ntry.apiPack;
            servP4langIfc res = lower.findIfc(ntry.ifc.cloned);
            if (res != null) {
                ned |= res.apiPack;
            }
            res = lower.findIfc(ntry.ifc.parent);
            if (res != null) {
                ned |= res.apiPack;
            }
        }
        apiPack = ned;
        if (ned) {
            ifc.ethtyp.sendClear = this;
        } else {
            ifc.ethtyp.sendClear = null;
        }
    }

    public void sendPack(packHolder pck) {
        if (apiPack) {
            apiSendPack(1, pck);
            return;
        }
        lower.sendPack(id, pck);
    }

}
