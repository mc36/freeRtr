package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabSession;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * one p4lang interface
 *
 * @author matecsaba
 */
public class servP4langIfc implements ifcDn, Comparator<servP4langIfc> {

    /**
     * dynamically created
     */
    public boolean dynamic;

    /**
     * hidden from exports
     */
    public boolean hidden;

    /**
     * config
     */
    public final servP4langCfg lower;

    /**
     * id
     */
    public int id;

    /**
     * speed
     */
    public int speed;

    /**
     * error correction
     */
    public int errCorr;

    /**
     * autoneg
     */
    public int autoNeg;

    /**
     * flow control
     */
    public int flowCtrl;

    /**
     * sent vrf
     */
    public int sentVrf;

    /**
     * sent monitoring
     */
    public int sentMon;

    /**
     * sent vlan
     */
    public int sentVlan;

    /**
     * sent bundle
     */
    public int sentBundle;

    /**
     * sent hairpin
     */
    public int sentHairpin;

    /**
     * sent mtu
     */
    public int sentMtu;

    /**
     * sent pppoe
     */
    public int sentPppoe;

    /**
     * sent label
     */
    public int sentLabel;

    /**
     * sent polka
     */
    public int sentPolka;

    /**
     * sent mpolka
     */
    public int sentMpolka;

    /**
     * sent sgt taq
     */
    public int sentSgtTag;

    /**
     * sent sgt set
     */
    public int sentSgtSet;

    /**
     * sent mss
     */
    public int sentMss4in;

    /**
     * sent mss
     */
    public int sentMss4out;

    /**
     * sent mss
     */
    public int sentMss6in;

    /**
     * sent mss
     */
    public int sentMss6out;

    /**
     * sent verify
     */
    public int sentVerify4;

    /**
     * sent verify
     */
    public int sentVerify6;

    /**
     * sent mpls
     */
    public int sentMpls;

    /**
     * sent nsh
     */
    public int sentNsh;

    /**
     * sent macsec
     */
    public String sentMacsec;

    /**
     * sent interface state
     */
    public state.states sentState = state.states.close;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4in1;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4out1;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6in1;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6out1;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4in2;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4out2;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6in2;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6out2;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4inF;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4outF;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6inF;

    /**
     * sent acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6outF;

    /**
     * sent sessions
     */
    public tabSession sentInsp4;

    /**
     * sent sessions
     */
    public tabSession sentInsp6;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4in;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4out;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6in;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6out;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4inF;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4outF;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6inF;

    /**
     * sent qos
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6outF;

    /**
     * sent bridge encapsulation
     */
    public String sentBrTun;

    /**
     * controlling interface
     */
    public servP4langIfc master;

    /**
     * member interfaces
     */
    public List<servP4langIfc> members;

    /**
     * pppoe headend
     */
    public servP4langIfc pppoe;

    /**
     * cloned from
     */
    public servP4langIfc cloned;

    /**
     * intreface config
     */
    public cfgIfc ifc;

    /**
     * bridge interface
     */
    public ifcBridgeIfc brif;

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * counters
     */
    public counter cntr = new counter();

    /**
     * last state
     */
    public state.states lastState = state.states.up;

    /**
     * create instance
     *
     * @param p parent
     * @param i id
     */
    public servP4langIfc(servP4langCfg p, int i) {
        id = i;
        lower = p;
    }

    public int compare(servP4langIfc o1, servP4langIfc o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    /**
     * check if state needed
     *
     * @return true if suppression needed
     */
    public boolean suppressState() {
        if (ifc == null) {
            return true;
        }
        return (master != null) || (ifc.type == cfgIfc.ifaceType.bundle) || (ifc.type == cfgIfc.ifaceType.bridge) || (ifc.type == cfgIfc.ifaceType.dialer) || (ifc.type == cfgIfc.ifaceType.hairpin) || (ifc.type == cfgIfc.ifaceType.tunnel) || (ifc.type == cfgIfc.ifaceType.virtppp);
    }

    /**
     * clear down interface
     */
    public void tearDown() {
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
            lower.sendLine("portvlan_del " + id + " " + master.id + " " + ifc.vlanNum);
        }
        if (!suppressState()) {
            lower.sendLine("state " + id + " 0 " + getStateEnding());
            lower.sendLine("ports_del " + id + " " + getStateEnding());
        }
        if ((ifc.type == cfgIfc.ifaceType.sdn) && (ifc.vlanNum == 0)) {
            ifcNull nul = new ifcNull();
            nul.setUpper(ifc.ethtyp);
        }
    }

    /**
     * get config line
     *
     * @return config
     */
    public String getCfgLine() {
        String a;
        if (dynamic) {
            a = "dynamic";
        } else {
            a = "" + id;
        }
        return ifc.name + " " + a + " " + speed + " " + errCorr + " " + autoNeg + " " + flowCtrl;
    }

    public String toString() {
        return "p4lang port " + id;
    }

    /**
     * get unicast interface
     *
     * @return interface
     */
    public servP4langIfc getUcast() {
        if (master == null) {
            return this;
        }
        return master;
    }

    /**
     * get multicast interface
     *
     * @param gid group id
     * @param hop nexthop
     * @return interface
     */
    public servP4langIfc getMcast(int gid, servP4langNei hop) {
        servP4langIfc ifc;
        if (master == null) {
            ifc = this;
        } else {
            ifc = master;
        }
        if (ifc.members == null) {
            return ifc;
        }
        if (ifc.members.size() < 1) {
            return ifc;
        }
        if (hop != null) {
            gid ^= hop.id;
        }
        return ifc.members.get(gid % ifc.members.size());
    }

    /**
     * get source address
     *
     * @return mac address
     */
    public addrMac getMac() {
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
    public String getStateEnding() {
        return speed + " " + errCorr + " " + autoNeg + " " + flowCtrl;
    }

    /**
     * clear sent state
     */
    public void doClear() {
        lastState = state.states.up;
        upper.setState(lastState);
        sentVlan = 0;
        sentBundle = 0;
        sentHairpin = 0;
        sentPppoe = -1;
        sentMacsec = null;
        sentVrf = 0;
        sentMon = -1;
        sentState = state.states.close;
        sentMtu = 0;
        sentLabel = -1;
        sentPolka = -1;
        sentMpolka = -1;
        sentSgtTag = 0;
        sentSgtSet = -1;
        sentMss4in = 0;
        sentMss4out = 0;
        sentMss6in = 0;
        sentMss6out = 0;
        sentVerify4 = 0;
        sentVerify6 = 0;
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
        sentQos4in = null;
        sentQos4inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos4out = null;
        sentQos4outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos6in = null;
        sentQos6inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos6out = null;
        sentQos6outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
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

    public void sendPack(packHolder pck) {
        lower.sendPack(id, pck);
    }

}
