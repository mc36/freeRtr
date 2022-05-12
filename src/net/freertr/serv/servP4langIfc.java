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
    protected boolean dynamic;

    /**
     * hidden from exports
     */
    protected boolean hidden;

    /**
     * config
     */
    protected final servP4langCfg lower;

    /**
     * id
     */
    protected int id;

    /**
     * speed
     */
    protected int speed;

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
     * sent mpolka
     */
    protected int sentMpolka;

    /**
     * sent sgt taq
     */
    protected int sentSgtTag;

    /**
     * sent sgt set
     */
    protected int sentSgtSet;

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
     * sent verify
     */
    protected int sentVerify4;

    /**
     * sent verify
     */
    protected int sentVerify6;

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
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos4in;

    /**
     * sent qos
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos4out;

    /**
     * sent qos
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos6in;

    /**
     * sent qos
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos6out;

    /**
     * sent qos
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos4inF;

    /**
     * sent qos
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos4outF;

    /**
     * sent qos
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos6inF;

    /**
     * sent qos
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> sentQos6outF;

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
    protected state.states lastState = state.states.up;

    /**
     * create instance
     *
     * @param p parent
     * @param i id
     */
    protected servP4langIfc(servP4langCfg p, int i) {
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
    protected boolean suppressState() {
        if (ifc == null) {
            return true;
        }
        return (master != null) || (ifc.type == cfgIfc.ifaceType.bundle) || (ifc.type == cfgIfc.ifaceType.bridge) || (ifc.type == cfgIfc.ifaceType.dialer) || (ifc.type == cfgIfc.ifaceType.hairpin) || (ifc.type == cfgIfc.ifaceType.tunnel) || (ifc.type == cfgIfc.ifaceType.virtppp);
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
    protected String getCfgLine() {
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
    protected servP4langIfc getUcast() {
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
    protected servP4langIfc getMcast(int gid, servP4langNei hop) {
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
