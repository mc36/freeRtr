package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;
import org.freertr.enc.encTlv;
import org.freertr.prt.prtTcp;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoWrk;

/**
 * bgp4 speaker
 *
 * @author matecsaba
 */
public class rtrBgpSpeak implements rtrBfdClnt, Runnable {

    /**
     * connection
     */
    protected pipeSide pipe;

    private boolean resumed;

    /**
     * parent
     */
    protected final rtrBgp parent;

    /**
     * neighbor
     */
    protected final rtrBgpNeigh neigh;

    /**
     * ready to advertise
     */
    protected boolean ready2adv;

    /**
     * last received
     */
    public long lastRx;

    /**
     * uptime
     */
    public long upTime;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * advertised version
     */
    public final syncInt adversion = new syncInt(0);

    /**
     * need full advertisement
     */
    public final syncInt needFull = new syncInt(3);

    /**
     * buffer full pauses
     */
    public int buffFull = 0;

    /**
     * ipinfo result
     */
    public secInfoWrk ipInfoRes;

    /**
     * learned unicast prefixes
     */
    public final tabRoute<addrIP> lrnUni = new tabRoute<addrIP>("rx");

    /**
     * learned multicast prefixes
     */
    public final tabRoute<addrIP> lrnMlt = new tabRoute<addrIP>("rx");

    /**
     * learned other unicast prefixes
     */
    public final tabRoute<addrIP> lrnOuni = new tabRoute<addrIP>("rx");

    /**
     * learned other multicast prefixes
     */
    public final tabRoute<addrIP> lrnOmlt = new tabRoute<addrIP>("rx");

    /**
     * learned other flowspec prefixes
     */
    public final tabRoute<addrIP> lrnOflw = new tabRoute<addrIP>("rx");

    /**
     * learned other srte prefixes
     */
    public final tabRoute<addrIP> lrnOsrt = new tabRoute<addrIP>("rx");

    /**
     * learned flowspec prefixes
     */
    public final tabRoute<addrIP> lrnFlw = new tabRoute<addrIP>("rx");

    /**
     * learned vpnuni prefixes
     */
    public final tabRoute<addrIP> lrnVpnU = new tabRoute<addrIP>("rx");

    /**
     * learned vpnmulti prefixes
     */
    public final tabRoute<addrIP> lrnVpnM = new tabRoute<addrIP>("rx");

    /**
     * learned vpnflow prefixes
     */
    public final tabRoute<addrIP> lrnVpnF = new tabRoute<addrIP>("rx");

    /**
     * learned other vpnuni prefixes
     */
    public final tabRoute<addrIP> lrnVpoU = new tabRoute<addrIP>("rx");

    /**
     * learned other vpnmulti prefixes
     */
    public final tabRoute<addrIP> lrnVpoM = new tabRoute<addrIP>("rx");

    /**
     * learned other vpnflow prefixes
     */
    public final tabRoute<addrIP> lrnVpoF = new tabRoute<addrIP>("rx");

    /**
     * learned vpls prefixes
     */
    public final tabRoute<addrIP> lrnVpls = new tabRoute<addrIP>("rx");

    /**
     * learned mspw prefixes
     */
    public final tabRoute<addrIP> lrnMspw = new tabRoute<addrIP>("rx");

    /**
     * learned evpn prefixes
     */
    public final tabRoute<addrIP> lrnEvpn = new tabRoute<addrIP>("rx");

    /**
     * learned mdt prefixes
     */
    public final tabRoute<addrIP> lrnMdt = new tabRoute<addrIP>("rx");

    /**
     * learned nsh prefixes
     */
    public final tabRoute<addrIP> lrnNsh = new tabRoute<addrIP>("rx");

    /**
     * learned rpd prefixes
     */
    public final tabRoute<addrIP> lrnRpd = new tabRoute<addrIP>("rx");

    /**
     * learned spf prefixes
     */
    public final tabRoute<addrIP> lrnSpf = new tabRoute<addrIP>("rx");

    /**
     * learned rtfilter prefixes
     */
    public final tabRoute<addrIP> lrnRtf = new tabRoute<addrIP>("rx");

    /**
     * learned srte prefixes
     */
    public final tabRoute<addrIP> lrnSrte = new tabRoute<addrIP>("rx");

    /**
     * learned linkstate prefixes
     */
    public final tabRoute<addrIP> lrnLnks = new tabRoute<addrIP>("rx");

    /**
     * learned mvpn prefixes
     */
    public final tabRoute<addrIP> lrnMvpn = new tabRoute<addrIP>("rx");

    /**
     * learned other mvpn prefixes
     */
    public final tabRoute<addrIP> lrnMvpo = new tabRoute<addrIP>("rx");

    /**
     * learned mtree prefixes
     */
    public final tabRoute<addrIP> lrnMtre = new tabRoute<addrIP>("rx");

    /**
     * learned other mtree prefixes
     */
    public final tabRoute<addrIP> lrnMtro = new tabRoute<addrIP>("rx");

    /**
     * advertised unicast prefixes
     */
    public final tabRoute<addrIP> advUni = new tabRoute<addrIP>("tx");

    /**
     * advertised multicast prefixes
     */
    public final tabRoute<addrIP> advMlt = new tabRoute<addrIP>("tx");

    /**
     * advertised other unicast prefixes
     */
    public final tabRoute<addrIP> advOuni = new tabRoute<addrIP>("tx");

    /**
     * advertised other multicast prefixes
     */
    public final tabRoute<addrIP> advOmlt = new tabRoute<addrIP>("tx");

    /**
     * advertised other flowspec prefixes
     */
    public final tabRoute<addrIP> advOflw = new tabRoute<addrIP>("tx");

    /**
     * advertised other srte prefixes
     */
    public final tabRoute<addrIP> advOsrt = new tabRoute<addrIP>("tx");

    /**
     * advertised flowspec prefixes
     */
    public final tabRoute<addrIP> advFlw = new tabRoute<addrIP>("tx");

    /**
     * advertised vpnuni prefixes
     */
    public final tabRoute<addrIP> advVpnU = new tabRoute<addrIP>("tx");

    /**
     * advertised vpnmulti prefixes
     */
    public final tabRoute<addrIP> advVpnM = new tabRoute<addrIP>("tx");

    /**
     * advertised vpnflow prefixes
     */
    public final tabRoute<addrIP> advVpnF = new tabRoute<addrIP>("tx");

    /**
     * advertised other vpnuni prefixes
     */
    public final tabRoute<addrIP> advVpoU = new tabRoute<addrIP>("tx");

    /**
     * advertised other vpnmulti prefixes
     */
    public final tabRoute<addrIP> advVpoM = new tabRoute<addrIP>("tx");

    /**
     * advertised other vpnflow prefixes
     */
    public final tabRoute<addrIP> advVpoF = new tabRoute<addrIP>("tx");

    /**
     * advertised vpls prefixes
     */
    public final tabRoute<addrIP> advVpls = new tabRoute<addrIP>("tx");

    /**
     * advertised mspw prefixes
     */
    public final tabRoute<addrIP> advMspw = new tabRoute<addrIP>("tx");

    /**
     * advertised evpn prefixes
     */
    public final tabRoute<addrIP> advEvpn = new tabRoute<addrIP>("tx");

    /**
     * advertised mdt prefixes
     */
    public final tabRoute<addrIP> advMdt = new tabRoute<addrIP>("tx");

    /**
     * advertised nsh prefixes
     */
    public final tabRoute<addrIP> advNsh = new tabRoute<addrIP>("tx");

    /**
     * advertised rpd prefixes
     */
    public final tabRoute<addrIP> advRpd = new tabRoute<addrIP>("tx");

    /**
     * advertised spf prefixes
     */
    public final tabRoute<addrIP> advSpf = new tabRoute<addrIP>("tx");

    /**
     * advertised rtfilter prefixes
     */
    public final tabRoute<addrIP> advRtf = new tabRoute<addrIP>("tx");

    /**
     * advertised srte prefixes
     */
    public final tabRoute<addrIP> advSrte = new tabRoute<addrIP>("tx");

    /**
     * advertised linkstate prefixes
     */
    public final tabRoute<addrIP> advLnks = new tabRoute<addrIP>("tx");

    /**
     * advertised mvpn prefixes
     */
    public final tabRoute<addrIP> advMvpn = new tabRoute<addrIP>("tx");

    /**
     * advertised other mvpn prefixes
     */
    public final tabRoute<addrIP> advMvpo = new tabRoute<addrIP>("tx");

    /**
     * advertised mtree prefixes
     */
    public final tabRoute<addrIP> advMtre = new tabRoute<addrIP>("tx");

    /**
     * advertised other mtree prefixes
     */
    public final tabRoute<addrIP> advMtro = new tabRoute<addrIP>("tx");

    /**
     * currently updating unicast prefixes
     */
    private List<tabRouteEntry<addrIP>> currUni = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating multicast prefixes
     */
    private List<tabRouteEntry<addrIP>> currMlt = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other unicast prefixes
     */
    private List<tabRouteEntry<addrIP>> currOuni = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other multicast prefixes
     */
    private List<tabRouteEntry<addrIP>> currOmlt = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other flowspec prefixes
     */
    private List<tabRouteEntry<addrIP>> currOflw = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other srte prefixes
     */
    private List<tabRouteEntry<addrIP>> currOsrt = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating flowspec prefixes
     */
    private List<tabRouteEntry<addrIP>> currFlw = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating vpn uni prefixes
     */
    private List<tabRouteEntry<addrIP>> currVpnU = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating vpn multi prefixes
     */
    private List<tabRouteEntry<addrIP>> currVpnM = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating vpn flow prefixes
     */
    private List<tabRouteEntry<addrIP>> currVpnF = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other vpn uni prefixes
     */
    private List<tabRouteEntry<addrIP>> currVpoU = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other vpn multi prefixes
     */
    private List<tabRouteEntry<addrIP>> currVpoM = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other vpn flow prefixes
     */
    private List<tabRouteEntry<addrIP>> currVpoF = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating vpls prefixes
     */
    private List<tabRouteEntry<addrIP>> currVpls = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating mspw prefixes
     */
    private List<tabRouteEntry<addrIP>> currMspw = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating evpn prefixes
     */
    private List<tabRouteEntry<addrIP>> currEvpn = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating mdt prefixes
     */
    private List<tabRouteEntry<addrIP>> currMdt = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating nsh prefixes
     */
    private List<tabRouteEntry<addrIP>> currNsh = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating rpd prefixes
     */
    private List<tabRouteEntry<addrIP>> currRpd = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating spf prefixes
     */
    private List<tabRouteEntry<addrIP>> currSpf = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating rtfilter prefixes
     */
    private List<tabRouteEntry<addrIP>> currRtf = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating srte prefixes
     */
    private List<tabRouteEntry<addrIP>> currSrte = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating linkstate prefixes
     */
    private List<tabRouteEntry<addrIP>> currLnks = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating mvpn prefixes
     */
    private List<tabRouteEntry<addrIP>> currMvpn = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other mvpn prefixes
     */
    private List<tabRouteEntry<addrIP>> currMvpo = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating mtree prefixes
     */
    private List<tabRouteEntry<addrIP>> currMtre = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other mtree prefixes
     */
    private List<tabRouteEntry<addrIP>> currMtro = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently changed prefixes
     */
    public int currChg;

    /**
     * addpath beginning
     */
    private int addpathBeg;

    /**
     * peer hold time
     */
    public int peerHold;

    /**
     * negotiated keepalive time
     */
    public int peerKeep;

    /**
     * peer router id
     */
    public addrIPv4 peerRouterID = new addrIPv4();

    /**
     * peer 32bit asn capability
     */
    public boolean peer32bitAS;

    /**
     * peer address families
     */
    public long peerAfis;

    /**
     * peer dynamic capability exchange
     */
    public boolean peerDynCap;

    /**
     * dynamic capabilities sent
     */
    public int dynCapaTx;

    /**
     * dynamic capabilities received
     */
    public int dynCapaRx;

    /**
     * peer route refresh capability
     */
    public boolean peerRefreshOld;

    /**
     * peer route refresh capability
     */
    public boolean peerRefreshNew;

    /**
     * peer extended open capability
     */
    public boolean peerExtOpen;

    /**
     * peer extended message capability
     */
    public boolean peerExtUpd;

    /**
     * route refresh sent
     */
    public int refreshTx;

    /**
     * route refresh received
     */
    public int refreshRx;

    /**
     * eor needs in address families
     */
    public long needEorAfis;

    /**
     * eof needs in address families
     */
    public long needEofAfis;

    /**
     * peer graceful restart capability
     */
    public long peerGrace;

    /**
     * peer long lived graceful restart capability
     */
    public long peerLlGrace;

    /**
     * peer multiple labels capability
     */
    public long peerMltLab;

    /**
     * peer extended nexthop capability
     */
    public long peerExtNextCur;

    /**
     * peer extended nexthop capability
     */
    public long peerExtNextOtr;

    /**
     * peer leak prevention role capability
     */
    public int peerLeakRole;

    /**
     * peer hostname capability
     */
    public String peerHostname;

    /**
     * peer software capability
     */
    public String peerSoftware;

    /**
     * peer domain capability
     */
    public String peerDomainname;

    /**
     * peer sends additional paths
     */
    public long addpathRx;

    /**
     * peer receives additional paths
     */
    public long addpathTx;

    /**
     * strict bfd mode
     */
    public boolean strictBfd;

    /**
     * compressor transmitter
     */
    public Deflater compressTx;

    /**
     * compressor receiver
     */
    public Inflater[] compressRx;

    /**
     * compressor counter
     */
    public counter compressCntr = new counter();

    /**
     * unknown counter
     */
    public counter unknownCntr = new counter();

    /**
     * policy rejected prefixes
     */
    public int repPolRej;

    /**
     * aspath loop prefixes
     */
    public int repAsPath;

    /**
     * confed loop prefixes
     */
    public int repAsConf;

    /**
     * originator loop prefixes
     */
    public int repOrgnId;

    /**
     * cluster loop prefixes
     */
    public int repClstrL;

    /**
     * safi list sent by the peer
     */
    public long originalSafiList;

    /**
     * addpath list sent by the peer
     */
    public long originalAddRlist;

    /**
     * addpath list sent by the peer
     */
    public long originalAddTlist;

    private packHolder pckRx = new packHolder(true, true);

    private packHolder pckRh = new packHolder(true, true);

    private packHolder pckTx = new packHolder(true, true);

    private packHolder pckTh = new packHolder(true, true);

    /**
     * create bgp speaker
     *
     * @param protocol process
     * @param neighbor neighbor
     * @param socket tcp socket to use, null if none
     * @param res resume
     */
    public rtrBgpSpeak(rtrBgp protocol, rtrBgpNeigh neighbor, pipeSide socket, boolean res) {
        ready2adv = false;
        resumed = res;
        addpathBeg = bits.randomD();
        parent = protocol;
        neigh = neighbor;
        pipe = socket;
        peerLeakRole = -1;
        if (pipe == null) {
            return;
        }
        upTime = bits.getTime();
        lastRx = upTime;
        if (parent == null) {
            return;
        }
        new Thread(this).start();
    }

    private boolean afiMsk(long val, long safi) {
        if (safi == parent.afiUni) {
            return (val & rtrBgpParam.mskUni) != 0;
        }
        if (safi == parent.afiLab) {
            return (val & rtrBgpParam.mskLab) != 0;
        }
        if (safi == parent.afiCtp) {
            return (val & rtrBgpParam.mskCtp) != 0;
        }
        if (safi == parent.afiCar) {
            return (val & rtrBgpParam.mskCar) != 0;
        }
        if (safi == parent.afiMlt) {
            return (val & rtrBgpParam.mskMlt) != 0;
        }
        if (safi == parent.afiOlab) {
            return (val & rtrBgpParam.mskOlab) != 0;
        }
        if (safi == parent.afiOctp) {
            return (val & rtrBgpParam.mskOctp) != 0;
        }
        if (safi == parent.afiOcar) {
            return (val & rtrBgpParam.mskOcar) != 0;
        }
        if (safi == parent.afiOuni) {
            return (val & rtrBgpParam.mskOuni) != 0;
        }
        if (safi == parent.afiOmlt) {
            return (val & rtrBgpParam.mskOmlt) != 0;
        }
        if (safi == parent.afiOflw) {
            return (val & rtrBgpParam.mskOflw) != 0;
        }
        if (safi == parent.afiOsrt) {
            return (val & rtrBgpParam.mskOsrt) != 0;
        }
        if (safi == parent.afiFlw) {
            return (val & rtrBgpParam.mskFlw) != 0;
        }
        if (safi == parent.afiVpnU) {
            return (val & rtrBgpParam.mskVpnU) != 0;
        }
        if (safi == parent.afiVpnM) {
            return (val & rtrBgpParam.mskVpnM) != 0;
        }
        if (safi == parent.afiVpnF) {
            return (val & rtrBgpParam.mskVpnF) != 0;
        }
        if (safi == parent.afiVpoU) {
            return (val & rtrBgpParam.mskVpoU) != 0;
        }
        if (safi == parent.afiVpoM) {
            return (val & rtrBgpParam.mskVpoM) != 0;
        }
        if (safi == parent.afiVpoF) {
            return (val & rtrBgpParam.mskVpoF) != 0;
        }
        if (safi == parent.afiVpls) {
            return (val & rtrBgpParam.mskVpls) != 0;
        }
        if (safi == parent.afiMspw) {
            return (val & rtrBgpParam.mskMspw) != 0;
        }
        if (safi == parent.afiEvpn) {
            return (val & rtrBgpParam.mskEvpn) != 0;
        }
        if (safi == parent.afiMdt) {
            return (val & rtrBgpParam.mskMdt) != 0;
        }
        if (safi == parent.afiNsh) {
            return (val & rtrBgpParam.mskNsh) != 0;
        }
        if (safi == parent.afiRpd) {
            return (val & rtrBgpParam.mskRpd) != 0;
        }
        if (safi == parent.afiSpf) {
            return (val & rtrBgpParam.mskSpf) != 0;
        }
        if (safi == parent.afiRtf) {
            return (val & rtrBgpParam.mskRtf) != 0;
        }
        if (safi == parent.afiSrte) {
            return (val & rtrBgpParam.mskSrte) != 0;
        }
        if (safi == parent.afiLnks) {
            return (val & rtrBgpParam.mskLnks) != 0;
        }
        if (safi == parent.afiMvpn) {
            return (val & rtrBgpParam.mskMvpn) != 0;
        }
        if (safi == parent.afiMvpo) {
            return (val & rtrBgpParam.mskMvpo) != 0;
        }
        if (safi == parent.afiMtre) {
            return (val & rtrBgpParam.mskMtre) != 0;
        }
        if (safi == parent.afiMtro) {
            return (val & rtrBgpParam.mskMtro) != 0;
        }
        return false;
    }

    /**
     * get learned
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getLearned(int safi) {
        if (safi == parent.afiUni) {
            return lrnUni;
        }
        if (safi == parent.afiLab) {
            return lrnUni;
        }
        if (safi == parent.afiCtp) {
            return lrnUni;
        }
        if (safi == parent.afiCar) {
            return lrnUni;
        }
        if (safi == parent.afiMlt) {
            return lrnMlt;
        }
        if (safi == parent.afiOlab) {
            return lrnOuni;
        }
        if (safi == parent.afiOctp) {
            return lrnOuni;
        }
        if (safi == parent.afiOcar) {
            return lrnOuni;
        }
        if (safi == parent.afiOuni) {
            return lrnOuni;
        }
        if (safi == parent.afiOmlt) {
            return lrnOmlt;
        }
        if (safi == parent.afiOflw) {
            return lrnOflw;
        }
        if (safi == parent.afiOsrt) {
            return lrnOsrt;
        }
        if (safi == parent.afiFlw) {
            return lrnFlw;
        }
        if (safi == parent.afiVpnU) {
            return lrnVpnU;
        }
        if (safi == parent.afiVpnM) {
            return lrnVpnM;
        }
        if (safi == parent.afiVpnF) {
            return lrnVpnF;
        }
        if (safi == parent.afiVpoU) {
            return lrnVpoU;
        }
        if (safi == parent.afiVpoM) {
            return lrnVpoM;
        }
        if (safi == parent.afiVpoF) {
            return lrnVpoF;
        }
        if (safi == parent.afiVpls) {
            return lrnVpls;
        }
        if (safi == parent.afiMspw) {
            return lrnMspw;
        }
        if (safi == parent.afiEvpn) {
            return lrnEvpn;
        }
        if (safi == parent.afiMdt) {
            return lrnMdt;
        }
        if (safi == parent.afiNsh) {
            return lrnNsh;
        }
        if (safi == parent.afiRpd) {
            return lrnRpd;
        }
        if (safi == parent.afiSpf) {
            return lrnSpf;
        }
        if (safi == parent.afiRtf) {
            return lrnRtf;
        }
        if (safi == parent.afiSrte) {
            return lrnSrte;
        }
        if (safi == parent.afiLnks) {
            return lrnLnks;
        }
        if (safi == parent.afiMvpn) {
            return lrnMvpn;
        }
        if (safi == parent.afiMvpo) {
            return lrnMvpo;
        }
        if (safi == parent.afiMtre) {
            return lrnMtre;
        }
        if (safi == parent.afiMtro) {
            return lrnMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * get adverted
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getAdverted(int safi) {
        if (safi == parent.afiUni) {
            return advUni;
        }
        if (safi == parent.afiLab) {
            return advUni;
        }
        if (safi == parent.afiCtp) {
            return advUni;
        }
        if (safi == parent.afiCar) {
            return advUni;
        }
        if (safi == parent.afiMlt) {
            return advMlt;
        }
        if (safi == parent.afiOlab) {
            return advOuni;
        }
        if (safi == parent.afiOctp) {
            return advOuni;
        }
        if (safi == parent.afiOcar) {
            return advOuni;
        }
        if (safi == parent.afiOuni) {
            return advOuni;
        }
        if (safi == parent.afiOmlt) {
            return advOmlt;
        }
        if (safi == parent.afiOflw) {
            return advOflw;
        }
        if (safi == parent.afiOsrt) {
            return advOsrt;
        }
        if (safi == parent.afiFlw) {
            return advFlw;
        }
        if (safi == parent.afiVpnU) {
            return advVpnU;
        }
        if (safi == parent.afiVpnM) {
            return advVpnM;
        }
        if (safi == parent.afiVpnF) {
            return advVpnF;
        }
        if (safi == parent.afiVpoU) {
            return advVpoU;
        }
        if (safi == parent.afiVpoM) {
            return advVpoM;
        }
        if (safi == parent.afiVpoF) {
            return advVpoF;
        }
        if (safi == parent.afiVpls) {
            return advVpls;
        }
        if (safi == parent.afiMspw) {
            return advMspw;
        }
        if (safi == parent.afiEvpn) {
            return advEvpn;
        }
        if (safi == parent.afiMdt) {
            return advMdt;
        }
        if (safi == parent.afiNsh) {
            return advNsh;
        }
        if (safi == parent.afiRpd) {
            return advRpd;
        }
        if (safi == parent.afiSpf) {
            return advSpf;
        }
        if (safi == parent.afiRtf) {
            return advRtf;
        }
        if (safi == parent.afiSrte) {
            return advSrte;
        }
        if (safi == parent.afiLnks) {
            return advLnks;
        }
        if (safi == parent.afiMvpn) {
            return advMvpn;
        }
        if (safi == parent.afiMvpo) {
            return advMvpo;
        }
        if (safi == parent.afiMtre) {
            return advMtre;
        }
        if (safi == parent.afiMtro) {
            return advMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * addpath rx test
     *
     * @param safi safi to test
     * @return true if addpath in use
     */
    public boolean addPthRx(int safi) {
        return afiMsk(addpathRx, safi);
    }

    /**
     * addpath tx test
     *
     * @param safi safi to test
     * @return true if addpath in use
     */
    public boolean addPthTx(int safi) {
        return afiMsk(addpathTx, safi);
    }

    /**
     * close this session
     */
    public void closeNow() {
        neigh.delListenPeer();
        if (pipe != null) {
            pipe.setClose();
        }
        if (neigh.localIfc != null) {
            neigh.localIfc.bfdDel(neigh.peerAddr, this);
        }
        lrnUni.clear();
        lrnMlt.clear();
        lrnOuni.clear();
        lrnOmlt.clear();
        lrnOflw.clear();
        lrnOsrt.clear();
        lrnFlw.clear();
        lrnVpnU.clear();
        lrnVpnM.clear();
        lrnVpnF.clear();
        lrnVpoU.clear();
        lrnVpoM.clear();
        lrnVpoF.clear();
        lrnVpls.clear();
        lrnMspw.clear();
        lrnEvpn.clear();
        lrnMdt.clear();
        lrnNsh.clear();
        lrnRpd.clear();
        lrnSpf.clear();
        lrnRtf.clear();
        lrnSrte.clear();
        lrnLnks.clear();
        lrnMvpn.clear();
        lrnMvpo.clear();
        lrnMtre.clear();
        lrnMtro.clear();
        advUni.clear();
        advMlt.clear();
        advOuni.clear();
        advOmlt.clear();
        advOflw.clear();
        advOsrt.clear();
        advFlw.clear();
        advVpnU.clear();
        advVpnM.clear();
        advVpnF.clear();
        advVpoU.clear();
        advVpoM.clear();
        advVpoF.clear();
        advVpls.clear();
        advMspw.clear();
        advEvpn.clear();
        advMdt.clear();
        advNsh.clear();
        advRpd.clear();
        advSpf.clear();
        advRtf.clear();
        advSrte.clear();
        advLnks.clear();
        advMvpn.clear();
        advMvpo.clear();
        advMtre.clear();
        advMtro.clear();
        neigh.accUni = new tabRoute<addrIP>("rx");
        neigh.accMlt = new tabRoute<addrIP>("rx");
        neigh.accOuni = new tabRoute<addrIP>("rx");
        neigh.accOmlt = new tabRoute<addrIP>("rx");
        neigh.accOflw = new tabRoute<addrIP>("rx");
        neigh.accOsrt = new tabRoute<addrIP>("rx");
        neigh.accFlw = new tabRoute<addrIP>("rx");
        neigh.accVpnU = new tabRoute<addrIP>("rx");
        neigh.accVpnM = new tabRoute<addrIP>("rx");
        neigh.accVpnF = new tabRoute<addrIP>("rx");
        neigh.accVpoU = new tabRoute<addrIP>("rx");
        neigh.accVpoM = new tabRoute<addrIP>("rx");
        neigh.accVpoF = new tabRoute<addrIP>("rx");
        neigh.accVpls = new tabRoute<addrIP>("rx");
        neigh.accMspw = new tabRoute<addrIP>("rx");
        neigh.accEvpn = new tabRoute<addrIP>("rx");
        neigh.accMdt = new tabRoute<addrIP>("rx");
        neigh.accNsh = new tabRoute<addrIP>("rx");
        neigh.accRpd = new tabRoute<addrIP>("rx");
        neigh.accSpf = new tabRoute<addrIP>("rx");
        neigh.accRtf = new tabRoute<addrIP>("rx");
        neigh.accSrte = new tabRoute<addrIP>("rx");
        neigh.accLnks = new tabRoute<addrIP>("rx");
        neigh.accMvpn = new tabRoute<addrIP>("rx");
        neigh.accMvpo = new tabRoute<addrIP>("rx");
        neigh.accMtre = new tabRoute<addrIP>("rx");
        neigh.accMtro = new tabRoute<addrIP>("rx");
        neigh.wilUni = new tabRoute<addrIP>("rx");
        neigh.wilMlt = new tabRoute<addrIP>("rx");
        neigh.wilOuni = new tabRoute<addrIP>("rx");
        neigh.wilOmlt = new tabRoute<addrIP>("rx");
        neigh.wilOflw = new tabRoute<addrIP>("rx");
        neigh.wilOsrt = new tabRoute<addrIP>("rx");
        neigh.wilFlw = new tabRoute<addrIP>("rx");
        neigh.wilVpnU = new tabRoute<addrIP>("rx");
        neigh.wilVpnM = new tabRoute<addrIP>("rx");
        neigh.wilVpnF = new tabRoute<addrIP>("rx");
        neigh.wilVpoU = new tabRoute<addrIP>("rx");
        neigh.wilVpoM = new tabRoute<addrIP>("rx");
        neigh.wilVpoF = new tabRoute<addrIP>("rx");
        neigh.wilVpls = new tabRoute<addrIP>("rx");
        neigh.wilMspw = new tabRoute<addrIP>("rx");
        neigh.wilEvpn = new tabRoute<addrIP>("rx");
        neigh.wilMdt = new tabRoute<addrIP>("rx");
        neigh.wilNsh = new tabRoute<addrIP>("rx");
        neigh.wilRpd = new tabRoute<addrIP>("rx");
        neigh.wilSpf = new tabRoute<addrIP>("rx");
        neigh.wilRtf = new tabRoute<addrIP>("rx");
        neigh.wilSrte = new tabRoute<addrIP>("rx");
        neigh.wilLnks = new tabRoute<addrIP>("rx");
        neigh.wilMvpn = new tabRoute<addrIP>("rx");
        neigh.wilMvpo = new tabRoute<addrIP>("rx");
        neigh.wilMtre = new tabRoute<addrIP>("rx");
        neigh.wilMtro = new tabRoute<addrIP>("rx");
        neigh.chgUni = new tabRoute<addrIP>("chg");
        neigh.chgMlt = new tabRoute<addrIP>("chg");
        neigh.chgOuni = new tabRoute<addrIP>("chg");
        neigh.chgOmlt = new tabRoute<addrIP>("chg");
        neigh.chgOflw = new tabRoute<addrIP>("chg");
        neigh.chgOsrt = new tabRoute<addrIP>("chg");
        neigh.chgFlw = new tabRoute<addrIP>("chg");
        neigh.chgVpnU = new tabRoute<addrIP>("chg");
        neigh.chgVpnM = new tabRoute<addrIP>("chg");
        neigh.chgVpnF = new tabRoute<addrIP>("chg");
        neigh.chgVpoU = new tabRoute<addrIP>("chg");
        neigh.chgVpoM = new tabRoute<addrIP>("chg");
        neigh.chgVpoF = new tabRoute<addrIP>("chg");
        neigh.chgVpls = new tabRoute<addrIP>("chg");
        neigh.chgMspw = new tabRoute<addrIP>("chg");
        neigh.chgEvpn = new tabRoute<addrIP>("chg");
        neigh.chgMdt = new tabRoute<addrIP>("chg");
        neigh.chgNsh = new tabRoute<addrIP>("chg");
        neigh.chgRpd = new tabRoute<addrIP>("chg");
        neigh.chgSpf = new tabRoute<addrIP>("chg");
        neigh.chgRtf = new tabRoute<addrIP>("chg");
        neigh.chgSrte = new tabRoute<addrIP>("chg");
        neigh.chgLnks = new tabRoute<addrIP>("chg");
        neigh.chgMvpn = new tabRoute<addrIP>("chg");
        neigh.chgMvpo = new tabRoute<addrIP>("chg");
        neigh.chgMtre = new tabRoute<addrIP>("chg");
        neigh.chgMtro = new tabRoute<addrIP>("chg");
        if (!ready2adv) {
            return;
        }
        ready2adv = false;
        adversion.set(0);
        needFull.set(3);
        if (debugger.rtrBgpFull) {
            logger.debug("neighbor down");
        }
        parent.needFull.add(1);
        parent.compute.wakeup();
    }

    /**
     * stop work
     */
    public void bfdPeerDown() {
        closeNow();
    }

    /**
     * check tx buffer size
     *
     * @return less than 0 means error, otherwise bytes free
     */
    public int txFree() {
        if (pipe == null) {
            return -1;
        }
        if (pipe.isClosed() != 0) {
            return -1;
        }
        if (!ready2adv) {
            return 0;
        }
        return pipe.ready2tx();
    }

    /**
     * check rx buffer size
     *
     * @return less than 0 means error, otherwise bytes ready
     */
    public int rxReady() {
        if (pipe == null) {
            return -1;
        }
        if (pipe.isClosed() != 0) {
            return -1;
        }
        if (!ready2adv) {
            return 0;
        }
        return pipe.ready2rx();
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        closeNow();
    }

    private void doWork() {
        if (debugger.rtrBgpEvnt) {
            logger.debug("starting neighbor " + neigh.peerAddr);
        }
        pipe.setTime(neigh.holdTimer);
        peerKeep = neigh.keepAlive;
        pipe.setReady();
        pipe.wait4ready(neigh.holdTimer);
        if (pipe.isReady() != 3) {
            closeNow();
            return;
        }
        if (resumed) {
            if (packScan()) {
                closeNow();
                return;
            }
            for (int i = 0; i < 63; i++) {
                long p = 1L << i;
                if ((p & peerAfis) == 0) {
                    continue;
                }
                int o = parent.mask2safi(p);
                sendRefresh(o);
                gotRefresh(o);
            }
        } else {
            sendOpen();
            sendKeepAlive();
            if (neigh.ipInfoCfg != null) {
                secInfoCls cls = new secInfoCls(null, null, null, parent.fwdCore, neigh.peerAddr, prtTcp.protoNum, neigh.localAddr);
                ipInfoRes = new secInfoWrk(neigh.ipInfoCfg, cls);
                ipInfoRes.doWork(false);
                if (ipInfoRes.need2drop()) {
                    logger.error("pmtud failed to " + neigh.peerAddr);
                    sendNotify(1, 2);
                    closeNow();
                    return;
                }
            }
            int typ = packRecv(pckRx);
            if (typ == rtrBgpUtil.msgNotify) {
                logger.info("got notify " + rtrBgpUtil.notify2string(pckRx.getByte(0), pckRx.getByte(1)) + " from " + neigh.peerAddr);
                closeNow();
                return;
            }
            if (typ != rtrBgpUtil.msgOpen) {
                logger.info("got " + rtrBgpUtil.msgType2string(typ) + " from " + neigh.peerAddr);
                sendNotify(1, 3);
                return;
            }
            if (parseOpen(pckRx)) {
                closeNow();
                return;
            }
        }
        if (neigh.monitor != null) {
            neigh.monitor.gotEvent(true, this, neigh);
        }
        if (neigh.bfdTrigger != 0) {
            neigh.localIfc.bfdAdd(neigh.peerAddr, this, "bgp");
        }
        if (strictBfd && (neigh.bfdTrigger == 2)) {
            if (debugger.rtrBgpEvnt) {
                logger.debug("starting bfd " + neigh.peerAddr);
            }
            if (neigh.localIfc.bfdWait(neigh.peerAddr, neigh.holdTimer)) {
                logger.error("neighbor " + neigh.peerAddr + " bfd timeout");
                return;
            }
        }
        lrnUni.clear();
        lrnMlt.clear();
        lrnOuni.clear();
        lrnOmlt.clear();
        lrnOflw.clear();
        lrnOsrt.clear();
        lrnFlw.clear();
        lrnVpnU.clear();
        lrnVpnM.clear();
        lrnVpnF.clear();
        lrnVpoU.clear();
        lrnVpoM.clear();
        lrnVpoF.clear();
        lrnVpls.clear();
        lrnMspw.clear();
        lrnEvpn.clear();
        lrnMdt.clear();
        lrnNsh.clear();
        lrnRpd.clear();
        lrnSpf.clear();
        lrnRtf.clear();
        lrnSrte.clear();
        lrnLnks.clear();
        lrnMvpn.clear();
        lrnMvpo.clear();
        lrnMtre.clear();
        lrnMtro.clear();
        advUni.clear();
        advMlt.clear();
        advOuni.clear();
        advOmlt.clear();
        advOflw.clear();
        advOsrt.clear();
        advFlw.clear();
        advVpnU.clear();
        advVpnM.clear();
        advVpnF.clear();
        advVpoU.clear();
        advVpoM.clear();
        advVpoF.clear();
        advVpls.clear();
        advMspw.clear();
        advEvpn.clear();
        advMdt.clear();
        advNsh.clear();
        advRpd.clear();
        advSpf.clear();
        advRtf.clear();
        advSrte.clear();
        advLnks.clear();
        advMvpn.clear();
        advMvpo.clear();
        advMtre.clear();
        advMtro.clear();
        neigh.accUni = new tabRoute<addrIP>("rx");
        neigh.accMlt = new tabRoute<addrIP>("rx");
        neigh.accOuni = new tabRoute<addrIP>("rx");
        neigh.accOmlt = new tabRoute<addrIP>("rx");
        neigh.accOflw = new tabRoute<addrIP>("rx");
        neigh.accOsrt = new tabRoute<addrIP>("rx");
        neigh.accFlw = new tabRoute<addrIP>("rx");
        neigh.accVpnU = new tabRoute<addrIP>("rx");
        neigh.accVpnM = new tabRoute<addrIP>("rx");
        neigh.accVpnF = new tabRoute<addrIP>("rx");
        neigh.accVpoU = new tabRoute<addrIP>("rx");
        neigh.accVpoM = new tabRoute<addrIP>("rx");
        neigh.accVpoF = new tabRoute<addrIP>("rx");
        neigh.accVpls = new tabRoute<addrIP>("rx");
        neigh.accMspw = new tabRoute<addrIP>("rx");
        neigh.accEvpn = new tabRoute<addrIP>("rx");
        neigh.accMdt = new tabRoute<addrIP>("rx");
        neigh.accNsh = new tabRoute<addrIP>("rx");
        neigh.accRpd = new tabRoute<addrIP>("rx");
        neigh.accSpf = new tabRoute<addrIP>("rx");
        neigh.accRtf = new tabRoute<addrIP>("rx");
        neigh.accSrte = new tabRoute<addrIP>("rx");
        neigh.accLnks = new tabRoute<addrIP>("rx");
        neigh.accMvpn = new tabRoute<addrIP>("rx");
        neigh.accMvpo = new tabRoute<addrIP>("rx");
        neigh.accMtre = new tabRoute<addrIP>("rx");
        neigh.accMtro = new tabRoute<addrIP>("rx");
        if (neigh.dampenPfxs != null) {
            neigh.dampenPfxs = new tabGen<rtrBgpDamp>();
        }
        ready2adv = true;
        neigh.sessNum++;
        logger.warn("neighbor " + neigh.peerAddr + " up");
        if (debugger.rtrBgpFull) {
            logger.debug("neighbor up");
        }
        parent.needFull.add(1);
        parent.compute.wakeup();
        for (;;) {
            if (neigh.advertIntRx > 0) {
                bits.sleep(neigh.advertIntRx);
            }
            int typ = packRecv(pckRx);
            if (typ < 0) {
                sendNotify(1, 1);
                break;
            }
            lastRx = bits.getTime();
            if (typ == rtrBgpUtil.msgKeepLiv) {
                continue;
            }
            if (typ == rtrBgpUtil.msgNotify) {
                logger.info("got notify " + rtrBgpUtil.notify2string(pckRx.getByte(0), pckRx.getByte(1)) + " from " + neigh.peerAddr);
                break;
            }
            if (typ == rtrBgpUtil.msgRefrsh) {
                gotRefresh(pckRx.msbGetD(0));
                continue;
            }
            if (typ == rtrBgpUtil.msgUpdate) {
                if (parseUpdate(pckRx, pckRh)) {
                    logger.info("got malformed update from " + neigh.peerAddr);
                    sendNotify(3, 1);
                    break;
                }
                continue;
            }
            if (typ == rtrBgpUtil.msgCapability) {
                dynCapaRx++;
                if (!neigh.dynamicCapab) {
                    sendNotify(8, 4);
                    break;
                }
                int i = pckRx.getByte(0);
                boolean init = (i & 0x80) == 0;
                boolean ack = (i & 0x40) != 0;
                boolean add = (i & 0x1) == 0;
                int seq = pckRx.msbGetD(1);
                if (debugger.rtrBgpTraf) {
                    logger.debug("got dynamic capability from peer " + neigh.peerAddr + " init=" + init + " ack=" + ack + " add=" + add + " seq=" + seq);
                }
                if (!init) {
                    continue;
                }
                pckRx.getSkip(5);
                encTlv tlv = rtrBgpUtil.getCapabilityTlv(peerExtOpen);
                if (tlv.getBytes(pckRx)) {
                    sendNotify(8, 2);
                    break;
                }
                if (tlv.valTyp != 2) {
                    sendNotify(8, 3);
                    break;
                }
                pckRh.clear();
                pckRh.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
                pckRh.putSkip(tlv.valSiz);
                pckRh.merge2beg();
                List<Integer> afiS = new ArrayList<Integer>();
                List<Long> afiM = new ArrayList<Long>();
                List<Integer> afiD = new ArrayList<Integer>();
                for (;;) {
                    tlv = rtrBgpUtil.getCapabilityTlv(false);
                    if (tlv.getBytes(pckRh)) {
                        break;
                    }
                    if (tlv.valTyp != rtrBgpUtil.capaMultiProto) {
                        sendNotify(8, 4);
                        break;
                    }
                    parseMultiProtoCapa(tlv, afiS, afiM);
                }
                for (i = afiS.size() - 1; i >= 0; i--) {
                    int o = afiS.get(i);
                    long p = afiM.get(i);
                    if (afiMsk(peerAfis, o) == add) {
                        renegotiatingSafi(p, o, add, false);
                        continue;
                    }
                    afiS.remove(i);
                    afiM.remove(i);
                    renegotiatingSafi(p, o, add, false);
                }
                packHolder pck = new packHolder(true, true);
                for (i = 0; i < afiS.size(); i++) {
                    byte[] buf = new byte[4];
                    int o = afiS.get(i);
                    bits.msbPutD(buf, 0, o);
                    rtrBgpUtil.placeCapability(pck, peerExtOpen, rtrBgpUtil.capaMultiProto, buf);
                }
                if (ack) {
                    sendDynCapaMsg(!init, false, add, seq, pck);
                }
                for (i = 0; i < afiS.size(); i++) {
                    int o = afiS.get(i);
                    long p = afiM.get(i);
                    renegotiatingSafi(p, o, add, false);
                }
                continue;
            }
            if (typ != rtrBgpUtil.msgCompress) {
                logger.info("got unknown type (" + typ + ") from " + neigh.peerAddr);
                sendNotify(1, 3);
                break;
            }
            if (compressRx == null) {
                logger.info("got unwanted compressed from " + neigh.peerAddr);
                sendNotify(6, 10);
                break;
            }
            int flg = pckRx.getByte(0);
            pckRx.getSkip(1);
            byte[] buf = pckRx.getCopy();
            if ((flg & 0x40) != 0) { // overflow
                typ = packRecv(pckRx);
                if (typ != rtrBgpUtil.msgCompress) {
                    logger.info("missing overflow (" + typ + ") from " + neigh.peerAddr);
                    sendNotify(6, 10);
                    break;
                }
                pckRx.getSkip(1);
                buf = bits.byteConcat(buf, pckRx.getCopy());
            }
            Inflater comp = compressRx[flg & 7]; // id
            if ((flg & 0x80) != 0) { // reset
                comp.reset();
            }
            comp.setInput(buf);
            buf = new byte[1 << (((flg >>> 3) & 7) + 11)];
            try {
                flg = comp.inflate(buf);
            } catch (Exception e) {
                logger.info("error decompressing from " + neigh.peerAddr);
                sendNotify(6, 10);
                break;
            }
            int pos;
            for (pos = 0; pos < flg;) {
                int len = bits.msbGetW(buf, pos) - rtrBgpUtil.sizeU;
                if (len < 0) {
                    break;
                }
                typ = buf[pos + 2] & 0xff;
                pos += rtrBgpUtil.sizeC;
                if ((pos + len) > flg) {
                    logger.info("got truncated compressed from " + neigh.peerAddr);
                    sendNotify(6, 10);
                    break;
                }
                pckRx.clear();
                pckRx.putCopy(buf, pos, 0, len);
                pckRx.putSkip(len);
                pckRx.merge2beg();
                pos += len;
                compressCntr.rx(pckRx);
                if (debugger.rtrBgpEvnt) {
                    logger.debug("got " + rtrBgpUtil.msgType2string(typ) + " from " + neigh.peerAddr);
                }
                if (neigh.monitor != null) {
                    neigh.monitor.gotMessage(false, typ, this, neigh, pckRx.getCopy());
                }
                if (neigh.dump != null) {
                    neigh.dump.gotMessage(false, typ, neigh, pckRx.getCopy());
                }
                if (typ == rtrBgpUtil.msgKeepLiv) {
                    continue;
                }
                if (typ == rtrBgpUtil.msgNotify) {
                    logger.info("got compressed notify " + rtrBgpUtil.notify2string(pckRx.getByte(0), pckRx.getByte(1)) + " from " + neigh.peerAddr);
                    break;
                }
                if (typ == rtrBgpUtil.msgRefrsh) {
                    gotRefresh(pckRx.msbGetD(0));
                    continue;
                }
                if (typ != rtrBgpUtil.msgUpdate) {
                    logger.info("got unknown compressed type (" + typ + ") from " + neigh.peerAddr);
                    sendNotify(1, 3);
                    break;
                }
                updateMsgCtr(pckRx, typ, false);
                if (parseUpdate(pckRx, pckRh)) {
                    logger.info("got malformed compressed update from " + neigh.peerAddr);
                    sendNotify(3, 1);
                    break;
                }
            }
            if (pos != flg) {
                logger.info("got compressed garbage (" + (flg - pos) + ") from " + neigh.peerAddr);
                sendNotify(6, 10);
                break;
            }
        }
        closeNow();
        if (neigh.monitor != null) {
            neigh.monitor.gotEvent(false, this, neigh);
        }
        logger.error("neighbor " + neigh.peerAddr + " down");
    }

    /**
     * update transmit message statistics
     *
     * @param pck packet to use
     * @param typ type to use
     * @param dir direction, true=tx, false=rx
     */
    protected void updateMsgCtr(packHolder pck, int typ, boolean dir) {
        if (parent != null) {
            rtrBgpUtil.updtStatsArr(dir, parent.msgStats, typ, pck);
        }
        if (neigh != null) {
            rtrBgpUtil.updtStatsArr(dir, neigh.msgStats, typ, pck);
        }
        if (!rtrBgpUtil.isUnknownMsg(typ)) {
            return;
        }
        if (neigh == null) {
            return;
        }
        if (neigh.unknownsLog) {
            logger.info((dir ? "sent" : "got") + " unknowns (" + typ + ") message " + neigh.peerAddr + " -> " + neigh.localAddr + " " + pck.dump());
        }
        if (neigh.unknownsColl == null) {
            return;
        }
        packHolder cpy = new packHolder(true, true);
        int ofs = cpy.dataSize() + cpy.dataOffset();
        cpy.copyFrom(pck, true, true);
        cpy.setDataSize(ofs);
        cpy.getSkip(16);
        neigh.unknownsColl.gotMessage(dir, rtrBgpUtil.msgUpdate, neigh, cpy.getCopy());
    }

    /**
     * update transmit attribute statistics
     *
     * @param dir direction, true=tx, false=rx
     * @param pck packet to use
     * @param typ type to use
     */
    protected void updateAttrCtr(boolean dir, packHolder pck, int typ) {
        if (parent != null) {
            rtrBgpUtil.updtStatsArr(dir, parent.attrStats, typ, pck);
        }
        if (neigh != null) {
            rtrBgpUtil.updtStatsArr(dir, neigh.attrStats, typ, pck);
        }
        if (!rtrBgpUtil.isUnknownAttr(typ)) {
            return;
        }
        if (neigh == null) {
            return;
        }
        if (neigh.unknownsLog) {
            logger.info((dir ? "sent" : "got") + " unknowns (" + typ + ") attributes " + neigh.peerAddr + " -> " + neigh.localAddr + " " + pck.dump());
        }
    }

    /**
     * update prefix reachable statistics
     *
     * @param dir direction, 0=rx-reach, 1=tx-reach, 2=tx-unreach, 3=rx-unreach
     * @param pck packet to use
     */
    protected void updateRchblCntr(int dir, packHolder pck) {
        switch (dir) {
            case 0:
                if (parent != null) {
                    parent.reachabStat.rx(pck);
                }
                if (neigh != null) {
                    neigh.reachabStat.rx(pck);
                }
                break;
            case 1:
                if (parent != null) {
                    parent.reachabStat.tx(pck);
                }
                if (neigh != null) {
                    neigh.reachabStat.tx(pck);
                }
                break;
            case 2:
                if (parent != null) {
                    parent.unreachStat.rx(pck);
                }
                if (neigh != null) {
                    neigh.unreachStat.rx(pck);
                }
                break;
            case 3:
                if (parent != null) {
                    parent.unreachStat.tx(pck);
                }
                if (neigh != null) {
                    neigh.unreachStat.tx(pck);
                }
                break;
        }
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     * @param typ type of packet
     */
    public void packSend(packHolder pck, int typ) {
        if (pipe == null) {
            return;
        }
        updateMsgCtr(pck, typ, true);
        pck.merge2beg();
        if ((compressTx != null) && (typ == rtrBgpUtil.msgUpdate)) {
            if (debugger.rtrBgpEvnt) {
                logger.debug("sending compressed " + rtrBgpUtil.msgType2string(typ) + " to " + neigh.peerAddr);
            }
            if (neigh.monitor != null) {
                neigh.monitor.gotMessage(true, typ, this, neigh, pck.getCopy());
            }
            if (neigh.dump != null) {
                neigh.dump.gotMessage(true, typ, neigh, pck.getCopy());
            }
            compressCntr.tx(pck);
            pck.msbPutW(0, pck.dataSize() + rtrBgpUtil.sizeU);
            pck.putByte(2, typ);
            pck.putSkip(rtrBgpUtil.sizeC);
            pck.merge2beg();
            compressTx.setInput(pck.getCopy());
            byte[] buf = new byte[packHolder.maxData];
            int i = compressTx.deflate(buf, 0, buf.length, Deflater.SYNC_FLUSH);
            pck.clear();
            pck.putByte(0, 8); // flags
            pck.putCopy(buf, 0, 1, i);
            pck.putSkip(i + 1);
            pck.merge2beg();
            typ = rtrBgpUtil.msgCompress;
            updateMsgCtr(pck, typ, true);
        }
        if (debugger.rtrBgpEvnt) {
            logger.debug("sending " + rtrBgpUtil.msgType2string(typ) + " to " + neigh.peerAddr);
        }
        if (neigh.monitor != null) {
            neigh.monitor.gotMessage(true, typ, this, neigh, pck.getCopy());
        }
        if (neigh.dump != null) {
            neigh.dump.gotMessage(true, typ, neigh, pck.getCopy());
        }
        cntr.tx(pck);
        rtrBgpUtil.createHeader(pck, typ);
        pck.pipeSend(pipe, 0, pck.dataSize(), 2);
    }

    /**
     * receive one packet
     *
     * @param pck packet to store to
     * @return type of message, negative on error
     */
    public int packRecv(packHolder pck) {
        pck.clear();
        if (pipe == null) {
            return -1;
        }
        if (pck.pipeRecv(pipe, 0, rtrBgpUtil.sizeU, 144) != rtrBgpUtil.sizeU) {
            return -1;
        }
        if (rtrBgpUtil.checkHeader(pck)) {
            return -1;
        }
        int len = pck.IPsiz;
        int typ = pck.IPprt;
        pck.clear();
        if (len > 0) {
            if (pck.pipeRecv(pipe, 0, len, 144) != len) {
                return -1;
            }
        }
        cntr.rx(pck);
        updateMsgCtr(pck, typ, false);
        if (debugger.rtrBgpEvnt) {
            logger.debug("got " + rtrBgpUtil.msgType2string(typ) + " from " + neigh.peerAddr);
        }
        if (neigh.monitor != null) {
            neigh.monitor.gotMessage(false, typ, this, neigh, pck.getCopy());
        }
        if (neigh.dump != null) {
            neigh.dump.gotMessage(false, typ, neigh, pck.getCopy());
        }
        return typ;
    }

    /**
     * scan for header
     *
     * @return true on error, false on success
     */
    public boolean packScan() {
        packHolder pck = new packHolder(true, true);
        byte[] buf = new byte[rtrBgpUtil.sizeU];
        for (;;) {
            pck.clear();
            if (pck.pipeRecv(pipe, 0, buf.length, 141) != buf.length) {
                if (pipe.isClosed() != 0) {
                    return true;
                }
                bits.sleep(100);
                continue;
            }
            if (!rtrBgpUtil.checkHeader(pck)) {
                return false;
            }
            pck.pipeRecv(pipe, 0, 1, 144);
        }
    }

    /**
     * send keep alive message
     */
    public void sendKeepAlive() {
        packHolder pck = new packHolder(true, true);
        packSend(pck, rtrBgpUtil.msgKeepLiv);
    }

    private byte[] encodeHostname(String s) {
        byte[] buf;
        if (s == null) {
            buf = new byte[0];
        } else {
            buf = s.getBytes();
        }
        byte[] tmp = new byte[1];
        tmp[0] = (byte) buf.length;
        return bits.byteConcat(tmp, buf);
    }

    /**
     * send open message
     */
    public void sendOpen() {
        List<Integer> safis = parent.mask2list(neigh.addrFams);
        packHolder pck = new packHolder(true, true);
        for (int i = 0; i < safis.size(); i++) {
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, safis.get(i));
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaMultiProto, buf);
        }
        if (neigh.dynamicCapab) {
            byte[] buf = new byte[1];
            buf[0] = rtrBgpUtil.capaMultiProto;
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaDynamicCapa, buf);
        }
        if (neigh.wideAsPath) {
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, neigh.localAs);
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capa32bitAsNum, buf);
        }
        if (neigh.routeRefreshOld) {
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaRouteRefresh, new byte[0]);
        }
        if (neigh.routeRefreshNew) {
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaEnhancedRefresh, new byte[0]);
        }
        safis = parent.mask2list((neigh.addpathRmode | neigh.addpathTmode) & neigh.addrFams);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 4];
            for (int i = 0; i < safis.size(); i++) {
                int o = safis.get(i);
                bits.msbPutD(buf, i * 4, rtrBgpUtil.safi2triplet(o));
                long p = parent.safi2mask(o);
                int m = 0;
                if ((neigh.addpathRmode & p) != 0) {
                    m |= 1;
                }
                if ((neigh.addpathTmode & p) != 0) {
                    m |= 2;
                }
                buf[(i * 4) + 3] = (byte) m;
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaAdditionPath, buf);
        }
        safis = parent.mask2list(neigh.extNextCur & neigh.addrFams);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 6];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 6) + 0, safis.get(i));
                bits.msbPutW(buf, (i * 6) + 4, parent.afiUni >>> 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtNextHop, buf);
        }
        safis = parent.mask2list(neigh.extNextOtr & neigh.addrFams);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 6];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 6) + 0, safis.get(i));
                bits.msbPutW(buf, (i * 6) + 4, parent.afiOuni >>> 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtNextHop, buf);
        }
        safis = parent.mask2list(neigh.graceRestart & neigh.addrFams);
        if (safis.size() > 0) {
            byte[] buf = new byte[2 + (safis.size() * 4)];
            bits.msbPutW(buf, 0, ((parent.restartTime / 1000) & 0xfff) | 0x8000);
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 4) + 2, rtrBgpUtil.safi2triplet(safis.get(i)));
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaGraceRestart, buf);
        }
        safis = parent.mask2list(neigh.llGraceRestart & neigh.addrFams);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 7];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 7) + 0, rtrBgpUtil.safi2triplet(safis.get(i)));
                bits.msbPutD(buf, (i * 7) + 3, (parent.llRestartTime / 1000) & 0xffffff);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaLongGrace, buf);
        }
        safis = parent.mask2list(neigh.multiLabel & neigh.addrFams);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 4];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, i * 4, rtrBgpUtil.safi2triplet(safis.get(i)) | 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaMultiLabel, buf);
        }
        if (neigh.extUpdate) {
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtMessage, new byte[0]);
        }
        if ((neigh.compressMode & 1) != 0) {
            compressRx = new Inflater[8];
            for (int i = 0; i < compressRx.length; i++) {
                compressRx[i] = new Inflater();
            }
            byte[] buf = new byte[2];
            buf[0] = (byte) 0x87; // deflate, 32k window
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaCompress, buf);
        }
        if (neigh.leakRole >= 0) {
            byte[] buf = new byte[1];
            buf[0] = (byte) rtrBgpUtil.leakInverter(neigh.leakRole);
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaLeakRole, buf);
        }
        if (neigh.hostname > 0) {
            byte[] buf = encodeHostname(cfgAll.hostName);
            if (neigh.hostname > 1) {
                buf = bits.byteConcat(buf, encodeHostname(cfgAll.domainName));
            } else {
                buf = bits.byteConcat(buf, encodeHostname(""));
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaHostname, buf);
        }
        if (neigh.software) {
            byte[] buf = encodeHostname(cfgInit.versionAgent);
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaSoftware, buf);
        }
        if (neigh.bfdTrigger == 2) {
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaStrictBfd, new byte[0]);
        }
        pck.merge2beg();
        int i = pck.dataSize();
        if ((!neigh.extOpen) && (i >= 0xff)) {
            logger.error("too much capabilities for peer " + neigh.peerAddr);
            i = 0xfe;
        }
        pck.putByte(0, rtrBgpUtil.version);
        pck.msbPutW(1, tabRouteUtil.asNum16bit(neigh.localAs));
        pck.msbPutW(3, neigh.holdTimer / 1000);
        pck.putAddr(5, parent.routerID);
        pck.putSkip(9);
        if (neigh.extOpen) {
            pck.msbPutW(0, 0xffff);
            pck.msbPutW(2, i);
            pck.putSkip(4);
        } else {
            pck.putByte(0, i);
            pck.putSkip(1);
        }
        pck.merge2beg();
        packSend(pck, rtrBgpUtil.msgOpen);
    }

    private long parseMultiProtoCapa(encTlv tlv, List<Integer> afi, List<Long> msk) {
        long res = 0;
        for (int i = 0; i < tlv.valSiz; i += 4) {
            int p = bits.msbGetD(tlv.valDat, i);
            long o = parent.safi2mask(p);
            if (o < 1) {
                if (debugger.rtrBgpError) {
                    logger.debug("unknown (" + p + ") afi");
                }
                continue;
            }
            afi.add(p);
            msk.add(o);
            res |= o;
        }
        return res;
    }

    /**
     * parse open packet
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseOpen(packHolder pck) {
        if (pck.getByte(0) != rtrBgpUtil.version) {
            logger.info("neighbor " + neigh.peerAddr + " has bad version");
            sendNotify(2, 1);
            return true;
        }
        int i = pck.msbGetW(1);
        if (neigh.remoteAny) {
            neigh.remoteAs = i;
        }
        if (i != tabRouteUtil.asNum16bit(neigh.remoteAs)) {
            logger.info("neighbor " + neigh.peerAddr + " in wrong (" + i + ") as");
            sendNotify(2, 2);
            return true;
        }
        peerHold = pck.msbGetW(3) * 1000;
        if (peerHold < neigh.holdTimer) {
            peerKeep = peerHold / 3;
            if (pipe != null) {
                pipe.setTime(peerHold);
            }
        }
        pck.getAddr(peerRouterID, 5);
        pck.getSkip(9);
        if (pck.msbGetW(0) == 0xffff) {
            i = pck.msbGetW(2);
            pck.getSkip(4);
            peerExtOpen = true;
        } else {
            i = pck.getByte(0);
            pck.getSkip(1);
        }
        if (i < pck.dataSize()) {
            pck.setDataSize(i);
        }
        boolean mpGot = false;
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            encTlv tlv = rtrBgpUtil.getCapabilityTlv(peerExtOpen);
            if (tlv.getBytes(pck)) {
                break;
            }
            if (tlv.valTyp != 2) {
                if (debugger.rtrBgpError) {
                    logger.debug("unknown parameter " + tlv.dump());
                }
                continue;
            }
            packHolder pck2 = new packHolder(true, true);
            pck2.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
            pck2.putSkip(tlv.valSiz);
            pck2.merge2beg();
            for (;;) {
                tlv = rtrBgpUtil.getCapabilityTlv(false);
                if (tlv.getBytes(pck2)) {
                    break;
                }
                switch (tlv.valTyp) {
                    case rtrBgpUtil.capa32bitAsNum:
                        i = bits.msbGetD(tlv.valDat, 0);
                        if (neigh.remoteAny) {
                            neigh.remoteAs = i;
                        }
                        if (i != neigh.remoteAs) {
                            logger.info("neighbor " + neigh.peerAddr + " in wrong (" + bits.num2str(i) + ") as");
                            sendNotify(2, 2);
                            return true;
                        }
                        peer32bitAS = true;
                        break;
                    case rtrBgpUtil.capaLeakRole:
                        peerLeakRole = tlv.valDat[0];
                        break;
                    case rtrBgpUtil.capaHostname:
                        byte[] buf = new byte[tlv.valDat[0] & 0xff];
                        bits.byteCopy(tlv.valDat, 1, buf, 0, buf.length);
                        peerHostname = new String(buf);
                        i = buf.length + 1;
                        buf = new byte[tlv.valDat[i] & 0xff];
                        bits.byteCopy(tlv.valDat, i + 1, buf, 0, buf.length);
                        peerDomainname = new String(buf);
                        break;
                    case rtrBgpUtil.capaSoftware:
                        buf = new byte[tlv.valDat[0] & 0xff];
                        bits.byteCopy(tlv.valDat, 1, buf, 0, buf.length);
                        peerSoftware = new String(buf);
                        break;
                    case rtrBgpUtil.capaStrictBfd:
                        strictBfd = true;
                        break;
                    case rtrBgpUtil.capaCompress:
                        if ((neigh.compressMode & 2) == 0) {
                            break;
                        }
                        compressTx = new Deflater();
                        break;
                    case rtrBgpUtil.capaDynamicCapa:
                        for (i = 0; i < tlv.valSiz; i++) {
                            if (tlv.valDat[i] == rtrBgpUtil.capaMultiProto) {
                                peerDynCap = true;
                                break;
                            }
                        }
                        break;
                    case rtrBgpUtil.capaMultiProto:
                        mpGot = true;
                        peerAfis |= parseMultiProtoCapa(tlv, new ArrayList<Integer>(), new ArrayList<Long>());
                        break;
                    case rtrBgpUtil.capaMultiLabel:
                        for (i = 0; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            long p = parent.safi2mask(o);
                            if (p < 1) {
                                continue;
                            }
                            peerMltLab |= p;
                        }
                        break;
                    case rtrBgpUtil.capaGraceRestart:
                        for (i = 2; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            long p = parent.safi2mask(o);
                            if (p < 1) {
                                continue;
                            }
                            peerGrace |= p;
                        }
                        break;
                    case rtrBgpUtil.capaLongGrace:
                        for (i = 0; i < tlv.valSiz; i += 7) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            long p = parent.safi2mask(o);
                            if (p < 1) {
                                continue;
                            }
                            peerLlGrace |= p;
                        }
                        break;
                    case rtrBgpUtil.capaExtNextHop:
                        for (i = 0; i < tlv.valSiz; i += 6) {
                            int o = bits.msbGetD(tlv.valDat, i + 0);
                            int p = bits.msbGetW(tlv.valDat, i + 4);
                            long q = parent.safi2mask(o);
                            if (q < 1) {
                                continue;
                            }
                            if (p == (parent.afiUni >>> 16)) {
                                peerExtNextCur |= q;
                            }
                            if (p == (parent.afiOuni >>> 16)) {
                                peerExtNextOtr |= q;
                            }
                        }
                        break;
                    case rtrBgpUtil.capaAdditionPath:
                        for (i = 0; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            long p = parent.safi2mask(o);
                            if (p < 1) {
                                continue;
                            }
                            int m = tlv.valDat[i + 3];
                            if ((m & 2) != 0) {
                                addpathRx |= p;
                            }
                            if ((m & 1) != 0) {
                                addpathTx |= p;
                            }
                        }
                        break;
                    case rtrBgpUtil.capaRouteRefresh:
                        peerRefreshOld = true;
                        break;
                    case rtrBgpUtil.capaEnhancedRefresh:
                        peerRefreshNew = true;
                        break;
                    case rtrBgpUtil.capaExtMessage:
                        peerExtUpd = true;
                        break;
                    default:
                        if (debugger.rtrBgpError) {
                            logger.debug("unknown capability " + tlv.dump());
                        }
                        break;
                }
            }
        }
        if (!mpGot) {
            long o = parent.safi2mask(rtrBgpUtil.safiIp4uni);
            if (o > 0) {
                peerAfis |= o;
            }
        }
        if (!neigh.capaNego) {
            peerAfis = neigh.addrFams;
            peerDynCap = neigh.dynamicCapab;
            peerGrace = neigh.graceRestart;
            peerLlGrace = neigh.llGraceRestart;
            peerMltLab = neigh.multiLabel;
            peerExtNextCur = neigh.extNextCur;
            peerExtNextOtr = neigh.extNextOtr;
            addpathRx = neigh.addpathRmode;
            addpathTx = neigh.addpathTmode;
            peerRefreshOld = neigh.routeRefreshOld;
            peerRefreshNew = neigh.routeRefreshNew;
            peer32bitAS = neigh.wideAsPath;
            if ((neigh.compressMode & 2) != 0) {
                compressTx = new Deflater();
            }
        }
        if (neigh.leakForce || ((neigh.leakRole >= 0) && (peerLeakRole >= 0))) {
            if (peerLeakRole != neigh.leakRole) {
                logger.info("neighbor " + neigh.peerAddr + " sent wrong role " + rtrBgpUtil.leakRole2string(peerLeakRole, false));
                sendNotify(2, 8);
                return true;
            }
        }
        originalSafiList = peerAfis;
        originalAddRlist = addpathRx;
        originalAddTlist = addpathTx;
        peer32bitAS &= neigh.wideAsPath;
        peerRefreshOld &= neigh.routeRefreshOld;
        peerRefreshNew &= neigh.routeRefreshNew;
        peerAfis &= neigh.addrFams;
        if (peerAfis == 0) {
            logger.info("neighbor " + neigh.peerAddr + " in wrong safi");
            sendNotify(6, 3);
            return true;
        }
        needEorAfis = peerAfis;
        addpathRx &= neigh.addpathRmode;
        addpathTx &= neigh.addpathTmode;
        addpathRx &= neigh.addrFams;
        addpathTx &= neigh.addrFams;
        if (debugger.rtrBgpEvnt) {
            logger.debug("peer " + neigh.peerAddr + " id=" + peerRouterID + " hold=" + peerHold + " 32bitAS=" + peer32bitAS + " refresh=" + peerRefreshOld + " " + peerRefreshNew);
        }
        return false;
    }

    /**
     * send notify message
     *
     * @param err error code
     * @param sub sub code
     */
    public void sendNotify(int err, int sub) {
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, err);
        pck.putByte(1, sub);
        pck.putSkip(2);
        packSend(pck, rtrBgpUtil.msgNotify);
        closeNow();
        logger.info("sent notify " + rtrBgpUtil.notify2string(err, sub) + " to " + neigh.peerAddr);
        neigh.delListenPeer();
    }

    /**
     * got route refresh request
     *
     * @param safi safi to refresh
     */
    public void gotRefresh(int safi) {
        int mode = (safi >>> 8) & 0xff;
        safi &= 0xffff00ff;
        if (debugger.rtrBgpTraf) {
            logger.debug("got refresh mode " + mode + " from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
        }
        if (!afiMsk(peerAfis, safi)) {
            if (debugger.rtrBgpError) {
                logger.debug("got unknown refresh from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
            }
            return;
        }
        if (mode == 1) {
            tabRoute<addrIP> learned = getLearned(safi);
            if (learned == null) {
                return;
            }
            learned.clear();
            if (debugger.rtrBgpFull) {
                logger.debug("refresh begin");
            }
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (mode == 2) {
            return;
        }
        if (mode != 0) {
            return;
        }
        refreshRx++;
        tabRoute<addrIP> adverted = getAdverted(safi);
        if (adverted == null) {
            return;
        }
        if (peerRefreshNew) {
            long p = parent.safi2mask(safi);
            if (p >= 0) {
                needEofAfis |= p;
            }
            sendFreshMark(safi, 1);
        }
        adverted.clear();
        needFull.set(3);
        adversion.sub(1);
        neigh.transmit.wakeup();
    }

    /**
     * send one dynamic capability
     *
     * @param init true to start the exchange, false to answer
     * @param add set true to add, false to remove
     * @param msk afi mask to configure
     * @param safi safi to configure
     */
    public void sendDynamicCapa(boolean init, boolean add, long msk, int safi) {
        if (!peerDynCap) {
            return;
        }
        if (afiMsk(peerAfis, safi) == add) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, safi);
        rtrBgpUtil.placeCapability(pck, peerExtOpen, rtrBgpUtil.capaMultiProto, buf);
        dynCapaTx++;
        sendDynCapaMsg(init, true, add, dynCapaTx, pck);
        renegotiatingSafi(msk, safi, add, true);
    }

    private void sendDynCapaMsg(boolean init, boolean ack, boolean add, int seq, packHolder pck) {
        pck.merge2beg();
        int i = 0;
        if (!init) {
            i |= 0x80;
        }
        if (ack) {
            i |= 0x40;
        }
        if (!add) {
            i |= 1;
        }
        pck.putByte(0, i); // flags
        pck.msbPutD(1, seq);
        pck.putSkip(5);
        packSend(pck, rtrBgpUtil.msgCapability);
        if (debugger.rtrBgpTraf) {
            logger.debug("sent dynamic capability to peer " + neigh.peerAddr + " init=" + init + " ack=" + ack + " add=" + add + " seq=" + seq);
        }
    }

    private static void clearOneTable(tabRoute<addrIP> tab) {
        if (tab == null) {
            return;
        }
        tab.clear();
    }

    private void renegotiatingSafi(long msk, int safi, boolean add, boolean cfg) {
        sendEndOfRib(safi);
        clearOneTable(getLearned(safi));
        clearOneTable(getAdverted(safi));
        clearOneTable(neigh.getWilling(safi));
        clearOneTable(neigh.getAccepted(safi));
        needEorAfis |= msk;
        if (add) {
            peerAfis |= msk;
            originalSafiList |= msk;
        } else {
            peerAfis &= ~msk;
            originalSafiList &= ~msk;
        }
        if (cfg) {
            if (add) {
                neigh.addrFams |= msk;
            } else {
                neigh.addrFams &= ~msk;
            }
        }
        if (debugger.rtrBgpFull) {
            logger.debug("peer afi changed");
        }
        needFull.set(3);
        parent.needFull.add(1);
        parent.compute.wakeup();
        sendEndOfRib(safi);
    }

    /**
     * send route refresh request
     *
     * @param safi safi to refresh
     */
    public void sendRefresh(int safi) {
        if ((peerRefreshOld == false) && (peerRefreshNew == false)) {
            return;
        }
        if (!afiMsk(peerAfis, safi)) {
            return;
        }
        refreshTx++;
        tabRoute<addrIP> learned = getLearned(safi);
        if (learned == null) {
            return;
        }
        learned.clear();
        if (debugger.rtrBgpFull) {
            logger.debug("refresh request");
        }
        parent.needFull.add(1);
        parent.compute.wakeup();
        sendFreshMark(safi, 0);
    }

    /**
     * get prefix count
     *
     * @return number
     */
    public int getPrefixGot() {
        return lrnUni.size() + lrnMlt.size() + lrnOuni.size()
                + lrnOmlt.size() + lrnOflw.size() + lrnOsrt.size()
                + lrnFlw.size() + lrnVpnU.size() + lrnVpnM.size()
                + lrnVpnF.size() + lrnVpoU.size() + lrnVpoM.size()
                + lrnVpoF.size() + lrnVpls.size() + lrnMspw.size()
                + lrnEvpn.size() + lrnMdt.size() + lrnNsh.size()
                + lrnRpd.size() + lrnSpf.size() + lrnSrte.size()
                + lrnLnks.size() + lrnRtf.size() + lrnMvpn.size()
                + lrnMvpo.size() + lrnMtre.size() + lrnMtro.size();
    }

    /**
     * get prefix count
     *
     * @return number
     */
    public int getPrefixSent() {
        return advUni.size() + advMlt.size() + advOuni.size()
                + advOmlt.size() + advOflw.size() + advOsrt.size()
                + advFlw.size() + advVpnU.size() + advVpnM.size()
                + advVpnF.size() + advVpoU.size() + advVpoM.size()
                + advVpoF.size() + advVpls.size() + advMspw.size()
                + advEvpn.size() + advMdt.size() + advNsh.size()
                + advRpd.size() + advSpf.size() + advSrte.size()
                + advLnks.size() + advRtf.size() + advMvpn.size()
                + advMvpo.size() + advMtre.size() + advMtro.size();
    }

    /**
     * send update packet
     *
     * @param safi safi to update
     */
    public void sendEndOfRib(int safi) {
        if (debugger.rtrBgpTraf) {
            logger.debug("eor to peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
        }
        packHolder pck = new packHolder(true, true);
        rtrBgpUtil.createEndOfRib(this, pck, new packHolder(true, true), safi);
        packSend(pck, rtrBgpUtil.msgUpdate);
    }

    /**
     * send refresh marker
     *
     * @param safi safi to update
     * @param mode mode to send
     */
    public void sendFreshMark(int safi, int mode) {
        if (debugger.rtrBgpTraf) {
            logger.debug("refresh mode " + mode + " to peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, safi | (mode << 8));
        pck.putSkip(4);
        packSend(pck, rtrBgpUtil.msgRefrsh);
    }

    /**
     * send update packet without addpath
     *
     * @param safi safi to update
     * @param lst list of prefixes to advertise
     * @param reach true to reachable, false to withdraw
     */
    public void sendUpdateSP(int safi, List<tabRouteEntry<addrIP>> lst, boolean reach) {
        if (debugger.rtrBgpTraf) {
            String s = "";
            for (int i = 0; i < lst.size(); i++) {
                tabRouteEntry<addrIP> ntry = lst.get(i);
                s += " " + tabRouteUtil.rd2string(ntry.rouDst) + " " + ntry.prefix;
            }
            logger.debug("update to peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi) + ": " + (reach ? "reachable" : "withdraw") + s);
        }
        pckTx.clear();
        if (!reach) {
            rtrBgpUtil.createWithdraw(this, pckTx, pckTh, safi, false, lst);
        } else {
            rtrBgpUtil.createReachable(this, pckTx, pckTh, safi, false, peer32bitAS, peerMltLab == 0, lst);
        }
        packSend(pckTx, rtrBgpUtil.msgUpdate);
    }

    /**
     * send update packet with addpath
     *
     * @param safi safi to update
     * @param wil prefix to advertise, null to withdraw
     * @param don old already advertised data
     */
    public void sendUpdateAP(int safi, tabRouteEntry<addrIP> wil, tabRouteEntry<addrIP> don) {
        if (debugger.rtrBgpTraf) {
            String a;
            String s;
            if (wil == null) {
                a = "withdraw";
                s = tabRouteUtil.rd2string(don.rouDst) + " " + don.prefix;
            } else {
                a = "reachable";
                s = tabRouteUtil.rd2string(wil.rouDst) + " " + wil.prefix;
            }
            logger.debug("update to peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi) + ": " + a + " " + s);
        }
        List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
        if (wil == null) {
            for (int i = 0; i < don.alts.size(); i++) {
                tabRouteEntry<addrIP> ntry = don.copyBytes(tabRoute.addType.notyet);
                don.alts.get(i).copyBytes(ntry.best, true);
                ntry.best.ident = addpathBeg + i;
                lst.add(ntry);
            }
            pckTx.clear();
            rtrBgpUtil.createWithdraw(this, pckTx, pckTh, safi, true, lst);
            packSend(pckTx, rtrBgpUtil.msgUpdate);
            return;
        }
        if (don == null) {
            for (int i = 0; i < wil.alts.size(); i++) {
                tabRouteEntry<addrIP> ntry = wil.copyBytes(tabRoute.addType.notyet);
                wil.alts.get(i).copyBytes(ntry.best, true);
                ntry.best.ident = addpathBeg + i;
                lst.clear();
                lst.add(ntry);
                pckTx.clear();
                rtrBgpUtil.createReachable(this, pckTx, pckTh, safi, true, peer32bitAS, peerMltLab == 0, lst);
                packSend(pckTx, rtrBgpUtil.msgUpdate);
            }
            return;
        }
        for (int i = 0; i < wil.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = wil.alts.get(i);
            if (i < don.alts.size()) {
                if (attr.differs(don.alts.get(i)) == 0) {
                    continue;
                }
            }
            tabRouteEntry<addrIP> ntry = wil.copyBytes(tabRoute.addType.notyet);
            attr.copyBytes(ntry.best, true);
            ntry.best.ident = addpathBeg + i;
            lst.clear();
            lst.add(ntry);
            pckTx.clear();
            rtrBgpUtil.createReachable(this, pckTx, pckTh, safi, true, peer32bitAS, peerMltLab == 0, lst);
            packSend(pckTx, rtrBgpUtil.msgUpdate);
        }
        lst.clear();
        for (int i = wil.alts.size(); i < don.alts.size(); i++) {
            tabRouteEntry<addrIP> ntry = don.copyBytes(tabRoute.addType.notyet);
            don.alts.get(i).copyBytes(ntry.best, true);
            ntry.best.ident = addpathBeg + i;
            lst.add(ntry);
        }
        if (lst.size() < 1) {
            return;
        }
        pckTx.clear();
        rtrBgpUtil.createWithdraw(this, pckTx, pckTh, safi, true, lst);
        packSend(pckTx, rtrBgpUtil.msgUpdate);
    }

    /**
     * parse update packet
     *
     * @param pck packet to parse
     * @param hlp helper packet
     * @return false on success, true on error
     */
    public boolean parseUpdate(packHolder pck, packHolder hlp) {
        if (debugger.rtrBgpTraf) {
            logger.debug("update from peer " + neigh.peerAddr);
        }
        currUni.clear();
        currMlt.clear();
        currOuni.clear();
        currOmlt.clear();
        currOflw.clear();
        currOsrt.clear();
        currFlw.clear();
        currVpnU.clear();
        currVpnM.clear();
        currVpnF.clear();
        currVpoU.clear();
        currVpoM.clear();
        currVpoF.clear();
        currVpls.clear();
        currMspw.clear();
        currEvpn.clear();
        currMdt.clear();
        currNsh.clear();
        currRpd.clear();
        currSpf.clear();
        currRtf.clear();
        currSrte.clear();
        currLnks.clear();
        currMvpn.clear();
        currMvpo.clear();
        currMtre.clear();
        currMtro.clear();
        currChg = 0;
        int origOfs = pck.dataSize();
        int prt = pck.msbGetW(0);
        pck.getSkip(2);
        prt = pck.dataSize() - prt;
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.best.rouTyp = neigh.lower.rouTyp;
        ntry.best.protoNum = neigh.lower.rtrNum;
        ntry.best.distance = neigh.distance;
        ntry.best.rouSrc = neigh.peerType;
        ntry.best.srcRtr = neigh.peerAddr.copyBytes();
        ntry.best.locPref = neigh.preference;
        boolean addpath = addPthRx(rtrBgpUtil.safiIp4uni);
        int ident = 0;
        for (;;) {
            if (pck.dataSize() <= prt) {
                break;
            }
            if (addpath) {
                ident = pck.msbGetD(0);
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = rtrBgpUtil.readPrefix(rtrBgpUtil.safiIp4uni, true, pck);
            if (res == null) {
                continue;
            }
            res.best.ident = ident;
            prefixWithdraw(rtrBgpUtil.safiIp4uni, addpath, res, pck);
        }
        pck.setBytesLeft(prt);
        prt = pck.msbGetW(0);
        pck.getSkip(2);
        prt = pck.dataSize() - prt;
        for (;;) {
            if (pck.dataSize() <= prt) {
                break;
            }
            rtrBgpUtil.parseAttrib(pck, hlp);
            rtrBgpUtil.interpretAttribute(this, ntry, hlp);
        }
        pck.setBytesLeft(prt);
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            if (addpath) {
                ident = pck.msbGetD(0);
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = rtrBgpUtil.readPrefix(rtrBgpUtil.safiIp4uni, true, pck);
            if (res == null) {
                continue;
            }
            res.best.ident = ident;
            res.best.nextHop = ntry.best.nextHop;
            prefixReach(rtrBgpUtil.safiIp4uni, addpath, res, pck);
        }
        if (ntry.best.unknown != null) {
            packHolder origPck = pck.copyBytes(false, false);
            origPck.setBytesLeft(origOfs);
            unknownCntr.rx(origPck);
            if (neigh != null) {
                if (neigh.unknownsColl != null) {
                    neigh.unknownsColl.gotMessage(false, rtrBgpUtil.msgUpdate, neigh, origPck.getCopy());
                }
                if (neigh.unknownsLog) {
                    logger.info("got update with unknowns " + neigh.peerAddr + " -> " + neigh.localAddr + " " + origPck.dump());
                }
            }
        }
        tabRouteUtil.removeUnknowns(ntry.best, neigh.unknownsIn);
        addAttribedTab(currUni, parent.afiUni, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribedTab(currUni, parent.afiLab, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribedTab(currUni, parent.afiCtp, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribedTab(currUni, parent.afiCar, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribedTab(currMlt, parent.afiMlt, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribedTab(currOuni, parent.afiOlab, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribedTab(currOuni, parent.afiOctp, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribedTab(currOuni, parent.afiOcar, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribedTab(currOuni, parent.afiOuni, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribedTab(currOmlt, parent.afiOmlt, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribedTab(currFlw, parent.afiFlw, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currOflw, parent.afiOflw, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribedTab(currSrte, parent.afiSrte, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currOsrt, parent.afiOsrt, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribedTab(currVpnU, parent.afiVpnU, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currVpnM, parent.afiVpnM, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currVpnF, parent.afiVpnF, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currVpoU, parent.afiVpoU, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribedTab(currVpoM, parent.afiVpoM, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribedTab(currVpoF, parent.afiVpoF, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribedTab(currVpls, parent.afiVpls, ntry, neigh.eroumapIn, neigh.eroupolIn, null);
        addAttribedTab(currMspw, parent.afiMspw, ntry, neigh.eroumapIn, neigh.eroupolIn, null);
        addAttribedTab(currEvpn, parent.afiEvpn, ntry, neigh.eroumapIn, neigh.eroupolIn, null);
        addAttribedTab(currMdt, parent.afiMdt, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currNsh, parent.afiNsh, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currRpd, parent.afiRpd, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currSpf, parent.afiSpf, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currRtf, parent.afiRtf, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currMvpn, parent.afiMvpn, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currMvpo, parent.afiMvpo, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribedTab(currMtre, parent.afiMtre, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribedTab(currMtro, parent.afiMtro, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribedTab(currLnks, parent.afiLnks, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        if (neigh.rtfilterOut && (currRtf.size() > 0)) {
            if (debugger.rtrBgpFull) {
                logger.debug("rtfilter changed");
            }
            parent.needFull.add(1);
        }
        if ((currChg > 0) && (rxReady() < (neigh.bufferSize / 4))) {
            parent.compute.wakeup();
        }
        if (neigh.maxPrxInCnt < 1) {
            return false;
        }
        int i = getPrefixGot();
        if (i > ((neigh.maxPrxInCnt * neigh.maxPrxInPrc) / 100)) {
            logger.info("neighbor " + neigh.peerAddr + " sent " + i + " prefixes");
        }
        if (i > neigh.maxPrxInCnt) {
            sendNotify(6, 1);
        }
        return false;
    }

    private void addAttribedOne(tabRouteEntry<addrIP> cur, boolean addpath, tabRoute<addrIP> learned, tabRoute<addrIP> changed, int safi, tabListing<tabRtrmapN, addrIP> roumap, tabListing<tabRtrplcN, addrIP> roupol, tabListing<tabPrfxlstN, addrIP> prflst) {
        if (parent.flaps != null) {
            parent.prefixFlapped(safi, cur.rouDst, cur.prefix, cur.best.asPathInts(-1));
        }
        if (neigh.dampenPfxs != null) {
            neigh.prefixDampen(safi, cur.rouDst, cur.prefix, neigh.dampenAnno);
        }
        neigh.setValidity(safi, cur);
        if (!neigh.softReconfig) {
            tabRouteEntry<addrIP> res = tabRoute.doUpdateEntry(safi, neigh.remoteAs, cur, roumap, roupol, prflst);
            if (res == null) {
                repPolRej++;
                if (doPrefDel(learned, addpath, cur)) {
                    return;
                }
                currChg++;
                changed.add(tabRoute.addType.always, cur, false, false);
                return;
            }
            cur = res;
        }
        if (prefixReachable(cur, safi)) {
            if (doPrefDel(learned, addpath, cur)) {
                return;
            }
            currChg++;
            changed.add(tabRoute.addType.always, cur, false, false);
            return;
        }
        doPrefAdd(learned, addpath, cur);
        currChg++;
        changed.add(tabRoute.addType.always, cur, false, false);
    }

    private void addAttribedTab(List<tabRouteEntry<addrIP>> currAdd, int safi, tabRouteEntry<addrIP> attr, tabListing<tabRtrmapN, addrIP> roumap, tabListing<tabRtrplcN, addrIP> roupol, tabListing<tabPrfxlstN, addrIP> prflst) {
        if (!afiMsk(peerAfis, safi)) {
            return;
        }
        tabRoute<addrIP> learned = getLearned(safi);
        if (learned == null) {
            return;
        }
        tabRoute<addrIP> changed = parent.getChanged(safi);
        if (changed == null) {
            return;
        }
        boolean addpath = addPthRx(safi);
        for (int o = 0; o < currAdd.size(); o++) {
            tabRouteEntry<addrIP> pref = currAdd.get(o);
            if (pref == null) {
                continue;
            }
            attr.best.ident = pref.best.ident;
            attr.best.nextHop = pref.best.nextHop;
            attr.best.labelRem = pref.best.labelRem;
            attr.best.evpnLab = pref.best.evpnLab;
            if ((attr.best.segrouPrf != null) && (attr.best.labelRem != null) && (attr.best.segrouSiz > 0)) {
                addrIPv6 adr6 = new addrIPv6();
                int i = attr.best.labelRem.get(0) >>> 4;
                i &= (1 << attr.best.segrouSiz) - 1;
                bits.msbPutD(adr6.getBytes(), addrIPv6.size - 4, i);
                adr6.setShl(adr6, 128 - attr.best.segrouOfs - attr.best.segrouSiz);
                adr6.setOr(attr.best.segrouPrf, adr6);
                attr.best.segrouPrf.fromIPv6addr(adr6);
            }
            attr.best.copyBytes(pref.best, false);
            addAttribedOne(pref, addpath, learned, changed, safi, roumap, roupol, prflst);
        }
    }

    private void doPrefAdd(tabRoute<addrIP> tab, boolean addpath, tabRouteEntry<addrIP> ntry) {
        ntry.best.time = bits.getTime();
        if (!addpath) {
            tab.add(tabRoute.addType.always, ntry, false, false);
            return;
        }
        tabRouteEntry<addrIP> old = tab.find(ntry);
        if (old == null) {
            tab.add(tabRoute.addType.always, ntry, false, false);
            return;
        }
        if (neigh != null) {
            if (neigh.hopChanges) {
                if ((old.best.nextHop != null) && (ntry.best.nextHop != null)) {
                    if (old.best.nextHop.compareTo(ntry.best.nextHop) != 0) {
                        logger.info("prefix " + tabRouteUtil.rd2string(ntry.rouDst) + " " + addrPrefix.ip2str(ntry.prefix) + " from " + neigh.peerAddr + " changed from nexthop " + old.best.nextHop + " to " + ntry.best.nextHop);
                    }
                }
            }
            if (neigh.endChanges) {
                int o = old.best.asPathEnd();
                int c = ntry.best.asPathEnd();
                if (o != c) {
                    logger.info("prefix " + tabRouteUtil.rd2string(ntry.rouDst) + " " + addrPrefix.ip2str(ntry.prefix) + " from " + neigh.peerAddr + " changed from lastasn " + o + " to " + c);
                }
            }
            if (neigh.lengthChanges != null) {
                int o = old.best.asPathLen();
                int c = ntry.best.asPathLen();
                int d = o - c;
                if (d < 0) {
                    d = -d;
                }
                if (neigh.lengthChanges.matches(d)) {
                    logger.info("prefix " + tabRouteUtil.rd2string(ntry.rouDst) + " " + addrPrefix.ip2str(ntry.prefix) + " from " + neigh.peerAddr + " changed from pathlen " + o + " to " + c);
                }
            }
        }
        old = old.copyBytes(tabRoute.addType.lnkAlters);
        int i = old.findId(ntry.best.ident);
        if (i >= 0) {
            old.setAlt(i, ntry.best);
        } else {
            old.addAlt(ntry.best);
        }
        old.selectBest();
        tab.add(tabRoute.addType.always, old, false, false);
    }

    private boolean doPrefDel(tabRoute<addrIP> tab, boolean addpath, tabRouteEntry<addrIP> ntry) {
        if (!addpath) {
            return tab.del(ntry);
        }
        tabRouteEntry<addrIP> old = tab.find(ntry);
        if (old == null) {
            return true;
        }
        old = old.copyBytes(tabRoute.addType.lnkAlters);
        int i = old.findId(ntry.best.ident);
        if (i < 0) {
            return true;
        }
        if (old.alts.size() <= 1) {
            tab.del(old);
        } else {
            old.delAlt(i);
            old.selectBest();
            tab.add(tabRoute.addType.always, old, false, false);
        }
        return false;
    }

    /**
     * prefix withdrawal received
     *
     * @param safi safi
     * @param addpath addpath mode
     * @param ntry prefix
     * @param pck packet to signal
     */
    protected void prefixWithdraw(int safi, boolean addpath, tabRouteEntry<addrIP> ntry, packHolder pck) {
        updateRchblCntr(2, pck);
        if (debugger.rtrBgpTraf) {
            logger.debug("withdraw " + rtrBgpUtil.safi2string(safi) + " " + tabRouteUtil.rd2string(ntry.rouDst) + " " + ntry.prefix + " " + ntry.best.ident);
        }
        if (!afiMsk(peerAfis, safi)) {
            if (debugger.rtrBgpError) {
                logger.debug("got unknown withdraw from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
            }
            return;
        }
        if (parent.flaps != null) {
            parent.prefixFlapped(safi, ntry.rouDst, ntry.prefix, null);
        }
        if (neigh.dampenPfxs != null) {
            neigh.prefixDampen(safi, ntry.rouDst, ntry.prefix, neigh.dampenWthd);
        }
        tabRoute<addrIP> learned = getLearned(safi);
        if (learned == null) {
            return;
        }
        if (doPrefDel(learned, addpath, ntry)) {
            return;
        }
        currChg++;
        tabRoute<addrIP> changed = parent.getChanged(safi);
        if (changed == null) {
            if (debugger.rtrBgpFull) {
                logger.debug("table not found");
            }
            parent.needFull.add(1);
            return;
        }
        changed.add(tabRoute.addType.always, ntry, true, false);
    }

    /**
     * process reachable prefix
     *
     * @param safi address family
     * @param addpath addpath mode
     * @param ntry route entry
     * @param pck packet to signal
     */
    protected void prefixReach(int safi, boolean addpath, tabRouteEntry<addrIP> ntry, packHolder pck) {
        updateRchblCntr(0, pck);
        if (debugger.rtrBgpTraf) {
            logger.debug("reachable " + rtrBgpUtil.safi2string(safi) + " " + tabRouteUtil.rd2string(ntry.rouDst) + " " + ntry.prefix + " " + ntry.best.ident);
        }
        if (!afiMsk(peerAfis, safi)) {
            if (debugger.rtrBgpError) {
                logger.debug("got unknown reachable from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
            }
            return;
        }
        List<tabRouteEntry<addrIP>> trg = null;
        if (safi == parent.afiUni) {
            trg = currUni;
        }
        if (safi == parent.afiLab) {
            trg = currUni;
        }
        if (safi == parent.afiCtp) {
            trg = currUni;
        }
        if (safi == parent.afiCar) {
            trg = currUni;
        }
        if (safi == parent.afiMlt) {
            trg = currMlt;
        }
        if (safi == parent.afiOlab) {
            trg = currOuni;
        }
        if (safi == parent.afiOctp) {
            trg = currOuni;
        }
        if (safi == parent.afiOcar) {
            trg = currOuni;
        }
        if (safi == parent.afiOuni) {
            trg = currOuni;
        }
        if (safi == parent.afiOmlt) {
            trg = currOmlt;
        }
        if (safi == parent.afiOflw) {
            trg = currOflw;
        }
        if (safi == parent.afiOsrt) {
            trg = currOsrt;
        }
        if (safi == parent.afiFlw) {
            trg = currFlw;
        }
        if (safi == parent.afiVpnU) {
            trg = currVpnU;
        }
        if (safi == parent.afiVpnM) {
            trg = currVpnM;
        }
        if (safi == parent.afiVpnF) {
            trg = currVpnF;
        }
        if (safi == parent.afiVpoU) {
            trg = currVpoU;
        }
        if (safi == parent.afiVpoM) {
            trg = currVpoM;
        }
        if (safi == parent.afiVpoF) {
            trg = currVpoF;
        }
        if (safi == parent.afiVpls) {
            trg = currVpls;
        }
        if (safi == parent.afiMspw) {
            trg = currMspw;
        }
        if (safi == parent.afiEvpn) {
            trg = currEvpn;
        }
        if (safi == parent.afiMdt) {
            trg = currMdt;
        }
        if (safi == parent.afiNsh) {
            trg = currNsh;
        }
        if (safi == parent.afiRpd) {
            trg = currRpd;
        }
        if (safi == parent.afiSpf) {
            trg = currSpf;
        }
        if (safi == parent.afiRtf) {
            trg = currRtf;
        }
        if (safi == parent.afiSrte) {
            trg = currSrte;
        }
        if (safi == parent.afiLnks) {
            trg = currLnks;
        }
        if (safi == parent.afiMvpn) {
            trg = currMvpn;
        }
        if (safi == parent.afiMvpo) {
            trg = currMvpo;
        }
        if (safi == parent.afiMtre) {
            trg = currMtre;
        }
        if (safi == parent.afiMtro) {
            trg = currMtro;
        }
        if (trg == null) {
            return;
        }
        if (ntry.best.nextHop == null) {
            ntry.best.nextHop = neigh.peerAddr.copyBytes();
        }
        trg.add(ntry);
    }

    private boolean prefixReachable(tabRouteEntry<addrIP> ntry, int safi) {
        if (debugger.rtrBgpTraf) {
            logger.debug("processing " + tabRouteUtil.rd2string(ntry.rouDst) + " " + ntry.prefix);
        }
        switch (neigh.leakRole) {
            case rtrBgpUtil.roleProv:
                if (ntry.best.onlyCust == 0) {
                    ntry.best.onlyCust = neigh.remoteAs;
                }
                break;
            case rtrBgpUtil.roleRs:
                if (ntry.best.onlyCust == 0) {
                    ntry.best.onlyCust = neigh.remoteAs;
                }
                break;
            case rtrBgpUtil.roleRsc:
                if (ntry.best.onlyCust != 0) {
                    repPolRej++;
                    return true;
                }
                break;
            case rtrBgpUtil.roleCust:
                if (ntry.best.onlyCust != 0) {
                    repPolRej++;
                    return true;
                }
                break;
            case rtrBgpUtil.rolePeer:
                if ((ntry.best.onlyCust != 0) && (ntry.best.onlyCust != neigh.remoteAs)) {
                    repPolRej++;
                    return true;
                }
                if (ntry.best.onlyCust == 0) {
                    ntry.best.onlyCust = neigh.remoteAs;
                }
                break;
            default:
                break;
        }
        if (neigh.enforceFirst) {
            switch (neigh.peerType) {
                case rtrBgpUtil.peerExtrn:
                case rtrBgpUtil.peerServr:
                    if (tabRouteUtil.firstIntList(ntry.best.pathSeq, neigh.remoteAs)) {
                        repAsPath++;
                        return true;
                    }
                    break;
                case rtrBgpUtil.peerCnfed:
                    if (tabRouteUtil.firstIntList(ntry.best.confSeq, neigh.remoteAs)) {
                        repAsConf++;
                        return true;
                    }
                    break;
                default:
                    break;
            }
        }
        if (!neigh.allowAsIn) {
            switch (neigh.peerType) {
                case rtrBgpUtil.peerExtrn:
                case rtrBgpUtil.peerServr:
                    if (tabRouteUtil.findIntList(ntry.best.pathSeq, neigh.localAs) >= 0) {
                        repAsPath++;
                        return true;
                    }
                    if (tabRouteUtil.findIntList(ntry.best.pathSet, neigh.localAs) >= 0) {
                        repAsPath++;
                        return true;
                    }
                    break;
                case rtrBgpUtil.peerCnfed:
                    if (tabRouteUtil.findIntList(ntry.best.confSeq, neigh.localAs) >= 0) {
                        repAsConf++;
                        return true;
                    }
                    if (tabRouteUtil.findIntList(ntry.best.confSet, neigh.localAs) >= 0) {
                        repAsConf++;
                        return true;
                    }
                    break;
                default:
                    break;
            }
        }
        switch (neigh.peerType) {
            case rtrBgpUtil.peerIntrn:
                addrIP a = new addrIP();
                a.fromIPv4addr(parent.routerID);
                if (ntry.best.originator != null) {
                    if (ntry.best.originator.compareTo(a) == 0) {
                        repOrgnId++;
                        return true;
                    }
                }
                if (ntry.best.clustList != null) {
                    if (tabRouteUtil.findAddrList(ntry.best.clustList, a) >= 0) {
                        repClstrL++;
                        return true;
                    }
                }
                break;
            case rtrBgpUtil.peerRflct:
                if (ntry.best.originator == null) {
                    a = new addrIP();
                    a.fromIPv4addr(peerRouterID);
                    ntry.best.originator = a;
                }
                a = new addrIP();
                a.fromIPv4addr(parent.routerID);
                if (ntry.best.originator.compareTo(a) == 0) {
                    repOrgnId++;
                    return true;
                }
                if (ntry.best.clustList == null) {
                    ntry.best.clustList = new ArrayList<addrIP>();
                }
                if (tabRouteUtil.findAddrList(ntry.best.clustList, a) >= 0) {
                    repClstrL++;
                    return true;
                }
                ntry.best.clustList.add(a);
                break;
            default:
                break;
        }
        if (neigh.rtfilterIn && neigh.shouldRtfilter(safi)) {
            if (tabRouteUtil.findRtfilterTab(ntry.best.extComm, neigh.remoteAs, parent.computedRtf, true) && tabRouteUtil.findRtfilterTab(ntry.best.extComm, neigh.localAs, parent.computedRtf, true)) {
                repPolRej++;
                return true;
            }
        }
        if (neigh.nxtHopPeer) {
            if ((neigh.otherAdr != null) && ((safi == parent.afiOuni) || (safi == parent.afiOmlt))) {
                ntry.best.nextHop = neigh.otherAdr.copyBytes();
            } else {
                ntry.best.nextHop = neigh.peerAddr.copyBytes();
            }
        }
        if ((ntry.best.labelRem == null) && (ntry.best.segrouIdx > 0) && (ntry.best.segrouBeg > 0)) {
            ntry.best.labelRem = tabLabel.int2labels(ntry.best.segrouBeg + ntry.best.segrouIdx);
        }
        if (neigh.egressEng > 0) {
            ntry.best.segrouIdx = neigh.egressEng;
        }
        if (neigh.dmzLinkBw >= 0) {
            if (ntry.best.extComm == null) {
                ntry.best.extComm = new ArrayList<Long>();
            }
            ntry.best.extComm.add(tabRouteUtil.dmzBw2comm(neigh.localAs, neigh.dmzLinkBw));
        }
        if (neigh.removePrivAsIn) {
            tabRouteUtil.removePrivateAs(ntry.best.pathSeq);
            tabRouteUtil.removePrivateAs(ntry.best.pathSet);
        }
        if (neigh.overridePeerIn) {
            tabRouteUtil.replaceIntList(ntry.best.pathSeq, neigh.remoteAs, neigh.localAs);
            tabRouteUtil.replaceIntList(ntry.best.pathSet, neigh.remoteAs, neigh.localAs);
        }
        if (neigh.intVpnClnt) {
            rtrBgpUtil.encodeAttribSet(this, neigh.localAs, ntry);
        }
        return false;
    }

}
