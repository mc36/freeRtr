package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.syncInt;
import net.freertr.util.typLenVal;

/**
 * bgp4 speaker
 *
 * @author matecsaba
 */
public class rtrBgpSpeak implements rtrBfdClnt, Runnable {

    /**
     * header size
     */
    public static final int sizeU = 19;

    /**
     * compressed header size
     */
    public static final int sizeC = 3;

    /**
     * connection
     */
    protected pipeSide pipe;

    private rtrBgp parent;

    private rtrBgpNeigh neigh;

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
    public final tabRoute<addrIP> lrnOtrU = new tabRoute<addrIP>("rx");

    /**
     * learned other multicast prefixes
     */
    public final tabRoute<addrIP> lrnOtrM = new tabRoute<addrIP>("rx");

    /**
     * learned other flowspec prefixes
     */
    public final tabRoute<addrIP> lrnOtrF = new tabRoute<addrIP>("rx");

    /**
     * learned other srte prefixes
     */
    public final tabRoute<addrIP> lrnOtrS = new tabRoute<addrIP>("rx");

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
    public final tabRoute<addrIP> advOtrU = new tabRoute<addrIP>("tx");

    /**
     * advertised other multicast prefixes
     */
    public final tabRoute<addrIP> advOtrM = new tabRoute<addrIP>("tx");

    /**
     * advertised other flowspec prefixes
     */
    public final tabRoute<addrIP> advOtrF = new tabRoute<addrIP>("tx");

    /**
     * advertised other srte prefixes
     */
    public final tabRoute<addrIP> advOtrS = new tabRoute<addrIP>("tx");

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
    private List<tabRouteEntry<addrIP>> currOtrU = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other multicast prefixes
     */
    private List<tabRouteEntry<addrIP>> currOtrM = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other flowspec prefixes
     */
    private List<tabRouteEntry<addrIP>> currOtrF = new ArrayList<tabRouteEntry<addrIP>>();

    /**
     * currently updating other srte prefixes
     */
    private List<tabRouteEntry<addrIP>> currOtrS = new ArrayList<tabRouteEntry<addrIP>>();

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
    public int peerAfis;

    /**
     * peer route refresh capability
     */
    public boolean peerRefresh;

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
    public int needEorAfis;

    /**
     * peer graceful restart capability
     */
    public int peerGrace;

    /**
     * peer multiple labels capability
     */
    public int peerMltLab;

    /**
     * peer extended nexthop capability
     */
    public int peerExtNextCur;

    /**
     * peer extended nexthop capability
     */
    public int peerExtNextOtr;

    /**
     * peer leak prevention role capability
     */
    public int peerLeakRole;

    /**
     * peer hostname capability
     */
    public String peerHostname;

    /**
     * peer domain capability
     */
    public String peerDomainname;

    /**
     * peer sends additional paths
     */
    public int addpathRx;

    /**
     * peer receives additional paths
     */
    public int addpathTx;

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
    public int originalSafiList;

    /**
     * addpath list sent by the peer
     */
    public int originalAddRlist;

    /**
     * addpath list sent by the peer
     */
    public int originalAddTlist;

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
     */
    public rtrBgpSpeak(rtrBgp protocol, rtrBgpNeigh neighbor, pipeSide socket) {
        ready2adv = false;
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

    private boolean afiMsk(int val, int safi) {
        if (safi == parent.afiUni) {
            return (val & rtrBgpParam.mskUni) != 0;
        }
        if (safi == parent.afiLab) {
            return (val & rtrBgpParam.mskLab) != 0;
        }
        if (safi == parent.afiMlt) {
            return (val & rtrBgpParam.mskMlt) != 0;
        }
        if (safi == parent.afiOtrL) {
            return (val & rtrBgpParam.mskOtrL) != 0;
        }
        if (safi == parent.afiOtrU) {
            return (val & rtrBgpParam.mskOtrU) != 0;
        }
        if (safi == parent.afiOtrM) {
            return (val & rtrBgpParam.mskOtrM) != 0;
        }
        if (safi == parent.afiOtrF) {
            return (val & rtrBgpParam.mskOtrF) != 0;
        }
        if (safi == parent.afiOtrS) {
            return (val & rtrBgpParam.mskOtrS) != 0;
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
        if (safi == parent.afiMlt) {
            return lrnMlt;
        }
        if (safi == parent.afiOtrL) {
            return lrnOtrU;
        }
        if (safi == parent.afiOtrU) {
            return lrnOtrU;
        }
        if (safi == parent.afiOtrM) {
            return lrnOtrM;
        }
        if (safi == parent.afiOtrF) {
            return lrnOtrF;
        }
        if (safi == parent.afiOtrS) {
            return lrnOtrS;
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
        if (safi == parent.afiMlt) {
            return advMlt;
        }
        if (safi == parent.afiOtrL) {
            return advOtrU;
        }
        if (safi == parent.afiOtrU) {
            return advOtrU;
        }
        if (safi == parent.afiOtrM) {
            return advOtrM;
        }
        if (safi == parent.afiOtrF) {
            return advOtrF;
        }
        if (safi == parent.afiOtrS) {
            return advOtrS;
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
        if (pipe != null) {
            pipe.setClose();
        }
        if (neigh.localIfc != null) {
            neigh.localIfc.bfdDel(neigh.peerAddr, this);
        }
        lrnUni.clear();
        lrnMlt.clear();
        lrnOtrU.clear();
        lrnOtrM.clear();
        lrnOtrF.clear();
        lrnOtrS.clear();
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
        lrnSrte.clear();
        lrnLnks.clear();
        lrnMvpn.clear();
        lrnMvpo.clear();
        advUni.clear();
        advMlt.clear();
        advOtrU.clear();
        advOtrM.clear();
        advOtrF.clear();
        advOtrS.clear();
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
        advSrte.clear();
        advLnks.clear();
        advMvpn.clear();
        advMvpo.clear();
        neigh.accUni = new tabRoute<addrIP>("rx");
        neigh.accMlt = new tabRoute<addrIP>("rx");
        neigh.accOtrU = new tabRoute<addrIP>("rx");
        neigh.accOtrM = new tabRoute<addrIP>("rx");
        neigh.accOtrF = new tabRoute<addrIP>("rx");
        neigh.accOtrS = new tabRoute<addrIP>("rx");
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
        neigh.accSrte = new tabRoute<addrIP>("rx");
        neigh.accLnks = new tabRoute<addrIP>("rx");
        neigh.accMvpn = new tabRoute<addrIP>("rx");
        neigh.accMvpo = new tabRoute<addrIP>("rx");
        neigh.wilUni = new tabRoute<addrIP>("rx");
        neigh.wilMlt = new tabRoute<addrIP>("rx");
        neigh.wilOtrU = new tabRoute<addrIP>("rx");
        neigh.wilOtrM = new tabRoute<addrIP>("rx");
        neigh.wilOtrF = new tabRoute<addrIP>("rx");
        neigh.wilOtrS = new tabRoute<addrIP>("rx");
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
        neigh.wilSrte = new tabRoute<addrIP>("rx");
        neigh.wilLnks = new tabRoute<addrIP>("rx");
        neigh.wilMvpn = new tabRoute<addrIP>("rx");
        neigh.wilMvpo = new tabRoute<addrIP>("rx");
        neigh.chgUni = new tabRoute<addrIP>("chg");
        neigh.chgMlt = new tabRoute<addrIP>("chg");
        neigh.chgOtrU = new tabRoute<addrIP>("chg");
        neigh.chgOtrM = new tabRoute<addrIP>("chg");
        neigh.chgOtrF = new tabRoute<addrIP>("chg");
        neigh.chgOtrS = new tabRoute<addrIP>("chg");
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
        neigh.chgSrte = new tabRoute<addrIP>("chg");
        neigh.chgLnks = new tabRoute<addrIP>("chg");
        neigh.chgMvpn = new tabRoute<addrIP>("chg");
        neigh.chgMvpo = new tabRoute<addrIP>("chg");
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
        sendOpen();
        sendKeepAlive();
        int typ = packRecv(pckRx);
        if (typ == rtrBgpUtil.msgNotify) {
            logger.info("got notify " + rtrBgpUtil.notify2string(pckRx.getByte(0), pckRx.getByte(1)) + " from " + neigh.peerAddr);
            closeNow();
            return;
        }
        if (typ != rtrBgpUtil.msgOpen) {
            sendNotify(1, 3);
            return;
        }
        if (parseOpen(pckRx)) {
            closeNow();
            return;
        }
        needEorAfis = peerAfis;
        logger.warn("neighbor " + neigh.peerAddr + " up");
        if (neigh.monitor != null) {
            neigh.monitor.gotEvent(true, this, neigh);
        }
        if (neigh.bfdTrigger) {
            neigh.localIfc.bfdAdd(neigh.peerAddr, this, "bgp");
        }
        lrnUni.clear();
        lrnMlt.clear();
        lrnOtrU.clear();
        lrnOtrM.clear();
        lrnOtrF.clear();
        lrnOtrS.clear();
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
        lrnSrte.clear();
        lrnLnks.clear();
        lrnMvpn.clear();
        lrnMvpo.clear();
        advUni.clear();
        advMlt.clear();
        advOtrU.clear();
        advOtrM.clear();
        advOtrF.clear();
        advOtrS.clear();
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
        advSrte.clear();
        advLnks.clear();
        advMvpn.clear();
        advMvpo.clear();
        neigh.accUni = new tabRoute<addrIP>("rx");
        neigh.accMlt = new tabRoute<addrIP>("rx");
        neigh.accOtrU = new tabRoute<addrIP>("rx");
        neigh.accOtrM = new tabRoute<addrIP>("rx");
        neigh.accOtrF = new tabRoute<addrIP>("rx");
        neigh.accOtrS = new tabRoute<addrIP>("rx");
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
        neigh.accSrte = new tabRoute<addrIP>("rx");
        neigh.accLnks = new tabRoute<addrIP>("rx");
        neigh.accMvpn = new tabRoute<addrIP>("rx");
        neigh.accMvpo = new tabRoute<addrIP>("rx");
        if (neigh.dampenPfxs != null) {
            neigh.dampenPfxs = new tabGen<rtrBgpDamp>();
        }
        ready2adv = true;
        if (debugger.rtrBgpFull) {
            logger.debug("neighbor up");
        }
        parent.needFull.add(1);
        parent.compute.wakeup();
        for (;;) {
            if (neigh.advertIntRx > 0) {
                bits.sleep(neigh.advertIntRx);
            }
            typ = packRecv(pckRx);
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
                int len = bits.msbGetW(buf, pos) - sizeU;
                if (len < 0) {
                    break;
                }
                typ = buf[pos + 2] & 0xff;
                pos += sizeC;
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
                    logger.debug("got " + rtrBgpUtil.type2string(typ) + " from " + neigh.peerAddr);
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
     * send one packet
     *
     * @param pck packet to send
     * @param typ type of packet
     */
    public void packSend(packHolder pck, int typ) {
        if (pipe == null) {
            return;
        }
        pck.merge2beg();
        if ((compressTx != null) && (typ == rtrBgpUtil.msgUpdate)) {
            if (debugger.rtrBgpEvnt) {
                logger.debug("sending compressed " + rtrBgpUtil.type2string(typ) + " to " + neigh.peerAddr);
            }
            if (neigh.monitor != null) {
                neigh.monitor.gotMessage(true, typ, this, neigh, pck.getCopy());
            }
            if (neigh.dump != null) {
                neigh.dump.gotMessage(true, typ, neigh, pck.getCopy());
            }
            compressCntr.tx(pck);
            pck.msbPutW(0, pck.dataSize() + sizeU);
            pck.putByte(2, typ);
            pck.putSkip(sizeC);
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
        }
        if (debugger.rtrBgpEvnt) {
            logger.debug("sending " + rtrBgpUtil.type2string(typ) + " to " + neigh.peerAddr);
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
    private int packRecv(packHolder pck) {
        pck.clear();
        if (pipe == null) {
            return -1;
        }
        if (pck.pipeRecv(pipe, 0, sizeU, 144) != sizeU) {
            return -1;
        }
        for (int i = 0; i < 16; i++) {
            if (pck.getByte(i) != 0xff) {
                return -1;
            }
        }
        int len = pck.msbGetW(16) - sizeU;
        int typ = pck.getByte(18);
        if (len < 0) {
            return -1;
        }
        if (len > packHolder.maxData) {
            return -1;
        }
        pck.clear();
        if (len > 0) {
            if (pck.pipeRecv(pipe, 0, len, 144) != len) {
                return -1;
            }
        }
        cntr.rx(pck);
        if (debugger.rtrBgpEvnt) {
            logger.debug("got " + rtrBgpUtil.type2string(typ) + " from " + neigh.peerAddr);
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
     * send keep alive message
     */
    public void sendKeepAlive() {
        packHolder pck = new packHolder(true, true);
        packSend(pck, rtrBgpUtil.msgKeepLiv);
    }

    private byte[] encodeHostname(String s) {
        byte[] buf = s.getBytes();
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
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, neigh.localAs);
        rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capa32bitAsNum, buf);
        buf = new byte[0];
        rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaRouteRefresh, buf);
        safis = parent.mask2list((neigh.addpathRmode | neigh.addpathTmode) & neigh.addrFams);
        if (safis.size() > 0) {
            buf = new byte[safis.size() * 4];
            for (int i = 0; i < safis.size(); i++) {
                int o = safis.get(i);
                bits.msbPutD(buf, i * 4, rtrBgpUtil.safi2triplet(o));
                o = parent.safi2mask(o);
                int m = 0;
                if ((neigh.addpathRmode & o) != 0) {
                    m |= 1;
                }
                if ((neigh.addpathTmode & o) != 0) {
                    m |= 2;
                }
                buf[(i * 4) + 3] = (byte) m;
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaAdditionPath, buf);
        }
        safis = parent.mask2list(neigh.extNextCur & neigh.addrFams);
        if (safis.size() > 0) {
            buf = new byte[safis.size() * 6];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 6) + 0, safis.get(i));
                bits.msbPutW(buf, (i * 6) + 4, parent.afiUni >>> 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtNextHop, buf);
        }
        safis = parent.mask2list(neigh.extNextOtr & neigh.addrFams);
        if (safis.size() > 0) {
            buf = new byte[safis.size() * 6];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 6) + 0, safis.get(i));
                bits.msbPutW(buf, (i * 6) + 4, parent.afiOtrU >>> 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtNextHop, buf);
        }
        safis = parent.mask2list(neigh.graceRestart & neigh.addrFams);
        if (safis.size() > 0) {
            buf = new byte[2 + (safis.size() * 4)];
            bits.msbPutW(buf, 0, ((parent.restartTime / 1000) & 0xfff) | 0x8000);
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 4) + 2, rtrBgpUtil.safi2triplet(safis.get(i)));
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaGraceRestart, buf);
        }
        safis = parent.mask2list(neigh.multiLabel & neigh.addrFams);
        if (safis.size() > 0) {
            buf = new byte[safis.size() * 4];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, i * 4, rtrBgpUtil.safi2triplet(safis.get(i)) | 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaMultiLabel, buf);
        }
        if (neigh.extUpdate) {
            buf = new byte[0];
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtMessage, buf);
        }
        if ((neigh.compressMode & 1) != 0) {
            compressRx = new Inflater[8];
            for (int i = 0; i < compressRx.length; i++) {
                compressRx[i] = new Inflater();
            }
            buf = new byte[2];
            buf[0] = (byte) 0x87; // deflate, 32k window
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaCompress, buf);
        }
        if (neigh.leakRole >= 0) {
            buf = new byte[1];
            buf[0] = (byte) neigh.leakRole;
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaLeakRole, buf);
        }
        if (neigh.hostname > 0) {
            buf = encodeHostname(cfgAll.hostName);
            if (neigh.hostname > 1) {
                buf = bits.byteConcat(buf, encodeHostname(cfgAll.domainName));
            } else {
                buf = bits.byteConcat(buf, encodeHostname(""));
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaHostname, buf);
        }
        pck.merge2beg();
        int i = pck.dataSize();
        if ((!neigh.extOpen) && (i >= 0xff)) {
            logger.error("too much capabilities for peer " + neigh.peerAddr);
            i = 0xfe;
        }
        pck.putByte(0, rtrBgpUtil.version);
        pck.msbPutW(1, rtrBgpUtil.asNum16bit(neigh.localAs));
        pck.msbPutW(3, neigh.holdTimer / 1000);
        buf = parent.routerID.getBytes();
        pck.putCopy(buf, 0, 5, buf.length);
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
        if ((neigh.remoteAs != -1) && (i != rtrBgpUtil.asNum16bit(neigh.remoteAs))) {
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
            typLenVal tlv = rtrBgpUtil.getCapabilityTlv(peerExtOpen);
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
                        if ((neigh.remoteAs != -1) && (i != neigh.remoteAs)) {
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
                    case rtrBgpUtil.capaCompress:
                        if ((neigh.compressMode & 2) == 0) {
                            break;
                        }
                        compressTx = new Deflater();
                        break;
                    case rtrBgpUtil.capaMultiProto:
                        mpGot = true;
                        for (i = 0; i < tlv.valSiz; i += 4) {
                            int p = bits.msbGetD(tlv.valDat, i);
                            int o = parent.safi2mask(p);
                            if (o < 1) {
                                if (debugger.rtrBgpError) {
                                    logger.debug("unknown (" + p + ") afi");
                                }
                                continue;
                            }
                            peerAfis |= o;
                        }
                        break;
                    case rtrBgpUtil.capaMultiLabel:
                        for (i = 0; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            o = parent.safi2mask(o);
                            if (o < 1) {
                                continue;
                            }
                            peerMltLab |= o;
                        }
                        break;
                    case rtrBgpUtil.capaGraceRestart:
                        for (i = 2; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            o = parent.safi2mask(o);
                            if (o < 1) {
                                continue;
                            }
                            peerGrace |= o;
                        }
                        break;
                    case rtrBgpUtil.capaExtNextHop:
                        for (i = 0; i < tlv.valSiz; i += 6) {
                            int o = bits.msbGetD(tlv.valDat, i + 0);
                            int p = bits.msbGetW(tlv.valDat, i + 4);
                            o = parent.safi2mask(o);
                            if (o < 1) {
                                continue;
                            }
                            if (p == (parent.afiUni >>> 16)) {
                                peerExtNextCur |= o;
                            }
                            if (p == (parent.afiOtrU >>> 16)) {
                                peerExtNextOtr |= o;
                            }
                        }
                        break;
                    case rtrBgpUtil.capaAdditionPath:
                        for (i = 0; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            o = parent.safi2mask(o);
                            if (o < 1) {
                                continue;
                            }
                            int m = tlv.valDat[i + 3];
                            if ((m & 2) != 0) {
                                addpathRx |= o;
                            }
                            if ((m & 1) != 0) {
                                addpathTx |= o;
                            }
                        }
                        break;
                    case rtrBgpUtil.capaRouteRefresh:
                        peerRefresh = true;
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
            int o = parent.safi2mask(rtrBgpUtil.safiIp4uni);
            if (o > 0) {
                peerAfis |= o;
            }
        }
        if (!neigh.capaNego) {
            peerAfis = neigh.addrFams;
            peerGrace = neigh.graceRestart;
            peerMltLab = neigh.multiLabel;
            peerExtNextCur = neigh.extNextCur;
            peerExtNextOtr = neigh.extNextOtr;
            addpathRx = neigh.addpathRmode;
            addpathTx = neigh.addpathTmode;
            peerRefresh = true;
            peer32bitAS = true;
            if ((neigh.compressMode & 2) != 0) {
                compressTx = new Deflater();
            }
        }
        if (neigh.leakForce || ((neigh.leakRole >= 0) && (peerLeakRole >= 0))) {
            int ned;
            switch (neigh.leakRole) {
                case rtrBgpUtil.roleProv:
                    ned = rtrBgpUtil.roleCust;
                    break;
                case rtrBgpUtil.roleRs:
                    ned = rtrBgpUtil.roleRsc;
                    break;
                case rtrBgpUtil.roleRsc:
                    ned = rtrBgpUtil.roleRs;
                    break;
                case rtrBgpUtil.roleCust:
                    ned = rtrBgpUtil.roleProv;
                    break;
                case rtrBgpUtil.rolePeer:
                    ned = rtrBgpUtil.rolePeer;
                    break;
                default:
                    ned = -1;
                    break;
            }
            if (peerLeakRole != ned) {
                logger.info("neighbor " + neigh.peerAddr + " sent wrong role " + rtrBgpUtil.leakRole2string(peerLeakRole, false));
                sendNotify(2, 8);
                return true;
            }
        }
        originalSafiList = peerAfis;
        originalAddRlist = addpathRx;
        originalAddTlist = addpathTx;
        peerAfis &= neigh.addrFams;
        if (peerAfis == 0) {
            logger.info("neighbor " + neigh.peerAddr + " in wrong safi");
            sendNotify(6, 3);
            return true;
        }
        addpathRx &= neigh.addpathRmode;
        addpathTx &= neigh.addpathTmode;
        addpathRx &= neigh.addrFams;
        addpathTx &= neigh.addrFams;
        if (debugger.rtrBgpEvnt) {
            logger.debug("peer " + neigh.peerAddr + " id=" + peerRouterID + " hold=" + peerHold + " 32bitAS=" + peer32bitAS + " refresh=" + peerRefresh);
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
    }

    /**
     * got route refresh request
     *
     * @param safi safi to refresh
     */
    public void gotRefresh(int safi) {
        if (!afiMsk(peerAfis, safi)) {
            if (debugger.rtrBgpError) {
                logger.debug("got unknown refresh from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
            }
            return;
        }
        refreshRx++;
        tabRoute<addrIP> adverted = getAdverted(safi);
        if (adverted == null) {
            return;
        }
        adverted.clear();
        needFull.set(3);
        adversion.sub(1);
        neigh.transmit.wakeup();
        if (debugger.rtrBgpTraf) {
            logger.debug("got refresh from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
        }
    }

    /**
     * send route refresh request
     *
     * @param safi safi to refresh
     */
    public void sendRefresh(int safi) {
        if (!peerRefresh) {
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
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, safi);
        pck.putSkip(4);
        packSend(pck, rtrBgpUtil.msgRefrsh);
        if (debugger.rtrBgpTraf) {
            logger.debug("sent refresh to peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
        }
    }

    /**
     * get prefix count
     *
     * @return number
     */
    public int getPrefixGot() {
        return lrnUni.size() + lrnMlt.size() + lrnOtrU.size()
                + lrnOtrM.size() + lrnOtrF.size() + lrnOtrS.size() + lrnFlw.size()
                + lrnVpnU.size() + lrnVpnM.size() + lrnVpnF.size()
                + lrnVpoU.size() + lrnVpoM.size() + lrnVpoF.size()
                + lrnVpls.size() + lrnMspw.size() + lrnEvpn.size()
                + lrnMdt.size() + lrnNsh.size() + lrnSrte.size()
                + lrnLnks.size() + lrnMvpn.size() + lrnMvpo.size();
    }

    /**
     * get prefix count
     *
     * @return number
     */
    public int getPrefixSent() {
        return advUni.size() + advMlt.size() + advOtrU.size()
                + advOtrM.size() + advOtrF.size() + advOtrS.size() + advFlw.size()
                + advVpnU.size() + advVpnM.size() + advVpnF.size()
                + advVpoU.size() + advVpoM.size() + advVpoF.size()
                + advVpls.size() + advMspw.size() + advEvpn.size()
                + advMdt.size() + advNsh.size() + advSrte.size()
                + advLnks.size() + advMvpn.size() + advMvpo.size();
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
        pckTx.clear();
        rtrBgpUtil.createEndOfRib(pckTx, pckTh, safi);
        packSend(pckTx, rtrBgpUtil.msgUpdate);
    }

    /**
     * send update packet
     *
     * @param safi safi to update
     * @param wil prefix to advertise, null to withdraw
     * @param don old already advertised data
     */
    public void sendUpdate(int safi, tabRouteEntry<addrIP> wil, tabRouteEntry<addrIP> don) {
        if (debugger.rtrBgpTraf) {
            String a;
            String s;
            if (wil == null) {
                a = "withdraw";
                s = tabRtrmapN.rd2string(don.rouDst) + " " + don.prefix;
            } else {
                a = "reachable";
                s = tabRtrmapN.rd2string(wil.rouDst) + " " + wil.prefix;
            }
            logger.debug("update to peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi) + ": " + a + " " + s);
        }
        boolean addpath = addPthTx(safi);
        List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
        if (wil == null) {
            if (!addpath) {
                lst.add(don);
                pckTx.clear();
                rtrBgpUtil.createWithdraw(pckTx, pckTh, safi, addpath, lst);
                packSend(pckTx, rtrBgpUtil.msgUpdate);
                return;
            }
            for (int i = 0; i < don.alts.size(); i++) {
                tabRouteEntry<addrIP> ntry = don.copyBytes(tabRoute.addType.notyet);
                don.alts.get(i).copyBytes(ntry.best, true);
                ntry.best.ident = addpathBeg + i;
                lst.add(ntry);
            }
            pckTx.clear();
            rtrBgpUtil.createWithdraw(pckTx, pckTh, safi, addpath, lst);
            packSend(pckTx, rtrBgpUtil.msgUpdate);
            return;
        }
        if (!addpath) {
            if (don != null) {
                if (!wil.best.differs(don.best)) {
                    return;
                }
            }
            lst.add(wil);
            pckTx.clear();
            rtrBgpUtil.createReachable(pckTx, pckTh, safi, addpath, peer32bitAS, lst, null);
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
                rtrBgpUtil.createReachable(pckTx, pckTh, safi, addpath, peer32bitAS, lst, null);
                packSend(pckTx, rtrBgpUtil.msgUpdate);
            }
            return;
        }
        for (int i = 0; i < wil.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = wil.alts.get(i);
            if (i < don.alts.size()) {
                if (!attr.differs(don.alts.get(i))) {
                    continue;
                }
            }
            tabRouteEntry<addrIP> ntry = wil.copyBytes(tabRoute.addType.notyet);
            attr.copyBytes(ntry.best, true);
            ntry.best.ident = addpathBeg + i;
            lst.clear();
            lst.add(ntry);
            pckTx.clear();
            rtrBgpUtil.createReachable(pckTx, pckTh, safi, addpath, peer32bitAS, lst, null);
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
        rtrBgpUtil.createWithdraw(pckTx, pckTh, safi, addpath, lst);
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
        currOtrU.clear();
        currOtrM.clear();
        currOtrF.clear();
        currOtrS.clear();
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
        currSrte.clear();
        currLnks.clear();
        currMvpn.clear();
        currMvpo.clear();
        currChg = 0;
        int prt = pck.msbGetW(0);
        pck.getSkip(2);
        prt = pck.dataSize() - prt;
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.best.rouTyp = neigh.lower.rouTyp;
        ntry.best.protoNum = neigh.lower.rtrNum;
        ntry.best.distance = neigh.distance;
        ntry.best.rouSrc = neigh.peerType;
        ntry.best.srcRtr = neigh.peerAddr.copyBytes();
        ntry.best.locPref = 100;
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
            prefixWithdraw(rtrBgpUtil.safiIp4uni, addpath, res);
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
            prefixReach(rtrBgpUtil.safiIp4uni, addpath, res);
        }
        addAttribed(currUni, parent.afiUni, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribed(currUni, parent.afiLab, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribed(currMlt, parent.afiMlt, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribed(currOtrU, parent.afiOtrL, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribed(currOtrU, parent.afiOtrU, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribed(currOtrM, parent.afiOtrM, ntry, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
        addAttribed(currFlw, parent.afiFlw, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currOtrF, parent.afiOtrF, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribed(currSrte, parent.afiSrte, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currOtrS, parent.afiOtrS, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribed(currVpnU, parent.afiVpnU, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currVpnM, parent.afiVpnM, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currVpnF, parent.afiVpnF, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currVpoU, parent.afiVpoU, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribed(currVpoM, parent.afiVpoM, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribed(currVpoF, parent.afiVpoF, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribed(currVpls, parent.afiVpls, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currMspw, parent.afiMspw, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currEvpn, parent.afiEvpn, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currMdt, parent.afiMdt, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currNsh, parent.afiNsh, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currMvpn, parent.afiMvpn, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        addAttribed(currMvpo, parent.afiMvpo, ntry, neigh.wroumapIn, neigh.wroupolIn, null);
        addAttribed(currLnks, parent.afiLnks, ntry, neigh.vroumapIn, neigh.vroupolIn, null);
        if ((currChg > 0) && (rxReady() < (neigh.bufferSize / 4))) {
            parent.compute.wakeup();
        }
        if (neigh.maxPrefixCnt < 1) {
            return false;
        }
        int i = getPrefixGot();
        if (i > ((neigh.maxPrefixCnt * neigh.maxPrefixPrc) / 100)) {
            logger.info("neighbor " + neigh.peerAddr + " sent " + i + " prefixes");
        }
        if (i > neigh.maxPrefixCnt) {
            sendNotify(6, 1);
        }
        return false;
    }

    private void addAttribed(tabRouteEntry<addrIP> cur, boolean addpath, tabRoute<addrIP> learned, tabRoute<addrIP> changed, int safi, tabListing<tabRtrmapN, addrIP> roumap, tabListing<tabRtrplcN, addrIP> roupol, tabListing<tabPrfxlstN, addrIP> prflst) {
        if (parent.flaps != null) {
            parent.prefixFlapped(safi, cur.rouDst, cur.prefix, cur.best.asPathStr());
        }
        if (neigh.dampenPfxs != null) {
            neigh.prefixDampen(safi, cur.rouDst, cur.prefix, neigh.dampenAnno);
        }
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

    private void addAttribed(List<tabRouteEntry<addrIP>> currAdd, int safi, tabRouteEntry<addrIP> attr, tabListing<tabRtrmapN, addrIP> roumap, tabListing<tabRtrplcN, addrIP> roupol, tabListing<tabPrfxlstN, addrIP> prflst) {
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
            attr.best.ident = pref.best.ident;
            attr.best.nextHop = pref.best.nextHop;
            attr.best.labelRem = pref.best.labelRem;
            attr.best.evpnLab = pref.best.evpnLab;
            attr.best.copyBytes(pref.best, false);
            addAttribed(pref, addpath, learned, changed, safi, roumap, roupol, prflst);
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
     */
    protected void prefixWithdraw(int safi, boolean addpath, tabRouteEntry<addrIP> ntry) {
        if (debugger.rtrBgpTraf) {
            logger.debug("withdraw " + rtrBgpUtil.safi2string(safi) + " " + tabRtrmapN.rd2string(ntry.rouDst) + " " + ntry.prefix + " " + ntry.best.ident);
        }
        if (!afiMsk(peerAfis, safi)) {
            if (debugger.rtrBgpError) {
                logger.debug("got unknown withdraw from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
            }
            return;
        }
        if (parent.flaps != null) {
            parent.prefixFlapped(safi, ntry.rouDst, ntry.prefix, "gone");
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
     */
    protected void prefixReach(int safi, boolean addpath, tabRouteEntry<addrIP> ntry) {
        if (debugger.rtrBgpTraf) {
            logger.debug("reachable " + rtrBgpUtil.safi2string(safi) + " " + tabRtrmapN.rd2string(ntry.rouDst) + " " + ntry.prefix + " " + ntry.best.ident);
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
        if (safi == parent.afiMlt) {
            trg = currMlt;
        }
        if (safi == parent.afiOtrL) {
            trg = currOtrU;
        }
        if (safi == parent.afiOtrU) {
            trg = currOtrU;
        }
        if (safi == parent.afiOtrM) {
            trg = currOtrM;
        }
        if (safi == parent.afiOtrF) {
            trg = currOtrF;
        }
        if (safi == parent.afiOtrS) {
            trg = currOtrS;
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
            logger.debug("processing " + tabRtrmapN.rd2string(ntry.rouDst) + " " + ntry.prefix);
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
                    if (rtrBgpUtil.firstIntList(ntry.best.pathSeq, neigh.remoteAs)) {
                        repAsPath++;
                        return true;
                    }
                    break;
                case rtrBgpUtil.peerCnfed:
                    if (rtrBgpUtil.firstIntList(ntry.best.confSeq, neigh.remoteAs)) {
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
                    if (rtrBgpUtil.findIntList(ntry.best.pathSeq, neigh.localAs) >= 0) {
                        repAsPath++;
                        return true;
                    }
                    if (rtrBgpUtil.findIntList(ntry.best.pathSet, neigh.localAs) >= 0) {
                        repAsPath++;
                        return true;
                    }
                    break;
                case rtrBgpUtil.peerCnfed:
                    if (rtrBgpUtil.findIntList(ntry.best.confSeq, neigh.localAs) >= 0) {
                        repAsConf++;
                        return true;
                    }
                    if (rtrBgpUtil.findIntList(ntry.best.confSet, neigh.localAs) >= 0) {
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
                    if (a.compare(ntry.best.originator, a) == 0) {
                        repOrgnId++;
                        return true;
                    }
                }
                if (ntry.best.clustList != null) {
                    if (rtrBgpUtil.findAddrList(ntry.best.clustList, a) >= 0) {
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
                if (a.compare(ntry.best.originator, a) == 0) {
                    repOrgnId++;
                    return true;
                }
                if (ntry.best.clustList == null) {
                    ntry.best.clustList = new ArrayList<addrIP>();
                }
                if (rtrBgpUtil.findAddrList(ntry.best.clustList, a) >= 0) {
                    repClstrL++;
                    return true;
                }
                ntry.best.clustList.add(a);
                break;
            default:
                break;
        }
        if (neigh.nxtHopPeer) {
            if ((neigh.otherAdr != null) && ((safi == parent.afiOtrU) || (safi == parent.afiOtrM))) {
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
            ntry.best.extComm.add(tabRtrmapN.dmzbw2comm(neigh.localAs, neigh.dmzLinkBw));
        }
        if (neigh.removePrivAsIn) {
            rtrBgpUtil.removePrivateAs(ntry.best.pathSeq);
            rtrBgpUtil.removePrivateAs(ntry.best.pathSet);
        }
        if (neigh.overridePeerIn) {
            rtrBgpUtil.replaceIntList(ntry.best.pathSeq, neigh.remoteAs, neigh.localAs);
            rtrBgpUtil.replaceIntList(ntry.best.pathSet, neigh.remoteAs, neigh.localAs);
        }
        if (neigh.intVpnClnt) {
            rtrBgpUtil.encodeAttribSet(neigh.localAs, ntry);
        }
        return false;
    }

}
