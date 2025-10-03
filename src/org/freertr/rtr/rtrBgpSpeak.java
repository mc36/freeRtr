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
     * learned sdwan prefixes
     */
    public final tabRoute<addrIP> lrnSdw = new tabRoute<addrIP>("rx");

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
     * advertised sdwan prefixes
     */
    public final tabRoute<addrIP> advSdw = new tabRoute<addrIP>("tx");

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
    public boolean[] peerAfis;

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
    public boolean[] needEorAfis;

    /**
     * eof needs in address families
     */
    public boolean[] needEofAfis;

    /**
     * peer graceful restart capability
     */
    public boolean[] peerGrace;

    /**
     * peer long lived graceful restart capability
     */
    public boolean[] peerLlGrace;

    /**
     * peer multiple labels capability
     */
    public boolean[] peerMltLab;

    /**
     * peer extended nexthop capability
     */
    public boolean[] peerExtNextCur;

    /**
     * peer extended nexthop capability
     */
    public boolean[] peerExtNextOtr;

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
    public boolean[] addpathRx;

    /**
     * peer receives additional paths
     */
    public boolean[] addpathTx;

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
    public boolean[] originalSafiList;

    /**
     * addpath list sent by the peer
     */
    public boolean[] originalAddRlist;

    /**
     * addpath list sent by the peer
     */
    public boolean[] originalAddTlist;

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
     * @param res resume, 0=disabled, 1=normal, 2=resume
     */
    public rtrBgpSpeak(rtrBgp protocol, rtrBgpNeigh neighbor, pipeSide socket, int res) {
        peerAfis = rtrBgpParam.boolsSet(false);
        originalSafiList = rtrBgpParam.boolsSet(false);
        peerGrace = rtrBgpParam.boolsSet(false);
        peerLlGrace = rtrBgpParam.boolsSet(false);
        peerMltLab = rtrBgpParam.boolsSet(false);
        peerExtNextCur = rtrBgpParam.boolsSet(false);
        peerExtNextOtr = rtrBgpParam.boolsSet(false);
        addpathRx = rtrBgpParam.boolsSet(false);
        addpathTx = rtrBgpParam.boolsSet(false);
        originalAddRlist = rtrBgpParam.boolsSet(false);
        originalAddTlist = rtrBgpParam.boolsSet(false);
        needEorAfis = rtrBgpParam.boolsSet(false);
        needEofAfis = rtrBgpParam.boolsSet(false);
        ready2adv = false;
        resumed = res == 2;
        addpathBeg = bits.randomD();
        parent = protocol;
        neigh = neighbor;
        pipe = socket;
        peerLeakRole = -1;
        upTime = bits.getTime();
        lastRx = upTime;
        if (res == 0) {
            peer32bitAS = true;
            return;
        }
        new Thread(this).start();
    }

    /**
     * get learned
     *
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getLearned(long mask, int safi) {
        if (mask == rtrBgpParam.mskUni) {
            return lrnUni;
        }
        if (mask == rtrBgpParam.mskLab) {
            return lrnUni;
        }
        if (mask == rtrBgpParam.mskCtp) {
            return lrnUni;
        }
        if (mask == rtrBgpParam.mskCar) {
            return lrnUni;
        }
        if (mask == rtrBgpParam.mskMlt) {
            return lrnMlt;
        }
        if (mask == rtrBgpParam.mskOlab) {
            return lrnOuni;
        }
        if (mask == rtrBgpParam.mskOctp) {
            return lrnOuni;
        }
        if (mask == rtrBgpParam.mskOcar) {
            return lrnOuni;
        }
        if (mask == rtrBgpParam.mskOuni) {
            return lrnOuni;
        }
        if (mask == rtrBgpParam.mskOmlt) {
            return lrnOmlt;
        }
        if (mask == rtrBgpParam.mskOflw) {
            return lrnOflw;
        }
        if (mask == rtrBgpParam.mskOsrt) {
            return lrnOsrt;
        }
        if (mask == rtrBgpParam.mskFlw) {
            return lrnFlw;
        }
        if (mask == rtrBgpParam.mskVpnU) {
            return lrnVpnU;
        }
        if (mask == rtrBgpParam.mskVpnM) {
            return lrnVpnM;
        }
        if (mask == rtrBgpParam.mskVpnF) {
            return lrnVpnF;
        }
        if (mask == rtrBgpParam.mskVpoU) {
            return lrnVpoU;
        }
        if (mask == rtrBgpParam.mskVpoM) {
            return lrnVpoM;
        }
        if (mask == rtrBgpParam.mskVpoF) {
            return lrnVpoF;
        }
        if (mask == rtrBgpParam.mskVpls) {
            return lrnVpls;
        }
        if (mask == rtrBgpParam.mskMspw) {
            return lrnMspw;
        }
        if (mask == rtrBgpParam.mskEvpn) {
            return lrnEvpn;
        }
        if (mask == rtrBgpParam.mskMdt) {
            return lrnMdt;
        }
        if (mask == rtrBgpParam.mskNsh) {
            return lrnNsh;
        }
        if (mask == rtrBgpParam.mskRpd) {
            return lrnRpd;
        }
        if (mask == rtrBgpParam.mskSdw) {
            return lrnSdw;
        }
        if (mask == rtrBgpParam.mskSpf) {
            return lrnSpf;
        }
        if (mask == rtrBgpParam.mskRtf) {
            return lrnRtf;
        }
        if (mask == rtrBgpParam.mskSrte) {
            return lrnSrte;
        }
        if (mask == rtrBgpParam.mskLnks) {
            return lrnLnks;
        }
        if (mask == rtrBgpParam.mskMvpn) {
            return lrnMvpn;
        }
        if (mask == rtrBgpParam.mskMvpo) {
            return lrnMvpo;
        }
        if (mask == rtrBgpParam.mskMtre) {
            return lrnMtre;
        }
        if (mask == rtrBgpParam.mskMtro) {
            return lrnMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * get adverted
     *
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getAdverted(long mask, int safi) {
        if (mask == rtrBgpParam.mskUni) {
            return advUni;
        }
        if (mask == rtrBgpParam.mskLab) {
            return advUni;
        }
        if (mask == rtrBgpParam.mskCtp) {
            return advUni;
        }
        if (mask == rtrBgpParam.mskCar) {
            return advUni;
        }
        if (mask == rtrBgpParam.mskMlt) {
            return advMlt;
        }
        if (mask == rtrBgpParam.mskOlab) {
            return advOuni;
        }
        if (mask == rtrBgpParam.mskOctp) {
            return advOuni;
        }
        if (mask == rtrBgpParam.mskOcar) {
            return advOuni;
        }
        if (mask == rtrBgpParam.mskOuni) {
            return advOuni;
        }
        if (mask == rtrBgpParam.mskOmlt) {
            return advOmlt;
        }
        if (mask == rtrBgpParam.mskOflw) {
            return advOflw;
        }
        if (mask == rtrBgpParam.mskOsrt) {
            return advOsrt;
        }
        if (mask == rtrBgpParam.mskFlw) {
            return advFlw;
        }
        if (mask == rtrBgpParam.mskVpnU) {
            return advVpnU;
        }
        if (mask == rtrBgpParam.mskVpnM) {
            return advVpnM;
        }
        if (mask == rtrBgpParam.mskVpnF) {
            return advVpnF;
        }
        if (mask == rtrBgpParam.mskVpoU) {
            return advVpoU;
        }
        if (mask == rtrBgpParam.mskVpoM) {
            return advVpoM;
        }
        if (mask == rtrBgpParam.mskVpoF) {
            return advVpoF;
        }
        if (mask == rtrBgpParam.mskVpls) {
            return advVpls;
        }
        if (mask == rtrBgpParam.mskMspw) {
            return advMspw;
        }
        if (mask == rtrBgpParam.mskEvpn) {
            return advEvpn;
        }
        if (mask == rtrBgpParam.mskMdt) {
            return advMdt;
        }
        if (mask == rtrBgpParam.mskNsh) {
            return advNsh;
        }
        if (mask == rtrBgpParam.mskRpd) {
            return advRpd;
        }
        if (mask == rtrBgpParam.mskSdw) {
            return advSdw;
        }
        if (mask == rtrBgpParam.mskSpf) {
            return advSpf;
        }
        if (mask == rtrBgpParam.mskRtf) {
            return advRtf;
        }
        if (mask == rtrBgpParam.mskSrte) {
            return advSrte;
        }
        if (mask == rtrBgpParam.mskLnks) {
            return advLnks;
        }
        if (mask == rtrBgpParam.mskMvpn) {
            return advMvpn;
        }
        if (mask == rtrBgpParam.mskMvpo) {
            return advMvpo;
        }
        if (mask == rtrBgpParam.mskMtre) {
            return advMtre;
        }
        if (mask == rtrBgpParam.mskMtro) {
            return advMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
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
        lrnSdw.clear();
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
        advSdw.clear();
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
        neigh.accSdw = new tabRoute<addrIP>("rx");
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
        neigh.wilSdw = new tabRoute<addrIP>("rx");
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
        neigh.chgSdw = new tabRoute<addrIP>("chg");
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
            for (int i = 0; i < peerAfis.length; i++) {
                if (!peerAfis[i]) {
                    continue;
                }
                int o = parent.idx2safi(i);
                if (o < 0) {
                    continue;
                }
                long p = 1L << i;
                sendRefresh(p, o);
                gotRefresh(p, o);
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
        lrnSdw.clear();
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
        advSdw.clear();
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
        neigh.accSdw = new tabRoute<addrIP>("rx");
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
                int i = pckRx.msbGetD(0);
                long o = parent.safi2mask(i & rtrBgpUtil.frsMask);
                if (o < 0) {
                    continue;
                }
                gotRefresh(o, i);
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
                List<Integer> afiI = new ArrayList<Integer>();
                for (;;) {
                    tlv = rtrBgpUtil.getCapabilityTlv(false);
                    if (tlv.getBytes(pckRh)) {
                        break;
                    }
                    if (tlv.valTyp != rtrBgpUtil.capaMultiProto) {
                        sendNotify(8, 4);
                        break;
                    }
                    parseMultiProtoCapa(tlv, afiS, afiM, afiI);
                }
                for (i = afiS.size() - 1; i >= 0; i--) {
                    int o = afiS.get(i);
                    long p = afiM.get(i);
                    int q = afiI.get(i);
                    if (peerAfis[q] == add) {
                        renegotiatingSafi(q, p, o, add, false);
                        continue;
                    }
                    afiS.remove(i);
                    afiM.remove(i);
                    afiI.remove(i);
                    renegotiatingSafi(q, p, o, add, false);
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
                    int q = afiI.get(i);
                    renegotiatingSafi(q, p, o, add, false);
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
                    int i = pckRx.msbGetD(0);
                    long o = parent.safi2mask(i & rtrBgpUtil.frsMask);
                    if (o < 0) {
                        continue;
                    }
                    gotRefresh(o, i);
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
        rtrBgpUtil.updtStatsArr(dir, parent.msgStats, typ, pck);
        rtrBgpUtil.updtStatsArr(dir, neigh.msgStats, typ, pck);
        if (!rtrBgpUtil.isUnknownMsg(typ)) {
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
        rtrBgpUtil.updtStatsArr(dir, parent.attrStats, typ, pck);
        rtrBgpUtil.updtStatsArr(dir, neigh.attrStats, typ, pck);
        if (!rtrBgpUtil.isUnknownAttr(typ)) {
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
                parent.reachabStat.rx(pck);
                neigh.reachabStat.rx(pck);
                break;
            case 1:
                parent.reachabStat.tx(pck);
                neigh.reachabStat.tx(pck);
                break;
            case 2:
                parent.unreachStat.rx(pck);
                neigh.unreachStat.rx(pck);
                break;
            case 3:
                parent.unreachStat.tx(pck);
                neigh.unreachStat.tx(pck);
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

    private List<Integer> mask2list(boolean[] mask) {
        mask = rtrBgpParam.boolsAnd(mask, neigh.addrFams);
        List<Integer> safis = new ArrayList<Integer>();
        for (int i = 0; i < mask.length; i++) {
            if (!mask[i]) {
                continue;
            }
            int o = parent.idx2safi(i);
            if (o < 0) {
                continue;
            }
            safis.add(o);
        }
        return safis;
    }

    /**
     * send open message
     */
    public void sendOpen() {
        List<Integer> safis = mask2list(neigh.addrFams);
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
        safis = mask2list(rtrBgpParam.boolsOr(neigh.addpathRmode, neigh.addpathTmode));
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 4];
            for (int i = 0; i < safis.size(); i++) {
                int o = safis.get(i);
                bits.msbPutD(buf, i * 4, rtrBgpUtil.safi2triplet(o));
                int p = parent.safi2idx(o);
                if (p < 0) {
                    continue;
                }
                int m = 0;
                if (neigh.addpathRmode[p]) {
                    m |= 1;
                }
                if (neigh.addpathTmode[p]) {
                    m |= 2;
                }
                buf[(i * 4) + 3] = (byte) m;
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaAdditionPath, buf);
        }
        safis = mask2list(neigh.extNextCur);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 6];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 6) + 0, safis.get(i));
                bits.msbPutW(buf, (i * 6) + 4, parent.afiUni >>> 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtNextHop, buf);
        }
        safis = mask2list(neigh.extNextOtr);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 6];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 6) + 0, safis.get(i));
                bits.msbPutW(buf, (i * 6) + 4, parent.afiOuni >>> 16);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaExtNextHop, buf);
        }
        safis = mask2list(neigh.graceRestart);
        if (safis.size() > 0) {
            byte[] buf = new byte[2 + (safis.size() * 4)];
            bits.msbPutW(buf, 0, ((parent.restartTime / 1000) & 0xfff) | 0x8000);
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 4) + 2, rtrBgpUtil.safi2triplet(safis.get(i)));
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaGraceRestart, buf);
        }
        safis = mask2list(neigh.llGraceRestart);
        if (safis.size() > 0) {
            byte[] buf = new byte[safis.size() * 7];
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 7) + 0, rtrBgpUtil.safi2triplet(safis.get(i)));
                bits.msbPutD(buf, (i * 7) + 3, (parent.llRestartTime / 1000) & 0xffffff);
            }
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaLongGrace, buf);
        }
        safis = mask2list(neigh.multiLabel);
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
        if (neigh.nxtHopLnkLoc) {
            rtrBgpUtil.placeCapability(pck, neigh.extOpen, rtrBgpUtil.capaLinkLocal, new byte[0]);
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

    private boolean[] parseMultiProtoCapa(encTlv tlv, List<Integer> afi, List<Long> msk, List<Integer> idx) {
        boolean[] res = rtrBgpParam.boolsSet(false);
        for (int i = 0; i < tlv.valSiz; i += 4) {
            int p = bits.msbGetD(tlv.valDat, i);
            long o = parent.safi2mask(p);
            if (o < 1) {
                continue;
            }
            int q = parent.safi2idx(p);
            if (q < 0) {
                continue;
            }
            afi.add(p);
            msk.add(o);
            idx.add(q);
            res[q] = true;
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
                        peerAfis = rtrBgpParam.boolsOr(peerAfis, parseMultiProtoCapa(tlv, new ArrayList<Integer>(), new ArrayList<Long>(), new ArrayList<Integer>()));
                        break;
                    case rtrBgpUtil.capaMultiLabel:
                        for (i = 0; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            int p = parent.safi2idx(o);
                            if (p < 0) {
                                continue;
                            }
                            peerMltLab[p] = true;
                        }
                        break;
                    case rtrBgpUtil.capaGraceRestart:
                        for (i = 2; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            int p = parent.safi2idx(o);
                            if (p < 0) {
                                continue;
                            }
                            peerGrace[p] = true;
                        }
                        break;
                    case rtrBgpUtil.capaLongGrace:
                        for (i = 0; i < tlv.valSiz; i += 7) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            int p = parent.safi2idx(o);
                            if (p < 0) {
                                continue;
                            }
                            peerLlGrace[p] = true;
                        }
                        break;
                    case rtrBgpUtil.capaExtNextHop:
                        for (i = 0; i < tlv.valSiz; i += 6) {
                            int o = bits.msbGetD(tlv.valDat, i + 0);
                            int p = bits.msbGetW(tlv.valDat, i + 4);
                            int q = parent.safi2idx(o);
                            if (q < 0) {
                                continue;
                            }
                            if (p == (parent.afiUni >>> 16)) {
                                peerExtNextCur[q] = true;
                            }
                            if (p == (parent.afiOuni >>> 16)) {
                                peerExtNextOtr[q] = true;
                            }
                        }
                        break;
                    case rtrBgpUtil.capaAdditionPath:
                        for (i = 0; i < tlv.valSiz; i += 4) {
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = rtrBgpUtil.triplet2safi(o);
                            int p = parent.safi2idx(o);
                            if (p < 0) {
                                continue;
                            }
                            int m = tlv.valDat[i + 3];
                            if ((m & 2) != 0) {
                                addpathRx[p] = true;
                            }
                            if ((m & 1) != 0) {
                                addpathTx[p] = true;
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
            int o = parent.safi2idx(rtrBgpUtil.safiIp4uni);
            if (o > 0) {
                peerAfis[o] = true;
            }
        }
        if (!neigh.capaNego) {
            peerAfis = rtrBgpParam.boolsCopy(neigh.addrFams);
            peerDynCap = neigh.dynamicCapab;
            peerGrace = rtrBgpParam.boolsCopy(neigh.graceRestart);
            peerLlGrace = rtrBgpParam.boolsCopy(neigh.llGraceRestart);
            peerMltLab = rtrBgpParam.boolsCopy(neigh.multiLabel);
            peerExtNextCur = rtrBgpParam.boolsCopy(neigh.extNextCur);
            peerExtNextOtr = rtrBgpParam.boolsCopy(neigh.extNextOtr);
            addpathRx = rtrBgpParam.boolsCopy(neigh.addpathRmode);
            addpathTx = rtrBgpParam.boolsCopy(neigh.addpathTmode);
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
        originalSafiList = rtrBgpParam.boolsCopy(peerAfis);
        originalAddRlist = rtrBgpParam.boolsCopy(addpathRx);
        originalAddTlist = rtrBgpParam.boolsCopy(addpathTx);
        peer32bitAS &= neigh.wideAsPath;
        peerRefreshOld &= neigh.routeRefreshOld;
        peerRefreshNew &= neigh.routeRefreshNew;
        peerAfis = rtrBgpParam.boolsAnd(peerAfis, neigh.addrFams);
        if (mask2list(peerAfis).size() < 1) {
            logger.info("neighbor " + neigh.peerAddr + " in wrong safi");
            sendNotify(6, 3);
            return true;
        }
        needEorAfis = rtrBgpParam.boolsCopy(peerAfis);
        addpathRx = rtrBgpParam.boolsAnd(addpathRx, neigh.addpathRmode);
        addpathRx = rtrBgpParam.boolsAnd(addpathRx, neigh.addrFams);
        addpathTx = rtrBgpParam.boolsAnd(addpathTx, neigh.addpathTmode);
        addpathTx = rtrBgpParam.boolsAnd(addpathTx, neigh.addrFams);
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
     * @param mask safi to refresh
     * @param safi safi to refresh
     */
    public void gotRefresh(long mask, int safi) {
        int mode = (safi >>> 8) & 0xff;
        safi &= rtrBgpUtil.frsMask;
        int idx = parent.safi2idx(safi);
        if (idx < 0) {
            return;
        }
        if (debugger.rtrBgpTraf) {
            logger.debug("got refresh mode " + mode + " from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
        }
        if (!peerAfis[idx]) {
            if (debugger.rtrBgpError) {
                logger.debug("got unknown refresh from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
            }
            return;
        }
        if (mode == 1) {
            tabRoute<addrIP> learned = getLearned(mask, safi);
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
        tabRoute<addrIP> adverted = getAdverted(mask, safi);
        if (adverted == null) {
            return;
        }
        if (peerRefreshNew) {
            needEofAfis[idx] = true;
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
        int idx = parent.safi2idx(safi);
        if (idx < 0) {
            return;
        }
        if (peerAfis[idx] == add) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, safi);
        rtrBgpUtil.placeCapability(pck, peerExtOpen, rtrBgpUtil.capaMultiProto, buf);
        dynCapaTx++;
        sendDynCapaMsg(init, true, add, dynCapaTx, pck);
        renegotiatingSafi(idx, msk, safi, add, true);
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

    private void renegotiatingSafi(int idx, long mask, int safi, boolean add, boolean cfg) {
        sendEndOfRib(safi);
        getLearned(mask, safi).clear();
        getAdverted(mask, safi).clear();
        neigh.getWilling(mask, safi).clear();
        neigh.getAccepted(mask, safi).clear();
        needEorAfis[idx] = true;
        peerAfis[idx] = add;
        originalSafiList[idx] = add;
        if (cfg) {
            neigh.addrFams[idx] = add;
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
     * @param mask safi to refresh
     * @param safi safi to refresh
     */
    public void sendRefresh(long mask, int safi) {
        if ((peerRefreshOld == false) && (peerRefreshNew == false)) {
            return;
        }
        int idx = parent.safi2idx(safi);
        if (idx < 0) {
            return;
        }
        if (!peerAfis[idx]) {
            return;
        }
        refreshTx++;
        tabRoute<addrIP> learned = getLearned(mask, safi);
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
                + lrnRpd.size() + lrnSdw.size() + lrnSpf.size()
                + lrnSrte.size() + lrnLnks.size() + lrnRtf.size()
                + lrnMvpn.size() + lrnMvpo.size() + lrnMtre.size()
                + lrnMtro.size();
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
                + advRpd.size() + advSdw.size() + advSpf.size()
                + advSrte.size() + advLnks.size() + advRtf.size()
                + advMvpn.size() + advMvpo.size() + advMtre.size()
                + advMtro.size();
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
     * @param oneLab just one label
     * @param lst list of prefixes to advertise
     * @param reach true to reachable, false to withdraw
     */
    public void sendUpdateSP(int safi, boolean oneLab, List<tabRouteEntry<addrIP>> lst, boolean reach) {
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
            rtrBgpUtil.createReachable(this, pckTx, pckTh, safi, false, oneLab, lst);
        }
        packSend(pckTx, rtrBgpUtil.msgUpdate);
    }

    /**
     * send update packet with addpath
     *
     * @param safi safi to update
     * @param oneLab just one label
     * @param wil prefix to advertise, null to withdraw
     * @param don old already advertised data
     */
    public void sendUpdateAP(int safi, boolean oneLab, tabRouteEntry<addrIP> wil, tabRouteEntry<addrIP> don) {
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
                rtrBgpUtil.createReachable(this, pckTx, pckTh, safi, true, oneLab, lst);
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
            rtrBgpUtil.createReachable(this, pckTx, pckTh, safi, true, oneLab, lst);
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
        currChg = 0;
        int origOfs = pck.dataSize();
        int prt = pck.msbGetW(0);
        pck.getSkip(2);
        prt = pck.dataSize() - prt;
        List<tabRouteEntry<addrIP>> currAdd = new ArrayList<tabRouteEntry<addrIP>>();
        List<tabRouteEntry<addrIP>> currDel = new ArrayList<tabRouteEntry<addrIP>>();
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.best.rouTyp = neigh.lower.rouTyp;
        ntry.best.protoNum = neigh.lower.rtrNum;
        ntry.best.distance = neigh.distance;
        ntry.best.rouSrc = neigh.peerType;
        ntry.best.srcRtr = neigh.peerAddr.copyBytes();
        ntry.best.locPref = neigh.preference;
        int idx = parent.safi2idx(rtrBgpUtil.safiIp4uni);
        if (idx < 0) {
            return true;
        }
        long mask = 1L << idx;
        boolean addpath = addpathRx[idx];
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
            res.oldDst = idx;
            res.best.ident = ident;
            currDel.add(res);
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
            rtrBgpUtil.interpretAttribute(this, ntry, currAdd, currDel, hlp);
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
            res.oldDst = idx;
            res.best.ident = ident;
            res.best.nextHop = ntry.best.nextHop;
            currAdd.add(res);
        }
        if (ntry.best.unknown != null) {
            packHolder origPck = pck.copyBytes(false, false);
            origPck.setBytesLeft(origOfs);
            unknownCntr.rx(origPck);
            if (neigh.unknownsColl != null) {
                neigh.unknownsColl.gotMessage(false, rtrBgpUtil.msgUpdate, neigh, origPck.getCopy());
            }
            if (neigh.unknownsLog) {
                logger.info("got update with unknowns " + neigh.peerAddr + " -> " + neigh.localAddr + " " + origPck.dump());
            }
        }
        tabRouteUtil.removeUnknowns(ntry.best, neigh.unknownsIn);
        int ortf = lrnRtf.size();
        for (int i = 0; i < currDel.size(); i++) {
            tabRouteEntry<addrIP> res = currDel.get(i);
            idx = (int) res.oldDst;
            res.oldDst = 0;
            mask = 1L << idx;
            int safi = parent.idx2safi(idx);
            addpath = addpathRx[idx];
            updateRchblCntr(2, pck);
            if (debugger.rtrBgpTraf) {
                logger.debug("withdraw " + rtrBgpUtil.safi2string(safi) + " " + tabRouteUtil.rd2string(res.rouDst) + " " + res.prefix + " " + res.best.ident);
            }
            if (!peerAfis[idx]) {
                if (debugger.rtrBgpError) {
                    logger.debug("got unknown withdraw from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
                }
                continue;
            }
            if (parent.flaps != null) {
                parent.prefixFlapped(mask, safi, res.rouDst, res.prefix, null);
            }
            if (neigh.dampenPfxs != null) {
                neigh.prefixDampen(mask, safi, res.rouDst, res.prefix, neigh.dampenWthd);
            }
            tabRoute<addrIP> learned = getLearned(mask, safi);
            if (learned == null) {
                continue;
            }
            if (doPrefDel(learned, addpath, res)) {
                continue;
            }
            currChg++;
            tabRoute<addrIP> changed = parent.getChanged(mask, safi);
            if (changed == null) {
                if (debugger.rtrBgpFull) {
                    logger.debug("table not found");
                }
                parent.needFull.add(1);
                continue;
            }
            changed.add(tabRoute.addType.always, res, true, false);
        }
        for (int i = 0; i < currAdd.size(); i++) {
            tabRouteEntry<addrIP> res = currAdd.get(i);
            idx = (int) res.oldDst;
            res.oldDst = 0;
            mask = 1L << idx;
            int safi = parent.idx2safi(idx);
            addpath = addpathRx[idx];
            updateRchblCntr(0, pck);
            if (debugger.rtrBgpTraf) {
                logger.debug("reachable " + rtrBgpUtil.safi2string(safi) + " " + tabRouteUtil.rd2string(res.rouDst) + " " + res.prefix + " " + res.best.ident);
            }
            if (!peerAfis[idx]) {
                if (debugger.rtrBgpError) {
                    logger.debug("got unknown reachable from peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi));
                }
                continue;
            }
            tabRoute<addrIP> learned = getLearned(mask, safi);
            if (learned == null) {
                continue;
            }
            tabRoute<addrIP> changed = parent.getChanged(mask, safi);
            if (changed == null) {
                continue;
            }
            if ((mask & rtrBgpParam.mskFltR) != 0) {
                addAttribedOne(res, ntry, addpath, learned, changed, mask, safi, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
                continue;
            }
            if ((mask & rtrBgpParam.mskFltO) != 0) {
                addAttribedOne(res, ntry, addpath, learned, changed, mask, safi, neigh.oroumapIn, neigh.oroupolIn, neigh.oprflstIn);
                continue;
            }
            if ((mask & rtrBgpParam.mskFltE) != 0) {
                addAttribedOne(res, ntry, addpath, learned, changed, mask, safi, neigh.eroumapIn, neigh.eroupolIn, null);
                continue;
            }
            if ((mask & rtrBgpParam.mskFltW) != 0) {
                addAttribedOne(res, ntry, addpath, learned, changed, mask, safi, neigh.wroumapIn, neigh.wroupolIn, null);
                continue;
            }
            if ((mask & rtrBgpParam.mskFltV) != 0) {
                addAttribedOne(res, ntry, addpath, learned, changed, mask, safi, neigh.vroumapIn, neigh.vroupolIn, null);
                continue;
            }
            addAttribedOne(res, ntry, addpath, learned, changed, mask, safi, null, null, null);
        }
        if (neigh.rtfilterOut && (ortf != lrnRtf.size())) {
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

    private void addAttribedOne(tabRouteEntry<addrIP> cur, tabRouteEntry<addrIP> attr, boolean addpath, tabRoute<addrIP> learned, tabRoute<addrIP> changed, long mask, int safi, tabListing<tabRtrmapN, addrIP> roumap, tabListing<tabRtrplcN, addrIP> roupol, tabListing<tabPrfxlstN, addrIP> prflst) {
        if (cur.best.nextHop == null) {
            cur.best.nextHop = neigh.peerAddr.copyBytes();
        }
        attr.best.ident = cur.best.ident;
        attr.best.nextHop = cur.best.nextHop;
        attr.best.labelRem = cur.best.labelRem;
        attr.best.evpnLab = cur.best.evpnLab;
        if ((attr.best.segrouPrf != null) && (attr.best.labelRem != null) && (attr.best.segrouSiz > 0)) {
            addrIPv6 adr6 = new addrIPv6();
            int i = attr.best.labelRem.get(0) >>> 4;
            i &= (1 << attr.best.segrouSiz) - 1;
            bits.msbPutD(adr6.getBytes(), addrIPv6.size - 4, i);
            adr6.setShl(adr6, 128 - attr.best.segrouOfs - attr.best.segrouSiz);
            adr6.setOr(attr.best.segrouPrf, adr6);
            attr.best.segrouPrf.fromIPv6addr(adr6);
        }
        attr.best.copyBytes(cur.best, false);
        if (parent.flaps != null) {
            parent.prefixFlapped(mask, safi, cur.rouDst, cur.prefix, cur.best.asPathInts(-1));
        }
        if (neigh.dampenPfxs != null) {
            neigh.prefixDampen(mask, safi, cur.rouDst, cur.prefix, neigh.dampenAnno);
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
