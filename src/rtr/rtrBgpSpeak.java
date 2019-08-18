package rtr;

import addr.addrIP;
import addr.addrIPv4;
import cfg.cfgAll;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
import pack.packHolder;
import pipe.pipeSide;
import tab.tabLabel;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplcN;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.syncInt;
import util.typLenVal;

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
     * learned other prefixes
     */
    public final tabRoute<addrIP> lrnOtr = new tabRoute<addrIP>("rx");

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
     * learned srte prefixes
     */
    public final tabRoute<addrIP> lrnSrte = new tabRoute<addrIP>("rx");

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
     * advertised other prefixes
     */
    public final tabRoute<addrIP> advOtr = new tabRoute<addrIP>("tx");

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
     * advertised srte prefixes
     */
    public final tabRoute<addrIP> advSrte = new tabRoute<addrIP>("tx");

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
    private tabRoute<addrIP> currUni = null;

    /**
     * currently updating multicast prefixes
     */
    private tabRoute<addrIP> currMlt = null;

    /**
     * currently updating other prefixes
     */
    private tabRoute<addrIP> currOtr = null;

    /**
     * currently updating flowspec prefixes
     */
    private tabRoute<addrIP> currFlw = null;

    /**
     * currently updating vpn uni prefixes
     */
    private tabRoute<addrIP> currVpnU = null;

    /**
     * currently updating vpn multi prefixes
     */
    private tabRoute<addrIP> currVpnM = null;

    /**
     * currently updating vpn flow prefixes
     */
    private tabRoute<addrIP> currVpnF = null;

    /**
     * currently updating other vpn uni prefixes
     */
    private tabRoute<addrIP> currVpoU = null;

    /**
     * currently updating other vpn multi prefixes
     */
    private tabRoute<addrIP> currVpoM = null;

    /**
     * currently updating other vpn flow prefixes
     */
    private tabRoute<addrIP> currVpoF = null;

    /**
     * currently updating vpls prefixes
     */
    private tabRoute<addrIP> currVpls = null;

    /**
     * currently updating mspw prefixes
     */
    private tabRoute<addrIP> currMspw = null;

    /**
     * currently updating evpn prefixes
     */
    private tabRoute<addrIP> currEvpn = null;

    /**
     * currently updating mdt prefixes
     */
    private tabRoute<addrIP> currMdt = null;

    /**
     * currently updating srte prefixes
     */
    private tabRoute<addrIP> currSrte = null;

    /**
     * currently updating mvpn prefixes
     */
    private tabRoute<addrIP> currMvpn = null;

    /**
     * currently updating other mvpn prefixes
     */
    private tabRoute<addrIP> currMvpo = null;

    /**
     * currently changed prefixes
     */
    private int currChg;

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
     * route refresh sent
     */
    public int refreshTx;

    /**
     * route refresh received
     */
    public int refreshRx;

    /**
     * peer graceful restart capability
     */
    public int peerGrace;

    /**
     * peer hostname capability
     */
    public String peerHostname;

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
        parent = protocol;
        neigh = neighbor;
        pipe = socket;
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
        if (safi == parent.afiOtr) {
            return (val & rtrBgpParam.mskOtr) != 0;
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
        if (safi == parent.afiSrte) {
            return (val & rtrBgpParam.mskSrte) != 0;
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
        if (safi == parent.afiOtr) {
            return lrnOtr;
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
        if (safi == parent.afiSrte) {
            return lrnSrte;
        }
        if (safi == parent.afiMvpn) {
            return lrnMvpn;
        }
        if (safi == parent.afiMvpo) {
            return lrnMvpo;
        }
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
        if (safi == parent.afiOtr) {
            return advOtr;
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
        if (safi == parent.afiSrte) {
            return advSrte;
        }
        if (safi == parent.afiMvpn) {
            return advMvpn;
        }
        if (safi == parent.afiMvpo) {
            return advMvpo;
        }
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
        lrnOtr.clear();
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
        lrnSrte.clear();
        lrnMvpn.clear();
        lrnMvpo.clear();
        advUni.clear();
        advMlt.clear();
        advOtr.clear();
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
        advSrte.clear();
        advMvpn.clear();
        advMvpo.clear();
        neigh.accUni = new tabRoute<addrIP>("rx");
        neigh.accMlt = new tabRoute<addrIP>("rx");
        neigh.accOtr = new tabRoute<addrIP>("rx");
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
        neigh.accSrte = new tabRoute<addrIP>("rx");
        neigh.accMvpn = new tabRoute<addrIP>("rx");
        neigh.accMvpo = new tabRoute<addrIP>("rx");
        neigh.wilUni = new tabRoute<addrIP>("rx");
        neigh.wilMlt = new tabRoute<addrIP>("rx");
        neigh.wilOtr = new tabRoute<addrIP>("rx");
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
        neigh.wilSrte = new tabRoute<addrIP>("rx");
        neigh.wilMvpn = new tabRoute<addrIP>("rx");
        neigh.wilMvpo = new tabRoute<addrIP>("rx");
        neigh.chgUni = new tabRoute<addrIP>("chg");
        neigh.chgMlt = new tabRoute<addrIP>("chg");
        neigh.chgOtr = new tabRoute<addrIP>("chg");
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
        neigh.chgSrte = new tabRoute<addrIP>("chg");
        neigh.chgMvpn = new tabRoute<addrIP>("chg");
        neigh.chgMvpo = new tabRoute<addrIP>("chg");
        if (!ready2adv) {
            return;
        }
        ready2adv = false;
        adversion.set(0);
        needFull.set(3);
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
        pipe.timeout = neigh.holdTimer;
        peerKeep = neigh.keepAlive;
        pipe.setReady();
        pipe.wait4ready(neigh.keepAlive);
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
        logger.warn("neighbor " + neigh.peerAddr + " up");
        if (neigh.monitor != null) {
            neigh.monitor.gotEvent(true, this, neigh);
        }
        if (neigh.bfdTrigger) {
            neigh.localIfc.bfdAdd(neigh.peerAddr, this, "bgp");
        }
        lrnUni.clear();
        lrnMlt.clear();
        lrnOtr.clear();
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
        lrnSrte.clear();
        lrnMvpn.clear();
        lrnMvpo.clear();
        advUni.clear();
        advMlt.clear();
        advOtr.clear();
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
        advSrte.clear();
        advMvpn.clear();
        advMvpo.clear();
        neigh.accUni = new tabRoute<addrIP>("rx");
        neigh.accMlt = new tabRoute<addrIP>("rx");
        neigh.accOtr = new tabRoute<addrIP>("rx");
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
        neigh.accSrte = new tabRoute<addrIP>("rx");
        neigh.accMvpn = new tabRoute<addrIP>("rx");
        neigh.accMvpo = new tabRoute<addrIP>("rx");
        ready2adv = true;
        parent.needFull.add(1);
        parent.compute.wakeup();
        for (;;) {
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
                logger.debug("sending " + rtrBgpUtil.type2string(typ) + " to " + neigh.peerAddr);
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
        for (int i = 0; i < 16; i++) {
            pck.putByte(i, 0xff);
        }
        pck.msbPutW(16, pck.dataSize() + sizeU);
        pck.putByte(18, typ);
        pck.putSkip(sizeU);
        pck.merge2beg();
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

    /**
     * send open message
     */
    public void sendOpen() {
        List<Integer> safis = parent.mask2list(neigh.addrFams);
        packHolder pck = new packHolder(true, true);
        for (int i = 0; i < safis.size(); i++) {
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, safis.get(i));
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaMultiProto, buf);
        }
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, neigh.localAs);
        rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capa32bitAsNum, buf);
        buf = new byte[0];
        rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaRouteRefresh, buf);
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
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaAdditionPath, buf);
        }
        safis = parent.mask2list(neigh.graceRestart & neigh.addrFams);
        if (safis.size() > 0) {
            buf = new byte[2 + (safis.size() * 4)];
            bits.msbPutW(buf, 0, ((parent.restartTime / 1000) & 0xfff) | 0x8000);
            for (int i = 0; i < safis.size(); i++) {
                bits.msbPutD(buf, (i * 4) + 2, rtrBgpUtil.safi2triplet(safis.get(i)));
            }
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaGraceRestart, buf);
        }
        if (neigh.hostname) {
            buf = cfgAll.hostName.getBytes();
            byte[] tmp = new byte[1];
            tmp[0] = (byte) buf.length;
            buf = bits.byteConcat(tmp, buf);
            tmp = new byte[1];
            buf = bits.byteConcat(buf, tmp);
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaHostname, buf);
        }
        if ((neigh.compressMode & 1) != 0) {
            compressRx = new Inflater[8];
            for (int i = 0; i < compressRx.length; i++) {
                compressRx[i] = new Inflater();
            }
            buf = new byte[2];
            buf[0] = (byte) 0x87; // deflate, 32k window
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaCompress, buf);
        }
        pck.merge2beg();
        int i = pck.dataSize();
        if (i > 0xff) {
            logger.error("too much capabilities with peer " + neigh.peerAddr);
            i = 0xff;
        }
        pck.putByte(0, rtrBgpUtil.version);
        pck.msbPutW(1, rtrBgpUtil.asNum16bit(neigh.localAs));
        pck.msbPutW(3, neigh.holdTimer / 1000);
        buf = parent.routerID.getBytes();
        pck.putCopy(buf, 0, 5, buf.length);
        pck.putByte(9, i);
        pck.putSkip(10);
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
                pipe.timeout = peerHold;
            }
        }
        pck.getAddr(peerRouterID, 5);
        i = pck.getByte(9);
        pck.getSkip(10);
        if (i < pck.dataSize()) {
            pck.setDataSize(i);
        }
        boolean mpGot = false;
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            typLenVal tlv = rtrBgpUtil.getCapabilityTlv();
            if (tlv.getBytes(pck)) {
                break;
            }
            if (tlv.valTyp != 2) {
                continue;
            }
            packHolder pck2 = new packHolder(true, true);
            pck2.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
            pck2.putSkip(tlv.valSiz);
            pck2.merge2beg();
            for (;;) {
                tlv = rtrBgpUtil.getCapabilityTlv();
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
                    case rtrBgpUtil.capaHostname:
                        byte[] buf = new byte[tlv.valDat[0] & 0xff];
                        bits.byteCopy(tlv.valDat, 1, buf, 0, buf.length);
                        peerHostname = new String(buf);
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
                            int o = bits.msbGetD(tlv.valDat, i);
                            o = parent.safi2mask(o);
                            if (o < 1) {
                                continue;
                            }
                            peerAfis |= o;
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
            peerGrace = neigh.addrFams;
            addpathRx = neigh.addpathRmode;
            addpathTx = neigh.addpathTmode;
            peerRefresh = true;
            peer32bitAS = true;
            if ((neigh.compressMode & 2) != 0) {
                compressTx = new Deflater();
            }
        }
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
     * send update packet
     *
     * @param safi safi to update
     * @param lst list of prefixes to advertise
     * @param reach true to reachable, false to withdraw
     */
    public void sendUpdate(int safi, List<tabRouteEntry<addrIP>> lst, boolean reach) {
        if (debugger.rtrBgpTraf) {
            String s = "";
            for (int i = 0; i < lst.size(); i++) {
                tabRouteEntry<addrIP> ntry = lst.get(i);
                s += " " + tabRtrmapN.rd2string(ntry.rouDst) + " " + ntry.prefix;
            }
            logger.debug("update to peer " + neigh.peerAddr + " in " + rtrBgpUtil.safi2string(safi) + ": " + (reach ? "reachable" : "withdraw") + s);
        }
        pckTx.clear();
        boolean addpath = addPthTx(safi);
        if (!reach) {
            rtrBgpUtil.createWithdraw(pckTx, pckTh, safi, addpath, lst);
        } else {
            rtrBgpUtil.createReachable(pckTx, pckTh, safi, addpath, peer32bitAS, lst, null);
        }
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
        currUni = new tabRoute<addrIP>("curr");
        currMlt = new tabRoute<addrIP>("curr");
        currOtr = new tabRoute<addrIP>("curr");
        currFlw = new tabRoute<addrIP>("curr");
        currVpnU = new tabRoute<addrIP>("curr");
        currVpnM = new tabRoute<addrIP>("curr");
        currVpnF = new tabRoute<addrIP>("curr");
        currVpoU = new tabRoute<addrIP>("curr");
        currVpoM = new tabRoute<addrIP>("curr");
        currVpoF = new tabRoute<addrIP>("curr");
        currVpls = new tabRoute<addrIP>("curr");
        currMspw = new tabRoute<addrIP>("curr");
        currEvpn = new tabRoute<addrIP>("curr");
        currMdt = new tabRoute<addrIP>("curr");
        currSrte = new tabRoute<addrIP>("curr");
        currMvpn = new tabRoute<addrIP>("curr");
        currMvpo = new tabRoute<addrIP>("curr");
        currChg = 0;
        int prt = pck.msbGetW(0);
        pck.getSkip(2);
        prt = pck.dataSize() - prt;
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.rouTyp = neigh.lower.rouTyp;
        ntry.protoNum = neigh.lower.rtrNum;
        ntry.distance = neigh.distance;
        ntry.rouSrc = neigh.peerType;
        ntry.srcRtr = neigh.peerAddr.copyBytes();
        ntry.locPref = 100;
        ntry.nextHop = neigh.peerAddr.copyBytes();
        for (;;) {
            if (pck.dataSize() <= prt) {
                break;
            }
            if (addPthRx(rtrBgpUtil.safiIp4uni)) {
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = rtrBgpUtil.readPrefix(rtrBgpUtil.safiIp4uni, false, pck);
            ntry.prefix = res.prefix;
            ntry.labelRem = res.labelRem;
            prefixWithdraw(rtrBgpUtil.safiIp4uni, ntry);
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
            if (addPthRx(rtrBgpUtil.safiIp4uni)) {
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = rtrBgpUtil.readPrefix(rtrBgpUtil.safiIp4uni, false, pck);
            ntry.prefix = res.prefix;
            ntry.labelRem = res.labelRem;
            prefixReach(rtrBgpUtil.safiIp4uni, ntry);
        }
        addAttribed(currUni, parent.afiUni, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribed(currUni, parent.afiLab, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribed(currMlt, parent.afiMlt, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribed(currOtr, parent.afiOtr, ntry, neigh.roumapIn, neigh.roupolIn, neigh.prflstIn);
        addAttribed(currFlw, parent.afiFlw, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currVpnU, parent.afiVpnU, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currVpnM, parent.afiVpnM, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currVpnF, parent.afiVpnF, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currVpoU, parent.afiVpoU, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currVpoM, parent.afiVpoM, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currVpoF, parent.afiVpoF, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currVpls, parent.afiVpls, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currMspw, parent.afiMspw, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currEvpn, parent.afiEvpn, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currMdt, parent.afiMdt, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currSrte, parent.afiSrte, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currMvpn, parent.afiMvpn, ntry, neigh.voumapIn, neigh.voupolIn, null);
        addAttribed(currMvpo, parent.afiMvpo, ntry, neigh.voumapIn, neigh.voupolIn, null);
        currUni = null;
        currMlt = null;
        currOtr = null;
        currFlw = null;
        currVpnU = null;
        currVpnM = null;
        currVpnF = null;
        currVpoU = null;
        currVpoM = null;
        currVpoF = null;
        currVpls = null;
        currMspw = null;
        currEvpn = null;
        currMdt = null;
        currSrte = null;
        currMvpn = null;
        currMvpo = null;
        if ((currChg > 0) && (rxReady() < (neigh.bufferSize / 4))) {
            parent.compute.wakeup();
        }
        if (neigh.maxPrefixCnt < 1) {
            return false;
        }
        int i = lrnUni.size() + lrnMlt.size();
        if (i > ((neigh.maxPrefixCnt * neigh.maxPrefixPrc) / 100)) {
            logger.info("neighbor " + neigh.peerAddr + " sent " + i + " prefixes");
        }
        if (i > neigh.maxPrefixCnt) {
            sendNotify(6, 1);
        }
        return false;
    }

    private void addAttribed(tabRoute<addrIP> currAdd, int safi, tabRouteEntry<addrIP> attr, tabListing<tabRtrmapN, addrIP> roumap, tabListing<tabRtrplcN, addrIP> roupol, tabListing<tabPrfxlstN, addrIP> prflst) {
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
        for (int i = 0; i < currAdd.size(); i++) {
            tabRouteEntry<addrIP> cur = currAdd.get(i);
            attr.prefix = cur.prefix;
            attr.nextHop = cur.nextHop;
            attr.rouDst = cur.rouDst;
            attr.labelRem = cur.labelRem;
            attr.evpnLab = cur.evpnLab;
            cur = attr.copyBytes();
            if (parent.flaps != null) {
                parent.prefixFlapped(safi, cur.rouDst, cur.prefix, cur.asPathStr());
            }
            if (!neigh.softReconfig) {
                tabRouteEntry<addrIP> res = tabRoute.doUpdateEntry(safi, cur, roumap, roupol, prflst);
                if (res == null) {
                    if (learned.del(cur)) {
                        continue;
                    }
                    currChg++;
                    changed.add(tabRoute.addType.always, cur, false, false);
                    continue;
                }
                cur = res;
            }
            if (prefixReachable(cur)) {
                if (learned.del(cur)) {
                    continue;
                }
                currChg++;
                changed.add(tabRoute.addType.always, cur, false, false);
                continue;
            }
            learned.add(tabRoute.addType.always, cur, false, true);
            currChg++;
            changed.add(tabRoute.addType.always, cur, false, false);
        }
    }

    /**
     * prefix withdrawal received
     *
     * @param safi safi
     * @param ntry prefix
     */
    protected void prefixWithdraw(int safi, tabRouteEntry<addrIP> ntry) {
        if (debugger.rtrBgpTraf) {
            logger.debug("withdraw " + rtrBgpUtil.safi2string(safi) + " " + tabRtrmapN.rd2string(ntry.rouDst) + " " + ntry.prefix);
        }
        if (!afiMsk(peerAfis, safi)) {
            return;
        }
        if (parent.flaps != null) {
            parent.prefixFlapped(safi, ntry.rouDst, ntry.prefix, "gone");
        }
        tabRoute<addrIP> learned = getLearned(safi);
        if (learned == null) {
            return;
        }
        if (learned.del(ntry)) {
            return;
        }
        currChg++;
        tabRoute<addrIP> changed = parent.getChanged(safi);
        if (changed == null) {
            parent.needFull.add(1);
            return;
        }
        changed.add(tabRoute.addType.always, ntry.copyBytes(), false, false);
    }

    /**
     * process reachable prefix
     *
     * @param safi address family
     * @param ntry route entry
     */
    protected void prefixReach(int safi, tabRouteEntry<addrIP> ntry) {
        if (debugger.rtrBgpTraf) {
            logger.debug("reachable " + rtrBgpUtil.safi2string(safi) + " " + tabRtrmapN.rd2string(ntry.rouDst) + " " + ntry.prefix);
        }
        if (!afiMsk(peerAfis, safi)) {
            return;
        }
        tabRoute<addrIP> trg = null;
        if (safi == parent.afiUni) {
            trg = currUni;
        }
        if (safi == parent.afiLab) {
            trg = currUni;
        }
        if (safi == parent.afiMlt) {
            trg = currMlt;
        }
        if (safi == parent.afiOtr) {
            trg = currOtr;
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
        if (safi == parent.afiSrte) {
            trg = currSrte;
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
        trg.add(tabRoute.addType.always, ntry, true, true);
    }

    private boolean prefixReachable(tabRouteEntry<addrIP> ntry) {
        if (debugger.rtrBgpTraf) {
            logger.debug("processing " + tabRtrmapN.rd2string(ntry.rouDst) + " " + ntry.prefix);
        }
        if (neigh.enforceFirst) {
            switch (neigh.peerType) {
                case rtrBgpUtil.peerExtrn:
                case rtrBgpUtil.peerServr:
                    if (rtrBgpUtil.firstIntList(ntry.pathSeq, neigh.remoteAs)) {
                        return true;
                    }
                    break;
                case rtrBgpUtil.peerCnfed:
                    if (rtrBgpUtil.firstIntList(ntry.confSeq, neigh.remoteAs)) {
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
                    if (rtrBgpUtil.findIntList(ntry.pathSeq, neigh.localAs) >= 0) {
                        return true;
                    }
                    if (rtrBgpUtil.findIntList(ntry.pathSet, neigh.localAs) >= 0) {
                        return true;
                    }
                    break;
                case rtrBgpUtil.peerCnfed:
                    if (rtrBgpUtil.findIntList(ntry.confSeq, neigh.localAs) >= 0) {
                        return true;
                    }
                    if (rtrBgpUtil.findIntList(ntry.confSet, neigh.localAs) >= 0) {
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
                if (ntry.originator != null) {
                    if (a.compare(ntry.originator, a) == 0) {
                        return true;
                    }
                }
                if (ntry.clustList != null) {
                    if (rtrBgpUtil.findAddrList(ntry.clustList, a) >= 0) {
                        return true;
                    }
                }
                break;
            case rtrBgpUtil.peerRflct:
                if (ntry.originator == null) {
                    a = new addrIP();
                    a.fromIPv4addr(peerRouterID);
                    ntry.originator = a;
                }
                a = new addrIP();
                a.fromIPv4addr(parent.routerID);
                if (a.compare(ntry.originator, a) == 0) {
                    return true;
                }
                if (ntry.clustList == null) {
                    ntry.clustList = new ArrayList<addrIP>();
                }
                if (rtrBgpUtil.findAddrList(ntry.clustList, a) >= 0) {
                    return true;
                }
                ntry.clustList.add(a);
                break;
            default:
                break;
        }
        if (neigh.nxtHopPeer) {
            ntry.nextHop = neigh.peerAddr.copyBytes();
        }
        if ((ntry.labelRem == null) && (ntry.segrouIdx > 0) && (ntry.segrouBeg > 0)) {
            ntry.labelRem = tabLabel.int2labels(ntry.segrouBeg + ntry.segrouIdx);
        }
        if (neigh.egressEng > 0) {
            ntry.segrouIdx = neigh.egressEng;
        }
        if (neigh.dmzLinkBw >= 0) {
            if (ntry.extComm == null) {
                ntry.extComm = new ArrayList<Long>();
            }
            ntry.extComm.add(tabRtrmapN.dmzbw2comm(neigh.localAs, neigh.dmzLinkBw));
        }
        if (neigh.removePrivAsIn) {
            rtrBgpUtil.removePrivateAs(ntry.pathSeq);
            rtrBgpUtil.removePrivateAs(ntry.pathSet);
        }
        if (neigh.overridePeerIn) {
            rtrBgpUtil.replaceIntList(ntry.pathSeq, neigh.remoteAs, neigh.localAs);
            rtrBgpUtil.replaceIntList(ntry.pathSet, neigh.remoteAs, neigh.localAs);
        }
        if (neigh.intVpnClnt) {
            rtrBgpUtil.encodeAttribSet(neigh.localAs, ntry);
        }
        return false;
    }

}
