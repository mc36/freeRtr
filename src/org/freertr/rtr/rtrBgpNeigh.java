package org.freertr.rtr;

import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntWhois;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.pack.packDnsRec;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtAccept;
import org.freertr.prt.prtGenConn;
import org.freertr.serv.servGeneric;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabRpkiRoa;
import org.freertr.tab.tabRpkiUtil;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabRtrplc;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * bgp4 neighbor
 *
 * @author matecsaba
 */
public class rtrBgpNeigh extends rtrBgpParam implements Comparable<rtrBgpNeigh>, Runnable {

    /**
     * address of peer
     */
    public final addrIP peerAddr;

    /**
     * local interface
     */
    public ipFwdIface localIfc = null;

    /**
     * sending interface
     */
    public ipFwdIface sendingIfc = null;

    /**
     * local address
     */
    public addrIP localAddr = new addrIP();

    /**
     * local other address
     */
    public addrIP localOddr = new addrIP();

    /**
     * accepted unicast prefixes
     */
    public tabRoute<addrIP> accUni = new tabRoute<addrIP>("rx");

    /**
     * accepted multicast prefixes
     */
    public tabRoute<addrIP> accMlt = new tabRoute<addrIP>("rx");

    /**
     * accepted other unicast prefixes
     */
    public tabRoute<addrIP> accOuni = new tabRoute<addrIP>("rx");

    /**
     * accepted other multicast prefixes
     */
    public tabRoute<addrIP> accOmlt = new tabRoute<addrIP>("rx");

    /**
     * accepted other flowspec prefixes
     */
    public tabRoute<addrIP> accOflw = new tabRoute<addrIP>("rx");

    /**
     * accepted other srte prefixes
     */
    public tabRoute<addrIP> accOsrt = new tabRoute<addrIP>("rx");

    /**
     * accepted flowspec prefixes
     */
    public tabRoute<addrIP> accFlw = new tabRoute<addrIP>("rx");

    /**
     * accepted vpnuni prefixes
     */
    public tabRoute<addrIP> accVpnU = new tabRoute<addrIP>("rx");

    /**
     * accepted vpnmulti prefixes
     */
    public tabRoute<addrIP> accVpnM = new tabRoute<addrIP>("rx");

    /**
     * accepted vpnflow prefixes
     */
    public tabRoute<addrIP> accVpnF = new tabRoute<addrIP>("rx");

    /**
     * accepted other vpnuni prefixes
     */
    public tabRoute<addrIP> accVpoU = new tabRoute<addrIP>("rx");

    /**
     * accepted other vpnmulti prefixes
     */
    public tabRoute<addrIP> accVpoM = new tabRoute<addrIP>("rx");

    /**
     * accepted other vpnflow prefixes
     */
    public tabRoute<addrIP> accVpoF = new tabRoute<addrIP>("rx");

    /**
     * accepted vpls prefixes
     */
    public tabRoute<addrIP> accVpls = new tabRoute<addrIP>("rx");

    /**
     * accepted mspw prefixes
     */
    public tabRoute<addrIP> accMspw = new tabRoute<addrIP>("rx");

    /**
     * accepted evpn prefixes
     */
    public tabRoute<addrIP> accEvpn = new tabRoute<addrIP>("rx");

    /**
     * accepted mdt prefixes
     */
    public tabRoute<addrIP> accMdt = new tabRoute<addrIP>("rx");

    /**
     * accepted nsh prefixes
     */
    public tabRoute<addrIP> accNsh = new tabRoute<addrIP>("rx");

    /**
     * accepted rpd prefixes
     */
    public tabRoute<addrIP> accRpd = new tabRoute<addrIP>("rx");

    /**
     * accepted sdwan prefixes
     */
    public tabRoute<addrIP> accSdw = new tabRoute<addrIP>("rx");

    /**
     * accepted spf prefixes
     */
    public tabRoute<addrIP> accSpf = new tabRoute<addrIP>("rx");

    /**
     * accepted rtfilter prefixes
     */
    public tabRoute<addrIP> accRtf = new tabRoute<addrIP>("rx");

    /**
     * accepted srte prefixes
     */
    public tabRoute<addrIP> accSrte = new tabRoute<addrIP>("rx");

    /**
     * accepted linkstate prefixes
     */
    public tabRoute<addrIP> accLnks = new tabRoute<addrIP>("rx");

    /**
     * accepted mvpn prefixes
     */
    public tabRoute<addrIP> accMvpn = new tabRoute<addrIP>("rx");

    /**
     * accepted other mvpn prefixes
     */
    public tabRoute<addrIP> accMvpo = new tabRoute<addrIP>("rx");

    /**
     * accepted mtree prefixes
     */
    public tabRoute<addrIP> accMtre = new tabRoute<addrIP>("rx");

    /**
     * accepted other mtree prefixes
     */
    public tabRoute<addrIP> accMtro = new tabRoute<addrIP>("rx");

    /**
     * willing prefixes
     */
    public final tabRoute<addrIP>[] willing;

    /**
     * changed prefixes
     */
    public final tabRoute<addrIP>[] changed;

    /**
     * update group number
     */
    public int groupMember = -1;

    /**
     * last time advertised
     */
    public long advertLast;

    /**
     * advertise count
     */
    public int advertCount;

    /**
     * neighbor reachable
     */
    public boolean reachable = false;

    /**
     * old reachable state
     */
    public boolean reachOld;

    /**
     * neighbor reachable change time
     */
    public long reachTim;

    /**
     * neighbor reachable change number
     */
    public long reachNum;

    /**
     * neighbor session change number
     */
    public long sessNum;

    /**
     * speaker
     */
    public rtrBgpSpeak conn;

    /**
     * type of peer
     */
    public int peerType;

    /**
     * full compute last
     */
    public long fullLast;

    /**
     * incremental compute last
     */
    public long incrLast;

    /**
     * full compute count
     */
    public int fullCount;

    /**
     * incremental compute count
     */
    public int incrCount;

    /**
     * full compute time
     */
    public int fullTime;

    /**
     * incremental compute time
     */
    public int incrTime;

    /**
     * transmit sleeper
     */
    public notifier transmit = new notifier();

    /**
     * reachability statistics
     */
    public final counter reachabStat = new counter();

    /**
     * unreachability statistics
     */
    public final counter unreachStat = new counter();

    /**
     * message types received
     */
    public final counter[] msgStats = new counter[256];

    /**
     * attribute types received
     */
    public final counter[] attrStats = new counter[256];

    private boolean need2run;

    /**
     * create neighbor
     *
     * @param parent bgp process
     * @param addr address of peer
     */
    public rtrBgpNeigh(rtrBgp parent, addrIP addr) {
        super(parent, false);
        willing = rtrBgpParam.freshTables();
        changed = rtrBgpParam.freshTables();
        peerAddr = addr;
        for (int i = 0; i < msgStats.length; i++) {
            msgStats[i] = new counter();
        }
        for (int i = 0; i < attrStats.length; i++) {
            attrStats[i] = new counter();
        }
        conn = new rtrBgpSpeak(lower, this, null, 0);
    }

    public int compareTo(rtrBgpNeigh o) {
        return peerAddr.compareTo(o.peerAddr);
    }

    public String toString() {
        return "" + peerAddr;
    }

    /**
     * flap connection
     */
    public void flapBgpConn() {
        conn.closeNow();
    }

    /**
     * save table
     *
     * @param fil file to use
     * @param idx safi to refresh
     * @param mask safi to refresh
     * @param safi safi to refresh
     */
    public void saveTable(RandomAccessFile fil, int idx, long mask, int safi) {
        rtrBgpMrt.dumpTable(fil, conn, safi, conn.learnt[idx], false, lower.fwdCore.ipVersion, remoteAs, localAs, peerAddr, localAddr);
        rtrBgpMrt.dumpTable(fil, conn, safi, conn.advert[idx], true, lower.fwdCore.ipVersion, remoteAs, localAs, peerAddr, localAddr);
    }

    public void doTempCfg(String cmd, boolean negated) {
    }

    public void getConfig(List<String> l, String beg, int filter) {
        l.addAll(getParamCfg(beg, "neighbor " + peerAddr + " ", filter));
        l.add(beg + cmds.comment);
    }

    /**
     * get dampening of peer
     *
     * @param mtch matcher
     * @return dampening
     */
    public userFormat getDampening(tabIntMatcher mtch) {
        if (dampenPfxs == null) {
            return null;
        }
        userFormat l = new userFormat("|", "afi|prefix|penalty|dampened|ago|last");
        for (int i = 0; i < dampenPfxs.size(); i++) {
            rtrBgpDamp ntry = dampenPfxs.get(i);
            if (ntry == null) {
                continue;
            }
            if (!mtch.matches(ntry.penalty)) {
                continue;
            }
            l.add("" + ntry);
        }
        return l;
    }

    /**
     * get status of peer
     *
     * @return status
     */
    public userFormat getStatus() {
        userFormat l = new userFormat("|", "category|value");
        l.add("peer|" + peerAddr);
        l.add("shutdown|" + checkShutdown());
        l.add("ready2adv|" + conn.ready2adv);
        l.add("reachable state|" + reachable);
        l.add("reachable changed|" + bits.timePast(reachTim) + " ago, at " + bits.time2str(cfgAll.timeZoneName, reachTim + cfgAll.timeServerOffset, 3));
        l.add("reachable changes|" + reachNum);
        l.add("session changes|" + sessNum);
        l.add("fallover|" + sendingIfc);
        l.add("update group|" + groupMember);
        l.add("socket mode|" + socketMode);
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgOpen, msgStats, "|tx=", " rx=");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgUpdate, msgStats, "|tx=", " rx=");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgRefrsh, msgStats, "|tx=", " rx=");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgCapability, msgStats, "|tx=", " rx=");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgNotify, msgStats, "|tx=", " rx=");
        rtrBgpDump.getUnReachStats(l, reachabStat, unreachStat, "|rx=", " tx=");
        rtrBgpDump.getUnknwSum(l, false, msgStats, "|rx=", " tx=");
        rtrBgpDump.getUnknwSum(l, true, attrStats, "|rx=", " tx=");
        l.add("local asn|" + clntWhois.asn2mixed(localAs, true));
        l.add("remote asn|" + clntWhois.asn2mixed(remoteAs, true));
        l.add("type|" + rtrBgpUtil.peerType2string(peerType));
        l.add("leak role|rx=" + rtrBgpUtil.leakRole2string(conn.peerLeakRole, false) + ", tx=" + rtrBgpUtil.leakRole2string(leakRole, leakAttr));
        l.add("dynamic capability|" + conn.peerDynCap + ", rx=" + conn.dynCapaRx + ", tx=" + conn.dynCapaTx);
        l.add("rpki in|" + rtrBgpUtil.rpkiMode2string(rpkiIn) + " vpn=" + rtrBgpUtil.rpkiMode2string(vpkiIn));
        l.add("rpki out|" + rtrBgpUtil.rpkiMode2string(rpkiOut) + " vpn=" + rtrBgpUtil.rpkiMode2string(vpkiOut));
        l.add("safi open|" + rtrBgpParam.bools2string(conn.peerAfis));
        l.add("safi got|" + rtrBgpParam.bools2string(conn.originalSafiList));
        l.add("safi not remote|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(addrFams, conn.peerAfis)));
        l.add("safi not local|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(conn.originalSafiList, conn.peerAfis)));
        l.add("ipinfo|" + conn.ipInfoRes);
        l.add("local address|" + localAddr);
        l.add("other address|" + localOddr);
        l.add("router id|" + conn.peerRouterID);
        l.add("uptime|" + bits.timePast(conn.upTime) + " ago, at " + bits.time2str(cfgAll.timeZoneName, conn.upTime + cfgAll.timeServerOffset, 3));
        l.add("hold time|" + bits.timeDump(conn.peerHold / 1000));
        l.add("keepalive time|" + bits.timeDump(conn.peerKeep / 1000));
        l.add("32bit as|" + conn.peer32bitAS);
        l.add("refresh|" + conn.peerRefreshOld + " " + conn.peerRefreshNew + ", rx=" + conn.refreshRx + ", tx=" + conn.refreshTx);
        l.add("extended open|rx=" + conn.peerExtOpen + ", tx=" + extOpen);
        l.add("extended message|rx=" + conn.peerExtUpd + ", tx=" + extUpdate);
        l.add("description|" + description);
        l.add("hostname|" + conn.peerHostname);
        l.add("domainname|" + conn.peerDomainname);
        l.add("software|" + conn.peerSoftware);
        l.add("compression|rx=" + (conn.compressRx != null) + ", tx=" + (conn.compressTx != null));
        l.add("strict bfd|" + conn.strictBfd);
        l.add("graceful got|" + rtrBgpParam.bools2string(conn.peerGrace));
        l.add("graceful sent|" + rtrBgpParam.bools2string(graceRestart));
        l.add("longlive graceful got|" + rtrBgpParam.bools2string(conn.peerLlGrace));
        l.add("longlive graceful sent|" + rtrBgpParam.bools2string(llGraceRestart));
        l.add("multilabel got|" + rtrBgpParam.bools2string(conn.peerMltLab));
        l.add("multilabel sent|" + rtrBgpParam.bools2string(multiLabel));
        l.add("extnexthop cur|" + rtrBgpParam.bools2string(conn.peerExtNextCur));
        l.add("extnexthop otr|" + rtrBgpParam.bools2string(conn.peerExtNextOtr));
        l.add("addpath rx open|" + rtrBgpParam.bools2string(conn.addpathRx));
        l.add("addpath tx open|" + rtrBgpParam.bools2string(conn.addpathTx));
        l.add("addpath rx got|" + rtrBgpParam.bools2string(conn.originalAddRlist));
        l.add("addpath tx got|" + rtrBgpParam.bools2string(conn.originalAddTlist));
        l.add("addpath rx not remote|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(addpathRmode, conn.addpathRx)));
        l.add("addpath tx not remote|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(addpathTmode, conn.addpathTx)));
        l.add("addpath rx not local|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(conn.originalAddRlist, conn.addpathRx)));
        l.add("addpath tx not local|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(conn.originalAddTlist, conn.addpathTx)));

        /*
        ////////////
        l.add("unicast advertised|" + conn.advUni.size() + " of " + wilUni.size() + ", list = " + chgUni.size() + ", accepted = " + accUni.size() + " of " + conn.lrnUni.size());
        l.add("multicast advertised|" + conn.advMlt.size() + " of " + wilMlt.size() + ", list = " + chgMlt.size() + ", accepted = " + accMlt.size() + " of " + conn.lrnMlt.size());
        l.add("ouni advertised|" + conn.advOuni.size() + " of " + wilOuni.size() + ", list = " + chgOuni.size() + ", accepted = " + accOuni.size() + " of " + conn.lrnOuni.size());
        l.add("omlt advertised|" + conn.advOmlt.size() + " of " + wilOmlt.size() + ", list = " + chgOmlt.size() + ", accepted = " + accOmlt.size() + " of " + conn.lrnOmlt.size());
        l.add("oflw advertised|" + conn.advOflw.size() + " of " + wilOflw.size() + ", list = " + chgOflw.size() + ", accepted = " + accOflw.size() + " of " + conn.lrnOflw.size());
        l.add("osrt advertised|" + conn.advOsrt.size() + " of " + wilOsrt.size() + ", list = " + chgOsrt.size() + ", accepted = " + accOsrt.size() + " of " + conn.lrnOsrt.size());
        l.add("flowspec advertised|" + conn.advFlw.size() + " of " + wilFlw.size() + ", list = " + chgFlw.size() + ", accepted = " + accFlw.size() + " of " + conn.lrnFlw.size());
        l.add("vpnuni advertised|" + conn.advVpnU.size() + " of " + wilVpnU.size() + ", list = " + chgVpnU.size() + ", accepted = " + accVpnU.size() + " of " + conn.lrnVpnU.size());
        l.add("vpnmlt advertised|" + conn.advVpnM.size() + " of " + wilVpnM.size() + ", list = " + chgVpnM.size() + ", accepted = " + accVpnM.size() + " of " + conn.lrnVpnM.size());
        l.add("vpnflw advertised|" + conn.advVpnF.size() + " of " + wilVpnF.size() + ", list = " + chgVpnF.size() + ", accepted = " + accVpnF.size() + " of " + conn.lrnVpnF.size());
        l.add("ovpnuni advertised|" + conn.advVpoU.size() + " of " + wilVpoU.size() + ", list = " + chgVpoU.size() + ", accepted = " + accVpoU.size() + " of " + conn.lrnVpoU.size());
        l.add("ovpnmlt advertised|" + conn.advVpoM.size() + " of " + wilVpoM.size() + ", list = " + chgVpoM.size() + ", accepted = " + accVpoM.size() + " of " + conn.lrnVpoM.size());
        l.add("ovpnflw advertised|" + conn.advVpoF.size() + " of " + wilVpoF.size() + ", list = " + chgVpoF.size() + ", accepted = " + accVpoF.size() + " of " + conn.lrnVpoF.size());
        l.add("vpls advertised|" + conn.advVpls.size() + " of " + wilVpls.size() + ", list = " + chgVpls.size() + ", accepted = " + accVpls.size() + " of " + conn.lrnVpls.size());
        l.add("mspw advertised|" + conn.advMspw.size() + " of " + wilMspw.size() + ", list = " + chgMspw.size() + ", accepted = " + accMspw.size() + " of " + conn.lrnMspw.size());
        l.add("evpn advertised|" + conn.advEvpn.size() + " of " + wilEvpn.size() + ", list = " + chgEvpn.size() + ", accepted = " + accEvpn.size() + " of " + conn.lrnEvpn.size());
        l.add("mdt advertised|" + conn.advMdt.size() + " of " + wilMdt.size() + ", list = " + chgMdt.size() + ", accepted = " + accMdt.size() + " of " + conn.lrnMdt.size());
        l.add("nsh advertised|" + conn.advNsh.size() + " of " + wilNsh.size() + ", list = " + chgNsh.size() + ", accepted = " + accNsh.size() + " of " + conn.lrnNsh.size());
        l.add("rpd advertised|" + conn.advRpd.size() + " of " + wilRpd.size() + ", list = " + chgRpd.size() + ", accepted = " + accRpd.size() + " of " + conn.lrnRpd.size());
        l.add("sdwan advertised|" + conn.advSdw.size() + " of " + wilSdw.size() + ", list = " + chgSdw.size() + ", accepted = " + accSdw.size() + " of " + conn.lrnSdw.size());
        l.add("spf advertised|" + conn.advSpf.size() + " of " + wilSpf.size() + ", list = " + chgSpf.size() + ", accepted = " + accSpf.size() + " of " + conn.lrnSpf.size());
        l.add("rtfilter advertised|" + conn.advRtf.size() + " of " + wilRtf.size() + ", list = " + chgRtf.size() + ", accepted = " + accRtf.size() + " of " + conn.lrnRtf.size());
        l.add("srte advertised|" + conn.advSrte.size() + " of " + wilSrte.size() + ", list = " + chgSrte.size() + ", accepted = " + accSrte.size() + " of " + conn.lrnSrte.size());
        l.add("linkstate advertised|" + conn.advLnks.size() + " of " + wilLnks.size() + ", list = " + chgLnks.size() + ", accepted = " + accLnks.size() + " of " + conn.lrnLnks.size());
        l.add("mvpn advertised|" + conn.advMvpn.size() + " of " + wilMvpn.size() + ", list = " + chgMvpn.size() + ", accepted = " + accMvpn.size() + " of " + conn.lrnMvpn.size());
        l.add("omvpn advertised|" + conn.advMvpo.size() + " of " + wilMvpo.size() + ", list = " + chgMvpo.size() + ", accepted = " + accMvpo.size() + " of " + conn.lrnMvpo.size());
        l.add("mtree advertised|" + conn.advMtre.size() + " of " + wilMtre.size() + ", list = " + chgMtre.size() + ", accepted = " + accMtre.size() + " of " + conn.lrnMtre.size());
        l.add("omtree advertised|" + conn.advMtro.size() + " of " + wilMtro.size() + ", list = " + chgMtro.size() + ", accepted = " + accMtro.size() + " of " + conn.lrnMtro.size());
         */
        l.add("version|" + conn.adversion + " of " + lower.compRound + ", needfull=" + conn.needFull + ", buffull=" + conn.buffFull);
        l.add("full|" + fullCount + ", " + bits.time2str(cfgAll.timeZoneName, fullLast + cfgAll.timeServerOffset, 3) + ", " + bits.timePast(fullLast) + " ago, " + fullTime + " ms");
        l.add("incremental|" + incrCount + ", " + bits.time2str(cfgAll.timeZoneName, incrLast + cfgAll.timeServerOffset, 3) + ", " + bits.timePast(incrLast) + " ago, " + incrTime + " ms");
        l.add("advertise|" + advertCount + ", " + bits.time2str(cfgAll.timeZoneName, advertLast + cfgAll.timeServerOffset, 3) + ", " + bits.timePast(advertLast) + " ago");
        l.add("connection|" + conn.cntr.getShStat());
        l.add("lastio|" + conn.cntr.getShTraff());
        l.add("uncompressed|" + conn.compressCntr.getShStat());
        l.add("uncompress lastio|" + conn.compressCntr.getShTraff());
        l.add("buffer|" + pipeSide.getStatus(conn.pipe));
        l.add("policy reject|" + conn.repPolRej);
        l.add("aspath loop|" + conn.repAsPath);
        l.add("confed loop|" + conn.repAsConf);
        l.add("originator id|" + conn.repOrgnId);
        l.add("cluster list|" + conn.repClstrL);
        return l;
    }

    /**
     * update peer structures
     *
     * @param ifc interface to use
     */
    public void updateAddr(ipFwdIface ifc) {
        localIfc = ifc;
        localAddr = ifc.addr.copyBytes();
        ifc = lower.vrfCore.getOtherIface(lower.fwdCore, localIfc);
        if (ifc == null) {
            localOddr = localAddr.copyBytes();
        } else {
            localOddr = ifc.addr.copyBytes();
        }
        if (!fallOver) {
            return;
        }
        sendingIfc = ipFwdTab.findSendingIface(lower.fwdCore, peerAddr);
    }

    /**
     * update peer structures
     */
    public void updatePeer() {
        if (localAs != remoteAs) {
            peerType = rtrBgpUtil.peerExtrn;
            if (remoteConfed) {
                peerType = rtrBgpUtil.peerCnfed;
            }
            if (serverClnt) {
                peerType = rtrBgpUtil.peerServr;
            }
        } else {
            peerType = rtrBgpUtil.peerIntrn;
            if (reflectClnt) {
                peerType = rtrBgpUtil.peerRflct;
            }
        }
    }

    /**
     * start this neighbor process
     */
    public void startNow() {
        if (need2run) {
            return;
        }
        need2run = true;
        new Thread(this).start();
    }

    /**
     * stop this neighbor process
     */
    public void stopNow() {
        doStopNow();
        delListenPeer();
    }

    /**
     * stop peer
     */
    private void doStopNow() {
        need2run = false;
        shutdown = true;
    }

    /**
     * delete listening peer
     */
    public void delListenPeer() {
        if ((socketMode > 0) && (socketMode < 4)) {
            return;
        }
        logger.warn("removing dynamic " + peerAddr);
        doStopNow();
        rtrBgpNeigh old = lower.lstnNei.del(this);
        if (old == null) {
            return;
        }
        old.doStopNow();
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        need2run = false;
        conn.closeNow();
    }

    private void doWork() {
        long lastKeep = 0;
        for (;;) {
            transmit.misleep(1000);
            long tim = bits.getTime();
            if ((lastKeep + conn.peerKeep) < tim) {
                if (conn.ready2adv) {
                    conn.sendKeepAlive();
                }
                lastKeep = tim - 1;
                prefixDampen();
            }
            if (!need2run) {
                return;
            }
            if (checkShutdown()) {
                if (conn.txFree() < 1) {
                    continue;
                }
                flapBgpConn();
                continue;
            }
            if (conn.txFree() >= 0) {
                doAdvert();
                continue;
            }
            lastKeep = 0;
            if (cfgInit.booting) {
                continue;
            }
            switch (socketMode) {
                case 1: // active
                    openConn(0);
                    break;
                case 2: // passive
                    openConn(60);
                    break;
                case 3: // both
                    if (!openConn(0)) {
                        break;
                    }
                    openConn(bits.random(randomStartF, randomStartL));
                    break;
                case 4: // dynamic
                case 5: // listen
                    doStopNow();
                    delListenPeer();
                    return;
            }
        }
    }

    private boolean openConn(int tim) {
        ipFwdIface ifc;
        if (srcIface == null) {
            ifc = ipFwdTab.findSendingIface(lower.fwdCore, peerAddr);
        } else {
            ifc = srcIface.getFwdIfc(peerAddr);
        }
        if (ifc == null) {
            return true;
        }
        if (ifc.addr == null) {
            return true;
        }
        pipeSide pipe;
        if (tim < 1) {
            if (proxy2use != null) {
                pipe = proxy2use.doConnect(servGeneric.protoTcp, proxy2adr, proxy2prt, "bgp");
            } else {
                pipe = lower.tcpCore.streamConnect(new pipeLine(bufferSize, false), ifc, 0, peerAddr, rtrBgp.port, "bgp", keyId, passwd, ttlSecurity, tosValue);
            }
        } else {
            if (proxy2use != null) {
                pipe = null;
                bits.sleep(tim);
            } else {
                prtAccept ac = new prtAccept(lower.tcpCore, new pipeLine(bufferSize, false), ifc, rtrBgp.port, peerAddr, 0, "bgp", keyId, passwd, ttlSecurity, tosValue);
                ac.wait4conn(tim * 1000);
                pipe = ac.getConn(true);
            }
        }
        if (pipe == null) {
            return true;
        }
        if (pipe.wait4ready(holdTimer)) {
            return true;
        }
        updateAddr(ifc);
        conn.closeNow();
        conn = new rtrBgpSpeak(lower, this, pipe, 1);
        return false;
    }

    private boolean advertFull() {
        long tim = bits.getTime();
        for (int idx = 0; idx < addrFams.length; idx++) {
            if (!conn.peerAfis[idx]) {
                continue;
            }
            int safi = lower.idx2safi(idx);
            long msk = 1L << idx;
            tabRoute<addrIP> will = willing[idx];
            tabRoute<addrIP> done = conn.advert[idx];
            boolean oneLab = !conn.peerMltLab[idx];
            boolean needEor = false;
            boolean needEof = false;
            if (conn.needFull.get() < 2) {
                will = new tabRoute<addrIP>(will);
                needEor = conn.needEorAfis[idx];
                needEof = conn.needEofAfis[idx];
            }
            if (conn.addpathTx[idx]) {
                for (int i = 0; i < will.size(); i++) {
                    tabRouteEntry<addrIP> wil = will.get(i);
                    if (wil == null) {
                        continue;
                    }
                    tabRouteEntry<addrIP> don = done.find(wil);
                    if (wil.differs(tabRoute.addType.alters, don) == 0) {
                        continue;
                    }
                    conn.sendUpdateAP(safi, oneLab, wil, don);
                    done.add(tabRoute.addType.always, wil, false, false);
                    if (conn.txFree() < 1024) {
                        return true;
                    }
                }
                for (int i = done.size() - 1; i >= 0; i--) {
                    tabRouteEntry<addrIP> don = done.get(i);
                    if (don == null) {
                        continue;
                    }
                    if (will.find(don) != null) {
                        continue;
                    }
                    done.del(don);
                    conn.sendUpdateAP(safi, oneLab, null, don);
                    if (conn.txFree() < 1024) {
                        return true;
                    }
                }
                if (needEor) {
                    conn.sendEndOfRib(safi);
                    conn.needEorAfis[idx] = false;
                }
                if (needEof) {
                    conn.sendFreshMark(safi, 2);
                    conn.needEofAfis[idx] = false;
                }
                continue;
            }
            List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
            tabRouteEntry<addrIP> sen = null;
            for (int i = 0; i < will.size(); i++) {
                tabRouteEntry<addrIP> ntry = will.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.differs(tabRoute.addType.notyet, done.find(ntry)) == 0) {
                    continue;
                }
                if (sen != null) {
                    sen.prefix = ntry.prefix;
                }
                if (ntry.differs(tabRoute.addType.notyet, sen) != 0) {
                    if (lst.size() > 0) {
                        conn.sendUpdateSP(safi, oneLab, lst, true);
                    }
                    if (conn.txFree() < 2048) {
                        return true;
                    }
                    lst.clear();
                    sen = ntry.copyBytes(tabRoute.addType.notyet);
                }
                done.add(tabRoute.addType.always, ntry, false, false);
                lst.add(ntry);
                if (lst.size() > 64) {
                    sen = null;
                }
            }
            if (lst.size() > 0) {
                conn.sendUpdateSP(safi, oneLab, lst, true);
                if (conn.txFree() < 2048) {
                    return true;
                }
            }
            lst.clear();
            for (int i = done.size() - 1; i >= 0; i--) {
                tabRouteEntry<addrIP> ntry = done.get(i);
                if (ntry == null) {
                    continue;
                }
                if (will.find(ntry) != null) {
                    continue;
                }
                done.del(ntry);
                lst.add(ntry);
                if (lst.size() < 64) {
                    continue;
                }
                conn.sendUpdateSP(safi, oneLab, lst, false);
                if (conn.txFree() < 2048) {
                    return true;
                }
                lst.clear();
            }
            if (lst.size() > 0) {
                conn.sendUpdateSP(safi, oneLab, lst, false);
                if (conn.txFree() < 2048) {
                    return true;
                }
            }
            if (needEor) {
                conn.sendEndOfRib(safi);
                conn.needEorAfis[idx] = false;
            }
            if (needEof) {
                conn.sendFreshMark(safi, 2);
                conn.needEofAfis[idx] = false;
            }
        }
        int ver = conn.needFull.ver();
        if (conn.needFull.get() == 1) {
            conn.needFull.setIf(0, ver);
        } else {
            conn.needFull.setIf(1, ver);
        }
        fullLast = bits.getTime();
        fullTime = (int) (fullLast - tim);
        fullCount++;
        return false;
    }

    private boolean advertIncr() {
        long tim = bits.getTime();
        for (int idx = 0; idx < addrFams.length; idx++) {
            if (!conn.peerAfis[idx]) {
                continue;
            }
            int safi = lower.idx2safi(idx);
            tabRoute<addrIP> will = willing[idx];
            tabRoute<addrIP> chgd = changed[idx];
            tabRoute<addrIP> done = conn.advert[idx];
            boolean oneLab = !conn.peerMltLab[idx];
            chgd = new tabRoute<addrIP>(chgd);
            if (conn.addpathTx[idx]) {
                for (int i = 0; i < chgd.size(); i++) {
                    tabRouteEntry<addrIP> cur = chgd.get(i);
                    if (cur == null) {
                        continue;
                    }
                    tabRouteEntry<addrIP> wil = will.find(cur);
                    tabRouteEntry<addrIP> don = done.find(cur);
                    if (wil == null) {
                        if (don == null) {
                            continue;
                        }
                        done.del(don);
                        conn.sendUpdateAP(safi, oneLab, wil, don);
                    } else {
                        if (wil.differs(tabRoute.addType.alters, don) == 0) {
                            continue;
                        }
                        done.add(tabRoute.addType.always, wil, false, false);
                        conn.sendUpdateAP(safi, oneLab, wil, don);
                    }
                    if (conn.txFree() < 1024) {
                        return true;
                    }
                }
                continue;
            }
            List<tabRouteEntry<addrIP>> lstA = new ArrayList<tabRouteEntry<addrIP>>();
            List<tabRouteEntry<addrIP>> lstW = new ArrayList<tabRouteEntry<addrIP>>();
            tabRouteEntry<addrIP> sen = null;
            for (int i = 0; i < chgd.size(); i++) {
                tabRouteEntry<addrIP> cur = chgd.get(i);
                if (cur == null) {
                    continue;
                }
                tabRouteEntry<addrIP> wil = will.find(cur);
                tabRouteEntry<addrIP> don = done.find(cur);
                if (wil == null) {
                    if (don == null) {
                        continue;
                    }
                    done.del(don);
                    lstW.add(don);
                    if (lstW.size() < 64) {
                        continue;
                    }
                    conn.sendUpdateSP(safi, oneLab, lstW, false);
                    if (conn.txFree() < 2048) {
                        return true;
                    }
                    lstW.clear();
                    continue;
                }
                if (wil.differs(tabRoute.addType.notyet, don) == 0) {
                    continue;
                }
                if (sen != null) {
                    sen.prefix = wil.prefix;
                }
                if (wil.differs(tabRoute.addType.notyet, sen) != 0) {
                    if (lstA.size() > 0) {
                        conn.sendUpdateSP(safi, oneLab, lstA, true);
                    }
                    if (conn.txFree() < 2048) {
                        return true;
                    }
                    lstA.clear();
                    sen = wil.copyBytes(tabRoute.addType.notyet);
                }
                done.add(tabRoute.addType.always, wil, false, false);
                lstA.add(wil);
                if (lstA.size() > 64) {
                    sen = null;
                }
            }
            if (lstW.size() > 0) {
                conn.sendUpdateSP(safi, oneLab, lstW, false);
                if (conn.txFree() < 2048) {
                    return true;
                }
            }
            if (lstA.size() > 0) {
                conn.sendUpdateSP(safi, oneLab, lstA, true);
                if (conn.txFree() < 2048) {
                    return true;
                }
            }
        }
        incrLast = bits.getTime();
        incrTime = (int) (incrLast - tim);
        incrCount++;
        return false;
    }

    private void doAdvert() {
        if ((bits.getTime() - conn.lastRx) > holdTimer) {
            conn.sendNotify(4, 0);
            return;
        }
        int doing = lower.compRound.get();
        if (doing == conn.adversion.get()) {
            return;
        }
        if (conn.txFree() < (bufferSize / 2)) {
            conn.needFull.add(1);
            conn.buffFull++;
            return;
        }
        if (unidirection && (conn.rxReady() > (bufferSize / 8))) {
            conn.needFull.add(1);
            return;
        }
        if (advertIntTx > 0) {
            if ((bits.getTime() - advertLast) < advertIntTx) {
                return;
            }
        }
        boolean b;
        long advs = conn.cntr.packTx;
        if (conn.needFull.get() != 0) {
            b = advertFull();
        } else {
            b = advertIncr();
        }
        if (maxPrxOutCnt > 0) {
            int i = conn.getPrefixSent();
            if (i > ((maxPrxOutCnt * maxPrxOutPrc) / 100)) {
                logger.info("neighbor " + peerAddr + " got " + i + " prefixes");
            }
            if (i > maxPrxOutCnt) {
                conn.sendNotify(6, 1);
            }
        }
        advs = conn.cntr.packTx - advs;
        if (advs > 0) {
            advertLast = bits.getTime();
            advertCount++;
        }
        if (b) {
            conn.needFull.add(1);
            return;
        }
        if (conn.needFull.get() != 0) {
            doing--;
        }
        conn.adversion.set(doing);
    }

    /**
     * set accepted list
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public void setAccepted() {
        accUni = new tabRoute<addrIP>("bgp");
        accMlt = new tabRoute<addrIP>("bgp");
        accOuni = new tabRoute<addrIP>("bgp");
        accOmlt = new tabRoute<addrIP>("bgp");
        accOflw = new tabRoute<addrIP>("bgp");
        accOsrt = new tabRoute<addrIP>("bgp");
        accFlw = new tabRoute<addrIP>("bgp");
        accVpnU = new tabRoute<addrIP>("bgp");
        accVpnM = new tabRoute<addrIP>("bgp");
        accVpnF = new tabRoute<addrIP>("bgp");
        accVpoU = new tabRoute<addrIP>("bgp");
        accVpoM = new tabRoute<addrIP>("bgp");
        accVpoF = new tabRoute<addrIP>("bgp");
        accVpls = new tabRoute<addrIP>("bgp");
        accMspw = new tabRoute<addrIP>("bgp");
        accEvpn = new tabRoute<addrIP>("bgp");
        accMdt = new tabRoute<addrIP>("bgp");
        accNsh = new tabRoute<addrIP>("bgp");
        accRpd = new tabRoute<addrIP>("bgp");
        accSdw = new tabRoute<addrIP>("bgp");
        accSpf = new tabRoute<addrIP>("bgp");
        accRtf = new tabRoute<addrIP>("bgp");
        accSrte = new tabRoute<addrIP>("bgp");
        accLnks = new tabRoute<addrIP>("bgp");
        accMvpn = new tabRoute<addrIP>("bgp");
        accMvpo = new tabRoute<addrIP>("bgp");
        accMtre = new tabRoute<addrIP>("bgp");
        accMtro = new tabRoute<addrIP>("bgp");
        rtfilterUsed = null;
        reachable = false;
        if (sendingIfc != null) {
            ipFwdIface ifc = ipFwdTab.findSendingIface(lower.fwdCore, peerAddr);
            if (ifc == null) {
                return;
            }
            if (ifc.ifwNum != sendingIfc.ifwNum) {
                return;
            }
        }
        if (trackNxthop) {
            if (lower.nhtRoumap != null) {
                tabRouteEntry<addrIP> rou = lower.fwdCore.actualU.route(peerAddr);
                if (rou == null) {
                    return;
                }
                if (!lower.nhtRoumap.matches(lower.afiUni, remoteAs, rou)) {
                    return;
                }
            }
            if (lower.nhtRouplc != null) {
                tabRouteEntry<addrIP> rou = lower.fwdCore.actualU.route(peerAddr);
                if (rou == null) {
                    return;
                }
                rou = tabRtrplc.doRpl(lower.afiUni, remoteAs, rou, lower.nhtRouplc, true);
                if (rou == null) {
                    return;
                }
            }
            if (lower.nhtPfxlst != null) {
                tabRouteEntry<addrIP> rou = lower.fwdCore.actualU.route(peerAddr);
                if (rou == null) {
                    return;
                }
                if (!lower.nhtPfxlst.matches(lower.afiUni, remoteAs, rou)) {
                    return;
                }
            }
        }
        reachable = true;
        if (!softReconfig) {
            accUni = conn.learnt[rtrBgpParam.idxUni];
            accMlt = conn.learnt[rtrBgpParam.idxMlt];
            accOuni = conn.learnt[rtrBgpParam.idxOuni];
            accOmlt = conn.learnt[rtrBgpParam.idxOmlt];
            accOflw = conn.learnt[rtrBgpParam.idxOflw];
            accOsrt = conn.learnt[rtrBgpParam.idxOsrt];
            accFlw = conn.learnt[rtrBgpParam.idxFlw];
            accVpnU = conn.learnt[rtrBgpParam.idxVpnU];
            accVpnM = conn.learnt[rtrBgpParam.idxVpnM];
            accVpnF = conn.learnt[rtrBgpParam.idxVpnF];
            accVpoU = conn.learnt[rtrBgpParam.idxVpoU];
            accVpoM = conn.learnt[rtrBgpParam.idxVpoM];
            accVpoF = conn.learnt[rtrBgpParam.idxVpoF];
            accVpls = conn.learnt[rtrBgpParam.idxVpls];
            accMspw = conn.learnt[rtrBgpParam.idxMspw];
            accEvpn = conn.learnt[rtrBgpParam.idxEvpn];
            accMdt = conn.learnt[rtrBgpParam.idxMdt];
            accNsh = conn.learnt[rtrBgpParam.idxNsh];
            accRpd = conn.learnt[rtrBgpParam.idxRpd];
            accSdw = conn.learnt[rtrBgpParam.idxSdw];
            accSpf = conn.learnt[rtrBgpParam.idxSpf];
            accRtf = conn.learnt[rtrBgpParam.idxRtf];
            accSrte = conn.learnt[rtrBgpParam.idxSrte];
            accLnks = conn.learnt[rtrBgpParam.idxLnks];
            accMvpn = conn.learnt[rtrBgpParam.idxMvpn];
            accMvpo = conn.learnt[rtrBgpParam.idxMvpo];
            accMtre = conn.learnt[rtrBgpParam.idxMtre];
            accMtro = conn.learnt[rtrBgpParam.idxMtro];
            if (rtfilterOut && conn.peerAfis[rtrBgpParam.idxRtf]) {
                rtfilterUsed = accRtf;
            }
            return;
        }
        setValidity(accUni, rpkiIn, lower.rpkiA);
        setValidity(accMlt, rpkiIn, lower.rpkiA);
        setValidity(accOuni, rpkiIn, lower.rpkiO);
        setValidity(accOmlt, rpkiIn, lower.rpkiO);
        setValidity(accVpnU, vpkiIn, lower.rpkiA);
        setValidity(accVpnM, vpkiIn, lower.rpkiA);
        setValidity(accVpoU, vpkiIn, lower.rpkiO);
        setValidity(accVpoM, vpkiIn, lower.rpkiO);
        for (int idx = 0; idx < addrFams.length; idx++) {
            if (!conn.peerAfis[idx]) {
                continue;
            }
            int afi = lower.idx2safi(idx);
            long msk = 1L << idx;
            tabRoute<addrIP> trg = getAccepted(idx, msk, afi);
            tabListing[] fltr = getInFilters(idx);
            tabRoute.addUpdatedTable(tabRoute.addType.ecmp, afi, remoteAs, trg, conn.learnt[idx], true, fltr[0], fltr[1], fltr[2]);
        }
        if (rtfilterOut && conn.peerAfis[rtrBgpParam.idxRtf]) {
            rtfilterUsed = accRtf;
        }
        if (dampenPfxs == null) {
            return;
        }
        for (int i = 0; i < dampenPfxs.size(); i++) {
            rtrBgpDamp ntry = dampenPfxs.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.dampened) {
                continue;
            }
            tabRoute<addrIP> lst = getAccepted(ntry.idx, ntry.mask, ntry.afi);
            if (lst == null) {
                continue;
            }
            tabRouteEntry<addrIP> prf = new tabRouteEntry<addrIP>();
            prf.rouDst = ntry.rd;
            prf.prefix = ntry.prefix;
            lst.del(prf);
        }
    }

    /**
     * set merged list
     */
    public void setMerge() {
        tabRoute.addType mod;
        if (lower.routerEcmp) {
            mod = tabRoute.addType.lnkEcmp;
        } else {
            mod = tabRoute.addType.lnkBcmp;
        }
        lower.newlyUni.mergeFrom(mod, new tabRoute<addrIP>(accUni), tabRouteAttr.distanLim);
        lower.newlyMlt.mergeFrom(mod, new tabRoute<addrIP>(accMlt), tabRouteAttr.distanLim);
        lower.newlyOuni.mergeFrom(mod, new tabRoute<addrIP>(accOuni), tabRouteAttr.distanLim);
        lower.newlyOmlt.mergeFrom(mod, new tabRoute<addrIP>(accOmlt), tabRouteAttr.distanLim);
        lower.newlyOflw.mergeFrom(mod, new tabRoute<addrIP>(accOflw), tabRouteAttr.distanLim);
        lower.newlyOsrt.mergeFrom(mod, new tabRoute<addrIP>(accOsrt), tabRouteAttr.distanLim);
        lower.newlyFlw.mergeFrom(mod, new tabRoute<addrIP>(accFlw), tabRouteAttr.distanLim);
        lower.newlyVpnU.mergeFrom(mod, new tabRoute<addrIP>(accVpnU), tabRouteAttr.distanLim);
        lower.newlyVpnM.mergeFrom(mod, new tabRoute<addrIP>(accVpnM), tabRouteAttr.distanLim);
        lower.newlyVpnF.mergeFrom(mod, new tabRoute<addrIP>(accVpnF), tabRouteAttr.distanLim);
        lower.newlyVpoU.mergeFrom(mod, new tabRoute<addrIP>(accVpoU), tabRouteAttr.distanLim);
        lower.newlyVpoM.mergeFrom(mod, new tabRoute<addrIP>(accVpoM), tabRouteAttr.distanLim);
        lower.newlyVpoF.mergeFrom(mod, new tabRoute<addrIP>(accVpoF), tabRouteAttr.distanLim);
        lower.newlyVpls.mergeFrom(mod, new tabRoute<addrIP>(accVpls), tabRouteAttr.distanLim);
        lower.newlyMspw.mergeFrom(mod, new tabRoute<addrIP>(accMspw), tabRouteAttr.distanLim);
        lower.newlyEvpn.mergeFrom(mod, new tabRoute<addrIP>(accEvpn), tabRouteAttr.distanLim);
        lower.newlyMdt.mergeFrom(mod, new tabRoute<addrIP>(accMdt), tabRouteAttr.distanLim);
        lower.newlyNsh.mergeFrom(mod, new tabRoute<addrIP>(accNsh), tabRouteAttr.distanLim);
        lower.newlyRpd.mergeFrom(mod, new tabRoute<addrIP>(accRpd), tabRouteAttr.distanLim);
        lower.newlySdw.mergeFrom(mod, new tabRoute<addrIP>(accSdw), tabRouteAttr.distanLim);
        lower.newlySpf.mergeFrom(mod, new tabRoute<addrIP>(accSpf), tabRouteAttr.distanLim);
        lower.newlyRtf.mergeFrom(mod, new tabRoute<addrIP>(accRtf), tabRouteAttr.distanLim);
        lower.newlySrte.mergeFrom(mod, new tabRoute<addrIP>(accSrte), tabRouteAttr.distanLim);
        lower.newlyLnks.mergeFrom(mod, new tabRoute<addrIP>(accLnks), tabRouteAttr.distanLim);
        lower.newlyMvpn.mergeFrom(mod, new tabRoute<addrIP>(accMvpn), tabRouteAttr.distanLim);
        lower.newlyMvpo.mergeFrom(mod, new tabRoute<addrIP>(accMvpo), tabRouteAttr.distanLim);
        lower.newlyMtre.mergeFrom(mod, new tabRoute<addrIP>(accMtre), tabRouteAttr.distanLim);
        lower.newlyMtro.mergeFrom(mod, new tabRoute<addrIP>(accMtro), tabRouteAttr.distanLim);
    }

    /**
     * set group membership
     */
    public void setGroup() {
        lower.have2reflect |= peerType == rtrBgpUtil.peerRflct;
        if (reachable != reachOld) {
            reachOld = reachable;
            reachTim = bits.getTime();
            reachNum++;
            if (debugger.rtrBgpEvnt) {
                logger.debug("reachable neighbor " + peerAddr + " " + reachable);
            }
        }
        groupMember = -1;
        if (checkShutdown()) {
            return;
        }
        if (conn == null) {
            return;
        }
        if (!conn.ready2adv) {
            return;
        }
        for (int i = 0; i < lower.groups.size(); i++) {
            rtrBgpGroup ntry = lower.groups.get(i);
            if (ntry.peerType != peerType) {
                continue;
            }
            if (nxtHopSelf || (!nxtHopUnchgd)) {
                if (localAddr.compareTo(ntry.localAddr) != 0) {
                    continue;
                }
                if (localOddr.compareTo(ntry.localOddr) != 0) {
                    continue;
                }
            }
            if (ntry.sameOutput(this)) {
                continue;
            }
            groupMember = i;
            return;
        }
        groupMember = lower.groups.size();
        rtrBgpGroup ntry = new rtrBgpGroup(lower, groupMember);
        ntry.copyFrom(this);
        ntry.peerType = peerType;
        ntry.localAddr = localAddr.copyBytes();
        ntry.localOddr = localOddr.copyBytes();
        lower.groups.add(ntry);
    }

    /**
     * set needed prefixes
     */
    public void setNeeded() {
        if (groupMember < 0) {
            for (int i = 0; i < willing.length; i++) {
                willing[i] = new tabRoute<addrIP>("tx");
                changed[i] = new tabRoute<addrIP>("chg");
            }
        } else {
            rtrBgpGroup grp = lower.groups.get(groupMember);
            for (int i = 0; i < willing.length; i++) {
                willing[i] = grp.getWilling(i, 0, 0);
                changed[i] = grp.getChanged(i, 0, 0);
            }
        }
        conn.needFull.add(1);
    }

    /**
     * validate a prefix
     *
     * @param afi address family
     * @param ntry route entry
     */
    public void setValidity(int afi, tabRouteEntry<addrIP> ntry) {
        if (lower.rpkiR == null) {
            return;
        }
        if ((afi == lower.afiUni) || (afi == lower.afiMlt)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiA, lower.rpkiP, rpkiIn);
        }
        if ((afi == lower.afiOuni) || (afi == lower.afiOmlt)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiO, lower.rpkiP, rpkiIn);
        }
        if ((afi == lower.afiVpnU) || (afi == lower.afiVpnM)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiA, lower.rpkiP, vpkiIn);
        }
        if ((afi == lower.afiVpoU) || (afi == lower.afiVpoM)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiO, lower.rpkiP, vpkiIn);
        }
    }

    /**
     * set validity
     *
     * @param tab table to use
     * @param mod mode to use
     * @param roa roa to use
     */
    public void setValidity(tabRoute<addrIP> tab, int mod, tabGen<tabRpkiRoa> roa) {
        if (lower.rpkiR == null) {
            return;
        }
        tabRpkiUtil.setValidityTable(localAs, tab, roa, lower.rpkiP, mod);
    }

    /**
     * group version
     */
    public void setGrpVer() {
        if (groupMember < 0) {
            return;
        }
        if (conn.needFull.get() > 1) {
            return;
        }
        rtrBgpGroup grp = lower.groups.get(groupMember);
        int i = conn.adversion.get();
        if (i < grp.minversion) {
            grp.minversion = i;
        }
    }

    /**
     * update dampening statistics
     *
     * @param idx afi
     * @param mask afi
     * @param afi afi
     * @param rd rd
     * @param prf prefix
     * @param pnlt penalty
     */
    protected void prefixDampen(int idx, long mask, int afi, long rd, addrPrefix<addrIP> prf, int pnlt) {
        rtrBgpDamp ntry = new rtrBgpDamp(idx, mask, afi, rd, prf);
        rtrBgpDamp old = dampenPfxs.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.penalty += pnlt;
        if (ntry.penalty > dampenMaxp) {
            ntry.penalty = dampenMaxp;
        }
        if (!ntry.dampened && (ntry.penalty > dampenSupp)) {
            ntry.dampened = true;
            if (debugger.rtrBgpDamp) {
                logger.debug("suppressing " + tabRouteUtil.rd2string(ntry.rd) + " " + ntry.prefix);
            }
        }
        ntry.last = bits.getTime();
    }

    /**
     * update dampening statistics
     */
    protected void prefixDampen() {
        if (dampenPfxs == null) {
            return;
        }
        long tim = bits.getTime();
        for (int i = dampenPfxs.size() - 1; i >= 0; i--) {
            rtrBgpDamp ntry = dampenPfxs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.last > (tim - dampenHalf)) {
                continue;
            }
            ntry.last = tim;
            ntry.penalty = ntry.penalty / 2;
            if (ntry.dampened && (ntry.penalty < dampenReus)) {
                ntry.dampened = false;
                if (debugger.rtrBgpDamp) {
                    logger.debug("unsuppressing " + tabRouteUtil.rd2string(ntry.rd) + " " + ntry.prefix);
                }
            }
            if (ntry.penalty < dampenMinp) {
                dampenPfxs.del(ntry);
                if (debugger.rtrBgpDamp) {
                    logger.debug("forgetting " + tabRouteUtil.rd2string(ntry.rd) + " " + ntry.prefix);
                }
            }
        }
    }

    /**
     * get accepted
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getAccepted(int idx, long mask, int safi) {
        if (idx == rtrBgpParam.idxUni) {
            return accUni;
        }
        if (idx == rtrBgpParam.idxLab) {
            return accUni;
        }
        if (idx == rtrBgpParam.idxCtp) {
            return accUni;
        }
        if (idx == rtrBgpParam.idxCar) {
            return accUni;
        }
        if (idx == rtrBgpParam.idxMlt) {
            return accMlt;
        }
        if (idx == rtrBgpParam.idxOlab) {
            return accOuni;
        }
        if (idx == rtrBgpParam.idxOctp) {
            return accOuni;
        }
        if (idx == rtrBgpParam.idxOcar) {
            return accOuni;
        }
        if (idx == rtrBgpParam.idxOuni) {
            return accOuni;
        }
        if (idx == rtrBgpParam.idxOmlt) {
            return accOmlt;
        }
        if (idx == rtrBgpParam.idxOflw) {
            return accOflw;
        }
        if (idx == rtrBgpParam.idxOsrt) {
            return accOsrt;
        }
        if (idx == rtrBgpParam.idxFlw) {
            return accFlw;
        }
        if (idx == rtrBgpParam.idxVpnU) {
            return accVpnU;
        }
        if (idx == rtrBgpParam.idxVpnM) {
            return accVpnM;
        }
        if (idx == rtrBgpParam.idxVpnF) {
            return accVpnF;
        }
        if (idx == rtrBgpParam.idxVpoU) {
            return accVpoU;
        }
        if (idx == rtrBgpParam.idxVpoM) {
            return accVpoM;
        }
        if (idx == rtrBgpParam.idxVpoF) {
            return accVpoF;
        }
        if (idx == rtrBgpParam.idxVpls) {
            return accVpls;
        }
        if (idx == rtrBgpParam.idxMspw) {
            return accMspw;
        }
        if (idx == rtrBgpParam.idxEvpn) {
            return accEvpn;
        }
        if (idx == rtrBgpParam.idxMdt) {
            return accMdt;
        }
        if (idx == rtrBgpParam.idxNsh) {
            return accNsh;
        }
        if (idx == rtrBgpParam.idxRpd) {
            return accRpd;
        }
        if (idx == rtrBgpParam.idxSdw) {
            return accSdw;
        }
        if (idx == rtrBgpParam.idxSpf) {
            return accSpf;
        }
        if (idx == rtrBgpParam.idxRtf) {
            return accRtf;
        }
        if (idx == rtrBgpParam.idxSrte) {
            return accSrte;
        }
        if (idx == rtrBgpParam.idxLnks) {
            return accLnks;
        }
        if (idx == rtrBgpParam.idxMvpn) {
            return accMvpn;
        }
        if (idx == rtrBgpParam.idxMvpo) {
            return accMvpo;
        }
        if (idx == rtrBgpParam.idxMtre) {
            return accMtre;
        }
        if (idx == rtrBgpParam.idxMtro) {
            return accMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * neighbor list entry
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return line of string
     */
    public String showNeighs(int idx, long mask, int safi) {
        return showSummry1() + "|" + conn.learnt[idx].size() + "|" + getAccepted(idx, mask, safi).size() + "|" + willing[idx].size() + "|" + conn.advert[idx].size() + "|" + bits.timePast(conn.upTime);
    }

    /**
     * check readiness status
     *
     * @return peer readiness string
     */
    public String getReadiness() {
        if (shutdown) {
            return "admin";
        }
        return cmds.upDown(conn.ready2adv);
    }

    /**
     * neighbor list entry
     *
     * @return line of string
     */
    public String showSummry1() {
        return peerAddr + "|" + bits.num2str(remoteAs);
    }

    /**
     * neighbor list entry
     *
     * @return line of string
     */
    public String showSummry2() {
        return showSummry1() + "|" + getReadiness() + "|" + conn.getPrefixGot() + "|" + conn.getPrefixSent() + "|" + bits.timePast(conn.upTime);
    }

    /**
     * neighbor list entry
     *
     * @param mod mode
     * @return line of string
     */
    public String showSummary(int mod) {
        switch (mod) {
            case 1:
                return showSummry1() + "|" + rtrBgpParam.bools2string(conn.peerAfis) + "|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(addrFams, conn.peerAfis)) + "|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(conn.originalSafiList, conn.peerAfis));
            case 2:
                return showSummry1() + "|" + groupMember + "|" + socketMode + "|" + bits.timePast(conn.upTime);
            case 3:
                return showSummry1() + "|" + reachable + "|" + bits.timePast(reachTim) + "|" + reachNum + "|" + sessNum + "|" + bits.timePast(conn.upTime);
            case 4:
                return showSummry1() + "|" + rtrBgpParam.bools2string(conn.peerGrace) + "|" + rtrBgpParam.bools2string(graceRestart);
            case 5:
                return showSummry1() + "|" + rtrBgpParam.bools2string(conn.addpathRx) + "|" + rtrBgpParam.bools2string(conn.addpathTx) + "|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(addpathRmode, conn.addpathRx)) + "|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(addpathTmode, conn.addpathTx)) + "|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(conn.originalAddRlist, conn.addpathRx)) + "|" + rtrBgpParam.bools2string(rtrBgpParam.boolsXor(conn.originalAddTlist, conn.addpathTx));
            case 6:
                return showSummry1() + "|" + conn.peerRouterID + "|" + conn.peer32bitAS + "|" + conn.peerRefreshOld + " " + conn.peerRefreshNew + "|" + conn.peerDynCap + "|" + conn.peerExtOpen + "|" + conn.peerExtUpd + "|" + rtrBgpUtil.peerType2string(peerType) + "|" + rtrBgpUtil.leakRole2string(leakRole, leakAttr);
            case 7:
                return showSummry1() + "|" + pipeSide.getStatus(conn.pipe) + "|" + conn.buffFull + "|" + conn.adversion + "|" + incrCount + "|" + fullCount + "|" + conn.needFull;
            case 8:
                return showSummry1() + "|" + description;
            case 9:
                return showSummry1() + "|" + conn.peerHostname + "|" + conn.peerDomainname;
            case 10:
                return showSummry1() + "|" + (conn.compressRx != null) + "|" + (conn.compressTx != null) + "|" + bits.percent(conn.cntr.byteRx, conn.compressCntr.byteRx) + "|" + bits.percent(conn.cntr.byteTx, conn.compressCntr.byteTx);
            case 11:
                return showSummry1() + "|" + conn.cntr.packRx + "|" + conn.cntr.packTx + "|" + conn.cntr.byteRx + "|" + conn.cntr.byteTx + "|" + conn.refreshRx + "|" + conn.refreshTx + "|" + conn.dynCapaRx + "|" + conn.dynCapaTx;
            case 12:
                clntDns clnt = new clntDns();
                clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(peerAddr), false, packDnsRec.typePTR);
                return showSummry1() + "|" + clnt.getPTR();
            case 13:
                return showSummry2();
            case 14:
                return showSummry1() + "|" + rtrBgpParam.bools2string(conn.peerMltLab) + "|" + rtrBgpParam.bools2string(multiLabel);
            case 15:
                return showSummry1() + "|" + rtrBgpParam.bools2string(conn.peerLlGrace) + "|" + rtrBgpParam.bools2string(llGraceRestart);
            case 16:
                return showSummry1() + "|" + conn.peerSoftware;
            case 17:
                return showSummry2() + "|" + description;
            case 18:
                return showSummry1() + "|" + conn.unknownCntr.packRx + "|" + conn.unknownCntr.byteRx + "|" + bits.timePast(conn.unknownCntr.lastRx);
            case 19:
                return showSummry2() + "|" + clntWhois.asn2name(remoteAs, true) + "|" + clntWhois.asn2info(remoteAs);
            case 20:
                return showSummry1() + "|" + reachabStat.packRx + "|" + reachabStat.packTx + "|" + unreachStat.packRx + "|" + unreachStat.packTx + "|" + bits.timePast(reachabStat.lastRx) + "|" + bits.timePast(reachabStat.lastTx);
            default:
                return null;
        }
    }

    /**
     * get message statistics
     *
     * @return list of statistics
     */
    public userFormat getMsgStats() {
        return rtrBgpDump.getMsgStats(msgStats);
    }

    /**
     * get message statistics
     *
     * @return list of statistics
     */
    public userFormat getAttrStats() {
        return rtrBgpDump.getAttrStats(attrStats);
    }

    /**
     * get state information
     *
     * @return state
     */
    public String stateGet() {
        if (!haMode) {
            return null;
        }
        if (!conn.ready2adv) {
            return null;
        }
        if (!conn.peer32bitAS) {
            return null;
        }
        if (!conn.peerRefreshOld) {
            return null;
        }
        if (!conn.peerRefreshNew) {
            return null;
        }
        if ((conn.compressRx != null) && (conn.compressTx != null)) {
            return null;
        }
        prtGenConn sock = lower.tcpCore.findOneConn(conn.pipe);
        if (sock == null) {
            return null;
        }
        sock.restartable = true;
        return peerAddr + " " + template + " " + sock.portLoc + " " + sock.portRem + " " + sock.iface + " " + bits.num2str(remoteAs) + " " + conn.peerHold + " " + conn.upTime + " " + stateGet(conn.peerAfis) + " " + stateGet(conn.addpathRx) + " " + stateGet(conn.addpathTx) + " " + stateGet(conn.peerMltLab) + " " + conn.peerDynCap + " " + conn.peerRouterID;
    }

    private String stateGet(boolean[] afi) {
        String a = rtrBgpParam.bools2string(afi);
        a = a.trim();
        a = a.replaceAll(" ", ",");
        return a;
    }

    private boolean[] stateSet(String a) {
        a = a.replaceAll(",", " ");
        a = a.trim();
        cmds c = new cmds("afi", a);
        return rtrBgpParam.string2bools(c);
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean stateSet(cmds cmd) {
        int i = bits.str2num(cmd.word());
        int o = bits.str2num(cmd.word());
        cfgIfc cfg = cfgAll.ifcFind(cmd.word(), 0);
        if (cfg == null) {
            return true;
        }
        ipFwdIface ifc = cfg.getFwdIfc(peerAddr);
        if (ifc == null) {
            return true;
        }
        updateAddr(ifc);
        pipeSide pip = lower.tcpCore.streamResume(new pipeLine(bufferSize, false), localIfc, i, peerAddr, o, "bgp", keyId, passwd, ttlSecurity, tosValue);
        if (pip == null) {
            return true;
        }
        conn = new rtrBgpSpeak(lower, this, pip, 2);
        i = bits.str2num(cmd.word());
        if (remoteAny) {
            remoteAs = i;
        }
        i = bits.str2num(cmd.word());
        conn.peerHold = i;
        conn.peerKeep = i / 3;
        pip.setTime(i);
        conn.upTime = bits.str2long(cmd.word());
        conn.peerAfis = stateSet(cmd.word());
        conn.addpathRx = stateSet(cmd.word());
        conn.addpathTx = stateSet(cmd.word());
        conn.peerMltLab = stateSet(cmd.word());
        conn.peerDynCap = cmd.word().equals("true");
        conn.peerRouterID = new addrIPv4();
        if (conn.peerRouterID.fromString(cmd.word())) {
            pip.setClose();
            return true;
        }
        conn.peer32bitAS = true;
        conn.peerRefreshOld = true;
        conn.peerRefreshNew = true;
        return false;
    }

}
