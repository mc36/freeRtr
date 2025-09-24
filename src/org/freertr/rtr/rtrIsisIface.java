package org.freertr.rtr;

import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.freertr.addr.addrIsis;
import org.freertr.addr.addrMac;
import org.freertr.auth.authLocal;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabAverage;
import org.freertr.tab.tabGen;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;
import org.freertr.sec.secInfoCfg;
import org.freertr.sec.secInfoUtl;

/**
 * isis interface
 *
 * @author matecsaba
 */
public class rtrIsisIface implements Comparable<rtrIsisIface>, ifcUp {

    /**
     * ipinfo config
     */
    public secInfoCfg ipInfoCfg;

    /**
     * the ip interface this works on
     */
    protected final ipFwdIface iface;

    /**
     * the other ip interface this works on
     */
    protected final ipFwdIface oface;

    /**
     * the l2 interface this works on
     */
    private final ifcEthTyp ethtyp;

    /**
     * the l2 packet handler
     */
    protected ifcDn upper = new ifcNull();

    /**
     * levels allowed (bitmask)
     */
    public int circuitLevel;

    /**
     * circuit id
     */
    public int circuitID;

    /**
     * other enabled
     */
    public boolean otherEna;

    /**
     * network type false=broadcast, true=point2point
     */
    public boolean netPnt2pnt;

    /**
     * raw ethertype encapsulation
     */
    public boolean rawEncap;

    private final rtrIsis lower;

    private Timer keepTimer;

    /**
     * own hardware address
     */
    protected addrMac hwaddr;

    /**
     * counter
     */
    protected counter cntr = new counter();

    /**
     * message types received
     */
    public final counter[] msgStats = new counter[256];

    /**
     * suppress interface address
     */
    public boolean suppressAddr;

    /**
     * unsuppress interface address
     */
    public boolean unsuppressAddr;

    /**
     * suppress interface address
     */
    public boolean suppressInt;

    /**
     * other unsuppress interface address
     */
    public boolean othUnsuppAddr;

    /**
     * other suppress interface address
     */
    public boolean othSuppAddr;

    /**
     * check neighbor address is connected
     */
    public boolean connectedCheck;

    /**
     * other suppress interface address
     */
    public boolean othSuppInt;

    /**
     * passive interface
     */
    public boolean passiveInt;

    /**
     * hello timer
     */
    public int helloTimer;

    /**
     * csnp timer
     */
    public int csnpTimer;

    /**
     * dead timer
     */
    public int deadTimer;

    /**
     * retransmit timer
     */
    public int retransTimer;

    /**
     * dis priority
     */
    public int disPriority;

    /**
     * interface metric
     */
    public int metric;

    /**
     * bfd enabled
     */
    public boolean bfdTrigger;

    /**
     * authentication password
     */
    public String authentication;

    /**
     * authentication mode: 1=cleartext, 2=md5, 3=sha1, 4=sha224, 5=sha256,
     * 6=sha384, 7=sha512
     */
    public int authenMode;

    /**
     * authentication id
     */
    public int authenKey;

    /**
     * traffic eng suppression
     */
    public boolean teSuppress;

    /**
     * traffic eng metric
     */
    public int teMetric;

    /**
     * traffic eng bandwidth
     */
    public long teBandwidth;

    /**
     * traffic eng affinity
     */
    public int teAffinity;

    /**
     * traffic eng srlg
     */
    public int teSrlg;

    /**
     * segment rou index
     */
    public int srIndex;

    /**
     * other segment rou index
     */
    public int srOthIdx;

    /**
     * segment rou node
     */
    public boolean srNode;

    /**
     * segment rou pop
     */
    public boolean srPop;

    /**
     * bier index
     */
    public int brIndex;

    /**
     * bier subdomain
     */
    public int brSub;

    /**
     * other bier index
     */
    public int brOthIdx;

    /**
     * other bier subdomain
     */
    public int brOthSub;

    /**
     * echo interval
     */
    public int echoTimer;

    /**
     * echo parameters
     */
    public tabAverage echoParam;

    /**
     * dynamic metric
     */
    public int dynamicMetric;

    /**
     * ldp metric syncrhonization
     */
    public boolean ldpSync;

    /**
     * neighbors
     */
    protected tabGen<rtrIsisNeigh> neighs;

    private addrIsis lev1disA;

    private addrIsis lev2disA;

    private int lev1disI;

    private int lev2disI;

    private int lev1csnp;

    private int lev2csnp;

    private long lastCsnp;

    /**
     * create one instance
     *
     * @param parent the isis protocol
     * @param ifc the ip interface to work on
     * @param oifc the other ip interface to work on
     * @param eth the eth interface to work on
     */
    public rtrIsisIface(rtrIsis parent, ipFwdIface ifc, ipFwdIface oifc, ifcEthTyp eth) {
        lower = parent;
        iface = ifc;
        oface = oifc;
        ethtyp = eth;
        netPnt2pnt = true;
        circuitID = lower.getCircuitId(netPnt2pnt);
        circuitLevel = lower.operateLevel;
        helloTimer = 10000;
        deadTimer = 30000;
        retransTimer = 3000;
        disPriority = 64;
        authenMode = 1;
        metric = 10;
        echoTimer = 60000;
        echoParam = new tabAverage(1, 16777210);
        suppressInt = true;
        othSuppInt = true;
        connectedCheck = true;
        neighs = new tabGen<rtrIsisNeigh>();
        hwaddr = new addrMac();
        lev1disA = new addrIsis();
        lev2disA = new addrIsis();
        teMetric = 10;
        for (int i = 0; i < msgStats.length; i++) {
            msgStats[i] = new counter();
        }
        if (iface != null) {
            teBandwidth = iface.lower.getBandwidth();
        }
    }

    public int compareTo(rtrIsisIface o) {
        if (iface.ifwNum < o.iface.ifwNum) {
            return -1;
        }
        if (iface.ifwNum > o.iface.ifwNum) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "isis on " + iface;
    }

    /**
     * get configuration
     *
     * @param l list to add
     * @param beg beginning
     * @param filter filter defaults
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(cmds.tabulator + beg + "enable");
        cmds.cfgLine(l, !otherEna, cmds.tabulator, beg + "other-enable", "");
        cmds.cfgLine(l, !passiveInt, cmds.tabulator, beg + "passive", "");
        cmds.cfgLine(l, !rawEncap, cmds.tabulator, beg + "raw-encapsulation", "");
        l.add(cmds.tabulator + beg + "circuit " + rtrIsis.level2string(circuitLevel));
        String s;
        if (netPnt2pnt) {
            s = "point2point";
        } else {
            s = "broadcast";
        }
        l.add(cmds.tabulator + beg + "network " + s);
        cmds.cfgLine(l, !bfdTrigger, cmds.tabulator, beg + "bfd", "");
        cmds.cfgLine(l, !suppressInt, cmds.tabulator, beg + "suppress-address", "");
        cmds.cfgLine(l, !suppressAddr, cmds.tabulator, beg + "suppress-prefix", "");
        cmds.cfgLine(l, !unsuppressAddr, cmds.tabulator, beg + "unsuppress-prefix", "");
        cmds.cfgLine(l, !othSuppInt, cmds.tabulator, beg + "other-suppress-address", "");
        cmds.cfgLine(l, !othSuppAddr, cmds.tabulator, beg + "other-suppress-prefix", "");
        cmds.cfgLine(l, !othUnsuppAddr, cmds.tabulator, beg + "other-unsuppress-prefix", "");
        cmds.cfgLine(l, !connectedCheck, cmds.tabulator, beg + "verify-source", "");
        cmds.cfgLine(l, authentication == null, cmds.tabulator, beg + "password", authLocal.passwdEncode(authentication, (filter & 2) != 0));
        String a;
        switch (authenMode) {
            case 0:
                a = "null";
                break;
            case 1:
                a = "clear";
                break;
            case 2:
                a = "md5";
                break;
            case 3:
                a = "sha1";
                break;
            case 4:
                a = "sha224";
                break;
            case 5:
                a = "sha256";
                break;
            case 6:
                a = "sha384";
                break;
            case 7:
                a = "sha512";
                break;
            default:
                a = "unknown=" + authenMode;
                break;
        }
        l.add(cmds.tabulator + beg + "authen-type " + a);
        l.add(cmds.tabulator + beg + "authen-id " + authenKey);
        l.add(cmds.tabulator + beg + "metric " + metric);
        l.add(cmds.tabulator + beg + "priority " + disPriority);
        l.add(cmds.tabulator + beg + "hello-time " + helloTimer);
        l.add(cmds.tabulator + beg + "dead-time " + deadTimer);
        l.add(cmds.tabulator + beg + "retransmit-time " + retransTimer);
        l.add(cmds.tabulator + beg + "csnp-time " + csnpTimer);
        boolean b = false;
        if ((circuitLevel & 1) != 0) {
            b |= lower.level1.traffEng;
        }
        if ((circuitLevel & 2) != 0) {
            b |= lower.level2.traffEng;
        }
        if (b) {
            s = beg + "traffeng ";
            cmds.cfgLine(l, !teSuppress, cmds.tabulator, s + "suppress", "");
            l.add(cmds.tabulator + s + "metric " + teMetric);
            l.add(cmds.tabulator + s + "bandwidth " + teBandwidth);
            l.add(cmds.tabulator + s + "affinity " + teAffinity);
            l.add(cmds.tabulator + s + "srlg " + teSrlg);
        }
        b = false;
        if ((circuitLevel & 1) != 0) {
            b |= lower.level1.segrouEna;
        }
        if ((circuitLevel & 2) != 0) {
            b |= lower.level2.segrouEna;
        }
        if (b) {
            s = beg + "segrout ";
            cmds.cfgLine(l, srIndex < 1, cmds.tabulator, s + "index", "" + srIndex);
            cmds.cfgLine(l, srOthIdx < 1, cmds.tabulator, s + "other-index", "" + srOthIdx);
            cmds.cfgLine(l, !srNode, cmds.tabulator, s + "node", "");
            cmds.cfgLine(l, !srPop, cmds.tabulator, s + "pop", "");
        }
        b = false;
        if ((circuitLevel & 1) != 0) {
            b |= lower.level1.bierEna;
        }
        if ((circuitLevel & 2) != 0) {
            b |= lower.level2.bierEna;
        }
        if (b) {
            s = beg + "bier ";
            cmds.cfgLine(l, brIndex < 1, cmds.tabulator, s + "index", "" + brIndex);
            cmds.cfgLine(l, brSub < 1, cmds.tabulator, s + "subdomain", "" + brSub);
            cmds.cfgLine(l, brOthIdx < 1, cmds.tabulator, s + "other-index", "" + brOthIdx);
            cmds.cfgLine(l, brOthSub < 1, cmds.tabulator, s + "other-subdomain", "" + brOthSub);
        }
        secInfoUtl.getConfig(l, ipInfoCfg, cmds.tabulator + beg + "ipinfo ");
        switch (dynamicMetric) {
            case 0:
                a = "disabled";
                break;
            case 1:
                a = "icmpecho";
                break;
            case 2:
                a = "udpecho";
                break;
            case 3:
                a = "twamp";
                break;
            default:
                a = "unknown=" + dynamicMetric;
        }
        cmds.cfgLine(l, dynamicMetric < 1, cmds.tabulator, beg + "dynamic-metric mode", a);
        l.add(cmds.tabulator + beg + "dynamic-metric time " + echoTimer);
        cmds.cfgLine(l, !ldpSync, cmds.tabulator, beg + "ldp-sync", "");
        echoParam.getConfig(l, beg);
    }

    /**
     * do configuration
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerDoConfig(String a, cmds cmd) {
        if (a.equals("circuit")) {
            circuitLevel = rtrIsis.string2level(cmd.word()) & lower.operateLevel;
            lower.genLsps(3);
            return;
        }
        if (a.equals("network")) {
            a = cmd.word();
            int i = -1;
            if (a.equals("point2point")) {
                i = 1;
            }
            if (a.equals("broadcast")) {
                i = 2;
            }
            if (i < 0) {
                return;
            }
            netPnt2pnt = i == 1;
            circuitID = lower.getCircuitId(netPnt2pnt);
            lower.genLsps(3);
            return;
        }
        if (a.equals("raw-encapsulation")) {
            rawEncap = true;
            unregister2eth();
            register2eth();
            return;
        }
        if (a.equals("passive")) {
            passiveInt = true;
            return;
        }
        if (a.equals("bfd")) {
            bfdTrigger = true;
            return;
        }
        if (a.equals("csnp-time")) {
            csnpTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = true;
            lower.genLsps(1);
            return;
        }
        if (a.equals("unsuppress-prefix")) {
            unsuppressAddr = true;
            lower.genLsps(1);
            return;
        }
        if (a.equals("suppress-address")) {
            suppressInt = true;
            lower.genLsps(1);
            return;
        }
        if (a.equals("other-suppress-prefix")) {
            othSuppAddr = true;
            lower.genLsps(1);
            return;
        }
        if (a.equals("other-unsuppress-prefix")) {
            othUnsuppAddr = true;
            lower.genLsps(1);
            return;
        }
        if (a.equals("other-suppress-address")) {
            othSuppInt = true;
            lower.genLsps(1);
            return;
        }
        if (a.equals("verify-source")) {
            connectedCheck = true;
            return;
        }
        if (a.equals("hello-time")) {
            helloTimer = bits.str2num(cmd.word());
            restartTimer(false);
            return;
        }
        if (a.equals("dead-time")) {
            deadTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("retransmit-time")) {
            retransTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("priority")) {
            disPriority = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("metric")) {
            metric = bits.str2num(cmd.word());
            lower.genLsps(3);
            return;
        }
        if (a.equals("other-enable")) {
            otherEna = oface != null;
            lower.genLsps(1);
            return;
        }
        if (a.equals("password")) {
            authentication = authLocal.passwdDecode(cmd.word());
            return;
        }
        if (a.equals("authen-id")) {
            authenKey = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("authen-type")) {
            authenMode = 0;
            a = cmd.word();
            if (a.equals("null")) {
                authenMode = 0;
                return;
            }
            if (a.equals("clear")) {
                authenMode = 1;
                return;
            }
            if (a.equals("md5")) {
                authenMode = 2;
                return;
            }
            if (a.equals("sha1")) {
                authenMode = 3;
                return;
            }
            if (a.equals("sha224")) {
                authenMode = 4;
                return;
            }
            if (a.equals("sha256")) {
                authenMode = 5;
                return;
            }
            if (a.equals("sha384")) {
                authenMode = 6;
                return;
            }
            if (a.equals("sha512")) {
                authenMode = 7;
                return;
            }
            return;
        }
        if (a.equals("traffeng")) {
            a = cmd.word();
            if (a.equals("suppress")) {
                teSuppress = true;
                lower.genLsps(1);
                return;
            }
            if (a.equals("metric")) {
                teMetric = bits.str2num(cmd.word());
                lower.genLsps(1);
                return;
            }
            if (a.equals("bandwidth")) {
                teBandwidth = bits.str2long(cmd.word());
                lower.genLsps(1);
                return;
            }
            if (a.equals("affinity")) {
                teAffinity = bits.str2num(cmd.word());
                lower.genLsps(1);
                return;
            }
            if (a.equals("srlg")) {
                teSrlg = bits.str2num(cmd.word());
                lower.genLsps(1);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("segrout")) {
            a = cmd.word();
            if (a.equals("index")) {
                srIndex = bits.str2num(cmd.word());
                lower.genLsps(3);
                return;
            }
            if (a.equals("other-index")) {
                srOthIdx = bits.str2num(cmd.word());
                lower.genLsps(3);
                return;
            }
            if (a.equals("node")) {
                srNode = true;
                lower.genLsps(3);
                return;
            }
            if (a.equals("pop")) {
                srPop = true;
                lower.genLsps(3);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("bier")) {
            a = cmd.word();
            if (a.equals("index")) {
                brIndex = bits.str2num(cmd.word());
                lower.genLsps(3);
                return;
            }
            if (a.equals("subdomain")) {
                brSub = bits.str2num(cmd.word());
                lower.genLsps(3);
                return;
            }
            if (a.equals("other-index")) {
                brOthIdx = bits.str2num(cmd.word());
                lower.genLsps(3);
                return;
            }
            if (a.equals("other-subdomain")) {
                brOthSub = bits.str2num(cmd.word());
                lower.genLsps(3);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ipinfo")) {
            ipInfoCfg = secInfoUtl.doCfgStr(ipInfoCfg, cmd, false);
            return;
        }
        if (a.equals("ldp-sync")) {
            ldpSync = true;
            lower.genLsps(3);
            return;
        }
        if (a.equals("dynamic-metric")) {
            a = cmd.word();
            if (a.equals("mode")) {
                a = cmd.word();
                dynamicMetric = 0;
                if (a.equals("disabled")) {
                    dynamicMetric = 0;
                }
                if (a.equals("icmpecho")) {
                    dynamicMetric = 1;
                }
                if (a.equals("udpecho")) {
                    dynamicMetric = 2;
                }
                if (a.equals("twamp")) {
                    dynamicMetric = 3;
                }
                lower.genLsps(3);
                return;
            }
            if (a.equals("time")) {
                echoTimer = bits.str2num(cmd.word());
                return;
            }
            if (echoParam.doConfig(a, cmd)) {
                return;
            }
            if (a.equals("algo")) {
                echoParam.string2algo(cmd.word());
                lower.genLsps(3);
                return;
            }
            cmd.badCmd();
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo configuration
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerUnConfig(String a, cmds cmd) {
        if (a.equals("metric")) {
            metric = 10;
            lower.genLsps(3);
            return;
        }
        if (a.equals("passive")) {
            passiveInt = false;
            return;
        }
        if (a.equals("bfd")) {
            bfdTrigger = false;
            return;
        }
        if (a.equals("csnp-time")) {
            csnpTimer = 0;
            return;
        }
        if (a.equals("raw-encapsulation")) {
            rawEncap = false;
            unregister2eth();
            register2eth();
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("unsuppress-prefix")) {
            unsuppressAddr = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("suppress-address")) {
            suppressInt = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("other-enable")) {
            otherEna = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("other-suppress-prefix")) {
            othSuppAddr = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("other-unsuppress-prefix")) {
            othUnsuppAddr = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("other-suppress-address")) {
            othSuppInt = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("verify-source")) {
            connectedCheck = false;
            return;
        }
        if (a.equals("password")) {
            authentication = null;
            return;
        }
        if (a.equals("authen-id")) {
            authenKey = 0;
            return;
        }
        if (a.equals("authen-type")) {
            authenMode = 1;
            return;
        }
        if (a.equals("traffeng")) {
            a = cmd.word();
            if (a.equals("suppress")) {
                teSuppress = false;
                lower.genLsps(1);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("segrout")) {
            a = cmd.word();
            if (a.equals("index")) {
                srIndex = 0;
                lower.genLsps(3);
                return;
            }
            if (a.equals("other-index")) {
                srOthIdx = 0;
                lower.genLsps(3);
                return;
            }
            if (a.equals("node")) {
                srNode = false;
                lower.genLsps(3);
                return;
            }
            if (a.equals("pop")) {
                srPop = false;
                lower.genLsps(3);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("bier")) {
            a = cmd.word();
            if (a.equals("index")) {
                brIndex = 0;
                lower.genLsps(3);
                return;
            }
            if (a.equals("subdomain")) {
                brSub = 0;
                lower.genLsps(3);
                return;
            }
            if (a.equals("other-index")) {
                brOthIdx = 0;
                lower.genLsps(3);
                return;
            }
            if (a.equals("other-subdomain")) {
                brOthSub = 0;
                lower.genLsps(3);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ipinfo")) {
            ipInfoCfg = secInfoUtl.doCfgStr(ipInfoCfg, cmd, true);
            return;
        }
        if (a.equals("ldp-sync")) {
            ldpSync = false;
            lower.genLsps(3);
            return;
        }
        if (a.equals("dynamic-metric")) {
            a = cmd.word();
            if (a.equals("mode")) {
                dynamicMetric = 0;
                lower.genLsps(3);
                return;
            }
            cmd.badCmd();
            return;
        }
        cmd.badCmd();
    }

    /**
     * get help text
     *
     * @param l list to update
     */
    public static void routerGetHelp(userHelp l) {
        l.add(null, false, 4, new int[]{-1}, "enable", "enable protocol processing");
        l.add(null, false, 4, new int[]{-1}, "other-enable", "enable other protocol processing");
        l.add(null, false, 4, new int[]{5}, "circuit", "set circuit type");
        l.add(null, false, 5, new int[]{-1}, "level1", "level-1 circuit");
        l.add(null, false, 5, new int[]{-1}, "level2", "level-2 circuit");
        l.add(null, false, 5, new int[]{-1}, "both", "level-1 and level-2 circuit");
        l.add(null, false, 4, new int[]{5}, "network", "set network type");
        l.add(null, false, 5, new int[]{-1}, "point2point", "point to point");
        l.add(null, false, 5, new int[]{-1}, "broadcast", "broadcast");
        l.add(null, false, 4, new int[]{-1}, "passive", "do not process packets");
        l.add(null, false, 4, new int[]{-1}, "bfd", "enable bfd triggered down");
        l.add(null, false, 4, new int[]{-1}, "raw-encapsulation", "use non-llc encapsulation");
        l.add(null, false, 4, new int[]{-1}, "suppress-prefix", "do not advertise interface");
        l.add(null, false, 4, new int[]{-1}, "unsuppress-prefix", "do advertise interface");
        l.add(null, false, 4, new int[]{-1}, "suppress-address", "do not advertise interface");
        l.add(null, false, 4, new int[]{5}, "csnp-time", "time between csnp");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "metric", "interface metric");
        l.add(null, false, 5, new int[]{-1}, "<num>", "metric");
        l.add(null, false, 4, new int[]{-1}, "other-suppress-prefix", "do not advertise other interface");
        l.add(null, false, 4, new int[]{-1}, "other-unsuppress-prefix", "do advertise other interface");
        l.add(null, false, 4, new int[]{-1}, "other-suppress-address", "do not advertise other interface");
        l.add(null, false, 4, new int[]{-1}, "verify-source", "check source address of updates");
        l.add(null, false, 4, new int[]{5}, "priority", "router priority");
        l.add(null, false, 5, new int[]{-1}, "<num>", "priority 0=disable");
        l.add(null, false, 4, new int[]{5}, "hello-time", "time between hellos");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "dead-time", "time before neighbor down");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "retransmit-time", "time before retarnsmitting");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "password", "password for authentication");
        l.add(null, false, 5, new int[]{-1}, "<text>", "set password");
        l.add(null, false, 4, new int[]{5}, "authen-type", "mode for authentication");
        l.add(null, false, 5, new int[]{-1}, "null", "use nothing");
        l.add(null, false, 5, new int[]{-1}, "clear", "use cleartext");
        l.add(null, false, 5, new int[]{-1}, "md5", "use md5");
        l.add(null, false, 5, new int[]{-1}, "sha1", "use sha1");
        l.add(null, false, 5, new int[]{-1}, "sha224", "use sha224");
        l.add(null, false, 5, new int[]{-1}, "sha256", "use sha256");
        l.add(null, false, 5, new int[]{-1}, "sha384", "use sha384");
        l.add(null, false, 5, new int[]{-1}, "sha512", "use sha512");
        l.add(null, false, 4, new int[]{5}, "authen-id", "id for authentication");
        l.add(null, false, 5, new int[]{-1}, "<num>", "key id");
        l.add(null, false, 4, new int[]{5}, "traffeng", "traffic engineering parameters");
        l.add(null, false, 5, new int[]{-1}, "suppress", "do not advertise interface");
        l.add(null, false, 5, new int[]{6}, "metric", "set metric");
        l.add(null, false, 6, new int[]{-1}, "<num>", "cost");
        l.add(null, false, 5, new int[]{6}, "bandwidth", "set bandwidth");
        l.add(null, false, 6, new int[]{-1}, "<num>", "bandwidth");
        l.add(null, false, 5, new int[]{6}, "affinity", "set affinity");
        l.add(null, false, 6, new int[]{-1}, "<num>", "affinity");
        l.add(null, false, 5, new int[]{6}, "srlg", "set srlg");
        l.add(null, false, 6, new int[]{-1}, "<num>", "affinity");
        l.add(null, false, 4, new int[]{5}, "segrout", "segment routing parameters");
        l.add(null, false, 5, new int[]{6}, "index", "set index");
        l.add(null, false, 6, new int[]{-1}, "<num>", "index");
        l.add(null, false, 5, new int[]{6}, "other-index", "set other index");
        l.add(null, false, 6, new int[]{-1}, "<num>", "index");
        l.add(null, false, 5, new int[]{-1}, "node", "set node flag");
        l.add(null, false, 5, new int[]{-1}, "pop", "request php");
        l.add(null, false, 4, new int[]{5}, "bier", "bier parameters");
        l.add(null, false, 5, new int[]{6}, "index", "set index");
        l.add(null, false, 6, new int[]{-1}, "<num>", "index");
        l.add(null, false, 5, new int[]{6}, "subdomain", "set subdomain");
        l.add(null, false, 6, new int[]{-1}, "<num>", "index");
        l.add(null, false, 5, new int[]{6}, "other-index", "set other index");
        l.add(null, false, 6, new int[]{-1}, "<num>", "index");
        l.add(null, false, 5, new int[]{6}, "other-subdomain", "set other subdomain");
        l.add(null, false, 6, new int[]{-1}, "<num>", "index");
        secInfoUtl.getHelp(l, 4, "ipinfo", "check peers");
        l.add(null, false, 4, new int[]{-1}, "ldp-sync", "synchronize metric to ldp");
        l.add(null, false, 4, new int[]{5}, "dynamic-metric", "dynamic peer metric");
        l.add(null, false, 5, new int[]{6}, "mode", "dynamic peer metric");
        l.add(null, false, 6, new int[]{-1}, "disabled", "forbid echo requests");
        l.add(null, false, 6, new int[]{-1}, "icmpecho", "icmp echo requests");
        l.add(null, false, 6, new int[]{-1}, "udpecho", "udp echo requests");
        l.add(null, false, 6, new int[]{-1}, "twamp", "twamp echo requests");
        tabAverage.getHelp(l);
    }

    /**
     * close all neighbors
     */
    protected void closeNeighbors() {
        for (int i = neighs.size(); i >= 0; i--) {
            rtrIsisNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopNow();
        }
        lower.genLsps(3);
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (shutdown) {
            return;
        }
        keepTimer = new Timer();
        rtrIsisIfaceHello task = new rtrIsisIfaceHello(this);
        keepTimer.schedule(task, 500, helloTimer);
    }

    /**
     * check if i've to peer with
     *
     * @param lev level to check
     * @param peer pere to check
     * @return true if yes, false if not
     */
    protected boolean shouldIanswer(int lev, addrIsis peer) {
        if (netPnt2pnt) {
            return true;
        }
        if (amIdis(lev)) {
            return true;
        }
        return peer.compareTo(getDisAddr(lev)) == 0;
    }

    private rtrIsisNeigh findDIS(int lev) {
        if (netPnt2pnt) {
            return null;
        }
        int pri = disPriority;
        addrMac adr = hwaddr;
        rtrIsisNeigh dis = null;
        for (int i = 0; i < neighs.size(); i++) {
            rtrIsisNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.level.level != lev) {
                continue;
            }
            if (ntry.peerDisA.compareTo(ntry.rtrID) != 0) {
                continue;
            }
            if (ntry.rtrPri < pri) {
                continue;
            }
            if (ntry.rtrPri == pri) {
                if (adr.compareTo(ntry.ethAddr) > 0) {
                    continue;
                }
            }
            dis = ntry;
            pri = ntry.rtrPri;
            adr = ntry.ethAddr;
        }
        return dis;
    }

    /**
     * elect dis for this interface
     *
     * @param lev level
     * @param adr dis address
     * @param cir dis circuit id
     * @return elected circuit id
     */
    protected int electDIS(int lev, addrIsis adr, int cir) {
        addrIsis old = adr.copyBytes();
        rtrIsisNeigh nei = findDIS(lev);
        int circ;
        if (nei == null) {
            adr.fromBuf(lower.routerID.getBytes(), 0);
            circ = circuitID;
        } else {
            adr.fromBuf(nei.peerDisA.getBytes(), 0);
            circ = nei.peerDisI;
        }
        if ((adr.compareTo(old) == 0) && (cir == circ)) {
            return circ;
        }
        if (debugger.rtrIsisEvnt) {
            logger.debug("l" + lev + " dis change, dis=" + adr);
        }
        lower.genLsps(3);
        return circ;
    }

    /**
     * get authentication data
     *
     * @param pck packet
     * @param typ message type
     * @param ofs offset of tlv
     * @return bytes in header
     */
    protected byte[] getAuthData(packHolder pck, int typ, int ofs) {
        return lower.calcAuthData(pck, typ, ofs, authenMode, authenKey, authentication);
    }

    /**
     * check if i'm dis on the net
     *
     * @param lev level to check
     * @return true means yes, false means no
     */
    protected boolean amIdis(int lev) {
        if (netPnt2pnt) {
            return false;
        }
        addrIsis adr = getDisAddr(lev);
        return lower.routerID.compareTo(adr) == 0;
    }

    /**
     * get dis address
     *
     * @param lev level to get
     * @return address
     */
    protected addrIsis getDisAddr(int lev) {
        if (lev == 1) {
            return lev1disA;
        } else {
            return lev2disA;
        }
    }

    /**
     * get dis circuit
     *
     * @param lev level to get
     * @return address
     */
    protected int getDisCirc(int lev) {
        if (lev == 1) {
            return lev1disI;
        } else {
            return lev2disI;
        }
    }

    /**
     * register to ethernet
     */
    public void register2eth() {
        try {
            hwaddr = (addrMac) ethtyp.getHwAddr().copyBytes();
        } catch (Exception e) {
        }
        if (rawEncap) {
            ethtyp.addET(rtrIsis.ethTyp, "isis", this);
            ethtyp.updateET(rtrIsis.ethTyp, this);
        } else {
            ethtyp.addLLC(rtrIsis.llcTyp, "isis", this);
            ethtyp.updateLLC(rtrIsis.llcTyp, this);
        }
    }

    /**
     * unregister from ethernet
     */
    public void unregister2eth() {
        ethtyp.delLLC(rtrIsis.llcTyp);
        ethtyp.delET(rtrIsis.ethTyp);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        upper = parent;
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        if (stat == state.states.up) {
            return;
        }
        closeNeighbors();
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (passiveInt) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        int i;
        if (rawEncap) {
            i = rtrIsis.ethTyp;
        } else {
            i = rtrIsis.llcTyp;
        }
        if (i != pck.msbGetW(0)) { // ethtype
            logger.info("got bad ethertype from " + pck.ETHsrc);
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        i = pck.getByte(0); // protocol discriminator
        if (i == 0x82) { // esis hello
            return;
        }
        if (i != rtrIsis.protDist) {
            logger.info("got bad protocol from " + pck.ETHsrc);
            iface.cntr.drop(pck, counter.reasons.badID);
            return;
        }
        int typ = pck.getByte(4) & 0x1f; // type
        i = pck.getByte(1); // length
        if (pck.dataSize() < i) {
            logger.info("got too small from " + pck.ETHsrc);
            iface.cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (i != rtrIsisNeigh.msgTyp2headSiz(typ)) {
            logger.info("got bad header length from " + pck.ETHsrc);
            iface.cntr.drop(pck, counter.reasons.badSiz);
            return;
        }
        i = pck.getByte(2); // version
        if (i != 1) {
            logger.info("got bad version from " + pck.ETHsrc);
            iface.cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        i = pck.getByte(3); // system id length
        if ((i != 0) && (i != addrIsis.size)) {
            logger.info("got bad sysid length from " + pck.ETHsrc);
            iface.cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        i = pck.getByte(5); // version
        if (i != 1) {
            logger.info("got bad version from " + pck.ETHsrc);
            iface.cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        // i = pck.getByte(6); // reserved
        // i = pck.getByte(7); // max areas
        pck.getSkip(8);
        int lev = rtrIsisNeigh.msgTyp2level(typ);
        if (lev == 3) {
            lev = pck.getByte(0); // circuit type
        }
        msgStats[typ].rx(pck);
        boolean needRetrans;
        switch (lev & circuitLevel) {
            case 1:
                needRetrans = recvPack(pck, typ, lower.level1);
                break;
            case 2:
                needRetrans = recvPack(pck, typ, lower.level2);
                break;
            case 3:
                needRetrans = recvPack(pck.copyBytes(true, true), typ, lower.level1);
                needRetrans |= recvPack(pck.copyBytes(true, true), typ, lower.level2);
                break;
            default:
                needRetrans = false;
                cntr.drop(pck, counter.reasons.badTyp);
                break;
        }
        if (needRetrans) {
            doRetrans();
        }
    }

    private boolean recvPack(packHolder pck, int typ, rtrIsisLevel lev) {
        rtrIsisNeigh nei;
        if (netPnt2pnt) {
            nei = new rtrIsisNeigh(lower, lev, this, new addrMac());
        } else {
            nei = new rtrIsisNeigh(lower, lev, this, pck.ETHsrc);
        }
        rtrIsisNeigh old = neighs.add(nei);
        if (old != null) {
            nei = old;
        } else {
            nei.startNow();
        }
        int oldSt = nei.peerAdjState;
        nei.recvPack(pck, typ);
        return oldSt != nei.peerAdjState;
    }

    private void sendPack(packHolder pck, int typ) {
        if (debugger.rtrIsisTraf) {
            logger.debug("sending " + rtrIsisNeigh.msgTyp2string(typ) + " on " + upper);
        }
        pck.merge2beg();
        msgStats[typ].tx(pck);
        switch (rtrIsisNeigh.msgTyp2level(typ)) {
            case 1:
                pck.ETHtrg.fromString("0180:c200:0014");
                break;
            case 2:
                pck.ETHtrg.fromString("0180:c200:0015");
                break;
            case 3:
                pck.ETHtrg.fromString("0900:2b00:0005");
                break;
        }
        pck.ETHsrc.setAddr(hwaddr);
        pck.putByte(0, rtrIsis.protDist); // protocol discriminator
        pck.putByte(1, rtrIsisNeigh.msgTyp2headSiz(typ)); // header length
        pck.putByte(2, 1); // version
        pck.putByte(3, 0); // system id length
        pck.putByte(4, typ); // pdu type
        pck.putByte(5, 1); // version
        pck.putByte(6, 0); // reserved
        pck.putByte(7, lower.getMaxAreaAddr()); // max areas
        pck.putSkip(8);
        pck.merge2beg();
        if (rawEncap) {
            pck.msbPutW(0, rtrIsis.ethTyp);
        } else {
            pck.msbPutW(0, rtrIsis.llcTyp);
        }
        pck.putSkip(2);
        pck.merge2beg();
        upper.sendPack(pck);
    }

    /**
     * send psnp packet
     *
     * @param l list of lsps
     * @param lev level
     */
    protected void sendPsnpPack(tabGen<rtrIsisLsp> l, rtrIsisLevel lev) {
        packHolder pck = new packHolder(true, true);
        writeLspList(pck, l);
        pck.merge2beg();
        pck.msbPutW(0, getAuthLen(lev) + pck.dataSize() + rtrIsisNeigh.msgTyp2headSiz(rtrIsisNeigh.msgTypL1psnp)); // pdu length
        pck.putAddr(2, lower.routerID); // source id
        pck.putByte(8, 0); // circuit id
        pck.putSkip(9);
        int typ;
        if (lev.level == 1) {
            typ = rtrIsisNeigh.msgTypL1psnp;
        } else {
            typ = rtrIsisNeigh.msgTypL2psnp;
        }
        writeAuthen(pck, lev, typ);
        sendPack(pck, typ);
    }

    /**
     * send csnp packet
     *
     * @param a first
     * @param b last
     * @param l list of lsps
     * @param lev level
     */
    protected void sendCsnpPack(rtrIsisLsp a, rtrIsisLsp b, tabGen<rtrIsisLsp> l, rtrIsisLevel lev) {
        packHolder pck = new packHolder(true, true);
        writeLspList(pck, l);
        pck.merge2beg();
        pck.msbPutW(0, getAuthLen(lev) + pck.dataSize() + rtrIsisNeigh.msgTyp2headSiz(rtrIsisNeigh.msgTypL1csnp)); // pdu length
        pck.putAddr(2, lower.routerID); // source id
        pck.putByte(8, 0); // circuit id
        a.writeId(pck, 9); // start lsp id
        b.writeId(pck, 17); // stop lsp id
        pck.putSkip(25);
        int typ;
        if (lev.level == 1) {
            typ = rtrIsisNeigh.msgTypL1csnp;
        } else {
            typ = rtrIsisNeigh.msgTypL2csnp;
        }
        writeAuthen(pck, lev, typ);
        sendPack(pck, typ);
    }

    private int getAuthLen(rtrIsisLevel lev) {
        byte[] buf = lev.getAuthen(new packHolder(true, true), 0, 0);
        if (buf == null) {
            return 0;
        }
        return buf.length + 2;
    }

    private void writeAuthen(packHolder pck, rtrIsisLevel lev, int typ) {
        byte[] buf = lev.getAuthen(new packHolder(true, true), 0, 0);
        if (buf == null) {
            return;
        }
        encTlv tlv = rtrIsis.getTlv();
        int siz = pck.headSize();
        tlv.putBytes(pck, rtrIsisLsp.tlvAuthen, buf);
        buf = lev.getAuthen(pck, typ, siz);
        int len = pck.headSize() - siz;
        pck.putSkip(-len);
        tlv.putBytes(pck, rtrIsisLsp.tlvAuthen, buf);
    }

    private void writeLspList(packHolder pck, tabGen<rtrIsisLsp> l) {
        packHolder p = new packHolder(true, true);
        encTlv tlv = rtrIsis.getTlv();
        for (int i = 0; i < l.size(); i++) {
            rtrIsisLsp lsp = l.get(i);
            if (debugger.rtrIsisTraf) {
                logger.debug("lsp " + lsp);
            }
            p.putSkip(lsp.writeSeq(p, 0));
            if (p.headSize() < 224) {
                continue;
            }
            p.merge2beg();
            tlv.putBytes(pck, rtrIsisLsp.tlvLspEntries, p.getCopy());
            p.clear();
        }
        if (p.headSize() < 1) {
            return;
        }
        p.merge2beg();
        tlv.putBytes(pck, rtrIsisLsp.tlvLspEntries, p.getCopy());
    }

    /**
     * send lsp packet
     *
     * @param lsp lsp to send
     * @param lev level
     */
    protected void sendLspPack(rtrIsisLsp lsp, rtrIsisLevel lev) {
        packHolder pck = new packHolder(true, true);
        pck.putSkip(lsp.writeData(pck, 0));
        pck.merge2beg();
        if (lev.level == 1) {
            sendPack(pck, rtrIsisNeigh.msgTypL1lsp);
        } else {
            sendPack(pck, rtrIsisNeigh.msgTypL2lsp);
        }
    }

    /**
     * send csnp packet
     *
     * @param lsp lsp to send
     * @param lev level
     */
    protected void sendCsnpPack(rtrIsisLsp lsp, rtrIsisLevel lev) {
        tabGen<rtrIsisLsp> l = new tabGen<rtrIsisLsp>();
        l.add(lsp);
        sendCsnpPack(lsp, lsp, l, lev);
    }

    /**
     * send psnp packet
     *
     * @param lsp lsp to send
     * @param lev level
     */
    protected void sendPsnpPack(rtrIsisLsp lsp, rtrIsisLevel lev) {
        tabGen<rtrIsisLsp> l = new tabGen<rtrIsisLsp>();
        l.add(lsp);
        sendPsnpPack(l, lev);
    }

    /**
     * send point2point hello
     */
    protected void sendHelloP2p() {
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, circuitLevel); // circuit type
        pck.putAddr(1, lower.routerID); // system id
        pck.msbPutW(7, deadTimer / 1000);
        pck.msbPutW(9, 0); // pdu length
        pck.putByte(11, circuitID); // local circuit id
        pck.putSkip(12);
        encTlv tlv = rtrIsis.getTlv();
        int i = rtrIsisNeigh.statDown;
        int crc = 0;
        addrIsis adr = new addrIsis();
        for (int o = 0; o < neighs.size(); o++) {
            rtrIsisNeigh ntry = neighs.get(o);
            if (ntry == null) {
                continue;
            }
            int p = ntry.getMyHandshake();
            if (p > i) {
                continue;
            }
            i = p;
            adr = ntry.rtrID;
            crc = ntry.peerExtCirc;
        }
        tlv.valDat[0] = (byte) i; // state
        bits.msbPutD(tlv.valDat, 1, circuitID); // circuit id
        adr.toBuffer(tlv.valDat, 5); // neighbor system id
        bits.msbPutD(tlv.valDat, 11, crc); // neighbor circuit id
        tlv.putBytes(pck, rtrIsisLsp.tlvHandshake, 15, tlv.valDat);
        writeHelloTlvs(pck, rtrIsisNeigh.msgTypP2Phello);
        i = pck.headSize();
        pck.msbPutW(9 - i, i + 8); // pdu length
        sendPack(pck, rtrIsisNeigh.msgTypP2Phello);
    }

    /**
     * send lan hello
     *
     * @param lev level to use
     */
    protected void sendHelloLan(int lev) {
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, circuitLevel); // circuit type
        pck.putAddr(1, lower.routerID); // system id
        pck.msbPutW(7, deadTimer / 1000);
        pck.msbPutW(9, 0); // pdu length
        pck.putByte(11, disPriority & 0x7f); // priority
        pck.putAddr(12, getDisAddr(lev)); // dis address
        pck.putByte(18, getDisCirc(lev)); // dis circuit
        pck.putSkip(19);
        encTlv tlv = rtrIsis.getTlv();
        int i = 0;
        for (int o = 0; o < neighs.size(); o++) {
            rtrIsisNeigh ntry = neighs.get(o);
            if (ntry == null) {
                continue;
            }
            if (ntry.level.level != lev) {
                continue;
            }
            ntry.ethAddr.toBuffer(tlv.valDat, i);
            i += addrMac.size;
        }
        tlv.putBytes(pck, rtrIsisLsp.tlvLanNeigh, i, tlv.valDat);
        writeHelloTlvs(pck, rtrIsisNeigh.msgTypL1hello);
        i = pck.headSize();
        pck.msbPutW(9 - i, i + 8); // pdu length
        if (lev == 1) {
            sendPack(pck, rtrIsisNeigh.msgTypL1hello);
        } else {
            sendPack(pck, rtrIsisNeigh.msgTypL2hello);
        }
    }

    private void writeHelloTlvs(packHolder pck, int typ) {
        encTlv tlv = rtrIsis.getTlv();
        tlv.putBytes(pck, rtrIsisLsp.tlvProtSupp, lower.getNLPIDlst(otherEna));
        if (lower.multiTopo || lower.other.multiTopo) {
            tlv.putBytes(pck, rtrIsisLsp.tlvMultiTopo, lower.getMTopoLst(otherEna, 0));
        }
        tlv.putBytes(pck, rtrIsisLsp.tlvAreaAddr, lower.areaID.getAddrDat(true));
        lower.putAddrIface(false, iface.addr).putThis(pck);
        if (otherEna) {
            lower.putAddrIface(true, oface.addr).putThis(pck);
        }
        byte[] buf = getAuthData(pck, typ, 0);
        if (buf == null) {
            return;
        }
        int ofs = pck.headSize();
        tlv.putBytes(pck, rtrIsisLsp.tlvAuthen, buf);
        int i = pck.headSize();
        pck.msbPutW(9 - i, i + 8); // pdu length
        buf = getAuthData(pck, typ, ofs);
        pck.putSkip(ofs - i);
        tlv.putBytes(pck, rtrIsisLsp.tlvAuthen, buf);
    }

    private int sendLevCsnp(rtrIsisLevel lev, int frstSeq) {
        if (csnpTimer < 1) {
            if (!amIdis(lev.level)) {
                return frstSeq;
            }
        } else {
            long tim = bits.getTime();
            if ((tim - lastCsnp) < csnpTimer) {
                return frstSeq;
            }
            lastCsnp = tim;
        }
        if (frstSeq >= lev.lsps.size()) {
            frstSeq = 0;
        }
        int lastSeq = 1 + frstSeq + (lev.maxLspSize / 24);
        tabGen<rtrIsisLsp> l = new tabGen<rtrIsisLsp>();
        for (int i = frstSeq; i <= lastSeq; i++) {
            rtrIsisLsp ntry = lev.lsps.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry);
        }
        rtrIsisLsp frstID = new rtrIsisLsp();
        rtrIsisLsp lastID = new rtrIsisLsp();
        if (frstSeq == 0) {
            frstID.setIDvalue(true);
        } else {
            frstID = l.get(0);
        }
        if (lastSeq >= lev.lsps.size()) {
            lastID.setIDvalue(false);
        } else {
            lastID = l.get(l.size() - 1);
        }
        sendCsnpPack(frstID, lastID, l, lev);
        return lastSeq;
    }

    /**
     * do timer work
     */
    protected void doRetrans() {
        if (passiveInt) {
            return;
        }
        if (netPnt2pnt) {
            sendHelloP2p();
            if ((circuitLevel & 1) != 0) {
                lev1csnp = sendLevCsnp(lower.level1, lev1csnp);
            }
            if ((circuitLevel & 2) != 0) {
                lev2csnp = sendLevCsnp(lower.level2, lev2csnp);
            }
            return;
        }
        if ((circuitLevel & 1) != 0) {
            lev1disI = electDIS(1, lev1disA, lev1disI);
            sendHelloLan(1);
            lev1csnp = sendLevCsnp(lower.level1, lev1csnp);
        }
        if ((circuitLevel & 2) != 0) {
            lev2disI = electDIS(2, lev2disA, lev2disI);
            sendHelloLan(2);
            lev2csnp = sendLevCsnp(lower.level2, lev2csnp);
        }
    }

}

class rtrIsisIfaceHello extends TimerTask {

    private final rtrIsisIface lower;

    public rtrIsisIfaceHello(rtrIsisIface parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.doRetrans();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
