package rtr;

import addr.addrIsis;
import addr.addrMac;
import auth.authLocal;
import ifc.ifcDn;
import ifc.ifcEthTyp;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwdIface;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import tab.tabGen;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

/**
 * isis interface
 *
 * @author matecsaba
 */
public class rtrIsisIface implements Comparator<rtrIsisIface>, ifcUp {

    /**
     * the ip interface this works on
     */
    protected final ipFwdIface iface;

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
     * suppress interface address
     */
    public boolean suppressAddr;

    /**
     * suppress interface address
     */
    public boolean suppressInt;

    /**
     * passive interface
     */
    public boolean passiveInt;

    /**
     * hello timer
     */
    public int helloTimer;

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
     * segment rou node
     */
    public boolean srNode;

    /**
     * bier index
     */
    public int brIndex;

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

    /**
     * create one instance
     *
     * @param parent the isis protocol
     * @param ifc the ip interface to work on
     * @param eth the eth interface to work on
     */
    public rtrIsisIface(rtrIsis parent, ipFwdIface ifc, ifcEthTyp eth) {
        lower = parent;
        iface = ifc;
        ethtyp = eth;
        netPnt2pnt = true;
        circuitID = lower.getCircuitId(netPnt2pnt);
        circuitLevel = lower.operateLevel;
        helloTimer = 10000;
        deadTimer = 30000;
        retransTimer = 3000;
        disPriority = 64;
        metric = 10;
        suppressInt = true;
        neighs = new tabGen<rtrIsisNeigh>();
        hwaddr = new addrMac();
        lev1disA = new addrIsis();
        lev2disA = new addrIsis();
        teMetric = 10;
        if (iface != null) {
            teBandwidth = iface.lower.getBandwidth();
        }
    }

    public int compare(rtrIsisIface o1, rtrIsisIface o2) {
        if (o1.iface.ifwNum < o2.iface.ifwNum) {
            return -1;
        }
        if (o1.iface.ifwNum > o2.iface.ifwNum) {
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
     */
    public void routerGetConfig(List<String> l, String beg) {
        l.add(cmds.tabulator + beg + "enable");
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
        cmds.cfgLine(l, authentication == null, cmds.tabulator, beg + "password", authLocal.passwdEncode(authentication));
        l.add(cmds.tabulator + beg + "metric " + metric);
        l.add(cmds.tabulator + beg + "priority " + disPriority);
        l.add(cmds.tabulator + beg + "hello-time " + helloTimer);
        l.add(cmds.tabulator + beg + "dead-time " + deadTimer);
        l.add(cmds.tabulator + beg + "retransmit-time " + retransTimer);
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
            cmds.cfgLine(l, !srNode, cmds.tabulator, s + "node", "");
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
        }
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
        if (a.equals("suppress-prefix")) {
            suppressAddr = true;
            lower.genLsps(1);
            return;
        }
        if (a.equals("suppress-address")) {
            suppressInt = true;
            lower.genLsps(1);
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
        if (a.contains("retransmit-time")) {
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
        if (a.equals("password")) {
            authentication = authLocal.passwdDecode(cmd.word());
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
                return;
            }
            if (a.equals("bandwidth")) {
                teBandwidth = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("affinity")) {
                teAffinity = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("srlg")) {
                teSrlg = bits.str2num(cmd.word());
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
            if (a.equals("node")) {
                srNode = true;
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
        if (a.equals("passive")) {
            passiveInt = false;
            return;
        }
        if (a.equals("bfd")) {
            bfdTrigger = false;
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
        if (a.equals("suppress-address")) {
            suppressInt = false;
            lower.genLsps(1);
            return;
        }
        if (a.equals("password")) {
            authentication = null;
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
            if (a.equals("node")) {
                srNode = false;
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
    public static void routerGetHelp(userHelping l) {
        l.add("4 .         enable                  enable protocol processing");
        l.add("4 5         circuit                 set circuit type");
        l.add("5 .           level1                level-1 circuit");
        l.add("5 .           level2                level-2 circuit");
        l.add("5 .           both                  level-1 and level-2 circuit");
        l.add("4 5         network                 set network type");
        l.add("5 .           point2point           point to point");
        l.add("5 .           broadcast             broadcast");
        l.add("4 .         passive                 do not process packets");
        l.add("4 .         bfd                     enable bfd triggered down");
        l.add("4 .         raw-encapsulation       use non-llc encapsulation");
        l.add("4 .         suppress-prefix         do not advertise interface");
        l.add("4 .         suppress-address        do not advertise interface");
        l.add("4 5         metric                  interface metric");
        l.add("5 .           <num>                 metric");
        l.add("4 5         priority                router priority");
        l.add("5 .           <num>                 priority 0=disable");
        l.add("4 5         hello-time              time between hellos");
        l.add("5 .           <num>                 time in ms");
        l.add("4 5         dead-time               time before neighbor down");
        l.add("5 .           <num>                 time in ms");
        l.add("4 5         retransmit-time         time before retarnsmitting");
        l.add("5 .           <num>                 time in ms");
        l.add("4 5         password                password for authentication");
        l.add("5 .           <text>                set password");
        l.add("4 5         traffeng                traffic engineering parameters");
        l.add("5 .           suppress              do not advertise interface");
        l.add("5 6           metric                set metric");
        l.add("6 .             <num>               cost");
        l.add("5 6           bandwidth             set bandwidth");
        l.add("6 .             <num>               bandwidth");
        l.add("5 6           affinity              set affinity");
        l.add("6 .             <num>               affinity");
        l.add("5 6           srlg                  set srlg");
        l.add("6 .             <num>               affinity");
        l.add("4 5         segrout                 segment routing parameters");
        l.add("5 6           index                 set index");
        l.add("6 .             <num>               index");
        l.add("5 .           node                  set node flag");
        l.add("4 5         bier                    bier parameters");
        l.add("5 6           index                 set index");
        l.add("6 .             <num>               index");
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
        return peer.compare(peer, getDisAddr(lev)) == 0;
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
            if (ntry.rtrID.compare(ntry.peerDisA, ntry.rtrID) != 0) {
                continue;
            }
            if (ntry.rtrPri < pri) {
                continue;
            }
            if (ntry.rtrPri == pri) {
                if (adr.compare(adr, ntry.ethAddr) > 0) {
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
     * @param adr my address
     * @return elected circuit id
     */
    protected int electDIS(int lev, addrIsis adr) {
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
        if (adr.compare(adr, old) == 0) {
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
     * @return bytes in header
     */
    protected byte[] getAuthData() {
        if (authentication == null) {
            return null;
        }
        byte[] b1 = authentication.getBytes();
        byte[] b2 = new byte[b1.length + 1];
        bits.byteCopy(b1, 0, b2, 1, b1.length);
        b2[0] = 1;
        return b2;
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
        return adr.compare(lower.routerID, adr) == 0;
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
        if (i != 0) {
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
    protected void sendPsnpPack(tabGen<rtrIsisLsp> l, int lev) {
        packHolder pck = new packHolder(true, true);
        writeLspList(pck, l);
        pck.merge2beg();
        pck.msbPutW(0, pck.dataSize() + rtrIsisNeigh.msgTyp2headSiz(rtrIsisNeigh.msgTypL1psnp)); // pdu length
        pck.putAddr(2, lower.routerID); // source id
        pck.putByte(8, 0); // circuit id
        pck.putSkip(9);
        if (lev == 1) {
            sendPack(pck, rtrIsisNeigh.msgTypL1psnp);
        } else {
            sendPack(pck, rtrIsisNeigh.msgTypL2psnp);
        }
    }

    /**
     * send csnp packet
     *
     * @param a first
     * @param b last
     * @param l list of lsps
     * @param lev level
     */
    protected void sendCsnpPack(rtrIsisLsp a, rtrIsisLsp b, tabGen<rtrIsisLsp> l, int lev) {
        packHolder pck = new packHolder(true, true);
        writeLspList(pck, l);
        pck.merge2beg();
        pck.msbPutW(0, pck.dataSize() + rtrIsisNeigh.msgTyp2headSiz(rtrIsisNeigh.msgTypL1csnp)); // pdu length
        pck.putAddr(2, lower.routerID); // source id
        pck.putByte(8, 0); // circuit id
        a.writeId(pck, 9); // start lsp id
        b.writeId(pck, 17); // stop lsp id
        pck.putSkip(25);
        if (lev == 1) {
            sendPack(pck, rtrIsisNeigh.msgTypL1csnp);
        } else {
            sendPack(pck, rtrIsisNeigh.msgTypL2csnp);
        }
    }

    private void writeLspList(packHolder pck, tabGen<rtrIsisLsp> l) {
        packHolder p = new packHolder(true, true);
        typLenVal tlv = rtrIsis.getTlv();
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
    protected void sendLspPack(rtrIsisLsp lsp, int lev) {
        packHolder pck = new packHolder(true, true);
        pck.putSkip(lsp.writeData(pck, 0));
        pck.merge2beg();
        if (lev == 1) {
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
    protected void sendCsnpPack(rtrIsisLsp lsp, int lev) {
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
    protected void sendPsnpPack(rtrIsisLsp lsp, int lev) {
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
        typLenVal tlv = rtrIsis.getTlv();
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
        writeHelloTlvs(pck);
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
        typLenVal tlv = rtrIsis.getTlv();
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
        writeHelloTlvs(pck);
        i = pck.headSize();
        pck.msbPutW(9 - i, i + 8); // pdu length
        if (lev == 1) {
            sendPack(pck, rtrIsisNeigh.msgTypL1hello);
        } else {
            sendPack(pck, rtrIsisNeigh.msgTypL2hello);
        }
    }

    private void writeHelloTlvs(packHolder pck) {
        typLenVal tlv = rtrIsis.getTlv();
        tlv.putBytes(pck, rtrIsisLsp.tlvProtSupp, lower.getNLPIDlst());
        if (lower.multiTopo) {
            tlv.putBytes(pck, rtrIsisLsp.tlvMultiTopo, lower.getMTopoLst());
        }
        tlv.putBytes(pck, rtrIsisLsp.tlvAreaAddr, lower.areaID.getAddrDat(true));
        lower.putAddrIface(iface.addr).putThis(pck);
        if (authentication == null) {
            return;
        }
        tlv.putBytes(pck, rtrIsisLsp.tlvAuthen, getAuthData());
    }

    private int sendLevCsnp(rtrIsisLevel lev, int frstSeq) {
        if (!amIdis(lev.level)) {
            return frstSeq;
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
        sendCsnpPack(frstID, lastID, l, lev.level);
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
            return;
        }
        if ((circuitLevel & 1) != 0) {
            lev1disI = electDIS(1, lev1disA);
            sendHelloLan(1);
            lev1csnp = sendLevCsnp(lower.level1, lev1csnp);
        }
        if ((circuitLevel & 2) != 0) {
            lev2disI = electDIS(2, lev2disA);
            sendHelloLan(2);
            lev2csnp = sendLevCsnp(lower.level2, lev2csnp);
        }
    }

}

class rtrIsisIfaceHello extends TimerTask {

    private rtrIsisIface lower;

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
