package ifc;

import addr.addrBridge;
import addr.addrMac;
import addr.addrType;
import ip.ipCor;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipIfc4;
import ip.ipIfc6;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packStp;
import tab.tabGen;
import tab.tabRtrmapN;
import tab.tabSession;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * bridges interfaces to one broadcast domain
 *
 * @author matecsaba
 */
public class ifcBridge implements ifcDn {

    /**
     * ethernet type of serial lines
     */
    public final static int serialType = 0x6558;

    /**
     * size of serial header
     */
    public final static int serialSize = 2;

    /**
     * description of this bridge
     */
    public String description = "";

    /**
     * route distinguisher
     */
    public long rd;

    /**
     * route target import
     */
    public long rtImp;

    /**
     * route target export
     */
    public long rtExp;

    /**
     * hardware address of the bridge
     */
    public addrMac hwaddr;

    /**
     * mac address aging time
     */
    public int macAgeTime = 10 * 60 * 1000;

    /**
     * log mac movements
     */
    public boolean macMove;

    /**
     * block unicast with unknown destination
     */
    public boolean blockUnicast;

    /**
     * disable peer communication
     */
    public boolean privateBridge;

    /**
     * pad to minimum
     */
    public boolean padupSmall;

    /**
     * hw address is static
     */
    public boolean staticAddr;

    /**
     * spantree is running
     */
    public boolean needStp;

    /**
     * spantree priority
     */
    public int stpPrio = 32768;

    /**
     * max age
     */
    public int stpAge = 20000;

    /**
     * forward time
     */
    public int stpFwd = 15000;

    /**
     * hello time
     */
    public int stpHlo = 2000;

    /**
     * mac router to notify
     */
    public ifcBridgeRtr macRouter = null;

    /**
     * inspection
     */
    public tabSession inspect;

    private tabGen<ifcBridgeIfc> ifaces;

    private tabGen<ifcBridgeMacAddr> learned;

    private ifcUp upper = new ifcNull();

    private int nextIfaceNum;

    private counter cntr = new counter();

    private Timer timer;

    private long currTim = bits.getTime();

    private int stpPort;

    private boolean upProm;

    private ifcBridgeIfc upNtry;

    private ipCor core4 = new ipCor4();

    private ipCor core6 = new ipCor6();

    /**
     * root id
     */
    protected addrBridge stpRoot;

    /**
     * root cost
     */
    protected int stpCost;

    public counter getCounter() {
        return cntr;
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
        upProm = promisc;
    }

    public addrType getHwAddr() {
        return hwaddr;
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void closeDn() {
        upper = null;
    }

    public void flapped() {
    }

    /**
     * create new instance
     */
    public ifcBridge() {
        hwaddr = addrMac.getRandom();
        if (debugger.ifcBridgeTraf) {
            logger.debug("started, addr=" + hwaddr);
        }
        nextIfaceNum = 1;
        ifaces = new tabGen<ifcBridgeIfc>();
        upNtry = new ifcBridgeIfc(null, true, false, false);
        upNtry.ifcNum = 0;
    }

    /**
     * get show output
     *
     * @return list of interface
     */
    public userFormat getShowIfc() {
        userFormat lst = new userFormat("|", "interface|forward|physical|tx|rx|drop");
        lst.add("" + upNtry);
        for (int i = 0; i < ifaces.size(); i++) {
            lst.add("" + ifaces.get(i));
        }
        return lst;
    }

    /**
     * get show output
     *
     * @return list of interface
     */
    public userFormat getShowAdr() {
        if (learned == null) {
            return null;
        }
        userFormat lst = new userFormat("|", "address|interface|time|tx|rx|drop");
        for (int i = 0; i < learned.size(); i++) {
            lst.add("" + learned.get(i));
        }
        return lst;
    }

    /**
     * get show output
     *
     * @return list of interface
     */
    public userFormat getShowInsp() {
        if (inspect == null) {
            return null;
        }
        return inspect.doShowInsp();
    }

    /**
     * get upper mac list
     *
     * @return list of macs
     */
    public List<addrMac> getMacList() {
        List<addrMac> lst = new ArrayList<addrMac>();
        if (learned == null) {
            return lst;
        }
        for (int i = 0; i < learned.size(); i++) {
            ifcBridgeMacAddr ntry = learned.get(i);
            if (ntry.ifc.lowerIf != null) {
                continue;
            }
            lst.add(ntry.adr);
        }
        return lst;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add("1 2,.   description                 description of this vrf");
        l.add("2 2,.     [text]                    text describing this vrf");
        l.add("1 2     rd                          specify route distinguisher");
        l.add("2 .       <rd>                      rd in ASnum:IDnum format");
        l.add("1 2     rt-import                   specify route target import");
        l.add("2 .       <rt>                      rt in ASnum:IDnum format");
        l.add("1 2     rt-export                   specify route target export");
        l.add("2 .       <rt>                      rt in ASnum:IDnum format");
        l.add("1 2     rt-both                     specify route target");
        l.add("2 .       <rt>                      rt in ASnum:IDnum format");
        l.add("1 .     mac-learn                   enable mac address learning");
        l.add("1 2,.   inspect                     enable session inspection");
        l.add("2 2,.     mac                       enable mac logging");
        l.add("2 2,.     before                    log on session start");
        l.add("2 2,.     after                     log on session stop");
        l.add("1 .     mac-move                    enable mac move logging");
        l.add("1 .     private-bridge              disable peer communication");
        l.add("1 .     block-unicast               block unknown destination unicast");
        l.add("1 .     padup-small                 pad up small packets");
        l.add("1 2     mac-age                     set mac aging time");
        l.add("2 .       <num>                     time in ms");
        l.add("1 2     stp-mode                    set spantree mode");
        l.add("2 .       ieee                      ieee");
        l.add("2 .       none                      nothing");
        l.add("1 2     stp-time                    set spantree timers");
        l.add("2 3       <num>                     hello in ms");
        l.add("3 4         <num>                   maxage in ms");
        l.add("4 .           <num>                 forward delay in ms");
        l.add("1 2     stp-priority                set spantree priority");
        l.add("2 .       <num>                     priority in 1024 increments");
        l.add("1 2     mac-address                 set mac address");
        l.add("2 .       <addr>                    mac address");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        l.add(cmds.tabulator + "rd " + tabRtrmapN.rd2string(rd));
        l.add(cmds.tabulator + "rt-import " + tabRtrmapN.rd2string(rtImp));
        l.add(cmds.tabulator + "rt-export " + tabRtrmapN.rd2string(rtExp));
        cmds.cfgLine(l, !staticAddr, beg, "mac-address", "" + hwaddr);
        cmds.cfgLine(l, learned == null, beg, "mac-learn", "");
        cmds.cfgLine(l, inspect == null, beg, "inspect", "" + inspect);
        cmds.cfgLine(l, !macMove, beg, "mac-move", "");
        cmds.cfgLine(l, !privateBridge, beg, "private-bridge", "");
        cmds.cfgLine(l, !blockUnicast, beg, "block-unicast", "");
        cmds.cfgLine(l, !padupSmall, beg, "padup-small", "");
        l.add(beg + "mac-age " + macAgeTime);
        l.add(beg + "stp-priority " + stpPrio);
        l.add(beg + "stp-time " + stpHlo + " " + stpAge + " " + stpFwd);
        if (needStp) {
            l.add(beg + "stp-mode ieee");
        } else {
            l.add(beg + "stp-mode none");
        }
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("rd")) {
            rd = tabRtrmapN.string2rd(cmd.word());
            return;
        }
        if (a.equals("rt-import")) {
            rtImp = tabRtrmapN.string2rd(cmd.word());
            return;
        }
        if (a.equals("rt-export")) {
            rtExp = tabRtrmapN.string2rd(cmd.word());
            return;
        }
        if (a.equals("rt-both")) {
            rtExp = tabRtrmapN.string2rd(cmd.word());
            rtImp = rtExp;
            return;
        }
        if (a.equals("inspect")) {
            if (inspect != null) {
                inspect.stopTimer();
            }
            inspect = new tabSession();
            inspect.fromString(cmd);
            inspect.startTimer();
            return;
        }
        if (a.equals("mac-move")) {
            macMove = true;
            return;
        }
        if (a.equals("mac-learn")) {
            learned = new tabGen<ifcBridgeMacAddr>();
            return;
        }
        if (a.equals("private-bridge")) {
            privateBridge = true;
            return;
        }
        if (a.equals("padup-small")) {
            padupSmall = true;
            return;
        }
        if (a.equals("block-unicast")) {
            blockUnicast = true;
            return;
        }
        if (a.equals("mac-age")) {
            macAgeTime = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("stp-mode")) {
            a = cmd.word();
            if (a.equals("ieee")) {
                needStp = true;
                setBlocking(true);
                return;
            }
            if (a.equals("none")) {
                needStp = false;
                setBlocking(false);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("stp-time")) {
            stpHlo = bits.str2num(cmd.word());
            stpAge = bits.str2num(cmd.word());
            stpFwd = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("mac-address")) {
            addrMac adr = new addrMac();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return;
            }
            hwaddr.setAddr(adr);
            staticAddr = true;
            return;
        }
        if (a.equals("stp-priority")) {
            stpPrio = bits.str2num(cmd.word());
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("mac-learn")) {
            learned = null;
            return;
        }
        if (a.equals("inspect")) {
            if (inspect != null) {
                inspect.stopTimer();
            }
            inspect = null;
            return;
        }
        if (a.equals("mac-move")) {
            macMove = false;
            return;
        }
        if (a.equals("private-bridge")) {
            privateBridge = false;
            return;
        }
        if (a.equals("padup-small")) {
            padupSmall = false;
            return;
        }
        if (a.equals("block-unicast")) {
            blockUnicast = false;
            return;
        }
        if (a.equals("mac-address")) {
            staticAddr = false;
            return;
        }
        cmd.badCmd();
    }

    /**
     * add new interface to this virtual bridge
     *
     * @param physical true if physical interface
     * @param notEther true if dst,src mac is embedded in data
     * @param needType true if ethertype need appened
     * @return interface handler that will pass packets to this bridge
     */
    public ifcBridgeIfc newIface(boolean physical, boolean notEther, boolean needType) {
        if (debugger.ifcBridgeTraf) {
            logger.debug("add iface");
        }
        ifcBridgeIfc ntry = new ifcBridgeIfc(this, physical, notEther, needType);
        ntry.ifcNum = nextIfaceNum++;
        ntry.blocked = needStp;
        ifaces.add(ntry);
        return ntry;
    }

    /**
     * delete one interface from this virtual bridge
     *
     * @param num inteface number
     * @return lower interface handler (null=error)
     */
    public ifcDn delIface(int num) {
        if (debugger.ifcBridgeTraf) {
            logger.debug("del iface");
        }
        ifcBridgeIfc ntry = new ifcBridgeIfc(this, false, false, false);
        ntry.ifcNum = num;
        ntry = ifaces.del(ntry);
        if (ntry == null) {
            return null;
        }
        portFlap();
        return ntry.lowerIf;
    }

    private void send2upper(int ifn, packHolder pck) {
        if (ifn == 0) {
            return;
        }
        cntr.tx(pck);
        if (upper == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.rx(pck);
        pck.merge2beg();
        doRxPack(upNtry, pck);
    }

    public String toString() {
        return "bridge " + hwaddr;
    }

    public int getMTUsize() {
        int p = 1500;
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBridgeIfc ntry = ifaces.get(i);
            int o = ntry.lowerIf.getMTUsize();
            if (p > o) {
                p = o;
            }
        }
        return p;
    }

    public long getBandwidth() {
        long p = 100000000;
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBridgeIfc ntry = ifaces.get(i);
            long o = ntry.lowerIf.getBandwidth();
            if (p > o) {
                p = o;
            }
        }
        return p;
    }

    private void resetTimer(boolean needRun) {
        try {
            timer.cancel();
        } catch (Exception e) {
        }
        if (!needRun) {
            return;
        }
        timer = new Timer();
        ifcBridgeTimer task = new ifcBridgeTimer(this);
        timer.schedule(task, 500, 1000);
    }

    /**
     * do startup
     */
    public void doStartup() {
        if (debugger.ifcBridgeTraf) {
            logger.debug("startup");
        }
        resetTimer(true);
    }

    /**
     * do startup
     */
    public void doShutdown() {
        if (debugger.ifcBridgeTraf) {
            logger.debug("shutdown");
        }
        resetTimer(false);
    }

    /**
     * purge mac address cache
     *
     * @param rnd round number
     */
    protected void doCachePurge(int rnd) {
        currTim = bits.getTime();
        if ((rnd % 60) != 0) {
            return;
        }
        if (debugger.ifcBridgeTraf) {
            logger.debug("purge");
        }
        if (learned == null) {
            return;
        }
        for (int i = learned.size() - 1; i >= 0; i--) {
            ifcBridgeMacAddr ntry = learned.get(i);
            if (ntry == null) {
                continue;
            }
            if ((currTim - ntry.time) < macAgeTime) {
                continue;
            }
            if (debugger.ifcBridgeTraf) {
                logger.debug("purge " + ntry);
            }
            if (macMove) {
                logger.info(ntry.adr + " disappeared from " + ntry.ifc.getIfcName());
            }
            learned.del(ntry);
            if ((macRouter != null) && (ntry.ifc.lowerIf == null)) {
                macRouter.bridgeChanged();
            }
        }
    }

    /**
     * flood stp packets
     *
     * @param rnd round number
     */
    protected void doStpFloop(int rnd) {
        if (!needStp) {
            return;
        }
        if ((rnd % (stpHlo / 1000)) != 0) {
            return;
        }
        long rootTime = 0;
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBridgeIfc ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if ((currTim - ntry.stpTime) > stpFwd) {
                ntry.blocked = false;
            }
            if (ntry.ifcNum == stpPort) {
                rootTime = ntry.stpTime;
            }
            packStp pckS = ntry.getStpId();
            if (debugger.ifcBridgeTraf) {
                logger.debug("tx " + pckS);
            }
            packHolder pckB = new packHolder(true, true);
            pckS.createHeader(pckB);
            ntry.doTxPack(pckB);
        }
        if (stpPort == 0) {
            return;
        }
        if ((currTim - rootTime) > stpAge) {
            setBlocking(true);
        }
    }

    private void floodPack(int ifn, boolean phy, packHolder pck) {
        if (privateBridge && (ifn != 0)) {
            send2upper(ifn, pck);
            return;
        }
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBridgeIfc ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ifn == ntry.ifcNum) {
                continue;
            }
            if (ntry.blocked) {
                continue;
            }
            if (!(phy | ntry.physical)) {
                continue;
            }
            ntry.doTxPack(pck.copyBytes(true, true));
        }
        send2upper(ifn, pck);
    }

    /**
     * notify the bridge that a packet arrived to a virtual interface
     *
     * @param ifc interface handler that got the packet
     * @param pck the packet that arrived
     */
    protected void doRxPack(ifcBridgeIfc ifc, packHolder pck) {
        if (!doRxStp(ifc, pck)) {
            return;
        }
        if (ifc.blocked) {
            return;
        }
        if ((inspect != null) && ((pck.ETHtype == ipIfc4.type) || (pck.ETHtype == ipIfc6.type))) {
            int i = pck.dataSize();
            pck.getSkip(2);
            boolean b;
            switch (pck.ETHtype) {
                case ipIfc4.type:
                    b = core4.parseIPheader(pck, false);
                    break;
                case ipIfc6.type:
                    b = core6.parseIPheader(pck, false);
                    break;
                default:
                    b = true;
                    break;
            }
            if (!b) {
                inspect.doPack(pck, false);
            }
            int o = pck.dataSize();
            pck.getSkip(o - i);
        }
        if (padupSmall) {
            int pad = 48 - pck.dataSize();
            if (pad > 0) {
                pck.putFill(0, pad, 0);
                pck.putSkip(pad);
                pck.merge2end();
            }
        }
        if (learned == null) {
            floodPack(ifc.ifcNum, ifc.physical, pck);
            return;
        }
        ifcBridgeMacAddr lrn = new ifcBridgeMacAddr(pck.ETHsrc.copyBytes());
        lrn.ifc = ifc;
        lrn.cntr = new counter();
        ifcBridgeMacAddr old = learned.add(lrn);
        if (old != null) {
            if (macMove) {
                if (ifc.compare(ifc, old.ifc) != 0) {
                    logger.info(pck.ETHsrc + " moved from " + old.ifc.getIfcName() + " to " + ifc.getIfcName());
                }
            }
            lrn = old;
        } else {
            if (macMove) {
                logger.info(pck.ETHsrc + " learned from " + ifc.getIfcName());
            }
            if ((ifc.lowerIf == null) && (macRouter != null)) {
                macRouter.bridgeChanged();
            }
        }
        lrn.ifc = ifc;
        lrn.time = currTim;
        lrn.cntr.rx(pck);
        if (pck.ETHtrg.isFloodable()) {
            floodPack(ifc.ifcNum, ifc.physical, pck);
            return;
        }
        lrn = new ifcBridgeMacAddr(pck.ETHtrg.copyBytes());
        lrn = learned.find(lrn);
        if (lrn == null) {
            if (blockUnicast) {
                if (upProm) {
                    send2upper(ifc.ifcNum, pck);
                }
                return;
            }
            floodPack(ifc.ifcNum, ifc.physical, pck);
            return;
        }
        if (lrn.ifc.ifcNum == ifc.ifcNum) {
            return;
        }
        lrn.cntr.tx(pck);
        if (lrn.ifc.ifcNum == 0) {
            send2upper(ifc.ifcNum, pck);
            return;
        }
        if (upProm) {
            send2upper(ifc.ifcNum, pck.copyBytes(true, true));
        }
        if (privateBridge && (ifc.ifcNum != 0)) {
            return;
        }
        if (!(ifc.physical | lrn.ifc.physical)) {
            return;
        }
        lrn.ifc.doTxPack(pck);
    }

    /**
     * get spantree id
     *
     * @return spantree id
     */
    protected addrBridge getStpId() {
        addrBridge b = new addrBridge();
        b.adr.setAddr(hwaddr);
        b.pri = stpPrio;
        return b;
    }

    /**
     * clear mac table
     */
    protected void portFlap() {
        if (learned != null) {
            learned.clear();
        }
        if (macMove) {
            logger.info("table flush");
        }
        if (macRouter != null) {
            macRouter.bridgeChanged();
        }
    }

    private void setBlocking(boolean blk) {
        if (debugger.ifcBridgeTraf) {
            logger.debug("setting ports");
        }
        stpRoot = getStpId();
        stpPort = 0;
        stpCost = 0;
        portFlap();
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBridgeIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifc.blocked = blk;
            ifc.stpTime = currTim;
        }
    }

    private boolean doRxStp(ifcBridgeIfc ifc, packHolder pck) {
        if (!needStp) {
            return true;
        }
        packStp stp = new packStp();
        if (stp.parseHeader(pck)) {
            return true;
        }
        if (debugger.ifcBridgeTraf) {
            logger.debug("rx " + stp);
        }
        int cst = stp.rootCost + addrBridge.bandwidth2cost(ifc.lowerIf.getBandwidth());
        ifc.stpTime = currTim;
        int i = stpRoot.compare(stpRoot, stp.rootId);
        if (i < 0) { // worst root
            ifc.blocked = true;
            return false;
        }
        if (i > 0) { // better root
            setBlocking(true);
            stpRoot = stp.rootId.copyBytes();
            stpCost = cst;
            stpPort = ifc.ifcNum;
            ifc.blocked = false;
            return false;
        }
        if (stpCost > cst) { // better cost
            stpCost = cst;
            stpPort = ifc.ifcNum;
            ifc.blocked = false;
            return false;
        }
        if (stpCost == cst) { // alternate port
            ifc.blocked = ifc.ifcNum != stpPort;
            return false;
        }
        if (stpCost < stp.rootCost) { // worst cost
            ifc.blocked = false;
            return false;
        }
        i = stpRoot.compare(getStpId(), stp.brdgId);
        if (i == 0) {
            ifc.blocked = ifc.ifcNum <= stp.portId;
        } else {
            ifc.blocked = i < 0;
        }
        return false;
    }

}

class ifcBridgeTimer extends TimerTask {

    private ifcBridge lower;

    private int cntr;

    public ifcBridgeTimer(ifcBridge parent) {
        lower = parent;
    }

    public void run() {
        try {
            cntr = (cntr + 1) & 0xffff;
            lower.doCachePurge(cntr);
            lower.doStpFloop(cntr);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ifcBridgeMacAddr implements Comparator<ifcBridgeMacAddr> {

    public final addrMac adr;

    public ifcBridgeIfc ifc;

    public long time;

    public counter cntr;

    public ifcBridgeMacAddr(addrMac addr) {
        adr = addr;
    }

    public int compare(ifcBridgeMacAddr o1, ifcBridgeMacAddr o2) {
        return o1.adr.compare(o1.adr, o2.adr);
    }

    public String toString() {
        return adr + "|" + ifc.getIfcName() + "|" + bits.timePast(time) + "|" + cntr.getShBsum();
    }

}
