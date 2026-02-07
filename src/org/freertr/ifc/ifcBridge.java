package org.freertr.ifc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrBridge;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.ip.ipCor;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipIcmp6;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc4arp;
import org.freertr.ip.ipIfc6;
import org.freertr.ip.ipMhost4;
import org.freertr.ip.ipMhost6;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPim;
import org.freertr.pack.packPimGrp;
import org.freertr.pack.packStp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabSession;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

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
     * label
     */
    public tabLabelEntry label;

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
     * group aging time
     */
    public int grpAgeTime = 3 * 60 * 1000;

    /**
     * limit mac addresses
     */
    public int macLimit;

    /**
     * log mac movements
     */
    public boolean macMove;

    /**
     * block unicast with unknown destination
     */
    public boolean blockUnicast;

    /**
     * block multicast with unknown destination
     */
    public boolean blockMulticast;

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
     * spantree mode: 0=none, 1=ieee, 2=drop
     */
    public int needStp;

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

    /**
     * timer
     */
    protected ifcBridgeTimer timer;

    private tabGen<ifcBridgeIfc> ifaces;

    private tabGen<ifcBridgeAdr> learned;

    private ifcUp upper = new ifcNull();

    private int nextIfaceNum;

    private counter cntr = new counter();

    private long currTim = bits.getTime();

    private int stpPort;

    private boolean upProm;

    private final ifcBridgeIfc upNtry;

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
        upNtry.publicPort = true;
        upNtry.lowerIf = null;
    }

    /**
     * get show output
     *
     * @return list of interface
     */
    public userFormat getShowIfc() {
        userFormat lst = new userFormat("|", "iface|fwd|phys|tx|rx|drop|tx|rx|drop|grp", "3|3packet|3byte|1");
        lst.add("" + upNtry);
        for (int i = 0; i < ifaces.size(); i++) {
            lst.add("" + ifaces.get(i).getShowIfc());
        }
        return lst;
    }

    /**
     * get show output
     *
     * @return list of interface
     */
    public userFormat getShowStp() {
        if (needStp == 0) {
            return null;
        }
        userFormat lst = new userFormat("|", "iface|fwd|phys|tx|rx|drop");
        lst.add("" + upNtry);
        for (int i = 0; i < ifaces.size(); i++) {
            lst.add("" + ifaces.get(i).getShowStp());
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
        userFormat lst = new userFormat("|", "addr|iface|static|time|tx|rx|drop|tx|rx|drop", "4|3packet|3byte");
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
            ifcBridgeAdr ntry = learned.get(i);
            if (!ntry.ifc.physical) {
                continue;
            }
            lst.add(ntry.adr);
        }
        return lst;
    }

    /**
     * get mac address
     *
     * @param i sequence
     * @return address, null if not found
     */
    public ifcBridgeAdr getMacAddr(int i) {
        if (learned == null) {
            return null;
        }
        return learned.get(i);
    }

    /**
     * find mac address
     *
     * @param mac address to find
     * @return address, null if not found
     */
    public ifcBridgeAdr findMacAddr(addrMac mac) {
        if (learned == null) {
            return null;
        }
        ifcBridgeAdr ntry = new ifcBridgeAdr(mac);
        return learned.find(ntry);
    }

    /**
     * get interface
     *
     * @param i sequence
     * @return interface, null if not found
     */
    public ifcBridgeIfc getIface(int i) {
        return ifaces.get(i);
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this bridge");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this bridge");
        l.add(null, false, 1, new int[]{2}, "rd", "specify route distinguisher");
        l.add(null, false, 2, new int[]{-1}, "<rd>", "rd in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt-import", "specify route target import");
        l.add(null, false, 2, new int[]{-1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt-export", "specify route target export");
        l.add(null, false, 2, new int[]{-1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt-both", "specify route target");
        l.add(null, false, 2, new int[]{-1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{-1}, "mac-learn", "enable mac address learning");
        l.add(null, false, 1, new int[]{2, -1}, "inspect", "enable session inspection");
        l.add(null, false, 2, new int[]{2, -1}, "mac", "enable mac logging");
        l.add(null, false, 2, new int[]{2, -1}, "before", "log on session start");
        l.add(null, false, 2, new int[]{2, -1}, "after", "log on session stop");
        l.add(null, false, 1, new int[]{-1}, "mac-move", "enable mac move logging");
        l.add(null, false, 1, new int[]{2}, "mac-limit", "limit number of addesses");
        l.add(null, false, 2, new int[]{-1}, "<num>", "maximum");
        l.add(null, false, 1, new int[]{-1}, "private-bridge", "disable peer communication");
        l.add(null, false, 1, new int[]{-1}, "block-unicast", "block unknown destination unicast");
        l.add(null, false, 1, new int[]{-1}, "block-multicast", "block unwanted destination multicast");
        l.add(null, false, 1, new int[]{-1}, "padup-small", "pad up small packets");
        l.add(null, false, 1, new int[]{2}, "mac-age", "set mac aging time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 1, new int[]{2}, "stp-mode", "set spantree mode");
        l.add(null, false, 2, new int[]{-1}, "ieee", "ieee");
        l.add(null, false, 2, new int[]{-1}, "none", "pass through");
        l.add(null, false, 2, new int[]{-1}, "drop", "drop");
        l.add(null, false, 1, new int[]{2}, "stp-time", "set spantree timers");
        l.add(null, false, 2, new int[]{3}, "<num>", "hello in ms");
        l.add(null, false, 3, new int[]{4}, "<num>", "maxage in ms");
        l.add(null, false, 4, new int[]{-1}, "<num>", "forward delay in ms");
        l.add(null, false, 1, new int[]{2}, "stp-priority", "set spantree priority");
        l.add(null, false, 2, new int[]{-1}, "<num>", "priority in 1024 increments");
        l.add(null, false, 1, new int[]{2}, "mac-address", "set mac address");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "mac address");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        l.add(cmds.tabulator + "rd " + tabRouteUtil.rd2string(rd));
        l.add(cmds.tabulator + "rt-import " + tabRouteUtil.rd2string(rtImp));
        l.add(cmds.tabulator + "rt-export " + tabRouteUtil.rd2string(rtExp));
        cmds.cfgLine(l, !staticAddr, beg, "mac-address", "" + hwaddr);
        cmds.cfgLine(l, learned == null, beg, "mac-learn", "");
        cmds.cfgLine(l, inspect == null, beg, "inspect", "" + inspect);
        cmds.cfgLine(l, !macMove, beg, "mac-move", "");
        cmds.cfgLine(l, !privateBridge, beg, "private-bridge", "");
        cmds.cfgLine(l, !blockUnicast, beg, "block-unicast", "");
        cmds.cfgLine(l, !blockMulticast, beg, "block-multicast", "");
        cmds.cfgLine(l, !padupSmall, beg, "padup-small", "");
        l.add(beg + "mac-limit " + macLimit);
        l.add(beg + "mac-age " + macAgeTime);
        l.add(beg + "stp-priority " + stpPrio);
        l.add(beg + "stp-time " + stpHlo + " " + stpAge + " " + stpFwd);
        String a;
        switch (needStp) {
            case 0:
                a = "none";
                break;
            case 1:
                a = "ieee";
                break;
            case 2:
                a = "drop";
                break;
            default:
                a = "unknown";
                break;
        }
        l.add(beg + "stp-mode " + a);
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
            rd = tabRouteUtil.string2rd(cmd.word());
            return;
        }
        if (a.equals("rt-import")) {
            rtImp = tabRouteUtil.string2rd(cmd.word());
            return;
        }
        if (a.equals("rt-export")) {
            rtExp = tabRouteUtil.string2rd(cmd.word());
            return;
        }
        if (a.equals("rt-both")) {
            rtExp = tabRouteUtil.string2rd(cmd.word());
            rtImp = rtExp;
            return;
        }
        if (a.equals("inspect")) {
            if (inspect != null) {
                inspect.stopTimer();
            }
            inspect = new tabSession(true, 180000);
            inspect.fromString(cmd);
            inspect.startTimer();
            return;
        }
        if (a.equals("mac-limit")) {
            macLimit = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("mac-move")) {
            macMove = true;
            return;
        }
        if (a.equals("mac-learn")) {
            learned = new tabGen<ifcBridgeAdr>();
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
        if (a.equals("block-multicast")) {
            blockMulticast = true;
            return;
        }
        if (a.equals("mac-age")) {
            macAgeTime = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("stp-mode")) {
            a = cmd.word();
            if (a.equals("none")) {
                needStp = 0;
                setBlocking(false);
                return;
            }
            if (a.equals("ieee")) {
                needStp = 1;
                setBlocking(true);
                return;
            }
            if (a.equals("drop")) {
                needStp = 2;
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
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("rd")) {
            rd = 0;
            return;
        }
        if (a.equals("rt-import")) {
            rtImp = 0;
            return;
        }
        if (a.equals("rt-export")) {
            rtExp = 0;
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
        if (a.equals("mac-limit")) {
            macLimit = 0;
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
        if (a.equals("block-multicast")) {
            blockMulticast = false;
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
        ntry.blocked = needStp == 1;
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
        delMacs(ntry);
        return ntry.lowerIf;
    }

    /**
     * add mac on interface
     *
     * @param ifc interface
     * @param adr address
     */
    public void addMac(ifcBridgeIfc ifc, addrMac adr) {
        if (adr == null) {
            return;
        }
        if (learned == null) {
            return;
        }
        ifcBridgeAdr ntry = new ifcBridgeAdr(adr.copyBytes());
        ntry.stat = true;
        ntry.ifc = ifc;
        ntry.cntr = new counter();
        learned.put(ntry);
    }

    /**
     * add macs on interface
     *
     * @param ifc interface
     * @param lst addresses
     */
    public void addMacs(ifcBridgeIfc ifc, tabGen<addrMac> lst) {
        if (lst == null) {
            return;
        }
        for (int i = 0; i < lst.size(); i++) {
            addMac(ifc, lst.get(i));
        }
    }

    /**
     * add macs on all the interface
     */
    public void addMacs() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBridgeIfc ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            addMacs(ntry, ntry.statAddr);
        }
    }

    /**
     * delete macs on interface
     *
     * @param ifc interface, null means all
     */
    public void delMacs(ifcBridgeIfc ifc) {
        if (learned == null) {
            return;
        }
        if (ifc == null) {
            learned.clear();
            addMacs();
            if (macRouter != null) {
                macRouter.bridgeChanged();
            }
            return;
        }
        int seen = 0;
        for (int i = learned.size() - 1; i >= 0; i--) {
            ifcBridgeAdr mac = learned.get(i);
            if (mac == null) {
                continue;
            }
            if (ifc != mac.ifc) {
                continue;
            }
            learned.del(mac);
            seen++;
        }
        if (seen < 1) {
            return;
        }
        if ((macRouter != null) && (ifc.physical)) {
            macRouter.bridgeChanged();
        }
    }

    private void send2upper(ifcBridgeIfc ifc, packHolder pck) {
        if (ifc.ifcNum == 0) {
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
        timer = null;
        if (!needRun) {
            return;
        }
        timer = new ifcBridgeTimer(this);
        timer.start();
    }

    /**
     * do startup
     */
    public void doStartup() {
        if (debugger.ifcBridgeTraf) {
            logger.debug("startup");
        }
        label = tabLabel.allocate(tabLabelEntry.owner.bridge);
        label.setFwdDrop(tabLabelEntry.owner.bridge);
        resetTimer(true);
    }

    /**
     * do startup
     */
    public void doShutdown() {
        if (debugger.ifcBridgeTraf) {
            logger.debug("shutdown");
        }
        tabLabel.release(label, tabLabelEntry.owner.bridge);
        resetTimer(false);
    }

    /**
     * purge group addresses
     *
     * @param rnd round number
     */
    protected void doGroupPurge(int rnd) {
        if ((rnd % 60) != 0) {
            return;
        }
        currTim = bits.getTime();
        if (debugger.ifcBridgeTraf) {
            logger.debug("purge");
        }
        for (int o = 0; o < ifaces.size(); o++) {
            ifcBridgeIfc ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.groups == null) {
                continue;
            }
            for (int i = ifc.groups.size() - 1; i >= 0; i--) {
                ifcBridgeGrp grp = ifc.groups.get(i);
                if (grp == null) {
                    continue;
                }
                if ((currTim - grp.time) < grpAgeTime) {
                    continue;
                }
                ifc.groups.del(grp);
            }
        }
    }

    /**
     * purge mac address cache
     *
     * @param rnd round number
     */
    protected void doCachePurge(int rnd) {
        if ((rnd % 60) != 0) {
            return;
        }
        currTim = bits.getTime();
        if (debugger.ifcBridgeTraf) {
            logger.debug("purge");
        }
        if (learned == null) {
            return;
        }
        for (int i = learned.size() - 1; i >= 0; i--) {
            ifcBridgeAdr ntry = learned.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.stat) {
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
            if ((macRouter != null) && (ntry.ifc.physical)) {
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
        if (needStp != 1) {
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
            if (ntry.fltrStpOut) {
                continue;
            }
            packStp pckS = ntry.getStpId();
            if (debugger.ifcBridgeTraf) {
                logger.debug("tx " + pckS);
            }
            packHolder pckB = new packHolder(true, true);
            pckS.createHeader(pckB);
            ntry.stpCntr.tx(pckB);
            ntry.doTxPack(pckB);
        }
        if (stpPort == 0) {
            return;
        }
        if ((currTim - rootTime) > stpAge) {
            setBlocking(true);
        }
    }

    private void floodPack(ifcBridgeIfc ifc, packHolder pck) {
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBridgeIfc ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ifc.ifcNum == ntry.ifcNum) {
                continue;
            }
            if (ntry.blocked) {
                continue;
            }
            if (!(ifc.physical | ntry.physical)) {
                continue;
            }
            if ((privateBridge | ifc.privatePort | ntry.privatePort) && !ifc.publicPort && !ntry.publicPort) {
                continue;
            }
            ntry.doTxPack(pck.copyBytes(true, true));
        }
        send2upper(ifc, pck);
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
        if (inspect != null) {
            int i = pck.dataSize();
            pck.getSkip(2);
            boolean b;
            boolean bb = false;
            switch (pck.ETHtype) {
                case ipIfc4arp.type:
                    b = false;
                    bb = true;
                    break;
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
            if (!b && !bb) {
                b = inspect.doPack(pck, false);
            }
            int o = pck.dataSize();
            pck.getSkip(o - i);
            if (b) {
                return;
            }
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
            floodPack(ifc, pck);
            return;
        }
        ifcBridgeAdr lrn = new ifcBridgeAdr(pck.ETHsrc.copyBytes());
        lrn.ifc = ifc;
        lrn.cntr = new counter();
        ifcBridgeAdr old = learned.add(lrn);
        if (old != null) {
            if (ifc != old.ifc) {
                if (old.stat) {
                    cntr.drop(pck, counter.reasons.denied);
                    return;
                }
                if (macMove) {
                    logger.info(pck.ETHsrc + " moved from " + old.ifc.getIfcName() + " to " + ifc.getIfcName());
                }
            }
            lrn = old;
        } else {
            if (macLimit > 0) {
                if (learned.size() > macLimit) {
                    learned.del(lrn);
                    return;
                }
            }
            if (macMove) {
                logger.info(pck.ETHsrc + " learned from " + ifc.getIfcName());
            }
            if ((ifc.physical) && (macRouter != null)) {
                macRouter.bridgeChanged();
            }
        }
        lrn.ifc = ifc;
        lrn.time = currTim;
        lrn.cntr.rx(pck);
        if (pck.ETHtrg.isFloodable()) {
            if (!blockMulticast) {
                floodPack(ifc, pck);
                return;
            }
            int i = pck.dataSize();
            pck.getSkip(2);
            boolean b = false;
            switch (pck.ETHtype) {
                case ipIfc4arp.type:
                    b = true;
                    break;
                case ipIfc4.type:
                    if (core4.parseIPheader(pck, false)) {
                        return;
                    }
                    pck.getSkip(pck.IPsiz);
                    if (pck.IPprt == ipMhost4.protoNum) {
                        b = !ipMhost4.parsePacket(ifc, ifc, pck);
                    }
                    break;
                case ipIfc6.type:
                    if (core6.parseIPheader(pck, false)) {
                        return;
                    }
                    pck.getSkip(pck.IPsiz);
                    if (pck.IPprt == ipIcmp6.protoNum) {
                        ipIcmp6.parseICMPports(pck);
                        pck.getSkip(ipIcmp6.size);
                        b = !ipMhost6.parsePacket(ifc, ifc, pck);
                    }
                    break;
                default:
                    return;
            }
            if (pck.IPprt == packPim.proto) {
                packPim pckPim = new packPim();
                if (!pckPim.parseHeader(pck)) {
                    pckPim.parsePayload(pck);
                }
                if (pckPim.groups != null) {
                    for (int o = 0; o < pckPim.groups.size(); o++) {
                        packPimGrp grp = pckPim.groups.get(o);
                        for (int p = 0; p < grp.joins.size(); p++) {
                            ifc.mhostReport(ifc, grp.group.network, grp.joins.get(p).network, true);
                        }
                        for (int p = 0; p < grp.prunes.size(); p++) {
                            ifc.mhostReport(ifc, grp.group.network, grp.prunes.get(p).network, false);
                        }
                    }
                }
                b = true;
            }
            int o = pck.dataSize();
            pck.getSkip(o - i);
            if (b) {
                floodPack(ifc, pck);
                return;
            }
            if (pck.IPbrd) {
                floodPack(ifc, pck);
                return;
            }
            if (!pck.IPmlt) {
                return;
            }
            if (!pck.IPmlr) {
                floodPack(ifc, pck);
                return;
            }
            ifcBridgeGrp grp = new ifcBridgeGrp(pck.IPtrg);
            for (i = 0; i < ifaces.size(); i++) {
                ifcBridgeIfc ntry = ifaces.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ifc.ifcNum == ntry.ifcNum) {
                    continue;
                }
                if (ntry.blocked) {
                    continue;
                }
                if (!(ifc.physical | ntry.physical)) {
                    continue;
                }
                if (ntry.groups == null) {
                    continue;
                }
                if ((privateBridge | ifc.privatePort | ntry.privatePort) && !ifc.publicPort && !ntry.publicPort) {
                    continue;
                }
                if (ntry.groups.find(grp) == null) {
                    continue;
                }
                ntry.doTxPack(pck.copyBytes(true, true));
            }
            send2upper(ifc, pck);
            return;
        }
        lrn = new ifcBridgeAdr(pck.ETHtrg.copyBytes());
        lrn = learned.find(lrn);
        if (lrn == null) {
            if (blockUnicast) {
                if (upProm) {
                    send2upper(ifc, pck);
                }
                return;
            }
            floodPack(ifc, pck);
            return;
        }
        if (lrn.ifc.ifcNum == ifc.ifcNum) {
            return;
        }
        lrn.cntr.tx(pck);
        if (lrn.ifc.ifcNum == 0) {
            send2upper(ifc, pck);
            return;
        }
        if (upProm) {
            send2upper(ifc, pck.copyBytes(true, true));
        }
        if ((privateBridge | ifc.privatePort | lrn.ifc.privatePort) && !ifc.publicPort && !lrn.ifc.publicPort) {
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

    private void setBlocking(boolean blk) {
        if (debugger.ifcBridgeTraf) {
            logger.debug("setting ports");
        }
        stpRoot = getStpId();
        stpPort = 0;
        stpCost = 0;
        if (learned != null) {
            learned.clear();
            addMacs();
        }
        if (macMove) {
            logger.info("table flush");
        }
        if (macRouter != null) {
            macRouter.bridgeChanged();
        }
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
        ifc.stpCntr.rx(pck);
        if (needStp == 0) {
            ifc.stpCntr.drop(pck, counter.reasons.notUp);
            return true;
        }
        packStp stp = new packStp();
        if (stp.parseHeader(pck)) {
            ifc.stpCntr.drop(pck, counter.reasons.badHdr);
            return true;
        }
        if (needStp == 2) {
            ifc.stpCntr.drop(pck, counter.reasons.noBuffer);
            return false;
        }
        if (debugger.ifcBridgeTraf) {
            logger.debug("rx " + stp);
        }
        ifc.stpTime = currTim;
        if (ifc.fltrStpIn) {
            ifc.blocked = true;
            return false;
        }
        int cst = stp.rootCost + addrBridge.bandwidth2cost(ifc.lowerIf.getBandwidth());
        int i = stpRoot.compareTo(stp.rootId);
        if (i < 0) { // worst root
            ifc.blocked = true;
            return false;
        }
        if (i > 0) { // better root
            if (ifc.fltrStpRoot) {
                ifc.blocked = true;
                return false;
            }
            setBlocking(true);
            stpRoot = stp.rootId.copyBytes();
            stpCost = cst;
            stpPort = ifc.ifcNum;
            ifc.blocked = false;
            return false;
        }
        if (stpCost > cst) { // better cost
            if (ifc.fltrStpRoot) {
                ifc.blocked = true;
                return false;
            }
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
        i = getStpId().compareTo(stp.brdgId);
        if (i == 0) {
            ifc.blocked = ifc.ifcNum <= stp.portId;
        } else {
            ifc.blocked = i < 0;
        }
        return false;
    }

}

class ifcBridgeTimer implements Runnable {

    private ifcBridge lower;

    private int cntr;

    public ifcBridgeTimer(ifcBridge parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.timer != this) {
                    break;
                }
                cntr = (cntr + 1) & 0xffff;
                lower.doGroupPurge(cntr);
                lower.doCachePurge(cntr);
                lower.doStpFloop(cntr);
                bits.sleep(1000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
