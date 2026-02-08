package org.freertr.ifc;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packHolder;
import org.freertr.pack.packReplicator;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabWindow;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;
import org.freertr.util.state.states;

/**
 * bundles interfaces to one interface
 *
 * @author matecsaba
 */
public class ifcBundle implements Runnable, ifcDn {

    /**
     * description of this bridge
     */
    public String description = "";

    /**
     * hardware address of the bundle that this class presents
     */
    public addrType hwaddr;

    /**
     * interface is not ethernet
     */
    public boolean notEther;

    /**
     * logging enabled
     */
    public boolean logging;

    /**
     * packets are sent to best priority
     */
    public int backup;

    /**
     * packets are replicated
     */
    public boolean replicate;

    /**
     * packets are sequenced
     */
    public tabWindow<packHolder> sequence;

    /**
     * peering handler
     */
    public ifcBundlePeer peering;

    /**
     * dejitter timer
     */
    public int dejitter;

    /**
     * reporter timer
     */
    public int reporter;

    /**
     * dynamic quota
     */
    public int dynamic;

    /**
     * load balance algorithm
     */
    public int loadBalance;

    /**
     * list of interfaces
     */
    public tabGen<ifcBundleIfc> ifaces;

    /**
     * selected interface
     */
    public int selected = -2;

    /**
     * timer
     */
    protected ifcBundleKeep timer1;

    /**
     * timer
     */
    protected ifcBundleCntr timer2;

    /**
     * need to run
     */
    protected boolean need2run = true;

    private ifcUp upper = new ifcNull();

    private int nextIfaceNum;

    private counter cntr = new counter();

    private int nextSender;

    private int currSender;

    private int seqTx;

    private notifier notif = new notifier();

    public counter getCounter() {
        return cntr;
    }

    public state.states getState() {
        if (backup > 0) {
            ifcBundleIfc ifc = ifaces.get(selected);
            if (ifc == null) {
                return state.states.down;
            }
            return state.states.up;
        }
        if (peering != null) {
            if (peering.remoteBetter) {
                return state.states.down;
            }
        }
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.stated != state.states.up) {
                continue;
            }
            return state.states.up;
        }
        return state.states.down;
    }

    public void setFilter(boolean promisc) {
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifc.lowerIf.setFilter(true);
        }
    }

    public addrType getHwAddr() {
        return hwaddr;
    }

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
    public ifcBundle() {
        hwaddr = addrMac.getRandom();
        if (debugger.ifcBundleTraf) {
            logger.debug("started, addr=" + hwaddr);
        }
        nextIfaceNum = 1;
        ifaces = new tabGen<ifcBundleIfc>();
    }

    private void allocateReports() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            int[] buf;
            if (dynamic < 1) {
                buf = null;
            } else {
                buf = new int[dynamic];
            }
            ifc.byteRprt = buf;
        }
    }

    /**
     * propagate state
     */
    public void propagateState() {
        upper.setState(getState());
        long min = Long.MAX_VALUE;
        long max = 0;
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if ((ifc.lowerIf == null) || (ifc.stated != state.states.up)) {
                ifc.byteNeed = 0;
                ifc.byteRcvd = 0;
                ifc.byteAvrg = 0;
                continue;
            }
            long o;
            if (dynamic > 0) {
                o = ifc.byteAvrg;
            } else {
                o = ifc.lowerIf.getBandwidth();
            }
            if (o < min) {
                min = o;
            }
            if (o > max) {
                max = o;
            }
        }
        if (min > max) {
            return;
        }
        min /= 2000;
        if (min < 1) {
            min = 1;
        }
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if ((ifc.lowerIf == null) || (ifc.stated != state.states.up)) {
                ifc.byteNeed = 0;
                continue;
            }
            long o;
            if (dynamic > 0) {
                o = ifc.byteAvrg;
            } else {
                o = ifc.lowerIf.getBandwidth();
            }
            ifc.byteNeed = (int) (o / min);
        }
    }

    /**
     * get show output
     *
     * @return list of interface
     */
    public userFormat getShowIfc() {
        userFormat lst = new userFormat("|", "interface|state|quota|report|priority");
        for (int i = 0; i < ifaces.size(); i++) {
            lst.add("" + ifaces.get(i).getShow());
        }
        return lst;
    }

    /**
     * get show output
     *
     * @return list of interface
     */
    public userFormat getShowStt() {
        userFormat lst = new userFormat("|", "parameter|state");
        lst.add("backup|" + backup);
        lst.add("selected|" + ifaces.get(selected));
        lst.add("replicate|" + replicate);
        lst.add("sequence|" + sequence);
        lst.add("dejitter|" + dejitter);
        lst.add("reporter|" + reporter);
        lst.add("dynamic|" + dynamic);
        if (peering != null) {
            lst.add("peer alive|" + peering.remoteAlive);
            lst.add("peer better|" + peering.remoteBetter);
        }
        return lst;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this bundle");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this bundle");
        l.add(null, false, 1, new int[]{-1}, "ethernet", "specify type of bundle");
        l.add(null, false, 1, new int[]{-1}, "replicate", "specify replicator mode");
        l.add(null, false, 1, new int[]{-1}, "logging", "enable logging");
        l.add(null, false, 1, new int[]{2}, "backup", "specify backup mode");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 1, new int[]{2}, "sequence", "specify sequence mode");
        l.add(null, false, 2, new int[]{-1}, "<num>", "window in packets");
        l.add(null, false, 1, new int[]{2}, "dejitter", "specify dejitter timer");
        l.add(null, false, 2, new int[]{-1}, "<num>", "dejitter in ms");
        l.add(null, false, 1, new int[]{2}, "reporter", "specify reporter timer");
        l.add(null, false, 2, new int[]{-1}, "<num>", "reports in ms");
        l.add(null, false, 1, new int[]{2}, "dynamic", "specify quota dynamically");
        l.add(null, false, 2, new int[]{-1}, "<num>", "reports to average");
        l.add(null, false, 1, new int[]{2}, "peering", "specify peering interface");
        l.add(null, false, 2, new int[]{3}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{-1}, "<num>", "local priority");
        l.add(null, false, 1, new int[]{2}, "loadbalance", "specify load balance method");
        l.add(null, false, 2, new int[]{-1}, "layer2", "xor source and destination mac");
        l.add(null, false, 2, new int[]{-1}, "layer3", "xor source and destination ip");
        l.add(null, false, 2, new int[]{-1}, "layer4", "xor source and destination port");
        l.add(null, false, 2, new int[]{-1}, "addr", "xor addresses");
        l.add(null, false, 2, new int[]{-1}, "all", "xor everything");
        l.add(null, false, 2, new int[]{-1}, "random", "randomize");
        l.add(null, false, 2, new int[]{-1}, "none", "nothing");
        l.add(null, false, 2, new int[]{-1}, "round", "round robin");
        l.add(null, false, 2, new int[]{-1}, "share", "share by bandwidth");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, notEther, beg, "ethernet", "");
        cmds.cfgLine(l, !logging, beg, "logging", "");
        cmds.cfgLine(l, backup < 1, beg, "backup", "" + backup);
        cmds.cfgLine(l, !replicate, beg, "replicate", "");
        cmds.cfgLine(l, sequence == null, beg, "sequence", "" + tabWindow.getSize(sequence));
        cmds.cfgLine(l, dejitter < 1, beg, "dejitter", "" + dejitter);
        cmds.cfgLine(l, reporter < 1, beg, "reporter", "" + reporter);
        cmds.cfgLine(l, dynamic < 1, beg, "dynamic", "" + dynamic);
        String a;
        switch (loadBalance) {
            case 0:
                a = "none";
                break;
            case 2:
            case 3:
            case 4:
                a = "layer" + loadBalance;
                break;
            case 5:
                a = "addr";
                break;
            case 7:
                a = "all";
                break;
            case 8:
                a = "random";
                break;
            case 9:
                a = "round";
                break;
            case 10:
                a = "share";
                break;
            default:
                a = "unknown=" + loadBalance;
                break;
        }
        cmds.cfgLine(l, loadBalance < 1, beg, "loadbalance", a);
        cmds.cfgLine(l, peering == null, beg, "peering", "" + peering);
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String s = cmd.word();
        if (s.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (s.equals("ethernet")) {
            setEtherMode(true);
            return;
        }
        if (s.equals("peering")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            try {
                peering.workStop();
            } catch (Exception e) {
            }
            peering = new ifcBundlePeer(this, ifc);
            peering.localId = bits.str2num(cmd.word());
            peering.workStart();
            return;
        }
        if (s.equals("loadbalance")) {
            s = cmd.word();
            if (s.equals("none")) {
                loadBalance = 0;
                return;
            }
            if (s.equals("random")) {
                loadBalance = 8;
                return;
            }
            if (s.equals("round")) {
                loadBalance = 9;
                return;
            }
            if (s.equals("share")) {
                loadBalance = 10;
                return;
            }
            if (s.equals("addr")) {
                s = "layer5";
            }
            if (s.equals("all")) {
                s = "layer7";
            }
            loadBalance = bits.str2num(s.substring(5, s.length()));
            return;
        }
        if (s.equals("replicate")) {
            replicate = true;
            return;
        }
        if (s.equals("logging")) {
            logging = true;
            return;
        }
        if (s.equals("backup")) {
            backup = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("sequence")) {
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                return;
            }
            sequence = new tabWindow<packHolder>(i);
            seqTx = 0;
            timer1 = new ifcBundleKeep(this);
            timer1.start();
            return;
        }
        if (s.equals("dejitter")) {
            dejitter = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("dynamic")) {
            dynamic = bits.str2num(cmd.word());
            allocateReports();
            propagateState();
            return;
        }
        if (s.equals("reporter")) {
            reporter = bits.str2num(cmd.word());
            timer2 = null;
            if (reporter < 1) {
                return;
            }
            timer2 = new ifcBundleCntr(this);
            timer2.start();
            return;
        }
        if (!s.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("description")) {
            description = "";
            return;
        }
        if (s.equals("ethernet")) {
            setEtherMode(false);
            return;
        }
        if (s.equals("peering")) {
            try {
                peering.workStop();
            } catch (Exception e) {
            }
            peering = null;
            return;
        }
        if (s.equals("loadbalance")) {
            loadBalance = 0;
            return;
        }
        if (s.equals("replicate")) {
            replicate = false;
            return;
        }
        if (s.equals("logging")) {
            logging = false;
            return;
        }
        if (s.equals("backup")) {
            backup = 0;
            return;
        }
        if (s.equals("sequence")) {
            sequence = null;
            timer1 = null;
            return;
        }
        if (s.equals("dejitter")) {
            dejitter = 0;
            return;
        }
        if (s.equals("dynamic")) {
            dynamic = 0;
            allocateReports();
            propagateState();
            return;
        }
        if (s.equals("reporter")) {
            reporter = 0;
            timer2 = null;
            return;
        }
        cmd.badCmd();
    }

    private void setEtherMode(boolean eth) {
        notEther = !eth;
        if (eth) {
            hwaddr = addrMac.getRandom();
        } else {
            hwaddr = new addrEmpty();
        }
    }

    /**
     * add new interface to this bundle
     *
     * @return interface handler that will pass packets to this bundle
     */
    public ifcBundleIfc newIface() {
        if (debugger.ifcBundleTraf) {
            logger.debug("add iface");
        }
        ifcBundleIfc ntry = new ifcBundleIfc(this);
        ntry.ifcNum = nextIfaceNum++;
        if (dynamic > 0) {
            ntry.byteRprt = new int[dynamic];
        }
        ifaces.add(ntry);
        propagateState();
        return ntry;
    }

    /**
     * delete one interface from this virtual bundle
     *
     * @param num inteface number
     * @return lower interface handler (null=error)
     */
    public ifcDn delIface(int num) {
        if (debugger.ifcBundleTraf) {
            logger.debug("del iface");
        }
        ifcBundleIfc ntry = new ifcBundleIfc(this);
        ntry.ifcNum = num;
        ntry = ifaces.del(ntry);
        if (ntry == null) {
            return null;
        }
        propagateState();
        return ntry.lowerIf;
    }

    /**
     * notify upper layer that packet arrived
     *
     * @param ifc source interface
     * @param pck the packet
     */
    protected void send2upper(ifcBundleIfc ifc, packHolder pck) {
        cntr.rx(pck);
        if (debugger.ifcBundleTraf) {
            logger.debug("tx upper");
        }
        if (backup > 0) {
            ifc.lastRx = bits.getTime();
            ifcBundleIfc sel = ifaces.get(selected);
            if (sel == null) {
                cntr.drop(pck, counter.reasons.noIface);
                return;
            }
            if (ifc.ifcNum != sel.ifcNum) {
                cntr.drop(pck, counter.reasons.notInTab);
                return;
            }
        }
        if (sequence != null) {
            packReplicator pckH = new packReplicator();
            if (pckH.parseHeader(pck)) {
                cntr.drop(pck, counter.reasons.badHdr);
                return;
            }
            switch (pckH.typ) {
                case packReplicator.typData:
                    break;
                case packReplicator.typKeep:
                    sequence.gotSet(pckH.seq);
                    return;
                case packReplicator.typCntr:
                    if (dynamic < 1) {
                        return;
                    }
                    ifc.byteNeed = (ifc.byteNeed + 1) % dynamic;
                    ifc.byteRprt[ifc.byteNeed] = pckH.seq;
                    int o = 0;
                    for (int i = 0; i < dynamic; i++) {
                        o += ifc.byteRprt[i];
                    }
                    ifc.byteAvrg = o;
                    propagateState();
                    return;
                default:
                    cntr.drop(pck, counter.reasons.badTyp);
                    return;
            }
            if (sequence.gotDat(pckH.seq)) {
                cntr.drop(pck, counter.reasons.badRxSeq);
                logger.info("replay check failed on " + ifc);
                return;
            }
            if (dejitter > 0) {
                pck.INTtime = bits.getTime() + dejitter;
                sequence.paySet(pckH.seq, pck.copyBytes(true, true));
                notif.wakeup();
                return;
            }
        }
        doTxUpper(pck);
    }

    public void sendPack(packHolder pck) {
        if (debugger.ifcBundleTraf) {
            logger.debug("rx upper");
        }
        cntr.tx(pck);
        pck.merge2beg();
        if (sequence != null) {
            packReplicator pckH = new packReplicator();
            pckH.typ = packReplicator.typData;
            pckH.seq = seqTx;
            seqTx++;
            pckH.createHeader(pck);
            pck.merge2beg();
        }
        if (backup > 0) {
            doTxSel(pck);
            return;
        }
        if (replicate) {
            doTxFlood(pck);
            return;
        }
        doTxNext(pck);
    }

    private int getHash(int i) {
        return (i >>> 8) ^ (i & 0xff);
    }

    /**
     * send over next link
     *
     * @param pck packet to send
     */
    protected void doTxNext(packHolder pck) {
        switch (loadBalance) {
            case 0:
                nextSender = 0;
                currSender = 0;
                break;
            case 2:
                nextSender = pck.ETHsrc.getHashB() ^ pck.ETHtrg.getHashB();
                currSender = 0;
                break;
            case 3:
                nextSender = pck.IPsrc.getHashB() ^ pck.IPtrg.getHashB();
                currSender = 0;
                break;
            case 4:
                nextSender = getHash(pck.UDPsrc ^ pck.UDPtrg);
                currSender = 0;
                break;
            case 5:
                nextSender = pck.ETHsrc.getHashB() ^ pck.ETHtrg.getHashB() ^ pck.IPsrc.getHashB() ^ pck.IPtrg.getHashB();
                currSender = 0;
                break;
            case 7:
                nextSender = pck.ETHsrc.getHashB() ^ pck.ETHtrg.getHashB() ^ pck.IPsrc.getHashB() ^ pck.IPtrg.getHashB() ^ getHash(pck.UDPsrc ^ pck.UDPtrg);
                currSender = 0;
                break;
            case 8:
                nextSender = bits.randomW();
                currSender = 0;
                break;
            case 9:
                nextSender++;
                currSender = 0;
                break;
            case 10:
                break;
        }
        int o = ifaces.size();
        nextSender %= o;
        ifcBundleIfc ifc = ifaces.get(nextSender);
        if (ifc != null) {
            if ((ifc.stated == state.states.up) && (ifc.byteNeed > currSender)) {
                currSender += pck.dataSize();
                ifc.doTxPack(pck);
                return;
            }
        }
        currSender = 0;
        for (int i = 0; i < o; i++) {
            nextSender = (nextSender + 1) % o;
            ifc = ifaces.get(nextSender);
            if (ifc == null) {
                continue;
            }
            if (ifc.stated != state.states.up) {
                continue;
            }
            currSender += pck.dataSize();
            ifc.doTxPack(pck);
            return;
        }
        cntr.drop(pck, counter.reasons.noIface);
    }

    /**
     * send over selected link
     *
     * @param pck packet to send
     */
    protected void doTxSel(packHolder pck) {
        ifcBundleIfc ifc = ifaces.get(selected);
        if (ifc == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        if (ifc.stated != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        ifc.doTxPack(pck);
    }

    /**
     * send over all links
     *
     * @param pck packet to send
     */
    protected void doTxFlood(packHolder pck) {
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                return;
            }
            if (ifc.stated != state.states.up) {
                continue;
            }
            ifc.doTxPack(pck.copyBytes(true, true));
        }
    }

    /**
     * send to upper layer
     *
     * @param pck packet to send
     */
    protected void doTxUpper(packHolder pck) {
        if (peering != null) {
            if (peering.remoteBetter) {
                peering.tx2peer(pck);
                return;
            }
        }
        if (upper == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        upper.recvPack(pck);
    }

    public String toString() {
        return "bundle " + hwaddr;
    }

    public int getMTUsize() {
        int p = 1500;
        if (backup > 0) {
            ifcBundleIfc ifc = ifaces.get(selected);
            if (ifc == null) {
                return p;
            }
            return ifc.lowerIf.getMTUsize();
        }
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.lowerIf == null) {
                continue;
            }
            int o = ifc.lowerIf.getMTUsize();
            if (p > o) {
                p = o;
            }
        }
        return p;
    }

    public long getBandwidth() {
        long p = 100000000;
        if (backup > 0) {
            ifcBundleIfc ifc = ifaces.get(selected);
            if (ifc == null) {
                return p;
            }
            return ifc.lowerIf.getBandwidth();
        }
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.lowerIf == null) {
                continue;
            }
            long o = ifc.lowerIf.getBandwidth();
            if (p > o) {
                p = o;
            }
        }
        return p;
    }

    /**
     * do startup
     */
    public void doStartup() {
        if (debugger.ifcBundleTraf) {
            logger.debug("startup");
        }
        logger.startThread(this);
    }

    /**
     * do shutdown
     */
    public void doShutdown() {
        if (debugger.ifcBundleTraf) {
            logger.debug("shutdown");
        }
        need2run = false;
        notif.wakeup();
        timer1 = null;
        timer2 = null;
        try {
            peering.workStop();
        } catch (Exception e) {
        }
    }

    /**
     * send keepalive message
     */
    protected void sendKeep() {
        if (sequence == null) {
            return;
        }
        if (debugger.ifcBundleTraf) {
            logger.debug("send keepalive");
        }
        packHolder pck = new packHolder(true, true);
        packReplicator pckH = new packReplicator();
        pckH.typ = packReplicator.typKeep;
        pckH.seq = seqTx;
        pckH.createHeader(pck);
        pck.merge2beg();
        doTxFlood(pck);
    }

    /**
     * send counter message
     */
    protected void sendCounter() {
        if (sequence == null) {
            return;
        }
        if (debugger.ifcBundleTraf) {
            logger.debug("send counter");
        }
        for (int i = 0; i < ifaces.size(); i++) {
            ifcBundleIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            packHolder pck = new packHolder(true, true);
            packReplicator pckH = new packReplicator();
            pckH.typ = packReplicator.typCntr;
            pckH.seq = ifc.byteRcvd;
            ifc.byteRcvd = 0;
            pckH.createHeader(pck);
            pck.merge2beg();
            ifc.doTxPack(pck);
        }
    }

    public void run() {
        for (;;) {
            if (!need2run) {
                return;
            }
            try {
                doWork();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

    private void doWork() {
        if (backup > 0) {
            int pri = Integer.MAX_VALUE;
            int num = -1;
            long tim = bits.getTime();
            for (int i = 0; i < ifaces.size(); i++) {
                ifcBundleIfc ifc = ifaces.get(i);
                if (ifc == null) {
                    continue;
                }
                if (ifc.stated != state.states.up) {
                    continue;
                }
                if (ifc.priority > pri) {
                    continue;
                }
                if ((tim - ifc.lastRx) > backup) {
                    continue;
                }
                num = i;
                pri = ifc.priority;
            }
            if ((num != selected) && logging) {
                logger.info("selection changed to " + ifaces.get(num));
            }
            selected = num;
            propagateState();
        }
        if ((dejitter < 1) || (sequence == null)) {
            notif.sleep(10000);
            return;
        }
        packHolder pck = null;
        int i = sequence.seq1st();
        int o = sequence.seqLst();
        for (; i <= o; i++) {
            pck = sequence.payGet(i);
            if (pck != null) {
                break;
            }
        }
        if (pck == null) {
            notif.misleep(5000);
            return;
        }
        o = (int) (pck.INTtime - bits.getTime());
        if (o > 0) {
            notif.sleep(o);
            return;
        }
        sequence.payClr(i);
        doTxUpper(pck);
    }

}

class ifcBundleKeep implements Runnable {

    private ifcBundle lower;

    public ifcBundleKeep(ifcBundle parent) {
        lower = parent;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                if (lower.timer1 != this) {
                    break;
                }
                if (!lower.need2run) {
                    break;
                }
                lower.sendKeep();
                bits.sleep(10000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ifcBundleCntr implements Runnable {

    private ifcBundle lower;

    public ifcBundleCntr(ifcBundle parent) {
        lower = parent;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                if (lower.timer2 != this) {
                    break;
                }
                if (!lower.need2run) {
                    break;
                }
                lower.sendCounter();
                bits.sleep(lower.reporter);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ifcBundlePeer implements ifcUp, Runnable {

    public boolean remoteAlive = false;

    public boolean remoteBetter = false;

    public int localId;

    private ifcBundle lower;

    private cfgIfc ifCfg;

    private ifcDn ifHnd = new ifcNull();

    private boolean need2work;

    private counter cntr = new counter();

    private long lastRx;

    public ifcBundlePeer(ifcBundle parent, cfgIfc ifc) {
        lower = parent;
        ifCfg = ifc;
    }

    public String toString() {
        return ifCfg.name + " " + localId;
    }

    public void workStop() {
        need2work = false;
        ifCfg.ethtyp.delET(packReplicator.ethTyp);
    }

    public void workStart() {
        need2work = true;
        ifCfg.ethtyp.addET(packReplicator.ethTyp, "peer", this);
        ifCfg.ethtyp.updateET(packReplicator.ethTyp, this);
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                bits.sleep(packReplicator.timeKeep);
                if (!need2work) {
                    break;
                }
                packHolder pck = new packHolder(true, true);
                pck.msbPutW(0, packReplicator.ethTyp);
                pck.putByte(2, packReplicator.typKeep);
                pck.msbPutD(3, localId);
                pck.putSkip(7);
                pck.merge2beg();
                pck.ETHsrc.setAddr(addrMac.getBroadcast());
                pck.ETHtrg.setAddr(addrMac.getBroadcast());
                ifHnd.sendPack(pck);
                boolean old = remoteAlive;
                if ((bits.getTime() - lastRx) < packReplicator.timeHold) {
                    remoteAlive = true;
                } else {
                    remoteAlive = false;
                    remoteBetter = false;
                }
                if (old == remoteAlive) {
                    continue;
                }
                if (lower.logging) {
                    logger.info("peer alive changed to " + remoteAlive);
                }
                lower.propagateState();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    public void tx2peer(packHolder pck) {
        if (!lower.notEther) {
            ifcEther.createETHheader(pck, false);
        }
        pck.msbPutW(0, packReplicator.ethTyp);
        pck.putByte(2, packReplicator.typData);
        pck.putSkip(3);
        pck.merge2beg();
        pck.ETHsrc.setAddr(addrMac.getBroadcast());
        pck.ETHtrg.setAddr(addrMac.getBroadcast());
        ifHnd.sendPack(pck);
    }

    public void recvPack(packHolder pck) {
        if (pck.msbGetW(0) != packReplicator.ethTyp) {
            return;
        }
        int i = pck.getByte(2);
        pck.getSkip(3);
        switch (i) {
            case packReplicator.typData:
                if (!lower.notEther) {
                    ifcEther.parseETHheader(pck, false);
                }
                lower.doTxUpper(pck);
                break;
            case packReplicator.typKeep:
                i = pck.msbGetD(0);
                lastRx = bits.getTime();
                boolean old = remoteBetter;
                remoteBetter = (i < localId);
                if (old == remoteBetter) {
                    break;
                }
                if (lower.logging) {
                    logger.info("peer better changed to " + remoteBetter);
                }
                lower.propagateState();
                break;
        }
    }

    public void setParent(ifcDn parent) {
        ifHnd = parent;
    }

    public void setState(states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}
