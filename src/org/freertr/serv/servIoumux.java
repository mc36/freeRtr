package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * ioumux server
 *
 * @author matecsaba
 */
public class servIoumux extends servGeneric implements ifcUp, prtServS {

    /**
     * create instance
     */
    public servIoumux() {
    }

    /**
     * port number
     */
    public final static int port = 9082;

    /**
     * cpuport interface
     */
    protected ifcEthTyp cpuport = null;

    /**
     * data interfaces
     */
    protected tabGen<servIoumuxPort> ports = new tabGen<servIoumuxPort>();

    /**
     * local node id
     */
    public int nodeLoc;

    /**
     * remote node id
     */
    public int nodeRem;

    /**
     * counter
     */
    protected counter cntr = new counter();

    private ifcDn cpuprt;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server ioumux .*", cmds.tabulator + "port " + port, null),
        new userFilter("server ioumux .*", cmds.tabulator + "protocol " + proto2string(protoAll), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        return true;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, cpuport == null, beg, "cpuport", "" + cpuport);
        l.add(beg + "local " + nodeLoc);
        l.add(beg + "remote " + nodeRem);
        for (int i = 0; i < ports.size(); i++) {
            l.add(beg + "dataport " + ports.get(i).getConfig());
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("local")) {
            nodeLoc = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("remote")) {
            nodeRem = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("cpuport")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            cpuport = ifc.ethtyp;
            cpuport.addET(-1, "ioumux", this);
            cpuport.updateET(-1, this);
            cpuprt.setFilter(true);
            return false;
        }
        if (s.equals("dataport")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            if ((ifc.type != tabRouteIface.ifaceType.sdn) && (ifc.type != tabRouteIface.ifaceType.dialer)) {
                cmd.error("not sdn nor dialer interface");
                return false;
            }
            s = cmd.word();
            servIoumuxPort ntry = new servIoumuxPort(this, servIoumuxPort.slotPort2id(s, cmd.word()));
            ntry.eth = ifc.type == tabRouteIface.ifaceType.sdn;
            ntry.ifc = ifc.ethtyp;
            if (ntry.eth) {
                ntry.setUpper(ifc.ethtyp);
            } else {
                ntry.setUpper(ifc.getEncapProto());
            }
            ports.add(ntry);
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("local")) {
            nodeLoc = 0;
            return false;
        }
        if (s.equals("remote")) {
            nodeRem = 0;
            return false;
        }
        if (s.equals("cpuport")) {
            if (cpuport == null) {
                return false;
            }
            cpuport.delET(-1);
            cpuport = null;
            return false;
        }
        if (s.equals("dataport")) {
            cmd.word();
            s = cmd.word();
            servIoumuxPort ntry = new servIoumuxPort(this, servIoumuxPort.slotPort2id(s, cmd.word()));
            ntry = ports.del(ntry);
            if (ntry == null) {
                cmd.error("no such downlink");
                return false;
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "cpuport", "specify port to for packetin");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "interface name");
        l.add(null, false, 1, new int[]{2}, "dataport", "specify port to for packetout");
        l.add(null, false, 2, new int[]{3}, "<name:ifc>", "interface name");
        l.add(null, false, 3, new int[]{4}, "<num>", "slot number");
        l.add(null, false, 4, new int[]{-1}, "<num>", "port number");
        l.add(null, false, 1, new int[]{2}, "local", "specify local node number");
        l.add(null, false, 2, new int[]{-1}, "<num>", "node number");
        l.add(null, false, 1, new int[]{2}, "remote", "specify remote node number");
        l.add(null, false, 2, new int[]{-1}, "<num>", "node number");
    }

    public String srvName() {
        return "ioumux";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAll;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    /**
     * send packet
     *
     * @param id id
     * @param pck binary
     */
    public void sendPack(int id, packHolder pck) {
        cntr.rx(pck);
        if (cpuprt == null) {
            return;
        }
        pck.msbPutW(0, nodeRem); // target node
        pck.msbPutW(2, nodeLoc); // source node
        pck.putByte(4, id); // target port
        pck.putByte(5, id); // source port
        pck.putByte(6, 1); // type
        pck.putByte(7, 0); // channel
        pck.putSkip(8);
        pck.merge2beg();
        cpuprt.sendPack(pck);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != nodeLoc) { // target node
            cntr.drop(pck, counter.reasons.badTrgAddr);
            return;
        }
        if (pck.msbGetW(2) != nodeRem) { // source node
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        int id = pck.getByte(4); // target port
        if (id != pck.getByte(5)) { // source port
            cntr.drop(pck, counter.reasons.badSrcPort);
            return;
        }
        if (pck.getByte(6) != 1) { // type
            cntr.drop(pck, counter.reasons.badTyp);
            return;
        }
        if (pck.getByte(7) != 0) { // channel
            cntr.drop(pck, counter.reasons.badCod);
            return;
        }
        pck.getSkip(8);
        servIoumuxPort ntry = new servIoumuxPort(this, id);
        ntry = ports.find(ntry);
        if (ntry == null) {
            cntr.drop(pck, counter.reasons.badTrgPort);
            return;
        }
        ntry.recvPack(pck);
    }

    public void setParent(ifcDn parent) {
        cpuprt = parent;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}

class servIoumuxPort implements Comparable<servIoumuxPort>, ifcDn {

    public final int id;

    public final servIoumux lower;

    public counter cntr = new counter();

    protected ifcUp upper = new ifcNull();

    protected ifcEthTyp ifc;

    public boolean eth;

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public servIoumuxPort(servIoumux l, int i) {
        lower = l;
        id = i;
    }

    public static int slotPort2id(String s, String p) {
        return (bits.str2num(s) & 0xf) | (bits.str2num(p) << 4);
    }

    public String toString() {
        return "ioumux port " + id;
    }

    public String getConfig() {
        return ifc + " " + (id & 0xf) + " " + (id >>> 4);
    }

    public int compareTo(servIoumuxPort o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        return 0;
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (eth) {
            ifcEther.createETHheader(pck, false);
        }
        lower.sendPack(id, pck);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (eth) {
            ifcEther.parseETHheader(pck, false);
        }
        upper.recvPack(pck);
    }

}
