package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * p4lang emulator
 *
 * @author matecsaba
 */
public class servPktmux extends servGeneric implements ifcUp, prtServS {

    /**
     * create instance
     */
    public servPktmux() {
    }

    /**
     * port number
     */
    public static final int port = 9081;

    /**
     * cpuport interface
     */
    protected ifcEthTyp cpuport = null;

    /**
     * data interfaces
     */
    protected tabGen<servPktmuxPort> ports = new tabGen<servPktmuxPort>();

    /**
     * counter
     */
    protected counter cntr = new counter();

    private ifcDn cpuprt;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server pktmux .*! port " + port,
        "server pktmux .*! protocol " + proto2string(protoAll)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        return true;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, cpuport == null, beg, "cpuport", "" + cpuport);
        for (int i = 0; i < ports.size(); i++) {
            l.add(beg + "dataport " + ports.get(i));
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("cpuport")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            cpuport = ifc.ethtyp;
            cpuport.addET(-1, "pktmux", this);
            cpuport.updateET(-1, this);
            return false;
        }
        if (s.equals("dataport")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            servPktmuxPort ntry = new servPktmuxPort(this, bits.str2num(cmd.word()));
            ntry.ifc = ifc.ethtyp;
            ntry.ifc.addET(-1, "p4lang", ntry);
            ntry.ifc.updateET(-1, ntry);
            ports.add(ntry);
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
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
            servPktmuxPort ntry = new servPktmuxPort(this, bits.str2num(cmd.word()));
            ntry = ports.del(ntry);
            if (ntry == null) {
                cmd.error("no such downlink");
                return false;
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  cpuport                   specify port to for packetin");
        l.add(null, "2 .    <name>                  interface name");
        l.add(null, "1 2  dataport                  specify port to for packetout");
        l.add(null, "2 3    <name>                  interface name");
        l.add(null, "3 .      <num>                 interface number");
    }

    public String srvName() {
        return "pktmux";
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
        ifcEther.createETHheader(pck, false);
        if (cpuprt == null) {
            return;
        }
        pck.msbPutW(0, id);
        pck.putSkip(2);
        pck.merge2beg();
        ifcEther.parseETHheader(pck, false);
        cpuprt.sendPack(pck);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        ifcEther.createETHheader(pck, false);
        int id = pck.msbGetW(0);
        pck.getSkip(2);
        ifcEther.parseETHheader(pck, false);
        servPktmuxPort ntry = new servPktmuxPort(this, id);
        ntry = ports.find(ntry);
        if (ntry == null) {
            return;
        }
        ntry.parent.sendPack(pck);
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

class servPktmuxPort implements Comparator<servPktmuxPort>, ifcUp {

    public final int id;

    public final servPktmux lower;

    public counter cntr = new counter();

    public ifcDn parent;

    public ifcEthTyp ifc;

    public servPktmuxPort(servPktmux l, int i) {
        lower = l;
        id = i;
    }

    public String toString() {
        return ifc + " " + id;
    }

    public int compare(servPktmuxPort o1, servPktmuxPort o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    public void recvPack(packHolder pck) {
        lower.sendPack(id, pck);
    }

    public void setParent(ifcDn prnt) {
        parent = prnt;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}
