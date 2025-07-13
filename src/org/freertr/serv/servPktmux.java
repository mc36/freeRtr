package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgProxy;
import org.freertr.clnt.clntProxy;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * pktmux server
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
    public final static int port = 9081;

    /**
     * cpuport interface
     */
    protected ifcEthTyp cpuport = null;

    /**
     * data interfaces
     */
    protected tabGen<servPktmuxPort> ports = new tabGen<servPktmuxPort>();

    /**
     * control connections
     */
    protected tabGen<servPktmuxConn> conns = new tabGen<servPktmuxConn>();

    /**
     * counter
     */
    protected counter cntr = new counter();

    private ifcDn cpuprt;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server pktmux .*", cmds.tabulator + "port " + port, null),
        new userFilter("server pktmux .*", cmds.tabulator + "protocol " + proto2string(protoAll), null)
    };

    public userFilter[] srvDefFlt() {
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
        for (int i = 0; i < conns.size(); i++) {
            l.add(beg + "controller " + conns.get(i));
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("cpuport")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            cpuport = ifc.ethtyp;
            cpuport.addET(-1, "pktmux", this);
            cpuport.updateET(-1, this);
            cpuprt.setFilter(true);
            return false;
        }
        if (s.equals("controller")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            servPktmuxConn ntry = new servPktmuxConn(this, prx.proxy, adr, bits.str2num(cmd.word()));
            if (conns.add(ntry) != null) {
                return false;
            }
            ntry.startWork();
            return false;
        }
        if (s.equals("dataport")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            servPktmuxPort ntry = new servPktmuxPort(this, bits.str2num(cmd.word()));
            ntry.ifc = ifc.ethtyp;
            ntry.ifc.addET(-1, "pktmux", ntry);
            ntry.ifc.updateET(-1, ntry);
            ntry.parent.setFilter(true);
            ports.add(ntry);
            return false;
        }
        if (!s.equals(cmds.negated)) {
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
        if (s.equals("controller")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            servPktmuxConn ntry = new servPktmuxConn(this, prx.proxy, adr, bits.str2num(cmd.word()));
            ntry = conns.del(ntry);
            if (ntry == null) {
                cmd.error("no such controller");
                return false;
            }
            ntry.stopWork();
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "cpuport", "specify port to for packetin");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "interface name");
        l.add(null, false, 1, new int[]{2}, "dataport", "specify port to for packetout");
        l.add(null, false, 2, new int[]{3}, "<name:ifc>", "interface name");
        l.add(null, false, 3, new int[]{-1}, "<num>", "interface number");
        l.add(null, false, 1, new int[]{2}, "controller", "specify controller to connect");
        l.add(null, false, 2, new int[]{3}, "<name:prx>", "proxy to use");
        l.add(null, false, 3, new int[]{4}, "<addr>", "peer address");
        l.add(null, false, 4, new int[]{-1}, "<num>", "peer port");
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
        pck.msbPutD(0, id);
        pck.putSkip(4);
        pck.merge2beg();
        ifcEther.parseETHheader(pck, false);
        cpuprt.sendPack(pck);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        ifcEther.createETHheader(pck, false);
        int id = pck.msbGetD(0);
        pck.getSkip(4);
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

class servPktmuxConn implements Runnable, Comparable<servPktmuxConn> {

    public final servPktmux lower;

    public final clntProxy proxy;

    public final addrIP addr;

    public final int port;

    private boolean need2work;

    public servPktmuxConn(servPktmux l, clntProxy c, addrIP a, int p) {
        lower = l;
        proxy = c;
        addr = a;
        port = p;
    }

    public void startWork() {
        need2work = true;
        new Thread(this).start();
    }

    public void stopWork() {
        need2work = false;
    }

    public String toString() {
        return proxy + " " + addr + " " + port;
    }

    public int compareTo(servPktmuxConn o) {
        if (port < o.port) {
            return -1;
        }
        if (port > o.port) {
            return +1;
        }
        return addr.compareTo(o.addr);
    }

    public void doRound() {
        bits.sleep(1000);
        pipeSide pipe = proxy.doConnect(servGeneric.protoTcp, addr, port, "pktmux");
        if (pipe == null) {
            return;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.setTime(120000);
        pipe.linePut("platform pktmux");
        pipe.linePut("capabilities nohw");
        for (int i = 0; i < lower.ports.size(); i++) {
            servPktmuxPort ntry = lower.ports.get(i);
            pipe.linePut("portname " + ntry.id + " pktmux-port" + ntry.id);
        }
        pipe.linePut("dynrange 1024 1073741823");
        pipe.linePut("vrfrange 1 1073741823");
        pipe.linePut("neirange 4096 1073741823");
        pipe.linePut("nomore");
        for (int i = 0; i < lower.ports.size(); i++) {
            servPktmuxPort ntry = lower.ports.get(i);
            pipe.linePut("state " + ntry.id + " 1");
        }
        for (;;) {
            pipe.lineGet(0x11);
            if (pipe.isClosed() != 0) {
                break;
            }
        }
        pipe.setClose();
    }

    public void run() {
        try {
            for (;;) {
                if (!need2work) {
                    break;
                }
                doRound();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class servPktmuxPort implements Comparable<servPktmuxPort>, ifcUp {

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

    public int compareTo(servPktmuxPort o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
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
