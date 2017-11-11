package serv;

import addr.addrIP;
import ip.ipFwd;
import ip.ipFwdIface;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packUpnpFwd;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import prt.prtUdp;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.logger;

/**
 * upnp forwarder
 *
 * @author matecsaba
 */
public class servUpnpFwd extends servGeneric implements prtServS {

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server upnpfwd .*! port " + packUpnpFwd.portNum,
        "server upnpfwd .*! protocol " + proto2string(protoNets + protoUdp),
        "server upnpfwd .*! target null"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    /**
     * target address
     */
    public addrIP target;

    private Timer purgeTimer;

    private pipeSide trgt;

    private addrIP grp = new addrIP();

    private ipFwd fwd;

    private ipFwdIface ifc;

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 10000;
        new servUpnpFwdClnt(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> lst) {
        lst.add(beg + "target " + target);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("target")) {
            target = new addrIP();
            if (target.fromString(cmd.word())) {
                target = null;
                cmd.error("bad address");
                return false;
            }
            return false;
        }
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
        if (a.equals("target")) {
            target = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  target                 hub address to forward");
        l.add("2 .    <addr>               address of hub");
    }

    public String srvName() {
        return "upnpfwd";
    }

    public int srvPort() {
        return packUpnpFwd.portNum;
    }

    public int srvProto() {
        return protoNets + protoUdp;
    }

    public boolean srvInit() {
        restartTimer(false);
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        restartTimer(true);
        return genericStop(0);
    }

    private void restartTimer(boolean shutdown) {
        if (trgt != null) {
            trgt.setClose();
        }
        try {
            purgeTimer.cancel();
        } catch (Exception e) {
        }
        purgeTimer = null;
        if (shutdown) {
            return;
        }
        purgeTimer = new Timer();
        servUpnpFwdKeep task = new servUpnpFwdKeep(this);
        purgeTimer.schedule(task, 1000, 30000);
    }

    /**
     * send keepalive
     */
    protected void doKeep() {
        boolean b = trgt == null;
        if (!b) {
            b = trgt.isClosed() != 0;
        }
        if (b) {
            if (trgt != null) {
                trgt.setClose();
            }
            trgt = null;
            if (target == null) {
                return;
            }
            if (srvVrf == null) {
                return;
            }
            if (srvIface == null) {
                return;
            }
            if (target.isIPv4()) {
                grp.fromString("239.255.255.250");
            } else {
                grp.fromString("ff02::c");
            }
            fwd = srvVrf.getFwd(target);
            ifc = srvIface.getFwdIfc(target);
            prtUdp udp = srvVrf.getUdp(target);
            if (udp == null) {
                return;
            }
            logger.info("reconnecting " + target);
            trgt = udp.streamConnect(new pipeLine(32768, true), ifc, 0, target, srvPort, srvName(), null, -1);
            if (trgt == null) {
                return;
            }
            trgt.timeout = 120000;
            new servUpnpFwdServ(this, trgt);
        }
        packHolder pckB = new packHolder(true, true);
        packUpnpFwd pckF = new packUpnpFwd();
        pckF.typ = packUpnpFwd.typKeep;
        pckF.createPacket(pckB);
        pckB.pipeSend(trgt, 0, pckB.dataSize(), 2);
    }

    /**
     * got packet from interface
     *
     * @param conn connection
     * @param pck packet
     */
    protected void doPackIfc(prtGenConn conn, packHolder pck) {
        if (trgt == null) {
            return;
        }
        packUpnpFwd pckF = new packUpnpFwd();
        pckF.typ = packUpnpFwd.typData;
        pckF.addr.setAddr(conn.peerAddr);
        pckF.port = conn.portRem;
        pckF.createPacket(pck);
        pck.pipeSend(trgt, 0, pck.dataSize(), 2);
    }

    /**
     * got packet from interface
     *
     * @param pck packet
     */
    protected void doPackSrv(packHolder pck) {
        packUpnpFwd pckF = new packUpnpFwd();
        pckF.parsePacket(pck);
        if (pckF.typ != packUpnpFwd.typData) {
            return;
        }
        pck.putDefaults();
        pck.IPttl = 2;
        pck.IPtos = 0;
        pck.IPsrc.setAddr(pckF.addr);
        pck.IPtrg.setAddr(grp);
        pck.UDPsrc = pckF.port;
        pck.UDPtrg = srvPort;
        prtUdp.createUDPheader(pck);
        fwd.protoPack(ifc, pck);
    }

}

class servUpnpFwdKeep extends TimerTask {

    private servUpnpFwd parent;

    public servUpnpFwdKeep(servUpnpFwd prnt) {
        parent = prnt;
    }

    public void run() {
        try {
            parent.doKeep();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class servUpnpFwdServ implements Runnable {

    private servUpnpFwd parent;

    private pipeSide pipe;

    public servUpnpFwdServ(servUpnpFwd prnt, pipeSide pip) {
        parent = prnt;
        pipe = pip;
        new Thread(this).start();
    }

    public void run() {
        try {
            pipe.wait4ready(0);
            for (;;) {
                packHolder pck = pipe.readPacket(true);
                if (pck == null) {
                    break;
                }
                parent.doPackSrv(pck);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}

class servUpnpFwdClnt implements Runnable {

    private servUpnpFwd parent;

    private pipeSide pipe;

    private prtGenConn conn;

    public servUpnpFwdClnt(servUpnpFwd prnt, pipeSide pip, prtGenConn id) {
        parent = prnt;
        pipe = pip;
        conn = id;
        new Thread(this).start();
    }

    public void run() {
        try {
            pipe.wait4ready(0);
            for (;;) {
                packHolder pck = pipe.readPacket(true);
                if (pck == null) {
                    break;
                }
                parent.doPackIfc(conn, pck);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
