package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packUpnpFwd;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtUdp;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * upnp forwarder
 *
 * @author matecsaba
 */
public class servUpnpFwd extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servUpnpFwd() {
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server upnpfwd .*", cmds.tabulator + "port " + packUpnpFwd.portNum, null),
        new userFilter("server upnpfwd .*", cmds.tabulator + "protocol " + proto2string(protoNets + protoUdp), null),
        new userFilter("server upnpfwd .*", cmds.tabulator + "target null", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    /**
     * target address
     */
    public addrIP target;

    /**
     * purge
     */
    protected servUpnpFwdKeep purgeTimer;

    private pipeSide trgt;

    private addrIP grp = new addrIP();

    private ipFwd fwd;

    private ipFwdIface ifc;

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        new servUpnpFwdClnt(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
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
        if (!a.equals(cmds.negated)) {
            return true;
        }
        a = cmd.word();
        if (a.equals("target")) {
            target = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "target", "hub address to forward");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "address of hub");
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
        purgeTimer = null;
        if (shutdown) {
            return;
        }
        purgeTimer = new servUpnpFwdKeep(this);
        purgeTimer.start();
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
            trgt = udp.streamConnect(new pipeLine(32768, true), ifc, 0, target, srvPort, srvName(), -1, null, -1, -1);
            if (trgt == null) {
                return;
            }
            trgt.setTime(120000);
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
        pck.IPid = 0;
        pck.IPsrc.setAddr(pckF.addr);
        pck.IPtrg.setAddr(grp);
        pck.UDPsrc = pckF.port;
        pck.UDPtrg = srvPort;
        prtUdp.createUDPheader(pck);
        fwd.protoPack(ifc, null, pck);
    }

}

class servUpnpFwdKeep implements Runnable {

    private servUpnpFwd parent;

    public servUpnpFwdKeep(servUpnpFwd prnt) {
        parent = prnt;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                if (parent.purgeTimer != this) {
                    break;
                }
                parent.doKeep();
                bits.sleep(30000);
            }
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
        logger.startThread(this);
    }

    public void run() {
        try {
            pipe.wait4ready(120000);
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
        logger.startThread(this);
    }

    public void run() {
        try {
            pipe.wait4ready(120000);
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
