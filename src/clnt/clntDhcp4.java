package clnt;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrMac;
import cfg.cfgIfc;
import ifc.ifcEthTyp;
import ip.ipFwdIface;
import java.util.List;
import pack.packDhcp4;
import pack.packHolder;
import prt.prtGen;
import prt.prtGenConn;
import prt.prtServP;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * dynamic host config protocol (rfc2131) client
 *
 * @author matecsaba
 */
public class clntDhcp4 implements prtServP {

    /**
     * early mode
     */
    public boolean earlyMode = false;

    /**
     * broadcast mode
     */
    public boolean broadcastMode = true;

    /**
     * minimum lease time
     */
    public int leaseMin = 60;

    /**
     * maximum lease time
     */
    public int leaseMax = 7200;

    /**
     * config class
     */
    public cfgIfc cfger;

    /**
     * my address
     */
    public addrIPv4 locAddr;

    /**
     * my netmask
     */
    public addrIPv4 locMask;

    /**
     * gw address
     */
    public addrIPv4 gwAddr;

    /**
     * dhcp server address
     */
    public addrIPv4 dhcpAddr;

    /**
     * dns1 address
     */
    public addrIPv4 dns1addr;

    /**
     * dns2 address
     */
    public addrIPv4 dns2addr;

    private prtGen lower;

    private prtGenConn sender;

    private ipFwdIface iface;

    private ifcEthTyp ethtyp;

    private int lastId;

    private long lastTime;

    private int lastStat; // 1-running, 2-txDiscovery, 4-txRequest

    private int lastSent;

    private int leaseTime;

    /**
     * create new dhcp client on interface
     *
     * @param wrkr udp worker
     * @param ifc forwarder interface
     * @param phy handler of interface
     * @param cfg config interface
     */
    public clntDhcp4(prtGen wrkr, ipFwdIface ifc, ifcEthTyp phy, cfgIfc cfg) {
        lower = wrkr;
        iface = ifc;
        ethtyp = phy;
        cfger = cfg;
        clearState();
        socketBind();
        if (debugger.clntDhcp4traf) {
            logger.debug("started");
        }
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     * @param cmd command
     */
    public void getConfig(List<String> l, String beg, String cmd) {
        cmds.cfgLine(l, !broadcastMode, beg, cmd + "broadcast", "");
        cmds.cfgLine(l, !earlyMode, beg, cmd + "early", "");
        l.add(beg + cmd + "renew-min " + leaseMin);
        l.add(beg + cmd + "renew-max " + leaseMax);
    }

    /**
     * do configuration
     *
     * @param a command
     * @param cmd commands
     * @return result code, true on error, false on success
     */
    public boolean doConfig(String a, cmds cmd) {
        if (a.equals("broadcast")) {
            broadcastMode = true;
            return false;
        }
        if (a.equals("early")) {
            earlyMode = true;
            return false;
        }
        if (a.equals("renew-min")) {
            leaseMin = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("renew-max")) {
            leaseMax = bits.str2num(cmd.word());
            return false;
        }
        return true;
    }

    /**
     * undo configuration
     *
     * @param a command
     * @return result code, true on error, false on success
     */
    public boolean unConfig(String a) {
        if (a.equals("broadcast")) {
            broadcastMode = false;
            return false;
        }
        if (a.equals("early")) {
            earlyMode = false;
            return false;
        }
        return true;
    }

    /**
     * stop client
     */
    public void closeClient() {
        clearState();
        socketUnbind();
        if (debugger.clntDhcp4traf) {
            logger.debug("stopped");
        }
    }

    private void clearState() {
        locAddr = addrIPv4.getEmpty();
        locMask = addrIPv4.getBroadcast();
        gwAddr = addrIPv4.getEmpty();
        dhcpAddr = addrIPv4.getEmpty();
        dns1addr = addrIPv4.getEmpty();
        dns2addr = addrIPv4.getEmpty();
        lastStat = 1;
        lastId = bits.randomD();
        lastTime = 0;
        lastSent = 0;
    }

    private void socketBind() {
        if (lower.packetListen(this, iface, packDhcp4.portCnum, null, 0, packDhcp4.portSnum, "dhcp4c", null, -1)) {
            logger.info("failed to bind");
        }
        addrIP adr = new addrIP();
        adr.fromIPv4addr(addrIPv4.getBroadcast());
        sender = lower.packetConnect(this, iface, packDhcp4.portCnum, adr, packDhcp4.portSnum, "dhcp4c", null, -1);
        if (sender == null) {
            logger.info("failed to connect");
        } else {
            sender.timeout = 0;
        }
    }

    private void socketUnbind() {
        lower.listenStop(iface, packDhcp4.portCnum, null, 0, packDhcp4.portSnum);
        if (sender != null) {
            sender.setClosing();
        }
        sender = null;
    }

    /**
     * closed interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept datagram
     *
     * @param id connection
     * @return false on success, true on error
     */
    public boolean datagramAccept(prtGenConn id) {
        if (debugger.clntDhcp4traf) {
            logger.debug("accept " + id);
        }
        id.timeout = 5000;
        return false;
    }

    /**
     * close connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
        if (debugger.clntDhcp4traf) {
            logger.debug("close " + id);
        }
    }

    /**
     * set ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
        if (debugger.clntDhcp4traf) {
            logger.debug("ready " + id);
        }
        id.workInterval = 5000;
    }

    /**
     * work round
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        if (debugger.clntDhcp4traf) {
            logger.debug("work " + id);
        }
        sendKeepalive();
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (debugger.clntDhcp4traf) {
            logger.debug("rx " + id);
        }
        packDhcp4 pckd = new packDhcp4();
        if (pckd.parseHeader(pck, false)) {
            return false;
        }
        if (debugger.clntDhcp4traf) {
            logger.debug("rx " + pckd);
        }
        if (pckd.bootpOp != packDhcp4.bootpOpReply) {
            return false;
        }
        switch (pckd.dhcpOp) {
            case packDhcp4.dhcpOpOffer:
                if (lastStat != 1) {
                    return false;
                }
                if (pckd.bootpXid != lastId) {
                    return false;
                }
                if (locAddr == null) {
                    return false;
                }
                dhcpAddr = pckd.dhcpServer;
                locAddr = pckd.bootpYiaddr;
                locMask = pckd.dhcpNetMask;
                gwAddr = pckd.dhcpGateway;
                dns1addr = pckd.dhcpDns1srv;
                dns2addr = pckd.dhcpDns2srv;
                leaseTime = pckd.dhcpLeaseTime;
                if (leaseTime > leaseMax) {
                    leaseTime = leaseMax;
                }
                if (leaseTime < leaseMin) {
                    leaseTime = leaseMin;
                }
                lastStat |= 2;
                if (earlyMode) {
                    cfger.addr4changed(locAddr, locMask, gwAddr);
                }
                lastId++;
                break;
            case packDhcp4.dhcpOpAck:
                if (lastStat != 3) {
                    return false;
                }
                if (pckd.bootpXid != lastId) {
                    return false;
                }
                lastStat |= 4;
                if (!earlyMode) {
                    cfger.addr4changed(locAddr, locMask, gwAddr);
                }
                break;
        }
        return false;
    }

    private void sendDiscovery() {
        if ((bits.getTime() - lastTime) < 1000) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        packDhcp4 pckd = new packDhcp4();
        pckd.bootpOp = packDhcp4.bootpOpRequest;
        pckd.bootpXid = lastId;
        pckd.bootpBroadcast = broadcastMode;
        pckd.bootpChaddr = (addrMac) ethtyp.getHwAddr().copyBytes();
        pckd.dhcpOp = packDhcp4.dhcpOpDiscover;
        pckd.putParamReqList();
        pckd.dhcpClientId = true;
        pckd.createHeader(pck, null);
        sender.send2net(pck);
        lastTime = bits.getTime();
        if (debugger.clntDhcp4traf) {
            logger.debug("tx " + sender + " " + pckd);
        }
    }

    private void sendRequest() {
        if ((bits.getTime() - lastTime) < 1000) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        packDhcp4 pckd = new packDhcp4();
        pckd.bootpOp = packDhcp4.bootpOpRequest;
        pckd.bootpXid = lastId;
        pckd.bootpBroadcast = broadcastMode;
        pckd.bootpChaddr = (addrMac) ethtyp.getHwAddr().copyBytes();
        pckd.dhcpOp = packDhcp4.dhcpOpRequest;
        pckd.putParamReqList();
        pckd.dhcpClientId = true;
        pckd.bootpCiaddr = locAddr.copyBytes();
        pckd.dhcpRequested = locAddr.copyBytes();
        pckd.createHeader(pck, null);
        sender.send2net(pck);
        lastTime = bits.getTime();
        if (debugger.clntDhcp4traf) {
            logger.debug("tx " + sender + " " + pckd);
        }
    }

    private void sendKeepalive() {
        switch (lastStat) {
            case 1: // sending discovery
                if (lastSent++ > 32) {
                    clearState();
                    break;
                }
                cfger.addr4changed(locAddr, locMask, gwAddr);
                sendDiscovery();
                break;
            case 3: // sending request
                if (lastSent++ > 16) {
                    clearState();
                    break;
                }
                sendRequest();
                break;
            case 7: // allocated
                if ((bits.getTime() - lastTime) < (leaseTime * 700)) {
                    break;
                }
                if (debugger.clntDhcp4traf) {
                    logger.debug("renewing address");
                }
                lastStat = 3;
                lastSent = 0;
                break;
            default:
                clearState();
                break;
        }
    }

}
