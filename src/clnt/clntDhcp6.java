package clnt;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import cfg.cfgIfc;
import ifc.ifcEthTyp;
import ip.ipFwdIface;
import java.util.List;
import pack.packDhcp6;
import pack.packHolder;
import prt.prtGen;
import prt.prtGenConn;
import prt.prtServP;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * dynamic host config protocol (rfc3315) client
 *
 * @author matecsaba
 */
public class clntDhcp6 implements prtServP {

    /**
     * early mode
     */
    public boolean earlyMode = false;

    /**
     * prefix mode
     */
    public boolean prefixMode = false;

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
     * my mac
     */
    public addrMac locMac;

    /**
     * my address
     */
    public addrIPv6 locAddr;

    /**
     * my netmask
     */
    public addrIPv6 locMask;

    /**
     * gw address
     */
    public addrIPv6 gwAddr;

    /**
     * dns1 address
     */
    public addrIPv6 dns1addr;

    /**
     * dns2 address
     */
    public addrIPv6 dns2addr;

    /**
     * server id
     */
    public byte[] servId;

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
    public clntDhcp6(prtGen wrkr, ipFwdIface ifc, ifcEthTyp phy, cfgIfc cfg) {
        lower = wrkr;
        iface = ifc;
        ethtyp = phy;
        cfger = cfg;
        clearState();
        socketBind();
        if (debugger.clntDhcp6traf) {
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
        cmds.cfgLine(l, !prefixMode, beg, cmd + "prefix", "");
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
        if (a.equals("prefix")) {
            prefixMode = true;
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
        if (a.equals("prefix")) {
            prefixMode = false;
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
        if (debugger.clntDhcp6traf) {
            logger.debug("stopped");
        }
    }

    private void clearState() {
        try {
            locMac = (addrMac) ethtyp.getHwAddr();
        } catch (Exception e) {
            locMac = addrMac.getRandom();
        }
        locAddr = addrIPv6.genLinkLocal(locMac);
        locMask = new addrIPv6();
        locMask.fromString("ffff:ffff:ffff:ffff::");
        gwAddr = addrIPv6.getEmpty();
        dns1addr = addrIPv6.getEmpty();
        dns2addr = addrIPv6.getEmpty();
        lastStat = 1;
        lastId = bits.randomD() & 0xffffff;
        lastTime = 0;
        lastSent = 0;
    }

    private void socketBind() {
        if (lower.packetListen(this, iface, packDhcp6.portCnum, null, 0, packDhcp6.portSnum, "dhcp6c", null, -1)) {
            logger.info("failed to bind");
        }
        addrIP adr = new addrIP();
        adr.fromString("ff02::1:2");
        sender = lower.packetConnect(this, iface, packDhcp6.portCnum, adr, packDhcp6.portSnum, "dhcp6c", null, -1);
        if (sender == null) {
            logger.info("failed to connect");
        } else {
            sender.timeout = 0;
        }
    }

    private void socketUnbind() {
        lower.listenStop(iface, packDhcp6.portCnum, null, 0, packDhcp6.portSnum);
        if (sender != null) {
            sender.setClosing();
        }
        sender = null;
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept connection
     *
     * @param id connection
     * @return false on success, true on error
     */
    public boolean datagramAccept(prtGenConn id) {
        if (debugger.clntDhcp6traf) {
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
        if (debugger.clntDhcp6traf) {
            logger.debug("close " + id);
        }
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
        if (debugger.clntDhcp6traf) {
            logger.debug("ready " + id);
        }
        id.workInterval = 5000;
    }

    /**
     * connection work
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        if (debugger.clntDhcp6traf) {
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
        if (debugger.clntDhcp6traf) {
            logger.debug("rx " + id);
        }
        packDhcp6 pckd = new packDhcp6();
        if (pckd.parsePacket(pck)) {
            return false;
        }
        if (debugger.clntDhcp6traf) {
            logger.debug("rx " + pckd);
        }
        switch (pckd.msgTyp) {
            case packDhcp6.typAdvertise:
                if (lastStat != 1) {
                    return false;
                }
                if (pckd.msgId != lastId) {
                    return false;
                }
                if (locAddr == null) {
                    return false;
                }
                if (pckd.ipaddr == null) {
                    return false;
                }
                locAddr = pckd.ipaddr;
                locMask.fromNetmask(pckd.ipsize);
                gwAddr = pck.IPsrc.toIPv6();
                servId = pckd.servId;
                dns1addr = pckd.dns1srv;
                dns2addr = pckd.dns2srv;
                leaseTime = pckd.lifetimV;
                if (leaseTime > leaseMax) {
                    leaseTime = leaseMax;
                }
                if (leaseTime < leaseMin) {
                    leaseTime = leaseMin;
                }
                lastStat |= 2;
                if (earlyMode) {
                    cfger.addr6changed(locAddr, locMask, gwAddr);
                }
                lastId++;
                break;
            case packDhcp6.typReply:
                if (lastStat != 3) {
                    return false;
                }
                if (pckd.msgId != lastId) {
                    return false;
                }
                lastStat |= 4;
                if (!earlyMode) {
                    cfger.addr6changed(locAddr, locMask, gwAddr);
                }
                break;
        }
        return false;
    }

    private void sendSolicit() {
        if ((bits.getTime() - lastTime) < 1000) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        packDhcp6 pckd = new packDhcp6();
        pckd.msgTyp = packDhcp6.typSolicit;
        pckd.msgId = lastId;
        pckd.clntId = packDhcp6.encodeDUID(locMac);
        pckd.clntTime = 0;
        if (prefixMode) {
            pckd.iamod = 3;
        } else {
            pckd.iamod = 2;
        }
        pckd.putOptionsReqList();
        pckd.createPacket(pck, null);
        sender.send2net(pck);
        lastTime = bits.getTime();
        if (debugger.clntDhcp6traf) {
            logger.debug("tx " + sender + " " + pckd);
        }
    }

    private void sendRequest() {
        if ((bits.getTime() - lastTime) < 1000) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        packDhcp6 pckd = new packDhcp6();
        pckd.msgTyp = packDhcp6.typRequest;
        pckd.msgId = lastId;
        pckd.clntId = packDhcp6.encodeDUID(locMac);
        pckd.servId = servId;
        pckd.clntTime = 0;
        if (prefixMode) {
            pckd.iamod = 3;
        } else {
            pckd.iamod = 2;
        }
        pckd.ipaddr = locAddr.copyBytes();
        pckd.ipsize = locMask.toNetmask();
        pckd.lifetimP = leaseTime;
        pckd.lifetimV = leaseTime;
        pckd.putOptionsReqList();
        pckd.createPacket(pck, null);
        sender.send2net(pck);
        lastTime = bits.getTime();
        if (debugger.clntDhcp6traf) {
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
                cfger.addr6changed(locAddr, locMask, gwAddr);
                sendSolicit();
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
                if (debugger.clntDhcp6traf) {
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
