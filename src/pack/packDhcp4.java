package pack;

import util.bits;
import util.typLenVal;
import addr.addrIPv4;
import addr.addrMac;
import tab.tabGen;

/**
 * dynamic host config protocol (rfc2131) packet
 *
 * @author matecsaba
 */
public class packDhcp4 {

    /**
     * default client port number
     */
    public final static int portCnum = 68;

    /**
     * default server port number
     */
    public final static int portSnum = 67;

    /**
     * size of bootp header without magic cookie and options
     */
    public final static int size1 = 236;

    /**
     * size of dhcp packet with magic cookie and options
     */
    public final static int size2 = 300;

    /**
     * magic cookie
     */
    public final static int magic = 0x63825363;

    /**
     * bootp request
     */
    public final static int bootpOpRequest = 1;

    /**
     * bootp reply
     */
    public final static int bootpOpReply = 2;

    /**
     * dhcp discover
     */
    public final static int dhcpOpDiscover = 1;

    /**
     * dhcp offer
     */
    public final static int dhcpOpOffer = 2;

    /**
     * dhcp request
     */
    public final static int dhcpOpRequest = 3;

    /**
     * dhcp decline
     */
    public final static int dhcpOpDecline = 4;

    /**
     * dhcp acknowledgement
     */
    public final static int dhcpOpAck = 5;

    /**
     * dhcp negative acknowledgement
     */
    public final static int dhcpOpNak = 6;

    /**
     * dhcp release
     */
    public final static int dhcpOpRelease = 7;

    /**
     * dhcp inform
     */
    public final static int dhcpOpInform = 8;

    /**
     * message op code
     */
    public int bootpOp;

    /**
     * dhcp relay hop count
     */
    public int bootpHops;

    /**
     * random transaction id
     */
    public int bootpXid;

    /**
     * seconds since client tries
     */
    public int bootpSecs;

    /**
     * need broadcast reply
     */
    public boolean bootpBroadcast;

    /**
     * client ip address
     */
    public addrIPv4 bootpCiaddr = new addrIPv4();

    /**
     * your ip address
     */
    public addrIPv4 bootpYiaddr = new addrIPv4();

    /**
     * server ip address
     */
    public addrIPv4 bootpSiaddr = new addrIPv4();

    /**
     * relay gateway ip address
     */
    public addrIPv4 bootpGiaddr = new addrIPv4();

    /**
     * client hardware addreess
     */
    public addrMac bootpChaddr = new addrMac();

    /**
     * server name
     */
    public String bootpSname;

    /**
     * server file
     */
    public String bootpSfile;

    /**
     * message op code
     */
    public int dhcpOp;

    /**
     * dhcp server identifier
     */
    public addrIPv4 dhcpServer;

    /**
     * dhcp dddress requested
     */
    public addrIPv4 dhcpRequested;

    /**
     * put client id
     */
    public boolean dhcpClientId;

    /**
     * name of host
     */
    public String dhcpHostName;

    /**
     * domain name
     */
    public String dhcpDomainName;

    /**
     * parameter request list
     */
    public byte[] dhcpParamReqList;

    /**
     * lease time of address
     */
    public int dhcpLeaseTime;

    /**
     * renewal time of address
     */
    public int dhcpRenewTime;

    /**
     * subnet mask
     */
    public addrIPv4 dhcpNetMask;

    /**
     * gateway address
     */
    public addrIPv4 dhcpGateway;

    /**
     * dns server address
     */
    public addrIPv4 dhcpDns1srv;

    /**
     * dns server address
     */
    public addrIPv4 dhcpDns2srv;

    private typLenVal tlv = new typLenVal(0, 8, 8, 8, 1, 0, 2, 1, 0, 512, true);

    private static int hwType = 1;

    private int flagBrdcst = 0x8000;

    /**
     * convert bootp opcode to string
     *
     * @param i opcode
     * @return string representing opcode
     */
    public static String bootpOp2str(int i) {
        switch (i) {
            case bootpOpRequest:
                return "req";
            case bootpOpReply:
                return "rep";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert dhcp opcode to string
     *
     * @param i opcode
     * @return string representing opcode
     */
    public static String dhcpOp2str(int i) {
        switch (i) {
            case dhcpOpDiscover:
                return "discover";
            case dhcpOpOffer:
                return "offer";
            case dhcpOpRequest:
                return "request";
            case dhcpOpDecline:
                return "decline";
            case dhcpOpAck:
                return "ack";
            case dhcpOpNak:
                return "nak";
            case dhcpOpRelease:
                return "release";
            case dhcpOpInform:
                return "inform";
            default:
                return "unknown=" + i;
        }
    }

    public String toString() {
        return "op=" + bootpOp2str(bootpOp) + " sec=" + bootpSecs + " cia=" + bootpCiaddr + " yia=" + bootpYiaddr + " sia=" + bootpSiaddr + " gia=" + bootpGiaddr + " cha=" + bootpChaddr + " srv=" + bootpSname + " fil=" + bootpSfile + " op=" + dhcpOp2str(dhcpOp) + " dhcpsrv=" + dhcpServer + " hstnm=" + dhcpHostName + " dom=" + dhcpDomainName + " lease=" + dhcpLeaseTime + " renew=" + dhcpRenewTime + " mask=" + dhcpNetMask + " gw=" + dhcpGateway + " dns1=" + dhcpDns1srv + " dns2=" + dhcpDns2srv + " req=" + dhcpRequested;
    }

    /**
     * put parameter request list
     */
    public void putParamReqList() {
        dhcpParamReqList = new byte[4];
        dhcpParamReqList[0] = 1;
        dhcpParamReqList[1] = 3;
        dhcpParamReqList[2] = 6;
        dhcpParamReqList[3] = 15;
    }

    /**
     * parse header
     *
     * @param pck packet to use
     * @param justHeader set true to not parse options
     * @return false if successful, true if error happened
     */
    public boolean parseHeader(packHolder pck, boolean justHeader) {
        if (pck.dataSize() < size1) {
            return true;
        }
        bootpOp = pck.getByte(0); // opcode
        if (pck.getByte(1) != hwType) { // hw type
            return true;
        }
        if (pck.getByte(2) != bootpChaddr.getSize()) { // hw len
            return true;
        }
        bootpHops = pck.getByte(3); // relay hops
        bootpXid = pck.msbGetD(4); // transaction id
        bootpSecs = pck.msbGetW(8); // seconds
        int i = pck.msbGetW(10); // flags
        bootpBroadcast = (i & flagBrdcst) != 0;
        pck.getAddr(bootpCiaddr, 12); // client address
        pck.getAddr(bootpYiaddr, 16); // your address
        pck.getAddr(bootpSiaddr, 20); // server address
        pck.getAddr(bootpGiaddr, 24); // relay address
        pck.getAddr(bootpChaddr, 28); // hardware address
        bootpSname = pck.getAsciiZ(44, 64, 0); // sname
        bootpSfile = pck.getAsciiZ(108, 128, 0); // sfile
        pck.getSkip(size1);
        if (justHeader) {
            return false;
        }
        if (pck.msbGetD(0) != magic) {
            return true;
        }
        pck.getSkip(4);
        for (;;) {
            if (pck.dataSize() < 0) {
                return false;
            }
            i = pck.getByte(0);
            if (i == 255) {
                return false;
            }
            if (i == 0) {
                continue;
            }
            if (tlv.getBytes(pck)) {
                return false;
            }
            switch (tlv.valTyp) {
                case 53: // dhcp message type
                    dhcpOp = bits.getByte(tlv.valDat, 0);
                    break;
                case 54: // dhcp server identifier
                    dhcpServer = new addrIPv4();
                    dhcpServer.fromBuf(tlv.valDat, 0);
                    break;
                case 50: // requested address
                    dhcpRequested = new addrIPv4();
                    dhcpRequested.fromBuf(tlv.valDat, 0);
                    break;
                case 51: // address lease time
                    dhcpLeaseTime = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 58: // address renewal time
                    dhcpRenewTime = bits.msbGetD(tlv.valDat, 0);
                    break;
                case 1: // subnet mask
                    dhcpNetMask = new addrIPv4();
                    dhcpNetMask.fromBuf(tlv.valDat, 0);
                    break;
                case 3: // router address
                    dhcpGateway = new addrIPv4();
                    dhcpGateway.fromBuf(tlv.valDat, 0);
                    break;
                case 6: // dns server addr
                    if (tlv.valSiz < addrIPv4.size) {
                        break;
                    }
                    dhcpDns1srv = new addrIPv4();
                    dhcpDns1srv.fromBuf(tlv.valDat, 0);
                    if (tlv.valSiz < (addrIPv4.size * 2)) {
                        break;
                    }
                    dhcpDns2srv = new addrIPv4();
                    dhcpDns2srv.fromBuf(tlv.valDat, addrIPv4.size);
                    break;
                case 12: // client hostname
                    dhcpHostName = tlv.getStr();
                    break;
                case 15: // domain name
                    dhcpDomainName = tlv.getStr();
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * create header
     *
     * @param pck packet to use
     * @param opts options to pass, null if nothing
     */
    public void createHeader(packHolder pck, tabGen<packDhcpOption> opts) {
        pck.putStart();
        pck.putFill(0, size2, 0);
        pck.putByte(0, bootpOp); // opcode
        pck.putByte(1, hwType); // hw type
        pck.putByte(2, bootpChaddr.getSize()); // hw len
        pck.putByte(3, bootpHops); // relay hops
        pck.msbPutD(4, bootpXid); // transaction id
        pck.msbPutW(8, bootpSecs); // seconds
        int i = 0;
        if (bootpBroadcast) {
            i |= flagBrdcst;
        }
        pck.msbPutW(10, i); // flags
        pck.putAddr(12, bootpCiaddr); // client address
        pck.putAddr(16, bootpYiaddr); // your address
        pck.putAddr(20, bootpSiaddr); // server address
        pck.putAddr(24, bootpGiaddr); // relay address
        pck.putAddr(28, bootpChaddr); // hardware address
        if (bootpSname != null) {
            pck.putAsciiZ(44, 64, bootpSname, 0); // sname
        }
        if (bootpSfile != null) {
            pck.putAsciiZ(108, 128, bootpSfile, 0); // sfile
        }
        pck.putSkip(size1);
        pck.msbPutD(0, magic);
        pck.putSkip(4);
        bits.putByte(tlv.valDat, 0, dhcpOp);
        tlv.putBytes(pck, 53, 1, tlv.valDat);
        if (dhcpClientId) {
            bootpChaddr.toBuffer(tlv.valDat, 1);
            tlv.valDat[0] = 1;
            tlv.putBytes(pck, 61, bootpChaddr.getSize() + 1, tlv.valDat);
        }
        if (dhcpLeaseTime != 0) {
            bits.msbPutD(tlv.valDat, 0, dhcpLeaseTime);
            tlv.putBytes(pck, 51, 4, tlv.valDat);
        }
        if (dhcpRenewTime != 0) {
            bits.msbPutD(tlv.valDat, 0, dhcpRenewTime);
            tlv.putBytes(pck, 58, 4, tlv.valDat);
        }
        if (dhcpHostName != null) {
            tlv.putStr(pck, 12, dhcpHostName);
        }
        if (dhcpDomainName != null) {
            tlv.putStr(pck, 15, dhcpDomainName);
        }
        if (dhcpParamReqList != null) {
            tlv.putBytes(pck, 55, dhcpParamReqList.length, dhcpParamReqList);
        }
        if (dhcpServer != null) {
            tlv.putAddr(pck, 54, dhcpServer);
        }
        if (dhcpRequested != null) {
            tlv.putAddr(pck, 50, dhcpRequested);
        }
        if (dhcpNetMask != null) {
            tlv.putAddr(pck, 1, dhcpNetMask);
        }
        if (dhcpGateway != null) {
            tlv.putAddr(pck, 3, dhcpGateway);
        }
        if (dhcpDns1srv != null) {
            i = 0;
            if (dhcpDns1srv != null) {
                dhcpDns1srv.toBuffer(tlv.valDat, i);
                i += dhcpDns1srv.getSize();
            }
            if (dhcpDns2srv != null) {
                dhcpDns2srv.toBuffer(tlv.valDat, i);
                i += dhcpDns2srv.getSize();
            }
            tlv.putBytes(pck, 6, i, tlv.valDat);
        }
        if (opts != null) {
            for (i = 0; i < opts.size(); i++) {
                packDhcpOption opt = opts.get(i);
                tlv.putBytes(pck, opt.number, opt.buffer.length, opt.buffer);
            }
        }
        pck.putByte(0, 0xff);
        pck.putSkip(1);
        for (; pck.headSize() < size2;) {
            pck.putByte(0, 0);
            pck.putSkip(1);
        }
        pck.merge2beg();
    }

}
