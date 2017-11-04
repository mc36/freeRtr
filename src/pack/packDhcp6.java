package pack;

import addr.addrIPv6;
import addr.addrMac;
import addr.addrType;
import tab.tabGen;
import util.bits;
import util.typLenVal;

/**
 * dynamic host config protocol (rfc3315) packet
 *
 * @author matecsaba
 */
public class packDhcp6 {

    /**
     * default client port number
     */
    public final static int portCnum = 546;

    /**
     * default server port number
     */
    public final static int portSnum = 547;

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * solicit
     */
    public final static int typSolicit = 1;

    /**
     * advertise
     */
    public final static int typAdvertise = 2;

    /**
     * request
     */
    public final static int typRequest = 3;

    /**
     * confirm
     */
    public final static int typConfirm = 4;

    /**
     * renew
     */
    public final static int typRenew = 5;

    /**
     * rebind
     */
    public final static int typRebind = 6;

    /**
     * reply
     */
    public final static int typReply = 7;

    /**
     * release
     */
    public final static int typRelease = 8;

    /**
     * decline
     */
    public final static int typDecline = 9;

    /**
     * reconfigure
     */
    public final static int typReconfig = 10;

    /**
     * information request
     */
    public final static int typInfo = 11;

    /**
     * relay request
     */
    public final static int typReReq = 12;

    /**
     * relay reply
     */
    public final static int typReRep = 13;

    /**
     * lease query
     */
    public final static int typLQ = 14;

    /**
     * lease query reply
     */
    public final static int typLQrep = 15;

    /**
     * lease query done
     */
    public final static int typLQdon = 16;

    /**
     * lease query data
     */
    public final static int typLQdat = 17;

    /**
     * message type
     */
    public int msgTyp;

    /**
     * message id
     */
    public int msgId;

    /**
     * client id
     */
    public byte[] clntId;

    /**
     * server id
     */
    public byte[] servId;

    /**
     * identity association mode: 1=temp, 2=perm, 3=pref
     */
    public int iamod;

    /**
     * identity association id
     */
    public int iaid;

    /**
     * identity association t1
     */
    public int iat1;

    /**
     * identity association t2
     */
    public int iat2;

    /**
     * ip address
     */
    public addrIPv6 ipaddr;

    /**
     * address bits
     */
    public int ipsize;

    /**
     * preferred lifetime
     */
    public int lifetimP;

    /**
     * valid lifetime
     */
    public int lifetimV;

    /**
     * requested options
     */
    public int[] optionsReq;

    /**
     * server preference
     */
    public int servPref = -1;

    /**
     * time elapsed
     */
    public int clntTime = -1;

    /**
     * server address
     */
    public addrIPv6 servAddr;

    /**
     * status code
     */
    public int status;

    /**
     * rapid commit
     */
    public boolean rapid;

    /**
     * dns server address
     */
    public addrIPv6 dns1srv;

    /**
     * dns server address
     */
    public addrIPv6 dns2srv;

    /**
     * domain name
     */
    public String domainName;

    /**
     * boot url
     */
    public String bootUrl;

    private typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 0, 4, 1, 0, 512, true);

    private static int hwType = 1;

    public String toString() {
        return "typ=" + type2string(msgTyp) + " id=" + msgId + " clnt=" + bits.byteDump(clntId, 0, -1) + " serv=" + bits.byteDump(servId, 0, -1) + " mode=" + iamod + " iaid=" + iaid + " t1=" + iat1 + " t2=" + iat2 + " addr=" + ipaddr + "/" + ipsize + " lifeP=" + lifetimP + " lifeV=" + lifetimV + " pref=" + servPref + " time=" + clntTime + " serv=" + servAddr + " stat=" + status + " rapid=" + rapid + " dns1=" + dns1srv + " dns2=" + dns2srv;
    }

    /**
     * decode dhcp unique identifier
     *
     * @param val value to decode
     * @return decoded mac address
     */
    public static addrMac decodeDUID(byte[] val) {
        if (val == null) {
            return null;
        }
        if (val.length < 4) {
            return null;
        }
        addrMac adr = new addrMac();
        if (bits.msbGetW(val, 2) != hwType) {
            return null;
        }
        switch (bits.msbGetW(val, 0)) {
            case 1: // linklocal plus time
                if (val.length < 14) {
                    return null;
                }
                adr.fromBuf(val, 8);
                return adr;
            case 3: // link local
                if (val.length < 10) {
                    return null;
                }
                adr.fromBuf(val, 4);
                return adr;
            default:
                return null;
        }
    }

    /**
     * encode dhcp unique identifier
     *
     * @param adr address to encode
     * @return encoded address
     */
    public static byte[] encodeDUID(addrType adr) {
        if (adr == null) {
            return null;
        }
        if (adr.getSize() != addrMac.size) {
            return null;
        }
        byte[] res = new byte[10];
        bits.msbPutW(res, 0, 3); // type
        bits.msbPutW(res, 2, hwType); // hw type
        adr.toBuffer(res, 4);
        return res;
    }

    /**
     * convert type to string
     *
     * @param typ type
     * @return string
     */
    public static String type2string(int typ) {
        switch (typ) {
            case typSolicit:
                return "solicit";
            case typAdvertise:
                return "advert";
            case typRequest:
                return "request";
            case typConfirm:
                return "confirm";
            case typRenew:
                return "renew";
            case typRebind:
                return "rebind";
            case typReply:
                return "reply";
            case typRelease:
                return "release";
            case typDecline:
                return "decline";
            case typReconfig:
                return "reconfig";
            case typInfo:
                return "info";
            case typReReq:
                return "relayReq";
            case typReRep:
                return "relayRep";
            case typLQ:
                return "lq";
            case typLQrep:
                return "lqRep";
            case typLQdon:
                return "lqDone";
            case typLQdat:
                return "lqData";
            default:
                return "unknown=" + typ;
        }
    }

    /**
     * parse one packet
     *
     * @param pck packet to parse
     * @return true on error, false on success
     */
    public boolean parsePacket(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        msgTyp = pck.getByte(0) & 0xff;
        msgId = pck.msbGetD(0) & 0xffffff;
        pck.getSkip(size);
        for (;;) {
            if (pck.dataSize() < 1) {
                return false;
            }
            if (tlv.getBytes(pck)) {
                return false;
            }
            switch (tlv.valTyp) {
                case 1: // client id
                    clntId = tlv.copyBytes();
                    break;
                case 2: // server id
                    servId = tlv.copyBytes();
                    break;
                case 3: // identity association - non temporary addreess
                    iamod = 2;
                    iaid = bits.msbGetD(tlv.valDat, 0);
                    iat1 = bits.msbGetD(tlv.valDat, 4);
                    iat2 = bits.msbGetD(tlv.valDat, 8);
                    getOptions(pck, 12);
                    break;
                case 4: // identity association - temporary addreess
                    iamod = 1;
                    iaid = bits.msbGetD(tlv.valDat, 0);
                    getOptions(pck, 4);
                    break;
                case 5: // ipv6 address
                    ipaddr = new addrIPv6();
                    ipsize = ipaddr.maxBits();
                    ipaddr.fromBuf(tlv.valDat, 0);
                    lifetimP = bits.msbGetD(tlv.valDat, 16);
                    lifetimV = bits.msbGetD(tlv.valDat, 20);
                    getOptions(pck, 24);
                    break;
                case 6: // option request
                    optionsReq = new int[tlv.valSiz / 2];
                    for (int i = 0; i < optionsReq.length; i++) {
                        optionsReq[i] = bits.msbGetW(tlv.valDat, i * 2);
                    }
                    break;
                case 7: // server preference
                    servPref = tlv.valDat[0] & 0xff;
                    break;
                case 8: // time elapsed
                    clntTime = bits.msbGetW(tlv.valDat, 0);
                    break;
                case 12: // server address
                    servAddr = new addrIPv6();
                    servAddr.fromBuf(tlv.valDat, 0);
                    break;
                case 13: // status
                    status = bits.msbGetW(tlv.valDat, 0);
                    break;
                case 14: // rapid commit
                    rapid = true;
                    break;
                case 23: // dns servers
                    if (tlv.valSiz < addrIPv6.size) {
                        break;
                    }
                    dns1srv = new addrIPv6();
                    dns1srv.fromBuf(tlv.valDat, 0);
                    if (tlv.valSiz < (addrIPv6.size * 2)) {
                        break;
                    }
                    dns2srv = new addrIPv6();
                    dns2srv.fromBuf(tlv.valDat, addrIPv6.size);
                    break;
                case 24: // domain list
                    packHolder pck2 = new packHolder(true, true);
                    pck2.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
                    pck2.putSkip(tlv.valSiz);
                    pck2.merge2beg();
                    domainName = packDnsRec.getChain(pck2, 0);
                    break;
                case 25: // identity association - prefix delegation
                    iamod = 3;
                    iaid = bits.msbGetD(tlv.valDat, 0);
                    iat1 = bits.msbGetD(tlv.valDat, 4);
                    iat2 = bits.msbGetD(tlv.valDat, 8);
                    getOptions(pck, 12);
                    break;
                case 26: // ipv6 prefix
                    lifetimP = bits.msbGetD(tlv.valDat, 0);
                    lifetimV = bits.msbGetD(tlv.valDat, 4);
                    ipaddr = new addrIPv6();
                    ipsize = tlv.valDat[8] & 0xff;
                    ipaddr.fromBuf(tlv.valDat, 9);
                    getOptions(pck, 25);
                    break;
                case 59: // boot url
                    bootUrl = tlv.getStr();
                    break;
                default:
                    break;
            }
        }
    }

    private void getOptions(packHolder pck, int hdr) {
        if (tlv.valSiz < hdr) {
            return;
        }
        pck.putCopy(tlv.valDat, hdr, 0, tlv.valSiz - hdr);
        pck.putSkip(tlv.valSiz - hdr);
        pck.merge2beg();
    }

    /**
     * put parameter request list
     */
    public void putOptionsReqList() {
        optionsReq = new int[3];
        optionsReq[0] = 23;
        optionsReq[1] = 24;
        optionsReq[2] = 39;
    }

    /**
     * create one packet
     *
     * @param pck packet to update
     * @param opts options to pass, null if nothing
     */
    public void createPacket(packHolder pck, tabGen<packDhcpOption> opts) {
        pck.putStart();
        if (ipaddr != null) {
            if (iamod == 3) {
                bits.msbPutD(tlv.valDat, 0, lifetimP); // preferred lifetime
                bits.msbPutD(tlv.valDat, 4, lifetimV); // valid lifetime
                tlv.valDat[8] = (byte) ipsize;
                ipaddr.toBuffer(tlv.valDat, 9); // address
                tlv.putBytes(pck, 26, 25, tlv.valDat);
            } else {
                ipaddr.toBuffer(tlv.valDat, 0); // address
                bits.msbPutD(tlv.valDat, 16, lifetimP); // preferred lifetime
                bits.msbPutD(tlv.valDat, 20, lifetimV); // valid lifetime
                tlv.putBytes(pck, 5, 24, tlv.valDat);
            }
        }
        if (status > 0) {
            bits.msbPutW(tlv.valDat, 0, status);
            tlv.putBytes(pck, 13, 2, tlv.valDat);
        }
        pck.merge2beg();
        byte[] buf = pck.getCopy();
        pck.getSkip(buf.length);
        switch (iamod) {
            case 1: // temporary
                bits.msbPutD(tlv.valDat, 0, iaid); // ia id
                bits.byteCopy(buf, 0, tlv.valDat, 4, buf.length);
                tlv.putBytes(pck, 4, buf.length + 4, tlv.valDat);
                break;
            case 2: // permanent
                bits.msbPutD(tlv.valDat, 0, iaid); // ia id
                bits.msbPutD(tlv.valDat, 4, iat1); // ia t1
                bits.msbPutD(tlv.valDat, 8, iat2); // ia t2
                bits.byteCopy(buf, 0, tlv.valDat, 12, buf.length);
                tlv.putBytes(pck, 3, buf.length + 12, tlv.valDat);
                break;
            case 3: // prefix
                bits.msbPutD(tlv.valDat, 0, iaid); // ia id
                bits.msbPutD(tlv.valDat, 4, iat1); // ia t1
                bits.msbPutD(tlv.valDat, 8, iat2); // ia t2
                bits.byteCopy(buf, 0, tlv.valDat, 12, buf.length);
                tlv.putBytes(pck, 25, buf.length + 12, tlv.valDat);
                break;
        }
        pck.merge2beg();
        if (clntId != null) {
            tlv.putBytes(pck, 1, clntId);
        }
        if (servId != null) {
            tlv.putBytes(pck, 2, servId);
        }
        if (optionsReq != null) {
            for (int i = 0; i < optionsReq.length; i++) {
                bits.msbPutW(tlv.valDat, i * 2, optionsReq[i]);
            }
            tlv.putBytes(pck, 6, optionsReq.length * 2, tlv.valDat);
        }
        if (servPref >= 0) {
            tlv.valDat[0] = (byte) servPref;
            tlv.putBytes(pck, 7, 1, tlv.valDat);
        }
        if (clntTime >= 0) {
            bits.msbPutW(tlv.valDat, 0, clntTime);
            tlv.putBytes(pck, 8, 2, tlv.valDat);
        }
        if (servAddr != null) {
            servAddr.toBuffer(tlv.valDat, 0);
            tlv.putBytes(pck, 12, addrIPv6.size, tlv.valDat);
        }
        if (rapid) {
            tlv.putBytes(pck, 14, 0, tlv.valDat);
        }
        tlv.valSiz = 0;
        if (dns1srv != null) {
            dns1srv.toBuffer(tlv.valDat, tlv.valSiz);
            tlv.valSiz += addrIPv6.size;
        }
        if (dns2srv != null) {
            dns2srv.toBuffer(tlv.valDat, tlv.valSiz);
            tlv.valSiz += addrIPv6.size;
        }
        if (tlv.valSiz > 0) {
            tlv.putBytes(pck, 23, tlv.valSiz, tlv.valDat);
        }
        if (domainName != null) {
            packHolder pck2 = new packHolder(true, true);
            packDnsRec.putDomain(pck2, domainName);
            pck2.merge2beg();
            buf = pck2.getCopy();
            tlv.putBytes(pck, 24, buf);
        }
        if (bootUrl != null) {
            tlv.putStr(pck, 59, bootUrl);
        }
        if (opts != null) {
            for (int i = 0; i < opts.size(); i++) {
                packDhcpOption ntry = opts.get(i);
                tlv.putBytes(pck, ntry.number, ntry.buffer.length, ntry.buffer);
            }
        }
        pck.merge2end();
        pck.msbPutD(0, msgId);
        pck.putByte(0, msgTyp);
        pck.putSkip(4);
        pck.merge2beg();
    }

}
