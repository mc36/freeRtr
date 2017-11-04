package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import util.bits;
import util.typLenVal;

/**
 * hot standby router protocol (rfc2281) packet
 *
 * @author matecsaba
 */
public class packHsrp {

    /**
     * protocol version
     */
    public int version;

    /**
     * group number
     */
    public int group;

    /**
     * ip version
     */
    public boolean ipv4;

    /**
     * opcode
     */
    public int opcod;

    /**
     * state
     */
    public int state;

    /**
     * hello interval
     */
    public int hello;

    /**
     * hold interval
     */
    public int hold;

    /**
     * priority
     */
    public int priority;

    /**
     * authentication data
     */
    public String authen;

    /**
     * virtual ip address
     */
    public addrIP virtual;

    /**
     * identifier
     */
    public addrMac ident;

    /**
     * hello message
     */
    public static final int opHello = 0;

    /**
     * coup (want) message
     */
    public static final int opCoup = 1;

    /**
     * resign (don't want) message
     */
    public static final int opResign = 2;

    /**
     * initial
     */
    public static final int staInit = 1;

    /**
     * learn
     */
    public static final int staLern = 2;

    /**
     * listen
     */
    public static final int staLstn = 3;

    /**
     * speak
     */
    public static final int staSpk = 4;

    /**
     * standby
     */
    public static final int staStby = 5;

    /**
     * active
     */
    public static final int staActv = 6;

    /**
     * state of group
     */
    public static final int tlvGroup = 1;

    /**
     * state of interface
     */
    public static final int tlvIface = 2;

    /**
     * clear authentication
     */
    public static final int tlvAuthClr = 3;

    private typLenVal tlv = new typLenVal(0, 8, 8, 8, 1, 0, 2, 1, 0, 512, true);

    public String toString() {
        return "ver=" + version + " ip4=" + ipv4 + " opc=" + opcod + " stat=" + state2string(state) + " helo=" + hello + " hold=" + hold + " pri=" + priority + " grp=" + group + " virt=" + virtual + " auth=" + authen + " id=" + ident;
    }

    /**
     * generate group mac address
     *
     * @return generated
     */
    public addrMac genMacAddr() {
        addrMac adr = new addrMac();
        switch (version) {
            case 1:
                adr.fromString("0000.0c07.ac00");
                break;
            case 2:
                if (ipv4) {
                    adr.fromString("0000.0c9f.f000");
                } else {
                    adr.fromString("0005.73a0.0000");
                }
                break;
            default:
                return null;
        }
        byte[] buf = adr.getBytes();
        buf[buf.length - 1] = (byte) (group & 0xff);
        return adr;
    }

    /**
     * generate target ip address
     *
     * @return generated
     */
    public addrIP genIpAddr() {
        addrIP adr = new addrIP();
        switch (version) {
            case 1:
                adr.fromString("224.0.0.2");
                break;
            case 2:
                if (ipv4) {
                    adr.fromString("224.0.0.102");
                } else {
                    adr.fromString("ff02::66");
                }
                break;
            default:
                return null;
        }
        return adr;
    }

    /**
     * get port number * ;;
     *
     * @return port number
     */
    public int getPortNum() {
        if (ipv4) {
            return 1985;
        } else {
            return 2029;
        }
    }

    /**
     * convert v1 to v2 state
     *
     * @param i v1 state
     * @return v2 state
     */
    public static int stateV1V2(int i) {
        switch (i) {
            case 0:
                return staInit;
            case 1:
                return staLern;
            case 2:
                return staLstn;
            case 4:
                return staSpk;
            case 8:
                return staStby;
            case 16:
                return staActv;
            default:
                return 1024 + i;
        }
    }

    /**
     * convert v2 to v1 state
     *
     * @param i v2 state
     * @return v1 state
     */
    public static int stateV2V1(int i) {
        switch (i) {
            case staInit:
                return 0;
            case staLern:
                return 1;
            case staLstn:
                return 2;
            case staSpk:
                return 4;
            case staStby:
                return 8;
            case staActv:
                return 16;
            default:
                return 1024 + i;
        }
    }

    /**
     * convert state to string
     *
     * @param i state
     * @return string
     */
    public static String state2string(int i) {
        switch (i) {
            case staInit:
                return "init";
            case staLern:
                return "learn";
            case staLstn:
                return "listen";
            case staSpk:
                return "speak";
            case staStby:
                return "standby";
            case staActv:
                return "active";
            default:
                return "unknown=" + i;
        }
    }

    private addrIP getAddr(byte[] buf, int ofs) {
        try {
            addrIP res = new addrIP();
            if (ipv4) {
                addrIPv4 a4 = new addrIPv4();
                a4.fromBuf(buf, ofs);
                res.fromIPv4addr(a4);
            } else {
                addrIPv6 a6 = new addrIPv6();
                a6.fromBuf(buf, ofs);
                res.fromIPv6addr(a6);
            }
            return res;
        } catch (Exception e) {
        }
        return new addrIP();
    }

    private byte[] putAddr(addrIP adr) {
        if (ipv4) {
            return adr.toIPv4().getBytes();
        } else {
            return adr.toIPv6().getBytes();
        }
    }

    private int getIPver() {
        if (ipv4) {
            return 4;
        } else {
            return 6;
        }
    }

    /**
     * parse one header
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        switch (version) {
            case 1:
                if (pck.getByte(0) != 0) { // version
                    return true;
                }
                opcod = pck.getByte(1);
                state = stateV1V2(pck.getByte(2));
                hello = pck.getByte(3) * 1000;
                hold = pck.getByte(4) * 1000;
                priority = pck.getByte(5);
                group = pck.getByte(6);
                authen = pck.getAsciiZ(8, 8, 0);
                virtual = getAddr(pck.getCopy(), 16);
                return false;
            case 2:
                for (;;) {
                    if (tlv.getBytes(pck)) {
                        break;
                    }
                    switch (tlv.valTyp) {
                        case tlvGroup:
                            if (tlv.valSiz < 40) {
                                break;
                            }
                            if (tlv.valDat[0] != 2) { // version
                                return true;
                            }
                            opcod = tlv.valDat[1] & 0xff;
                            state = tlv.valDat[2] & 0xff;
                            if (tlv.valDat[3] != getIPver()) {
                                return true;
                            }
                            group = bits.msbGetW(tlv.valDat, 4);
                            ident = new addrMac();
                            ident.fromBuf(tlv.valDat, 6);
                            priority = bits.msbGetD(tlv.valDat, 12);
                            hello = bits.msbGetD(tlv.valDat, 16);
                            hold = bits.msbGetD(tlv.valDat, 20);
                            virtual = getAddr(tlv.valDat, 24);
                            break;
                        case tlvAuthClr:
                            authen = tlv.getStr().trim();
                            break;
                    }
                }
                return false;
            default:
                return true;
        }
    }

    /**
     * create one packet
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean createPacket(packHolder pck) {
        byte[] buf = putAddr(virtual);
        switch (version) {
            case 1:
                pck.putByte(0, 0); // version
                pck.putByte(1, opcod); // opcode
                pck.putByte(2, stateV2V1(state)); // state
                pck.putByte(3, hello / 1000); // hello time
                pck.putByte(4, hold / 1000); // hold time
                pck.putByte(5, priority); // priority
                pck.putByte(6, group); // group
                pck.putByte(7, 0); // reserved
                pck.putAsciiZ(8, 8, authen, 0);
                pck.putCopy(buf, 0, 16, buf.length);
                pck.putSkip(16 + buf.length);
                break;
            case 2:
                tlv.valDat = new byte[40];
                tlv.valDat[0] = 2; // version
                tlv.valDat[1] = (byte) opcod; // opcode
                tlv.valDat[2] = (byte) state; // state
                tlv.valDat[3] = (byte) getIPver(); // ip version
                bits.msbPutW(tlv.valDat, 4, group); // group
                ident.toBuffer(tlv.valDat, 6); // identifier
                bits.msbPutD(tlv.valDat, 12, priority); // priority
                bits.msbPutD(tlv.valDat, 16, hello); // hello time
                bits.msbPutD(tlv.valDat, 20, hold); // hold time
                bits.byteCopy(buf, 0, tlv.valDat, 24, buf.length);
                tlv.putBytes(pck, tlvGroup, tlv.valDat);
                tlv.valDat = new byte[8];
                buf = authen.getBytes();
                bits.byteCopy(bits.byteConcat(buf, tlv.valDat), 0, tlv.valDat, 0, 8);
                tlv.putBytes(pck, tlvAuthClr, tlv.valDat);
                break;
            default:
                return true;
        }
        return false;
    }

}
