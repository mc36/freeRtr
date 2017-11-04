package pack;

import pipe.pipeSide;
import util.bits;
import util.typLenVal;

/**
 * secure socket tunneling protocol
 *
 * @author matecsaba
 */
public class packSstp {

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * protocol version
     */
    public int version;

    /**
     * control packet
     */
    public boolean control;

    /**
     * message type
     */
    public int msgTyp;

    /**
     * encapsulated protocol
     */
    public int proto;

    /**
     * attribute id
     */
    public byte attr;

    /**
     * status
     */
    public int stat;

    /**
     * crypto nonce
     */
    public byte[] nonce;

    /**
     * connect request
     */
    public static final int typConnReq = 1;

    /**
     * connect ack
     */
    public static final int typConnAck = 2;

    /**
     * connect nak
     */
    public static final int typConnNak = 3;

    /**
     * connect done
     */
    public static final int typConnDon = 4;

    /**
     * abort
     */
    public static final int typAbort = 5;

    /**
     * disconnect request
     */
    public static final int typDiscReq = 6;

    /**
     * disconnect ack
     */
    public static final int typDiscAck = 7;

    /**
     * echo request
     */
    public static final int typEchoReq = 8;

    /**
     * echo reply
     */
    public static final int typEchoRep = 9;

    /**
     * encapsulated protocol
     */
    public static final int atrEncPrt = 1;

    /**
     * status
     */
    public static final int atrStatus = 2;

    /**
     * crypto binding
     */
    public static final int atrCryBnd = 3;

    /**
     * crypto binding request
     */
    public static final int atrCryReq = 4;

    private final pipeSide pipe;

    private typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 4, 4, 1, 0, 1024, true);

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typConnReq:
                return "connReq";
            case typConnAck:
                return "connAck";
            case typConnNak:
                return "connNak";
            case typConnDon:
                return "conned";
            case typAbort:
                return "abort";
            case typDiscReq:
                return "discReq";
            case typDiscAck:
                return "discAck";
            case typEchoReq:
                return "echoReq";
            case typEchoRep:
                return "echoREp";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * create new instance
     *
     * @param conn connection to use
     */
    public packSstp(pipeSide conn) {
        pipe = conn;
    }

    /**
     * dump this packet
     *
     * @return packet dump
     */
    public String dump() {
        return "ver=" + version + " ctrl=" + control + " type=" + type2string(msgTyp) + " proto=" + proto + " nonce=" + bits.byteDump(nonce, 0, -1);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void sendData(packHolder pck) {
        txPkt(false, pck);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void sendCtrl(packHolder pck) {
        txPkt(true, pck);
    }

    /**
     * receive one packet
     *
     * @return received packet, null if nothing
     */
    public packHolder recvPack() {
        byte[] buf = new byte[size];
        if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
            return null;
        }
        version = bits.getByte(buf, 0); // version
        int flags = bits.getByte(buf, 1); // flags
        control = (flags & 1) != 0;
        int len = bits.msbGetW(buf, 2) - size; // length
        if (len < 0) {
            return null;
        }
        packHolder pck = new packHolder(true, true);
        pck.pipeRecv(pipe, 0, len, 144);
        return pck;
    }

    private void txPkt(boolean ctrl, packHolder pck) {
        control = ctrl;
        version = 0x10;
        int flags = 0;
        if (control) {
            flags |= 1;
        }
        pck.merge2beg();
        pck.putByte(0, version); // version
        pck.putByte(1, flags); // flags
        pck.msbPutW(2, pck.dataSize() + size);
        pck.putSkip(size);
        pck.merge2beg();
        if (pipe.ready2tx() < pck.dataSize()) {
            return;
        }
        pipe.morePut(pck.getCopy(), 0, pck.dataSize());
    }

    /**
     * parse control packet
     *
     * @param pck packet to read
     * @return true on error, false on success
     */
    public boolean parseCtrl(packHolder pck) {
        if (version != 0x10) {
            return true;
        }
        if (pck.dataSize() < size) {
            return true;
        }
        msgTyp = pck.msbGetW(0); // message type
        int attrNum = pck.msbGetW(2); // number of attributes
        pck.getSkip(size);
        for (int attrCur = 0; attrCur < attrNum; attrCur++) {
            if (tlv.getBytes(pck)) {
                return true;
            }
            switch (tlv.valTyp) {
                case atrEncPrt:
                    proto = bits.msbGetW(tlv.valDat, 0);
                    break;
                case atrCryBnd:
                    proto = tlv.valDat[3];
                    nonce = new byte[32];
                    bits.byteCopy(tlv.valDat, 4, nonce, 0, nonce.length);
                    break;
                case atrCryReq:
                    proto = tlv.valDat[3];
                    break;
                case atrStatus:
                    attr = tlv.valDat[3];
                    stat = bits.msbGetD(tlv.valDat, 4);
                    break;
            }
        }
        return pck.dataSize() != 0;
    }

    /**
     * create control packet
     *
     * @param pck packet to update
     */
    public void createCtrl(packHolder pck) {
        pck.merge2beg();
        int num = 0;
        int len = pck.dataSize();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            num++;
        }
        pck.setBytesLeft(len);
        pck.msbPutW(0, msgTyp); // message type
        pck.msbPutW(2, num); // number of attributes
        pck.putSkip(size);
    }

    /**
     * fill connect request
     */
    public void fillConnReq() {
        proto = 1;
    }

    /**
     * parse connect request
     *
     * @return true on error, false on success
     */
    public boolean parseConnReq() {
        return msgTyp != typConnReq;
    }

    /**
     * create connect request
     *
     * @param pck packet to create
     */
    public void createConnReq(packHolder pck) {
        pck.clear();
        msgTyp = typConnReq;
        bits.msbPutW(tlv.valDat, 0, proto); // protocol id
        tlv.putBytes(pck, atrEncPrt, 2, tlv.valDat);
        createCtrl(pck);
    }

    /**
     * fill control ack
     */
    public void fillConnAck() {
        proto = 3;
        nonce = new byte[32];
        for (int i = 0; i < nonce.length; i++) {
            nonce[i] = (byte) bits.randomB();
        }
    }

    /**
     * parse connect ack
     *
     * @return true on error, false on success
     */
    public boolean parseConnAck() {
        return msgTyp != typConnAck;
    }

    /**
     * create connect acknowledgment
     *
     * @param pck packet to create
     */
    public void createConnAck(packHolder pck) {
        pck.clear();
        msgTyp = typConnAck;
        bits.msbPutD(tlv.valDat, 0, proto); // hash id
        bits.byteCopy(nonce, 0, tlv.valDat, 4, nonce.length);
        tlv.putBytes(pck, atrCryReq, 36, tlv.valDat);
        createCtrl(pck);
    }

}
