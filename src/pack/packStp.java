package pack;

import addr.addrBridge;

/**
 * spanning tree (ieee 802.1d) protocol packet
 *
 * @author matecsaba
 */
public class packStp {

    /**
     * protocol id
     */
    public int id;

    /**
     * protocol version
     */
    public int ver;

    /**
     * packet type
     */
    public int typ;

    /**
     * flags
     */
    public int flag;

    /**
     * root id
     */
    public addrBridge rootId = new addrBridge();

    /**
     * root cost
     */
    public int rootCost;

    /**
     * bridge id
     */
    public addrBridge brdgId = new addrBridge();

    /**
     * port id
     */
    public int portId;

    /**
     * message age
     */
    public int msgAge;

    /**
     * max age
     */
    public int maxAge;

    /**
     * hello time
     */
    public int hloTim;

    /**
     * forward delay
     */
    public int fwdTim;

    /**
     * size of header
     */
    public final static int size = 38;

    /**
     * topology change
     */
    public final static int flgTopChg = 0x1;

    /**
     * proposal
     */
    public final static int flgProp = 0x2;

    /**
     * port role
     */
    public final static int flgRole = 0xc;

    /**
     * learning
     */
    public final static int flgLrn = 0x10;

    /**
     * forwarding
     */
    public final static int flgFwd = 0x20;

    /**
     * agreement
     */
    public final static int flgAgr = 0x40;

    /**
     * topology change acknowledge
     */
    public final static int flgTopAck = 0x80;

    public String toString() {
        return "ver=" + id + "/" + ver + " typ=" + typ + " flg=" + flag + " root=" + rootId + " cost=" + rootCost + " brdg=" + brdgId + " prt=" + portId + " rly=" + msgAge + " age=" + maxAge + " helo=" + hloTim + " fwd=" + fwdTim;
    }

    /**
     * parse one packet
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        int i = pck.msbGetW(0); // length
        if (i < size) {
            return true;
        }
        if (i > 0x40) { // length
            return true;
        }
        if (pck.msbGetW(2) > 0x4242) { // llc sap
            return true;
        }
        if (pck.getByte(4) != 0x3) { // control
            return true;
        }
        id = pck.msbGetW(5); // protocol id
        ver = pck.getByte(7); // protocol version
        typ = pck.getByte(8); // packet type
        flag = pck.getByte(9); // flags
        rootId.fromPack(pck, 10); // root id
        rootCost = pck.msbGetD(18); // root cost
        brdgId.fromPack(pck, 22); // bridge id
        portId = pck.msbGetW(30); // port id
        msgAge = pck.msbGetW(32) / 256; // message age
        maxAge = pck.msbGetW(34) / 256; // max age
        hloTim = pck.msbGetW(36) / 256; // hello time
        fwdTim = pck.msbGetW(38) / 256; // foward delay
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutW(0, size); // length
        pck.msbPutW(2, 0x4242); // llc sap
        pck.putByte(4, 0x3); // control
        pck.msbPutW(5, id); // protocol id
        pck.putByte(7, ver); // protocol version
        pck.putByte(8, typ); // packet type
        pck.putByte(9, flag); // flags
        rootId.toPack(pck, 10); // root id
        pck.msbPutD(18, rootCost); // root cost
        brdgId.toPack(pck, 22); // bridge id
        pck.msbPutW(30, portId); // port id
        pck.msbPutW(32, msgAge * 256); // message age
        pck.msbPutW(34, maxAge * 256); // max age
        pck.msbPutW(36, hloTim * 256); // hello time
        pck.msbPutW(38, fwdTim * 256); // foward delay
        pck.putSkip(size + 2);
        pck.ETHtrg.fromString("01:80:c2:00:00:00");
        pck.ETHsrc.setAddr(rootId.adr);
    }

}
