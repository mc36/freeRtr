package org.freertr.pack;

import org.freertr.enc.encAsn1;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * simple network management protocol (rfc1157) packet
 *
 * @author matecsaba
 */
public class packSnmp {

    /**
     * create instance
     */
    public packSnmp() {
    }

    /**
     * port number
     */
    public final static int port = 161;

    /**
     * protocol version
     */
    public int version;

    /**
     * community
     */
    public String community;

    /**
     * type of payload
     */
    public int type;

    /**
     * request id
     */
    public int reqId;

    /**
     * error status
     */
    public int errStat;

    /**
     * error index
     */
    public int errIdx;

    /**
     * result in binary
     */
    public List<encAsn1> res = new ArrayList<encAsn1>();

    /**
     * get request
     */
    public final static int typGetReq = 0;

    /**
     * get next request
     */
    public final static int typGetNext = 1;

    /**
     * response
     */
    public final static int typResponse = 2;

    /**
     * set request
     */
    public final static int typSetReq = 3;

    /**
     * trap
     */
    public final static int typTrap = 4;

    /**
     * get bulk request
     */
    public final static int typGetBulk = 5;

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typGetReq:
                return "getReq";
            case typGetNext:
                return "getNext";
            case typResponse:
                return "response";
            case typSetReq:
                return "setReq";
            case typTrap:
                return "trap";
            case typGetBulk:
                return "getBulk";
            default:
                return "unknown=" + i;
        }
    }

    public String toString() {
        String s = "";
        for (int i = 0; i < res.size(); i++) {
            encAsn1 cur = res.get(i);
            s += " oid=" + encAsn1.oid2str(cur.oid) + " res=" + cur;
        }
        return "ver=" + version + " comm=" + community + " type=" + type2string(type) + " id=" + reqId + " errStat=" + errStat + " errIdx=" + errIdx + s;
    }

    /**
     * parse request
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        encAsn1 a = new encAsn1();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagInteger)) {
            return true;
        }
        version = a.getBigInt().intValue() + 1;
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagOctetString)) {
            return true;
        }
        community = new String(a.buf);
        if (a.tagRead(pck)) {
            return true;
        }
        type = a.tag;
        if (!a.cnst) {
            return true;
        }
        pck = a.getPack();
        switch (type) {
            case typGetReq:
            case typGetNext:
            case typSetReq:
            case typResponse:
            case typGetBulk:
                break;
            default:
                return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagInteger)) {
            return true;
        }
        reqId = a.getBigInt().intValue();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagInteger)) {
            return true;
        }
        errStat = a.getBigInt().intValue();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagInteger)) {
            return true;
        }
        errIdx = a.getBigInt().intValue();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        packHolder seq = a.getPack();
        for (;;) {
            a = new encAsn1();
            if (seq.dataSize() < 1) {
                break;
            }
            if (a.tagRead(seq)) {
                return true;
            }
            if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
                return true;
            }
            pck = a.getPack();
            if (a.tagRead(pck)) {
                return true;
            }
            if ((a.cnst) || (a.tag != encAsn1.tagObjectID)) {
                return true;
            }
            a.oid = encAsn1.buf2oid(a.buf);
            if (a.tagRead(pck)) {
                return true;
            }
            res.add(a);
        }
        return false;
    }

    /**
     * create packet
     *
     * @param pck packet to write
     * @return false on success, true on error
     */
    public boolean createPacket(packHolder pck) {
        encAsn1 a = new encAsn1();
        switch (type) {
            case typGetReq:
            case typGetNext:
            case typSetReq:
            case typResponse:
            case typGetBulk:
                break;
            default:
                return true;
        }
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        for (int i = 0; i < res.size(); i++) {
            encAsn1 cur = res.get(i);
            a.putObjectId(cur.oid);
            a.buf = encAsn1.oid2buf(cur.oid);
            a.tagWrite(p2);
            p2.merge2end();
            if (cur.buf == null) {
                encAsn1.writeNull(p2);
            } else {
                cur.tagWrite(p2);
            }
            p2.merge2end();
            encAsn1.writeSequence(p1, p2);
            p2.clear();
        }
        encAsn1.writeBigInt(p2, new BigInteger("" + reqId));
        encAsn1.writeBigInt(p2, new BigInteger("" + errStat));
        encAsn1.writeBigInt(p2, new BigInteger("" + errIdx));
        encAsn1.writeSequence(p2, p1);
        p1.clear();
        encAsn1.writeBigInt(p1, new BigInteger("" + (version - 1)));
        a.putOctString(p2);
        a.buf = community.getBytes();
        a.tagWrite(p1);
        p1.merge2end();
        a.buf = p2.getCopy();
        a.cnst = true;
        a.tag = type;
        a.cls = 2;
        a.tagWrite(p1);
        p1.merge2end();
        pck.putStart();
        encAsn1.writeSequence(pck, p1);
        return false;
    }

}
