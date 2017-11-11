package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import tab.tabSessionEntry;

/**
 * netflow (rfc3954) packet
 *
 * @author matecsaba
 */
public class packNetflow {

    /**
     * port number
     */
    public static final int port = 2055;

    /**
     * flows for ipv4
     */
    public static final int flow4 = 32;

    /**
     * flows for ipv6
     */
    public static final int flow6 = 17;

    /**
     * sequence
     */
    public int seq;

    /**
     * systime
     */
    public int tim;

    /**
     * uptime
     */
    public int upt;

    /**
     * source id
     */
    public int sou;

    /**
     * ipv4
     */
    public boolean ipv4;

    private tabGen<packNetflowTemp> temp = new tabGen<packNetflowTemp>();

    private int cnt;

    /**
     * put header
     *
     * @param pck packet to update
     */
    public void putHead(packHolder pck) {
        pck.msbPutW(0, 9); // version
        pck.msbPutW(2, cnt); // flows
        pck.msbPutD(4, upt); // uptime
        pck.msbPutD(8, tim); // time
        pck.msbPutD(12, seq); // sequence
        pck.msbPutD(16, sou); // source
        pck.putSkip(20);
        pck.merge2beg();
    }

    /**
     * put template
     *
     * @param pck packet to update
     */
    public void putTemp(packHolder pck) {
        pck.msbPutW(0, 0); // flowset id
        pck.msbPutW(2, 48); // flowset length
        pck.msbPutW(4, 256); // template id
        pck.msbPutW(6, 10); // field count
        pck.putSkip(8);
        pck.msbPutW(0, 2); // type=packets
        pck.msbPutW(2, 4); // length
        pck.putSkip(4);
        pck.msbPutW(0, 1); // type=bytes
        pck.msbPutW(2, 4); // length
        pck.putSkip(4);
        if (ipv4) {
            pck.msbPutW(0, 8); // type=srcip
            pck.msbPutW(2, addrIPv4.size); // length
            pck.putSkip(4);
            pck.msbPutW(0, 12); // type=dstip
            pck.msbPutW(2, addrIPv4.size); // length
            pck.putSkip(4);
        } else {
            pck.msbPutW(0, 27); // type=srcip
            pck.msbPutW(2, addrIPv6.size); // length
            pck.putSkip(4);
            pck.msbPutW(0, 28); // type=dstip
            pck.msbPutW(2, addrIPv6.size); // length
            pck.putSkip(4);
        }
        pck.msbPutW(0, 4); // type=protocol
        pck.msbPutW(2, 1); // length
        pck.putSkip(4);
        pck.msbPutW(0, 5); // type=tos
        pck.msbPutW(2, 1); // length
        pck.putSkip(4);
        pck.msbPutW(0, 7); // type=l4src
        pck.msbPutW(2, 2); // length
        pck.putSkip(4);
        pck.msbPutW(0, 11); // type=l4trg
        pck.msbPutW(2, 2); // length
        pck.putSkip(4);
        pck.msbPutW(0, 21); // type=last time
        pck.msbPutW(2, 4); // length
        pck.putSkip(4);
        pck.msbPutW(0, 22); // type=first time
        pck.msbPutW(2, 4); // length
        pck.putSkip(4);
        pck.merge2end();
        cnt++;
    }

    private void putFlow(packHolder pck, tabSessionEntry conn) {
        pck.msbPutD(0, (int) (conn.txPack + conn.rxPack)); // packets
        pck.msbPutD(4, (int) (conn.txByte + conn.rxByte)); // bytes
        pck.putSkip(8);
        if (ipv4) {
            pck.putAddr(0, conn.srcAdr.toIPv4()); // src
            pck.putSkip(addrIPv4.size);
            pck.putAddr(0, conn.trgAdr.toIPv4()); // trg
            pck.putSkip(addrIPv4.size);
        } else {
            pck.putAddr(0, conn.srcAdr.toIPv6()); // src
            pck.putSkip(addrIPv6.size);
            pck.putAddr(0, conn.trgAdr.toIPv6()); // trg
            pck.putSkip(addrIPv6.size);
        }
        pck.putByte(0, conn.ipPrt); // protocol
        pck.putByte(1, conn.ipTos); // protocol
        pck.msbPutW(2, conn.srcPrt); // l4src
        pck.msbPutW(4, conn.trgPrt); // l4src
        pck.putSkip(6);
        pck.msbPutD(0, (int) (conn.lastTime / 1000)); // last time
        pck.msbPutD(4, (int) (conn.startTime / 1000)); // first time
        pck.putSkip(8);
        pck.merge2end();
        cnt++;
    }

    /**
     * put flows
     *
     * @param pck packet to update
     * @param conns connections
     */
    public void putFlow(packHolder pck, List<tabSessionEntry> conns) {
        int i;
        if (ipv4) {
            i = addrIPv4.size;
        } else {
            i = addrIPv6.size;
        }
        i = (i * 2) + 22;
        pck.msbPutW(0, 256); // flowset id
        pck.msbPutW(2, 4 + (conns.size() * i)); // flowset length
        pck.putSkip(4);
        for (i = 0; i < conns.size(); i++) {
            putFlow(pck, conns.get(i));
        }
    }

    /**
     * parse packet
     *
     * @param pck packet to read
     * @return list of flows
     */
    public List<tabSessionEntry> parsePacket(packHolder pck) {
        List<tabSessionEntry> res = new ArrayList<tabSessionEntry>();
        pck.getSkip(20); // header
        for (;;) {
            if (pck.dataSize() < 4) {
                break;
            }
            int i = pck.msbGetW(0); // flowset it
            int o = pck.msbGetW(2) - 4; // length
            pck.getSkip(4);
            if (i == 0) { // template
                for (;;) {
                    if (o < 1) {
                        break;
                    }
                    packNetflowTemp tmp = new packNetflowTemp();
                    i = pck.dataSize();
                    tmp.readUp(pck);
                    o -= i - pck.dataSize();
                    temp.put(tmp);
                }
                continue;
            }
            packNetflowTemp tmp = new packNetflowTemp();
            tmp.id = i;
            tmp = temp.find(tmp);
            if (tmp == null) {
                pck.getSkip(o);
                continue;
            }
            for (;;) {
                if (o < tmp.siz) {
                    break;
                }
                tabSessionEntry ntry = new tabSessionEntry(false);
                ntry.rxPack = pck.msbGetD(tmp.oPck);
                ntry.rxByte = pck.msbGetD(tmp.oByt);
                ntry.srcPrt = pck.msbGetW(tmp.oUsrc);
                ntry.trgPrt = pck.msbGetW(tmp.oUtrg);
                ntry.ipPrt = pck.getByte(tmp.oPrt);
                ntry.ipTos = pck.getByte(tmp.oTos);
                ntry.srcAdr = new addrIP();
                ntry.trgAdr = new addrIP();
                if (tmp.oTsrc) {
                    addrIPv4 adr = new addrIPv4();
                    pck.getAddr(adr, tmp.oIsrc);
                    ntry.srcAdr.fromIPv4addr(adr);
                } else {
                    addrIPv6 adr = new addrIPv6();
                    pck.getAddr(adr, tmp.oIsrc);
                    ntry.srcAdr.fromIPv6addr(adr);
                }
                if (tmp.oTtrg) {
                    addrIPv4 adr = new addrIPv4();
                    pck.getAddr(adr, tmp.oItrg);
                    ntry.trgAdr.fromIPv4addr(adr);
                } else {
                    addrIPv6 adr = new addrIPv6();
                    pck.getAddr(adr, tmp.oItrg);
                    ntry.trgAdr.fromIPv6addr(adr);
                }
                res.add(ntry);
                o -= tmp.siz;
                pck.getSkip(tmp.siz);
            }
            pck.getSkip(o);
        }
        return res;
    }

}

class packNetflowTemp implements Comparator<packNetflowTemp> {

    public int id;

    public int siz;

    public int oPck;

    public int oByt;

    public int oPrt;

    public int oTos;

    public int oUsrc;

    public int oUtrg;

    public int oIsrc;

    public int oItrg;

    public boolean oTsrc;

    public boolean oTtrg;

    public int compare(packNetflowTemp o1, packNetflowTemp o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    public void readUp(packHolder pck) {
        id = pck.msbGetW(0); // template id
        int fld = pck.msbGetW(2); // field count
        pck.getSkip(4);
        int ofs = 0;
        for (int i = 0; i < fld; i++) {
            int typ = pck.msbGetW(0); // type
            int len = pck.msbGetW(2); // length
            pck.getSkip(4);
            switch (typ) {
                case 2: // packets
                    oPck = ofs;
                    break;
                case 1: // bytes
                    oByt = ofs;
                    break;
                case 4: // protocol
                    oPrt = ofs;
                    break;
                case 5: // tos
                    oTos = ofs;
                    break;
                case 7: // l4src
                    oUsrc = ofs;
                    break;
                case 11: // l4trg
                    oUtrg = ofs;
                    break;
                case 8: // l3src
                    oIsrc = ofs;
                    oTsrc = true;
                    break;
                case 12: // l3trg
                    oItrg = ofs;
                    oTtrg = true;
                    break;
                case 27: // l3src
                    oIsrc = ofs;
                    oTsrc = false;
                    break;
                case 28: // l3trg
                    oItrg = ofs;
                    oTtrg = false;
                    break;
            }
            ofs += len;
        }
        siz = ofs;
    }

}
