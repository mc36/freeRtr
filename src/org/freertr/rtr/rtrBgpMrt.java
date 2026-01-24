package org.freertr.rtr;

import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.logFil;
import org.freertr.addr.addrSafi;

/**
 * multi-threaded routing toolkit
 *
 * @author matecsaba
 */
public class rtrBgpMrt implements Comparable<rtrBgpMrt> {

    /**
     * bgp type
     */
    public final static int typBgp = 16;

    /**
     * rib type
     */
    public final static int typRib = 13;

    /**
     * remote as4 message
     */
    public final static int bgpRem32 = 4;

    /**
     * local as4 message
     */
    public final static int bgpLoc32 = 7;

    /**
     * remote as2 message
     */
    public final static int bgpRem16 = 1;

    /**
     * local as2 message
     */
    public final static int bgpLoc16 = 6;

    /**
     * remote addpath as4 message
     */
    public final static int bgpRem32ap = 9;

    /**
     * local addpath as4 message
     */
    public final static int bgpLoc32ap = 11;

    /**
     * remote addpath as2 message
     */
    public final static int bgpRem16ap = 8;

    /**
     * local addpath as2 message
     */
    public final static int bgpLoc16ap = 10;

    /**
     * ipv4 unicast rib
     */
    public final static int ribIp4uni = 2;

    /**
     * ipv4 multicast rib
     */
    public final static int ribIp4mul = 3;

    /**
     * ipv6 unicast rib
     */
    public final static int ribIp6uni = 4;

    /**
     * ipv6 multicast rib
     */
    public final static int ribIp6mul = 5;

    /**
     * ipv4 addpath unicast rib
     */
    public final static int ribIp4uniAp = 2;

    /**
     * ipv4 addpath multicast rib
     */
    public final static int ribIp4mulAp = 3;

    /**
     * ipv6 addpath unicast rib
     */
    public final static int ribIp6uniAp = 4;

    /**
     * ipv6 addpath multicast rib
     */
    public final static int ribIp6mulAp = 5;

    /**
     * name of dump
     */
    public final String dumpName;

    /**
     * handler of dump
     */
    public logFil fileHandle;

    /**
     * create instance
     *
     * @param nam name of dump
     */
    public rtrBgpMrt(String nam) {
        dumpName = nam;
    }

    public String toString() {
        return dumpName;
    }

    public int compareTo(rtrBgpMrt o) {
        return dumpName.compareTo(o.dumpName);
    }

    /**
     * get configuration
     *
     * @param l list to append
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        String a = fileHandle.rotate2();
        if (a == null) {
            a = "";
        } else {
            a = " " + a;
        }
        l.add(beg + "dump " + dumpName + " " + fileHandle.name() + a);
    }

    /**
     * create mrt header
     *
     * @param hdr array to update
     * @param tim time to write
     * @param dir direction: false=rx, true=tx
     * @param asR remote as
     * @param asL local as
     * @param adrR remote address
     * @param adrL local address
     * @param dat size of bgp message
     * @return bytes written
     */
    public static int putMrtHeader(byte[] hdr, long tim, boolean dir, int asR, int asL, addrIP adrR, addrIP adrL, int dat) {
        bits.msbPutD(hdr, 0, (int) (tim / 1000));
        bits.msbPutW(hdr, 4, typBgp); // type
        if (dir) {
            bits.msbPutW(hdr, 6, bgpLoc32); // tx
        } else {
            bits.msbPutW(hdr, 6, bgpRem32); // rx
        }
        bits.msbPutD(hdr, 12, asR);
        bits.msbPutD(hdr, 16, asL);
        bits.msbPutW(hdr, 20, 0); // ifindex
        int len = 24;
        if (adrR.isIPv4()) {
            bits.msbPutW(hdr, 22, 1); // addr type
            adrR.toIPv4().toBuffer(hdr, len);
            len += addrIPv4.size;
            adrL.toIPv4().toBuffer(hdr, len);
            len += addrIPv4.size;
        } else {
            bits.msbPutW(hdr, 22, 2); // addr type
            adrR.toIPv6().toBuffer(hdr, len);
            len += addrIPv6.size;
            adrL.toIPv6().toBuffer(hdr, len);
            len += addrIPv6.size;
        }
        bits.msbPutD(hdr, 8, dat + len - 12); // length
        return len;
    }

    /**
     * read up next mrt entry
     *
     * @param spk where to signal
     * @param hlp temporary packet
     * @param tmp temporary packet
     * @param pck packet to populate
     * @param fa file to read
     * @return result, 0=ok, 1=stop, 2=continue
     */
    public static int readNextMrt(rtrBgpSpeak spk, packHolder hlp, packHolder tmp, packHolder pck, RandomAccessFile fa) {
        pck.clear();
        byte[] buf = new byte[12];
        try {
            if (fa.read(buf, 0, buf.length) != buf.length) {
                return 1;
            }
        } catch (Exception e) {
            return 1;
        }
        pck.INTtime = bits.msbGetD(buf, 0);
        pck.INTtime *= 1000;
        int typ = bits.msbGetW(buf, 4);
        int cls = bits.msbGetW(buf, 6);
        int i = bits.msbGetD(buf, 8);
        if (i < 0) {
            return 2;
        }
        if (i > packHolder.maxHead) {
            return 2;
        }
        buf = new byte[i];
        try {
            if (fa.read(buf, 0, buf.length) != buf.length) {
                return 1;
            }
        } catch (Exception e) {
            return 1;
        }
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        switch (typ) {
            case typRib:
                addrSafi rdr;
                switch (cls) {
                    case ribIp4uni:
                        typ = rtrBgpUtil.safiIp4uni;
                        rdr = addrSafi.ipv4uni;
                        break;
                    case ribIp4mul:
                        typ = rtrBgpUtil.safiIp4multi;
                        rdr = addrSafi.ipv4uni;
                        break;
                    case ribIp6uni:
                        typ = rtrBgpUtil.safiIp6uni;
                        rdr = addrSafi.ipv6uni;
                        break;
                    case ribIp6mul:
                        typ = rtrBgpUtil.safiIp6multi;
                        rdr = addrSafi.ipv6uni;
                        break;
                    default:
                        return 2;
                }
                pck.getSkip(4);
                tabRouteEntry<addrIP> pfx = rdr.readPrefix(true, pck);
                if (pfx == null) {
                    return 2;
                }
                int o = pck.msbGetW(0);
                pck.getSkip(2);
                tmp.clear();
                for (i = 0; i < o; i++) {
                    int t = pck.msbGetD(2);
                    int p = pck.msbGetW(6);
                    pck.getSkip(8);
                    if (pck.dataSize() < p) {
                        break;
                    }
                    if (tmp.dataSize() > p) {
                        pck.getSkip(p);
                        continue;
                    }
                    buf = new byte[p];
                    pck.getCopy(buf, 0, 0, buf.length);
                    pck.getSkip(buf.length);
                    tmp.clear();
                    tmp.putCopy(buf, 0, 0, buf.length);
                    tmp.putSkip(buf.length);
                    tmp.merge2end();
                    tmp.INTtime = t;
                }
                pck.clear();
                pck.INTtime = tmp.INTtime * 1000;
                for (;;) {
                    if (tmp.dataSize() < 1) {
                        break;
                    }
                    if (rtrBgpUtil.parseAttrib(tmp, hlp)) {
                        break;
                    }
                    if (hlp.ETHtype == rtrBgpUtil.attrReachable) {
                        hlp.getSkip(1);
                        if (hlp.dataSize() == addrIPv4.size) {
                            addrIPv4 a4 = new addrIPv4();
                            hlp.getAddr(a4, 0);
                            pfx.best.nextHop = new addrIP();
                            pfx.best.nextHop.fromIPv4addr(a4);
                        } else {
                            addrIPv6 a6 = new addrIPv6();
                            hlp.getAddr(a6, 0);
                            pfx.best.nextHop = new addrIP();
                            pfx.best.nextHop.fromIPv6addr(a6);
                        }
                        continue;
                    }
                    rtrBgpUtil.placeAttrib(spk, hlp.ETHcos, hlp.ETHtype, pck, hlp);
                    pck.merge2end();
                }
                if (typ != rtrBgpUtil.safiIp4uni) {
                    if (pfx.best.nextHop == null) {
                        pfx.best.nextHop = new addrIP();
                    }
                    List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
                    lst.add(pfx);
                    rtrBgpUtil.placeReachable(spk, typ, false, true, pck, hlp, lst);
                    pck.merge2end();
                }
                pck.msbPutW(0, 0);
                pck.msbPutW(2, pck.dataSize());
                pck.putSkip(4);
                pck.merge2beg();
                if (typ == rtrBgpUtil.safiIp4uni) {
                    addrSafi.ipv4uni.writePrefix(true, pck, pfx);
                    pck.merge2end();
                }
                pck.ETHtype = typ;
                rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgUpdate);
                return 0;
            case typBgp:
                boolean xchg = false;
                switch (cls) {
                    case bgpLoc16:
                        pck.getSkip(4);
                        xchg = true;
                        break;
                    case bgpRem16:
                        pck.getSkip(4);
                        break;
                    case bgpLoc32:
                        xchg = true;
                        pck.getSkip(8);
                        break;
                    case bgpRem32:
                        pck.getSkip(8);
                        break;
                    default:
                        return 2;
                }
                typ = pck.msbGetW(2);
                pck.getSkip(4);
                switch (typ) {
                    case 1:
                        typ = rtrBgpUtil.safiIp4uni;
                        addrIPv4 a4 = new addrIPv4();
                        pck.getAddr(a4, 0);
                        pck.getSkip(addrIPv4.size);
                        pck.IPtrg.fromIPv4addr(a4);
                        pck.getAddr(a4, 0);
                        pck.getSkip(addrIPv4.size);
                        pck.IPsrc.fromIPv4addr(a4);
                        break;
                    case 2:
                        typ = rtrBgpUtil.safiIp6uni;
                        addrIPv6 a6 = new addrIPv6();
                        pck.getAddr(a6, 0);
                        pck.getSkip(addrIPv6.size);
                        pck.IPtrg.fromIPv6addr(a6);
                        pck.getAddr(a6, 0);
                        pck.getSkip(addrIPv6.size);
                        pck.IPsrc.fromIPv6addr(a6);
                        break;
                    default:
                        return 2;
                }
                pck.ETHtype = typ;
                if (xchg) {
                    return 0;
                }
                addrIP adr = new addrIP();
                adr.setAddr(pck.IPsrc);
                pck.IPsrc.setAddr(pck.IPtrg);
                pck.IPtrg.setAddr(adr);
                return 0;
            default:
                return 2;
        }
    }

    /**
     * dump route table
     *
     * @param fil file to use
     * @param spk where to signal
     * @param idx safi to use
     * @param safi safi to use
     * @param tab table to dump
     * @param dir direction
     * @param ipv ip version
     * @param asR remote as
     * @param asL local as
     * @param adrR peer address
     * @param adrL local address
     */
    public static void dumpTable(RandomAccessFile fil, rtrBgpSpeak spk, int idx, int safi, tabRoute<addrIP> tab, boolean dir, int ipv, int asR, int asL, addrIP adrR, addrIP adrL) {
        if (tab == null) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        packHolder tmp = new packHolder(true, true);
        byte[] hdr = new byte[128];
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            rtrBgpDump.witeFormat(spk, idx, safi, ntry, ipv, pck, tmp, true);
            int len = rtrBgpMrt.putMrtHeader(hdr, ntry.best.time, dir, asR, asL, adrR, adrL, pck.dataSize());
            pck.putCopy(hdr, 0, 0, len);
            pck.putSkip(len);
            pck.merge2beg();
            byte[] buf = pck.getCopy();
            try {
                fil.write(buf);
            } catch (Exception e) {
            }
        }
    }

    /**
     * got update
     *
     * @param dir direction: false=rx, true=tx
     * @param typ type
     * @param nei neighbor
     * @param dat data bytes
     */
    public void gotMessage(boolean dir, int typ, rtrBgpNeigh nei, byte[] dat) {
        byte[] hdr = new byte[128];
        int len = putMrtHeader(hdr, bits.getTime(), dir, nei.remoteAs, nei.localAs, nei.peerAddr, nei.localAddr, dat.length + rtrBgpUtil.sizeU);
        for (int i = 0; i < rtrBgpUtil.markS; i++) {
            hdr[len] = (byte) rtrBgpUtil.markV;
            len++;
        }
        bits.msbPutW(hdr, len, dat.length + rtrBgpUtil.sizeU);
        hdr[len + 2] = (byte) typ;
        len += 3;
        fileHandle.add(hdr, 0, len, dat, 0, dat.length);
    }

}
