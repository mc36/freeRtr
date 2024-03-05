package org.freertr.rtr;

import java.io.RandomAccessFile;
import java.util.Comparator;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.logFil;

/**
 * multi-threaded routing toolkit
 *
 * @author matecsaba
 */
public class rtrBgpMrt implements Comparator<rtrBgpMrt> {

    /**
     * bgp type
     */
    public final static int typBgp = 16;

    /**
     * remote message
     */
    public final static int typRem32 = 4;

    /**
     * local message
     */
    public final static int typLoc32 = 7;

    /**
     * remote message
     */
    public final static int typRem16 = 1;

    /**
     * local message
     */
    public final static int typLoc16 = 6;

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

    public int compare(rtrBgpMrt o1, rtrBgpMrt o2) {
        return o1.dumpName.compareTo(o2.dumpName);
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
            bits.msbPutW(hdr, 6, typLoc32); // tx
        } else {
            bits.msbPutW(hdr, 6, typRem32); // rx
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
     * @param pck packet to populate
     * @param fa file to read
     * @return result, 0=ok, 1=stop, 2=continue
     */
    public static int readNextMrt(packHolder pck, RandomAccessFile fa) {
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
        if (typ != typBgp) {
            return 2;
        }
        boolean xchg = false;
        switch (cls) {
            case typLoc16:
                pck.getSkip(4);
                xchg = true;
                break;
            case typRem16:
                pck.getSkip(4);
                break;
            case typLoc32:
                xchg = true;
                pck.getSkip(8);
                break;
            case typRem32:
                pck.getSkip(8);
                break;
            default:
                return 2;
        }
        typ = pck.msbGetW(2);
        pck.getSkip(4);
        switch (typ) {
            case 1:
                addrIPv4 a4 = new addrIPv4();
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                pck.IPtrg.fromIPv4addr(a4);
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                pck.IPsrc.fromIPv4addr(a4);
                break;
            case 2:
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
        if (pck.msbGetD(0) != -1) {
            try {
                fa.seek(fa.getFilePointer() - buf.length);
            } catch (Exception e) {
            }
            return 2;
        }
        if (xchg) {
            return 0;
        }
        addrIP adr = new addrIP();
        adr.setAddr(pck.IPsrc);
        pck.IPsrc.setAddr(pck.IPtrg);
        pck.IPtrg.setAddr(adr);
        return 0;
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
