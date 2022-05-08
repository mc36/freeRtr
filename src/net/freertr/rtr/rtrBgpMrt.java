package net.freertr.rtr;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.util.bits;
import net.freertr.util.logFil;

/**
 * multi-threaded routing toolkit
 *
 * @author matecsaba
 */
public class rtrBgpMrt implements Comparator<rtrBgpMrt> {

    /**
     * bgp type
     */
    public static final int typBgp = 16;

    /**
     * remote message
     */
    public static final int typRem32 = 4;

    /**
     * local message
     */
    public static final int typLoc32 = 7;

    /**
     * remote message
     */
    public static final int typRem16 = 1;

    /**
     * local message
     */
    public static final int typLoc16 = 6;

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
     * @param dir direction: false=rx, true=tx
     * @param asR remote as
     * @param asL local as
     * @param adrR remote address
     * @param adrL local address
     * @param dat size of bgp message
     * @return bytes written
     */
    public static int putMrtHeader(byte[] hdr, boolean dir, int asR, int asL, addrIP adrR, addrIP adrL, int dat) {
        bits.msbPutD(hdr, 0, (int) (bits.getTime() / 1000));
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
     * got update
     *
     * @param dir direction: false=rx, true=tx
     * @param typ type
     * @param nei neighbor
     * @param dat data bytes
     */
    public void gotMessage(boolean dir, int typ, rtrBgpNeigh nei, byte[] dat) {
        byte[] hdr = new byte[128];
        int len = putMrtHeader(hdr, dir, nei.remoteAs, nei.localAs, nei.peerAddr, nei.localAddr, dat.length + rtrBgpSpeak.sizeU);
        for (int i = 0; i < 16; i++) {
            hdr[len] = (byte) 0xff;
            len++;
        }
        bits.msbPutW(hdr, len, dat.length + rtrBgpSpeak.sizeU);
        hdr[len + 2] = (byte) typ;
        len += 3;
        fileHandle.add(hdr, 0, len, dat, 0, dat.length);
    }

}
