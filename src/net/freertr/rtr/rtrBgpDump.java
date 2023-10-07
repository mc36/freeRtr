package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.enc.enc7bit;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.util.bits;

/**
 * bgp message dumper
 *
 * @author matecsaba
 */
public class rtrBgpDump {

    /**
     * convert hexdump log to packet
     *
     * @param s string to convert
     * @return converted packet
     */
    public static packHolder log2pck(String s) {
        if (s == null) {
            return null;
        }
        String a = s.replaceAll("\\|", "");
        int i = a.indexOf("ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff");
        if (i < 1) {
            return null;
        }
        s = a.substring(i, a.length());
        a = a.substring(0, i);
        s = s.replaceAll(" ", "");
        packHolder pck = new packHolder(true, true);
        pck.INTtime = bits.str2time(cfgAll.timeZoneName, a);
        int o = s.length() & 0xfffffe;
        for (i = 0; i < o; i += 2) {
            pck.putByte(0, bits.fromHex(s.substring(i, i + 2)));
            pck.putSkip(1);
            pck.merge2end();
        }
        i = a.indexOf("->");
        if (i < 0) {
            return pck;
        }
        s = a.substring(i + 2, a.length()).trim();
        a = a.substring(0, i).trim();
        i = a.lastIndexOf(" ");
        if (i > 0) {
            a = a.substring(i + 1, a.length());
        }
        i = s.indexOf(" ");
        if (i > 0) {
            s = s.substring(0, i);
        }
        pck.IPsrc.fromString(a);
        pck.IPtrg.fromString(s);
        return pck;
    }

    /**
     * decode bgp dumps
     *
     * @param txt text to read
     * @return list of packets
     */
    public static List<packHolder> logs2pcks(List<String> txt) {
        List<packHolder> res = new ArrayList<packHolder>();
        if (txt == null) {
            return res;
        }
        for (int i = 0; i < txt.size(); i++) {
            String a = txt.get(i);
            packHolder p = log2pck(a);
            if (p == null) {
                continue;
            }
            res.add(p);
        }
        return res;
    }

    /**
     * dump one packet
     *
     * @param pck packet to dump
     */
    public static List<String> dumpPacket(packHolder pck) {
        List<String> res = new ArrayList<String>();
        res.add(bits.time2str(cfgAll.timeZoneName, pck.INTtime + cfgAll.timeServerOffset, 3) + " " + pck.IPsrc + " -> " + pck.IPtrg);
        enc7bit.buf2hex(res, pck.getCopy(), 0);
        //////////////////
        return res;
    }

}
