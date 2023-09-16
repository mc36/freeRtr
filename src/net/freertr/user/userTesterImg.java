package net.freertr.user;

import java.util.List;
import net.freertr.addr.addrMac;

/**
 * one tester image
 *
 * @author matecsaba
 */
public class userTesterImg {

    /**
     * filename
     */
    protected final String otherF;

    protected String otherP = null;

    protected String otherC1 = null;

    protected String otherC2 = null;

    protected String otherC3 = null;

    protected int otherNS = 0;

    protected String otherNC = null;

    protected String otherW = null;

    protected String otherS = null;

    protected List<String> otherD = null;

    /**
     * create instance
     *
     * @param s filename
     */
    protected userTesterImg(String s) {
        otherF = s;
    }

    /**
     * convert to udp endpoint
     *
     * @param cmd command to update
     * @param fn filename
     * @param cp control port
     * @param lp local port
     * @param rp remote port
     * @param ad mac address
     * @return hwcfg string
     */
    protected String convert(String cmd, String fn, int cp, List<Integer> lp, List<Integer> rp, List<addrMac> ad) {
        String nc = "";
        for (int i = otherNS; i < lp.size(); i++) {
            String a = otherNC;
            a = a.replaceAll("\\$id\\$", "" + i);
            a = a.replaceAll("\\$lp\\$", "" + lp.get(i));
            a = a.replaceAll("\\$rp\\$", "" + rp.get(i));
            a = a.replaceAll("\\$ad\\$", "" + ad.get(i).toEmuStr());
            nc += a;
        }
        cmd = cmd.replaceAll("\\$nc\\$", nc);
        cmd = cmd.replaceAll("\\$fn\\$", fn);
        cmd = cmd.replaceAll("\\$cp\\$", "" + cp);
        return cmd;
    }

}
