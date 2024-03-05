package org.freertr.user;

import java.util.List;
import org.freertr.addr.addrMac;

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

    /**
     * ports
     */
    protected String otherP = null;

    /**
     * command to execute
     */
    protected String otherC1 = null;

    /**
     * command to execute
     */
    protected String otherC2 = null;

    /**
     * command to execute
     */
    protected String otherC3 = null;

    /**
     * number of ports
     */
    protected int otherNS = 0;

    /**
     * ports string
     */
    protected String otherNC = null;

    /**
     * string to wait
     */
    protected String otherW = null;

    /**
     * syncer string
     */
    protected String otherS = null;

    /**
     * default config
     */
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
    protected String convert2udp(String cmd, String fn, int cp, List<Integer> lp, List<Integer> rp, List<addrMac> ad) {
        String nc = "";
        for (int i = otherNS; i < lp.size(); i++) {
            String a = "" + otherNC;
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
