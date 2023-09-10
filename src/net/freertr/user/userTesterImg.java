package net.freertr.user;

import java.util.List;
import net.freertr.addr.addrMac;

/**
 * one tester image
 *
 * @author matecsaba
 */
public class userTesterImg {

    public String otherF = null;

    public String otherP = null;

    public String otherC1 = null;

    public String otherC2 = null;

    public String otherC3 = null;

    public int otherNS = 0;

    public String otherNC = null;

    public String otherW = null;

    public String otherS = null;

    public List<String> otherD = null;

    public String convert(String cmd, String fn, int cp, List<Integer> lp, List<Integer> rp, List<addrMac> ad) {
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
