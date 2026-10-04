package org.freertr.user;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgInit;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * process hw addresser
 *
 * @author matecsaba
 */
public class userHwadr {

    /**
     * create instance
     */
    public userHwadr() {
    }

    private String filnam = "./rtr-" + cfgInit.swCfgEnd;

    private String iface = "ethernet20001";

    private int add = 0;

    private int sub = 0;

    private int xor = 0;

    private static addrIP num2adr(int num) {
        addrIP oth = new addrIP();
        bits.msbPutD(oth.getBytes(), addrIP.size - 4, num);
        return oth;
    }

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        cmds orig = cmd;
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            s = s.toLowerCase();
            if (s.equals("file")) {
                filnam = cmd.word();
                continue;
            }
            if (s.equals("iface")) {
                iface = cmd.word();
                continue;
            }
            if (s.equals("add")) {
                add = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("sub")) {
                sub = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("xor")) {
                xor = bits.str2num(cmd.word());
                continue;
            }
        }
        List<String> lst = bits.txt2buf(filnam);
        if (lst == null) {
            orig.error("error reading sw config");
            return;
        }
        int i = lst.indexOf("interface " + iface);
        if (i < 0) {
            orig.error("interface not found");
            return;
        }
        addrIP adr = null;
        for (; i < lst.size(); i++) {
            String s = lst.get(i);
            cmd = new cmds("lin", s.trim());
            s = cmd.word();
            if (s.equals(cmds.comment)) {
                break;
            }
            if (s.equals(cmds.finish)) {
                break;
            }
            if (!s.equals("ipv4")) {
                continue;
            }
            if (!cmd.word().equals("address")) {
                continue;
            }
            adr = new addrIP();
            adr.fromString(cmd.word());
            break;
        }
        if (adr == null) {
            orig.error("address not found");
            return;
        }
        adr.setAdd(adr, num2adr(add));
        adr.setSub(adr, num2adr(sub));
        adr.setXor(adr, num2adr(xor));
        orig.pipe.linePut("" + adr);
    }

}
