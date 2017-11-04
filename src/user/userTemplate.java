package user;

import addr.addrIP;
import addr.addrMac;
import cfg.cfgInit;
import java.util.ArrayList;
import java.util.List;
import util.bits;
import util.cmds;
import util.version;

/**
 * template creation
 *
 * @author matecsaba
 */
public class userTemplate {

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        List<String> t1 = bits.txt2buf(cmd.word());
        if (t1 == null) {
            cmd.error("template not found");
            return;
        }
        List<String> t2 = getPart(t1);
        addrIP loop4adr = new addrIP();
        addrIP loop6adr = new addrIP();
        addrIP net4adr = new addrIP();
        addrIP net6adr = new addrIP();
        addrMac macAdr = new addrMac();
        loop4adr.fromString(t2.get(0));
        net4adr.fromString(t2.get(1));
        loop6adr.fromString(t2.get(2));
        net6adr.fromString(t2.get(3));
        macAdr.fromString(t2.get(4));
        t2 = getPart(t1);
        int num = bits.str2num(cmd.word());
        String res = cmd.word();
        cmd.error("loop4=" + loop4adr + " net4=" + net4adr);
        cmd.error("loop6=" + loop6adr + " net6=" + net6adr);
        cmd.error("mac=" + macAdr + " output=" + res + " num=" + num);
        userTemplateInst[] rs = new userTemplateInst[num + 2];
        addrIP incIp1 = new addrIP();
        addrIP incIp4 = new addrIP();
        addrMac incMac = new addrMac();
        int sock = 30000;
        incIp1.fromString("::1");
        incIp4.fromString("::4");
        incMac.fromString("0000.0000.0001");
        for (int i = 0; i < rs.length; i++) {
            userTemplateInst c = new userTemplateInst();
            rs[i] = c;
            c.num = i;
            c.sockA = sock;
            sock++;
            c.sockB = sock;
            sock++;
            c.sockC = sock;
            sock++;
            c.sockD = sock;
            sock++;
            c.macA.setAddr(macAdr);
            macAdr.setAdd(macAdr, incMac);
            c.macB.setAddr(macAdr);
            macAdr.setAdd(macAdr, incMac);
            c.lo4a.setAddr(loop4adr);
            loop4adr.setAdd(loop4adr, incIp1);
            c.lo4b.setAddr(loop4adr);
            loop4adr.setAdd(loop4adr, incIp1);
            c.lo6a.setAddr(loop6adr);
            loop6adr.setAdd(loop6adr, incIp1);
            c.lo6b.setAddr(loop6adr);
            loop6adr.setAdd(loop6adr, incIp1);
            c.net4a.setAddr(net4adr);
            net4adr.setAdd(net4adr, incIp4);
            c.net4b.setAddr(net4adr);
            net4adr.setAdd(net4adr, incIp4);
            c.net6a.setAddr(net6adr);
            net6adr.setAdd(net6adr, incIp4);
            c.net6b.setAddr(net6adr);
            net6adr.setAdd(net6adr, incIp4);
            c.net4a1.setAdd(c.net4a, incIp1);
            c.net4b1.setAdd(c.net4b, incIp1);
            c.net6a1.setAdd(c.net6a, incIp1);
            c.net6b1.setAdd(c.net6b, incIp1);
            c.net4a2.setAdd(c.net4a1, incIp1);
            c.net4b2.setAdd(c.net4b1, incIp1);
            c.net6a2.setAdd(c.net6a1, incIp1);
            c.net6b2.setAdd(c.net6b1, incIp1);
        }
        List<String> t4 = new ArrayList<String>();
        List<String> t5 = new ArrayList<String>();
        for (int i = 1; i < rs.length - 1; i++) {
            String a = res + "r" + i + "-";
            List<String> t3 = getCfg(t2, rs[i - 1], rs[i], rs[i + 1]);
            bits.buf2txt(true, t3, a + cfgInit.hwCfgEnd);
            t3 = getCfg(t1, rs[i - 1], rs[i], rs[i + 1]);
            bits.buf2txt(true, t3, a + cfgInit.swCfgEnd);
            a = "java -Xmx256m -jar " + version.getFileName() + " router " + a;
            t4.add(a + "&");
            t5.add("start /b " + a);
        }
        bits.buf2txt(true, t4, res + ".sh");
        bits.buf2txt(true, t5, res + ".bat");
    }

    private List<String> getPart(List<String> t1) {
        List<String> t2 = new ArrayList<String>();
        for (int i = 0; i < t1.size(); i++) {
            String a = t1.get(0);
            if (a.length() > 0) {
                break;
            }
            t1.remove(0);
        }
        for (int i = 0; i < t1.size(); i++) {
            String a = t1.get(0);
            t1.remove(0);
            if (a.length() < 1) {
                break;
            }
            t2.add(a);
        }
        return t2;
    }

    private List<String> getCfg(List<String> t1, userTemplateInst p, userTemplateInst t, userTemplateInst n) {
        List<String> t2 = new ArrayList<String>();
        for (int i = 0; i < t1.size(); i++) {
            String a = t1.get(i);
            a = doReplace(a, "\\$prev", p);
            a = doReplace(a, "\\$this", t);
            a = doReplace(a, "\\$next", n);
            t2.add(a);
        }
        return t2;
    }

    private String doReplace(String a, String p, userTemplateInst d) {
        a = a.replaceAll(p + "Num\\$", "" + d.num);
        a = a.replaceAll(p + "Sock1\\$", "127.0.0.1 " + d.sockA);
        a = a.replaceAll(p + "Sock2\\$", "127.0.0.1 " + d.sockB);
        a = a.replaceAll(p + "Sock3\\$", "127.0.0.1 " + d.sockC);
        a = a.replaceAll(p + "Sock4\\$", "127.0.0.1 " + d.sockD);
        a = a.replaceAll(p + "Mac1\\$", "" + d.macA);
        a = a.replaceAll(p + "Mac2\\$", "" + d.macB);
        a = a.replaceAll(p + "Loop4a\\$", "" + d.lo4a);
        a = a.replaceAll(p + "Loop4b\\$", "" + d.lo4b);
        a = a.replaceAll(p + "Loop6a\\$", "" + d.lo6a);
        a = a.replaceAll(p + "Loop6b\\$", "" + d.lo6b);
        a = a.replaceAll(p + "Net4a\\$", "" + d.net4a);
        a = a.replaceAll(p + "Net4b\\$", "" + d.net4b);
        a = a.replaceAll(p + "Net6a\\$", "" + d.net6a);
        a = a.replaceAll(p + "Net6b\\$", "" + d.net6b);
        a = a.replaceAll(p + "Net4a1\\$", "" + d.net4a1);
        a = a.replaceAll(p + "Net4b1\\$", "" + d.net4b1);
        a = a.replaceAll(p + "Net6a1\\$", "" + d.net6a1);
        a = a.replaceAll(p + "Net6b1\\$", "" + d.net6b1);
        a = a.replaceAll(p + "Net4a2\\$", "" + d.net4a2);
        a = a.replaceAll(p + "Net4b2\\$", "" + d.net4b2);
        a = a.replaceAll(p + "Net6a2\\$", "" + d.net6a2);
        a = a.replaceAll(p + "Net6b2\\$", "" + d.net6b2);
        return a;
    }

}

class userTemplateInst {

    public int num;

    public int sockA;

    public int sockB;

    public int sockC;

    public int sockD;

    public addrMac macA = new addrMac();

    public addrMac macB = new addrMac();

    public addrIP lo4a = new addrIP();

    public addrIP lo4b = new addrIP();

    public addrIP lo6a = new addrIP();

    public addrIP lo6b = new addrIP();

    public addrIP net4a = new addrIP();

    public addrIP net4b = new addrIP();

    public addrIP net6a = new addrIP();

    public addrIP net6b = new addrIP();

    public addrIP net4a1 = new addrIP();

    public addrIP net4b1 = new addrIP();

    public addrIP net6a1 = new addrIP();

    public addrIP net6b1 = new addrIP();

    public addrIP net4a2 = new addrIP();

    public addrIP net4b2 = new addrIP();

    public addrIP net6a2 = new addrIP();

    public addrIP net6b2 = new addrIP();

}
