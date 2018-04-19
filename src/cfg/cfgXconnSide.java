package cfg;

import addr.addrIP;
import clnt.clntAx25;
import clnt.clntDlsw;
import clnt.clntErspan;
import clnt.clntEtherIp;
import clnt.clntUti;
import clnt.clntNvGre;
import clnt.clntL2tp2;
import clnt.clntL2tp3;
import clnt.clntMplsPwe;
import clnt.clntPckOudp;
import clnt.clntPptp;
import clnt.clntVxlan;
import clnt.clntGeneve;
import clnt.clntGrePpp;
import clnt.clntL2f;
import ifc.ifcNshFwd;
import ifc.ifcNull;
import ifc.ifcUp;
import pack.packLdpPwe;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * xconnect side
 *
 * @author matecsaba
 */
public class cfgXconnSide {

    /**
     * pw mode
     */
    public cfgVpdn.protocolType pwmod;

    /**
     * pw name
     */
    public String name = "xconn";

    /**
     * pw encapsulation
     */
    public int pwtype = packLdpPwe.pwtEthPort;

    /**
     * pw mtu
     */
    public int pwmtu = 1500;

    /**
     * vlan pw type
     */
    public boolean pwtVlan = false;

    /**
     * pw out direction
     */
    public boolean pwdirOut = false;

    /**
     * pw in direction
     */
    public boolean pwdirIn = false;

    /**
     * control word
     */
    public boolean ctrlWord = false;

    /**
     * upper layer handler
     */
    public ifcUp upper = new ifcNull();

    private cfgVrf vrf;

    private cfgIfc ifc;

    private addrIP adr;

    private int vcid;

    private clntPckOudp pou;

    private clntGrePpp pog;

    private clntAx25 ax25;

    private clntL2f l2f;

    private clntVxlan vxl;

    private clntGeneve gnv;

    private clntPptp pptp;

    private clntL2tp2 l2tp2;

    private clntL2tp3 l2tp3;

    private clntMplsPwe pwom;

    private clntErspan erspan;

    private clntDlsw dlsw;

    private clntEtherIp etherip;

    private clntUti uti;

    private clntNvGre nvgre;

    /**
     * get help text
     *
     * @param l help to append
     * @param p initial position
     */
    public static void getHelp(userHelping l, int p) {
        l.add((p + 0) + " " + (p + 1) + "     <name>                      vrf to use");
        l.add((p + 1) + " " + (p + 2) + "       <name>                    source interface to use");
        l.add((p + 2) + " " + (p + 3) + "         pckoudp                 pckoudp encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         pptp                    pptp encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         greppp                  ppp over gre encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         ax25                    ax25 encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         l2f                     l2f encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         l2tp2                   l2tp v2 encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         l2tp3                   l2tp v3 encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         pweompls                pwe over mpls encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         erspan                  erspan encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         dlsw                    dlsw encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         etherip                 etherip encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         uti                     uti encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         nvgre                   nvgre encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         vxlan                   vxlan encapsulation");
        l.add((p + 2) + " " + (p + 3) + "         geneve                  geneve encapsulation");
        l.add((p + 3) + " " + (p + 4) + "           <addr>                address of target");
        l.add((p + 4) + " " + (p + 5) + ",.           <num>               vc id");
        l.add((p + 5) + " " + (p + 5) + ",.             control-word      use control word");
        l.add((p + 5) + " " + (p + 5) + ",.             inbound           inbound direction");
        l.add((p + 5) + " " + (p + 5) + ",.             outbound          outbound direction");
        l.add((p + 5) + " " + (p + 5) + ",.             vlan              use vlan pw type");
    }

    /**
     * check if ready
     *
     * @return true if yes, false if no
     */
    public boolean ready2run() {
        return pwmod != null;
    }

    /**
     * stop this pseudowire
     */
    public void stop2run() {
        if (pou != null) {
            pou.workStop();
            pou = null;
        }
        if (pog != null) {
            pog.workStop();
            pog = null;
        }
        if (ax25 != null) {
            ax25.workStop();
            ax25 = null;
        }
        if (l2f != null) {
            l2f.workStop();
            l2f = null;
        }
        if (vxl != null) {
            vxl.workStop();
            vxl = null;
        }
        if (gnv != null) {
            gnv.workStop();
            gnv = null;
        }
        if (pptp != null) {
            pptp.workStop();
            pptp = null;
        }
        if (l2tp2 != null) {
            l2tp2.workStop();
            l2tp2 = null;
        }
        if (l2tp3 != null) {
            l2tp3.workStop();
            l2tp3 = null;
        }
        if (pwom != null) {
            pwom.workStop();
            pwom = null;
        }
        if (erspan != null) {
            erspan.workStop();
            erspan = null;
        }
        if (dlsw != null) {
            dlsw.workStop();
            dlsw = null;
        }
        if (etherip != null) {
            etherip.workStop();
            etherip = null;
        }
        if (uti != null) {
            uti.workStop();
            uti = null;
        }
        if (nvgre != null) {
            nvgre.workStop();
            nvgre = null;
        }
    }

    private boolean getDir() {
        if (pwdirOut) {
            return true;
        }
        if (pwdirIn) {
            return false;
        }
        switch (pwmod) {
            case prL2tp2:
                return false;
            case prPptp:
                return true;
            case prL2tp3:
                return adr.compare(adr, ifc.getLocAddr(adr)) < 0;
            default:
                return true;
        }
    }

    /**
     * start this pseudowire
     */
    public void start2run() {
        if (pwtVlan) {
            pwtype = packLdpPwe.pwtEthVlan;
        }
        switch (pwmod) {
            case prPou:
                pou = new clntPckOudp();
                pou.target = "" + adr;
                pou.vrf = vrf;
                pou.srcIfc = ifc;
                pou.prtR = vcid;
                pou.prtL = vcid;
                pou.setUpper(upper);
                pou.workStart();
                break;
            case prPog:
                pog = new clntGrePpp();
                pog.target = "" + adr;
                pog.vrf = vrf;
                pog.srcIfc = ifc;
                pog.vcid = vcid;
                pog.setUpper(upper);
                pog.workStart();
                break;
            case prAx25:
                ax25 = new clntAx25();
                ax25.target = "" + adr;
                ax25.vrf = vrf;
                ax25.srcIfc = ifc;
                ax25.setUpper(upper);
                ax25.workStart();
                break;
            case prL2f:
                l2f = new clntL2f();
                l2f.target = "" + adr;
                l2f.vrf = vrf;
                l2f.srcIfc = ifc;
                l2f.setUpper(upper);
                l2f.workStart();
                break;
            case prVxlan:
                vxl = new clntVxlan();
                vxl.target = "" + adr;
                vxl.vrf = vrf;
                vxl.srcIfc = ifc;
                vxl.inst = vcid;
                vxl.prot = ifcNshFwd.protEth;
                vxl.wildcard = ctrlWord;
                vxl.setUpper(upper);
                vxl.workStart();
                break;
            case prGeneve:
                gnv = new clntGeneve();
                gnv.target = "" + adr;
                gnv.vrf = vrf;
                gnv.srcIfc = ifc;
                gnv.vni = vcid;
                gnv.setUpper(upper);
                gnv.workStart();
                break;
            case prPptp:
                pptp = new clntPptp();
                pptp.target = "" + adr;
                pptp.vrf = vrf;
                pptp.srcIfc = ifc;
                pptp.direction = getDir();
                pptp.called = "" + vcid;
                pptp.setUpper(upper);
                pptp.workStart();
                break;
            case prL2tp2:
                l2tp2 = new clntL2tp2();
                l2tp2.target = "" + adr;
                l2tp2.vrf = vrf;
                l2tp2.srcIfc = ifc;
                l2tp2.direction = getDir();
                l2tp2.called = "" + vcid;
                l2tp2.calling = "" + vcid;
                l2tp2.setUpper(upper);
                l2tp2.workStart();
                break;
            case prL2tp3:
                l2tp3 = new clntL2tp3();
                l2tp3.pwType = pwtype;
                l2tp3.target = "" + adr;
                l2tp3.vrf = vrf;
                l2tp3.srcIfc = ifc;
                l2tp3.vcid = "" + vcid;
                l2tp3.direction = getDir();
                l2tp3.setUpper(upper);
                l2tp3.workStart();
                break;
            case prPwom:
                pwom = new clntMplsPwe();
                pwom.pwType = pwtype;
                pwom.pwMtu = pwmtu;
                pwom.target = "" + adr;
                pwom.vrf = vrf;
                pwom.srcIfc = ifc;
                pwom.vcid = vcid;
                pwom.ctrlWrd = ctrlWord;
                pwom.descr = name;
                pwom.setUpper(upper);
                pwom.workStart();
                break;
            case prErspan:
                erspan = new clntErspan();
                erspan.target = "" + adr;
                erspan.vrf = vrf;
                erspan.srcIfc = ifc;
                erspan.spnid = vcid;
                erspan.vlnid = vcid;
                erspan.setUpper(upper);
                erspan.workStart();
                break;
            case prDlsw:
                dlsw = new clntDlsw();
                dlsw.target = "" + adr;
                dlsw.vrf = vrf;
                dlsw.srcIfc = ifc;
                dlsw.setUpper(upper);
                dlsw.workStart();
                break;
            case prEtherip:
                etherip = new clntEtherIp();
                etherip.target = "" + adr;
                etherip.vrf = vrf;
                etherip.srcIfc = ifc;
                etherip.setUpper(upper);
                etherip.workStart();
                break;
            case prUti:
                uti = new clntUti();
                uti.target = "" + adr;
                uti.vrf = vrf;
                uti.srcIfc = ifc;
                uti.tunKey = vcid;
                uti.setUpper(upper);
                uti.workStart();
                break;
            case prNvgre:
                nvgre = new clntNvGre();
                nvgre.target = "" + adr;
                nvgre.vrf = vrf;
                nvgre.srcIfc = ifc;
                nvgre.vsid = vcid;
                nvgre.setUpper(upper);
                nvgre.workStart();
                break;
            default:
                break;
        }
    }

    /**
     * get config text
     *
     * @return text
     */
    public String getCfg() {
        if (!ready2run()) {
            return "";
        }
        String a = vrf.name + " " + ifc.name + " " + cfgVpdn.type2str(pwmod) + " " + adr + " " + vcid;
        if (ctrlWord) {
            a += " control-word";
        }
        if (pwtVlan) {
            a += " vlan";
        }
        if (pwdirIn) {
            a += " inbound";
        }
        if (pwdirOut) {
            a += " outbound";
        }
        return a;
    }

    /**
     * configure the pseudowire
     *
     * @param cmd commands to read
     */
    public void doCfg(cmds cmd) {
        vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        ifc = cfgAll.ifcFind(cmd.word(), false);
        if (ifc == null) {
            cmd.error("no such interface");
            return;
        }
        String md = cmd.word();
        adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        vcid = bits.str2num(cmd.word());
        if (vcid < 1) {
            cmd.error("bad vcid");
            return;
        }
        pwmod = cfgVpdn.str2type(md);
        if (pwmod == null) {
            cmd.error("bad mode");
            return;
        }
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("control-word")) {
                ctrlWord = true;
                continue;
            }
            if (a.equals("inbound")) {
                pwdirIn = true;
                continue;
            }
            if (a.equals("outbound")) {
                pwdirOut = true;
                continue;
            }
            if (a.equals("vlan")) {
                pwtVlan = true;
                continue;
            }
        }
    }

}
