package net.freertr.cfg;

import net.freertr.addr.addrIP;
import net.freertr.clnt.clntAx25;
import net.freertr.clnt.clntCapwap;
import net.freertr.clnt.clntDlsw;
import net.freertr.clnt.clntErspan;
import net.freertr.clnt.clntEtherIp;
import net.freertr.clnt.clntGeneve;
import net.freertr.clnt.clntGrePpp;
import net.freertr.clnt.clntGreTap;
import net.freertr.clnt.clntL2f;
import net.freertr.clnt.clntL2tp2;
import net.freertr.clnt.clntL2tp3;
import net.freertr.clnt.clntLlcudp;
import net.freertr.clnt.clntLwapp;
import net.freertr.clnt.clntMplsPwe;
import net.freertr.clnt.clntNvGre;
import net.freertr.clnt.clntPckOudp;
import net.freertr.clnt.clntPptp;
import net.freertr.clnt.clntSrEth;
import net.freertr.clnt.clntTzsp;
import net.freertr.clnt.clntUti;
import net.freertr.clnt.clntVxlan;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNshFwd;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packLdpPwe;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * xconnect side
 *
 * @author matecsaba
 */
public class cfgXconnSide {

    /**
     * create instance
     */
    public cfgXconnSide() {
    }

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

    /**
     * lower layer handler
     */
    public ifcDn lower = new ifcNull();

    /**
     * vrf
     */
    public cfgVrf vrf;

    /**
     * source interface
     */
    public cfgIfc ifc;

    /**
     * target
     */
    public addrIP adr;

    /**
     * vc id
     */
    public int vcid;

    /**
     * pckoudp
     */
    public clntPckOudp pou;

    /**
     * gre ppp
     */
    public clntGrePpp pog;

    /**
     * ax25
     */
    public clntAx25 ax25;

    /**
     * l2f
     */
    public clntL2f l2f;

    /**
     * vxlan
     */
    public clntVxlan vxl;

    /**
     * geneve
     */
    public clntGeneve gnv;

    /**
     * llcudp
     */
    public clntLlcudp lcu;

    /**
     * tzsp
     */
    public clntTzsp tzs;

    /**
     * capwap
     */
    public clntCapwap cpw;

    /**
     * lwapp
     */
    public clntLwapp lwp;

    /**
     * pptp
     */
    public clntPptp pptp;

    /**
     * l2tp v2
     */
    public clntL2tp2 l2tp2;

    /**
     * l2tp v3
     */
    public clntL2tp3 l2tp3;

    /**
     * eompls
     */
    public clntMplsPwe pwom;

    /**
     * erspan
     */
    public clntErspan erspan;

    /**
     * dlsw
     */
    public clntDlsw dlsw;

    /**
     * etherip
     */
    public clntEtherIp etherip;

    /**
     * sreth
     */
    public clntSrEth sreth;

    /**
     * gretap
     */
    public clntGreTap tog;

    /**
     * uti
     */
    public clntUti uti;

    /**
     * nvgre
     */
    public clntNvGre nvgre;

    /**
     * get help text
     *
     * @param l help to append
     * @param p initial position
     */
    public static void getHelp(userHelping l, int p) {
        l.add(null, (p + 0) + " " + (p + 1) + "     <name:vrf>                  vrf to use");
        l.add(null, (p + 1) + " " + (p + 2) + "       <name:ifc>                source interface to use");
        l.add(null, (p + 2) + " " + (p + 3) + "         pckoudp                 pckoudp encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         pptp                    pptp encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         greppp                  ppp over gre encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         ax25                    ax25 encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         l2f                     l2f encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         l2tp2                   l2tp v2 encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         l2tp3                   l2tp v3 encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         pweompls                pwe over mpls encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         erspan                  erspan encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         dlsw                    dlsw encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         etherip                 etherip encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         sreth                   sreth encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         gretap                  gretap encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         uti                     uti encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         nvgre                   nvgre encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         vxlan                   vxlan encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         geneve                  geneve encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         llcudp                  llcudp encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         tzsp                    tzsp encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         capwap                  capwap encapsulation");
        l.add(null, (p + 2) + " " + (p + 3) + "         lwapp                   lwapp encapsulation");
        l.add(null, (p + 3) + " " + (p + 4) + "           <addr>                address of target");
        l.add(null, (p + 4) + " " + (p + 5) + ",.           <num>               vc id");
        l.add(null, (p + 5) + " " + (p + 5) + ",.             control-word      use control word");
        l.add(null, (p + 5) + " " + (p + 5) + ",.             inbound           inbound direction");
        l.add(null, (p + 5) + " " + (p + 5) + ",.             outbound          outbound direction");
        l.add(null, (p + 5) + " " + (p + 5) + ",.             vlan              use vlan pw type");
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
        if (cpw != null) {
            cpw.workStop();
            cpw = null;
        }
        if (lwp != null) {
            lwp.workStop();
            lwp = null;
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
        if (sreth != null) {
            sreth.workStop();
            sreth = null;
        }
        if (tog != null) {
            tog.workStop();
            tog = null;
        }
        if (uti != null) {
            uti.workStop();
            uti = null;
        }
        if (nvgre != null) {
            nvgre.workStop();
            nvgre = null;
        }
        lower = new ifcNull();
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
            switch (pwtype) {
                case packLdpPwe.pwtPpp:
                    pwtype = packLdpPwe.pwtHdlc;
                    break;
                case packLdpPwe.pwtAtmAal5:
                    pwtype = packLdpPwe.pwtAtmPort;
                    break;
                default:
                    pwtype = packLdpPwe.pwtEthVlan;
                    break;
            }
        }
        switch (pwmod) {
            case prPou:
                pou = new clntPckOudp();
                pou.target = "" + adr;
                pou.vrf = vrf;
                pou.srcIfc = ifc;
                pou.prtR = vcid;
                if (ctrlWord) {
                    pou.prtL = -1;
                } else {
                    pou.prtL = vcid;
                }
                pou.setUpper(upper);
                pou.workStart();
                lower = pou;
                break;
            case prPog:
                pog = new clntGrePpp();
                pog.target = "" + adr;
                pog.vrf = vrf;
                pog.srcIfc = ifc;
                pog.vcid = vcid;
                pog.setUpper(upper);
                pog.workStart();
                lower = pog;
                break;
            case prAx25:
                ax25 = new clntAx25();
                ax25.target = "" + adr;
                ax25.vrf = vrf;
                ax25.srcIfc = ifc;
                ax25.setUpper(upper);
                ax25.workStart();
                lower = ax25;
                break;
            case prL2f:
                l2f = new clntL2f();
                l2f.target = "" + adr;
                l2f.vrf = vrf;
                l2f.srcIfc = ifc;
                l2f.setUpper(upper);
                l2f.workStart();
                lower = l2f;
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
                lower = vxl;
                break;
            case prGeneve:
                gnv = new clntGeneve();
                gnv.target = "" + adr;
                gnv.vrf = vrf;
                gnv.srcIfc = ifc;
                gnv.vni = vcid;
                gnv.setUpper(upper);
                gnv.workStart();
                lower = gnv;
                break;
            case prLlcudp:
                lcu = new clntLlcudp();
                lcu.target = "" + adr;
                lcu.vrf = vrf;
                lcu.srcIfc = ifc;
                lcu.setUpper(upper);
                lcu.workStart();
                lower = lcu;
                break;
            case prTzsp:
                tzs = new clntTzsp();
                tzs.target = "" + adr;
                tzs.vrf = vrf;
                tzs.srcIfc = ifc;
                tzs.setUpper(upper);
                tzs.workStart();
                lower = tzs;
                break;
            case prCapwap:
                cpw = new clntCapwap();
                cpw.target = "" + adr;
                cpw.vrf = vrf;
                cpw.srcIfc = ifc;
                cpw.setUpper(upper);
                cpw.workStart();
                lower = cpw;
                break;
            case prLwapp:
                lwp = new clntLwapp();
                lwp.target = "" + adr;
                lwp.vrf = vrf;
                lwp.srcIfc = ifc;
                lwp.setUpper(upper);
                lwp.workStart();
                lower = lwp;
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
                lower = pptp;
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
                lower = l2tp2;
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
                lower = l2tp3;
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
                lower = pwom;
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
                lower = erspan;
                break;
            case prDlsw:
                dlsw = new clntDlsw();
                dlsw.target = "" + adr;
                dlsw.vrf = vrf;
                dlsw.srcIfc = ifc;
                dlsw.setUpper(upper);
                dlsw.workStart();
                lower = dlsw;
                break;
            case prEtherip:
                etherip = new clntEtherIp();
                etherip.target = "" + adr;
                etherip.vrf = vrf;
                etherip.srcIfc = ifc;
                etherip.setUpper(upper);
                etherip.workStart();
                lower = etherip;
                break;
            case prSreth:
                sreth = new clntSrEth();
                sreth.target = "" + adr;
                sreth.vrf = vrf;
                sreth.srcIfc = ifc;
                sreth.setUpper(upper);
                sreth.workStart();
                lower = sreth;
                break;
            case prTog:
                tog = new clntGreTap();
                tog.target = "" + adr;
                tog.vrf = vrf;
                tog.srcIfc = ifc;
                tog.vcid = vcid;
                tog.setUpper(upper);
                tog.workStart();
                lower = tog;
                break;
            case prUti:
                uti = new clntUti();
                uti.target = "" + adr;
                uti.vrf = vrf;
                uti.srcIfc = ifc;
                uti.tunKey = vcid;
                uti.setUpper(upper);
                uti.workStart();
                lower = uti;
                break;
            case prNvgre:
                nvgre = new clntNvGre();
                nvgre.target = "" + adr;
                nvgre.vrf = vrf;
                nvgre.srcIfc = ifc;
                nvgre.vsid = vcid;
                nvgre.setUpper(upper);
                nvgre.workStart();
                lower = nvgre;
                break;
            default:
                lower = new ifcNull();
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
