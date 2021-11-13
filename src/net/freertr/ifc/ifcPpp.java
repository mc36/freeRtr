package net.freertr.ifc;

import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrEui;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrType;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authLocal;
import net.freertr.auth.authResult;
import net.freertr.auth.authenDown;
import net.freertr.auth.authenHead;
import net.freertr.auth.autherChap;
import net.freertr.auth.autherDoer;
import net.freertr.auth.autherEap;
import net.freertr.auth.autherPap;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgIfc;
import net.freertr.pack.packHolder;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * point to point protocol (rfc1661) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcPpp implements ifcUp, ifcDn, authenDown {

    /**
     * sent username
     */
    public String sentUser = null;

    /**
     * sent password
     */
    public String sentPass = null;

    /**
     * sent async char map
     */
    public int sentAccm;

    /**
     * sent max recv unit
     */
    public int sentMru;

    /**
     * authentication list to use for remote
     */
    public authGeneric authenRem = null;

    /**
     * my configured address
     */
    public addrEui locIfIdCfg;

    /**
     * peer configured address
     */
    public addrEui remIfIdCfg;

    /**
     * my configured address
     */
    public addrIPv4 locAddrCfg;

    /**
     * peer configured address
     */
    public addrIPv4 remAddrCfg;

    /**
     * dns1 address, null=negotiated
     */
    public addrIPv4 dns1addrCfg;

    /**
     * dns2 address, null=negotiated
     */
    public addrIPv4 dns2addrCfg;

    /**
     * keep ipv6 address
     */
    public boolean keepIpv6addr;

    /**
     * local address required
     */
    public boolean locAddrReq;

    /**
     * refuse pap
     */
    public boolean refusePap;

    /**
     * refuse chap
     */
    public boolean refuseChap;

    /**
     * refuse eap
     */
    public boolean refuseEap;

    /**
     * multilink configured, 0=off, 1=short, 1=long
     */
    public int multilinkCfg;

    /**
     * multilink mrru
     */
    public int multilinkMrru;

    /**
     * negotiated multilink rx
     */
    public int multilinkRx;

    /**
     * negotiated multilink tx
     */
    public int multilinkTx;

    /**
     * max payload size
     */
    public int fragLen = 0;

    /**
     * max payload delay
     */
    public int fragGap = 0;

    /**
     * tx sequence
     */
    public int fragSeqTx = 0;

    /**
     * rx sequence
     */
    public int fragSeqRx = -1;

    /**
     * reassembly buffer
     */
    public packHolder fragReasm = new packHolder(true, true);

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * type of fragment header
     */
    public final static int fragType = 0x003d;

    /**
     * first fragment
     */
    public final static int fragBeg = 0x8000;

    /**
     * last fragment
     */
    public final static int fragEnd = 0x4000;

    /**
     * last known state
     */
    public state.states lastState = state.states.down;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * current keepalive interval (0=disabled)
     */
    public int keepaliveInterval = 5000;

    /**
     * nak retry limit
     */
    public int nakRetryLimit = 16;

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * config class
     */
    public cfgIfc cfger;

    private Timer keepTimer;

    private int curMode = modeLcp;

    private int reqSent;

    private final static int modeLcp = 1;

    private final static int modeAuth = 2;

    private final static int modeUp = 3;

    /**
     * lcp
     */
    public ifcPppLcp ctrlLcp;

    /**
     * authentication
     */
    public autherDoer ctrlAuth;

    /**
     * ip4vp
     */
    public ifcPppIp4 ctrlIp4;

    /**
     * ip6cp
     */
    public ifcPppIp6 ctrlIp6;

    /**
     * bcp
     */
    public ifcPppBrdg ctrlBrdg;

    /**
     * mplscp
     */
    public ifcPppMpls ctrlMpls;

    /**
     * osicp
     */
    public ifcPppOsi ctrlOsi;

    /**
     * ipxcp
     */
    public ifcPppIpx ctrlIpx;

    /**
     * ecp
     */
    public ifcPppCrypt ctrlCrypt;

    /**
     * nshcp
     */
    public ifcPppNsh ctrlNsh;

    /**
     * polkacp
     */
    public ifcPppPolka ctrlPolka;

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * close interface
     */
    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
        restartTimer(true);
    }

    /**
     * close interface
     */
    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
        restartTimer(true);
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
        setState(lastState);
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return lastState;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        if (checkPeerState(stat)) {
            return;
        }
        restartTimer(false);
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return lower.getMTUsize() - size;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return lower.getBandwidth();
    }

    /**
     * create new instance
     */
    public ifcPpp() {
        ctrlLcp = new ifcPppLcp(this);
        ctrlIp4 = new ifcPppIp4(this);
        ctrlIp6 = new ifcPppIp6(this);
        ctrlBrdg = new ifcPppBrdg(this);
        ctrlMpls = new ifcPppMpls(this);
        ctrlOsi = new ifcPppOsi(this);
        ctrlIpx = new ifcPppIpx(this);
        ctrlCrypt = new ifcPppCrypt(this);
        ctrlNsh = new ifcPppNsh(this);
        ctrlPolka = new ifcPppPolka(this);
        clearState();
        restartTimer(false);
    }

    public String toString() {
        return "ppp on " + lower;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add(null, "2 3     keepalive                   keepalive timer");
        l.add(null, "3 .       <num>                     time in ms");
        l.add(null, "2 3     username                    name of user to send");
        l.add(null, "3 .       <text>                    username");
        l.add(null, "2 3     password                    password of user to send");
        l.add(null, "3 .       <text>                    password");
        l.add(null, "2 3     accm                        async character map");
        l.add(null, "3 .       <num>                     accm");
        l.add(null, "2 3     mru                         max receive unit");
        l.add(null, "3 .       <num>                     mru");
        l.add(null, "2 3     refuseauth                  never use authentication protocol");
        l.add(null, "3 .       pap                       password authentication protocol");
        l.add(null, "3 .       chap                      challenge handshake authentication protocol");
        l.add(null, "3 .       eap                       extensible authentication protocol");
        l.add(null, "2 3     authentication              set peer authentication list");
        l.add(null, "3 .       <text>                    name of list");
        l.add(null, "2 3     multilink                   multilink operation");
        l.add(null, "3 4       <num>                     mrru");
        l.add(null, "4 .         none                    disable operation");
        l.add(null, "4 .         short                   negotiate short header");
        l.add(null, "4 .         long                    negotiate long header");
        l.add(null, "2 3     fragment                    set payload size");
        l.add(null, "3 .       <num>                     number of bytes");
        l.add(null, "2 3     frgap                       inter fragment gap");
        l.add(null, "3 .       <num>                     milliseconds");
        l.add(null, "2 3     naktry                      nak retry limit");
        l.add(null, "3 .       <num>                     number of tries");
        l.add(null, "2 3     ip4cp                       ipv4 control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "3 .       reqaddr                   require local address");
        l.add(null, "3 4       peer                      set peer address");
        l.add(null, "4 .         <text>                  address");
        l.add(null, "3 4       local                     set local address");
        l.add(null, "4 .         <text>                  address");
        l.add(null, "3 4       dns1                      set dns1 address");
        l.add(null, "4 .         <text>                  address");
        l.add(null, "3 4       dns2                      set dns1 address");
        l.add(null, "4 .         <text>                  address");
        l.add(null, "2 3     ip6cp                       ipv6 control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "3 .       keep                      keet configured address");
        l.add(null, "3 4       local                     set local address");
        l.add(null, "4 .         <text>                  address");
        l.add(null, "3 4       peer                      set peer address");
        l.add(null, "4 .         <text>                  address");
        l.add(null, "2 3     bcp                         bridge control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "2 3     mplscp                      multiprotocol label switching control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "2 3     osicp                       open systems interconnect control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "2 3     ipxcp                       ipx control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "2 3     ecp                         encryption control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "2 3     nshcp                       service control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
        l.add(null, "2 3     polkacp                     polka control protocol");
        l.add(null, "3 .       open                      force to open state");
        l.add(null, "3 .       close                     force to close state");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     * @param filter filter defaults
     */
    public void getConfig(List<String> l, String beg, int filter) {
        l.add(beg + "keepalive " + keepaliveInterval);
        cmds.cfgLine(l, authenRem == null, cmds.tabulator, "ppp authentication", "" + authenRem);
        cmds.cfgLine(l, sentUser == null, cmds.tabulator, "ppp username", sentUser);
        cmds.cfgLine(l, sentPass == null, cmds.tabulator, "ppp password", authLocal.passwdEncode(sentPass, (filter & 2) != 0));
        String a;
        switch (multilinkCfg) {
            case 1:
                a = "short";
                break;
            case 2:
                a = "long";
                break;
            default:
                a = "none";
                break;
        }
        l.add(beg + "multilink " + multilinkMrru + " " + a);
        l.add(beg + "fragment " + fragLen);
        l.add(beg + "frgap " + fragGap);
        cmds.cfgLine(l, !refusePap, cmds.tabulator, "ppp refuseauth pap", "");
        cmds.cfgLine(l, !refuseChap, cmds.tabulator, "ppp refuseauth chap", "");
        cmds.cfgLine(l, !refuseEap, cmds.tabulator, "ppp refuseauth eap", "");
        l.add(beg + "naktry " + nakRetryLimit);
        l.add(beg + "accm " + sentAccm);
        l.add(beg + "mru " + sentMru);
        cmds.cfgLine(l, !ctrlIp4.forced2close(), cmds.tabulator, "ppp ip4cp close", "");
        cmds.cfgLine(l, !ctrlIp4.forced2open(), cmds.tabulator, "ppp ip4cp open", "");
        cmds.cfgLine(l, remAddrCfg == null, cmds.tabulator, "ppp ip4cp peer", "" + remAddrCfg);
        cmds.cfgLine(l, locAddrCfg == null, cmds.tabulator, "ppp ip4cp local", "" + locAddrCfg);
        cmds.cfgLine(l, dns1addrCfg == null, cmds.tabulator, "ppp ip4cp dns1", "" + dns1addrCfg);
        cmds.cfgLine(l, dns2addrCfg == null, cmds.tabulator, "ppp ip4cp dns2", "" + dns2addrCfg);
        cmds.cfgLine(l, !locAddrReq, cmds.tabulator, "ppp ip4cp reqaddr", "");
        cmds.cfgLine(l, !ctrlIp6.forced2close(), cmds.tabulator, "ppp ip6cp close", "");
        cmds.cfgLine(l, !ctrlIp6.forced2open(), cmds.tabulator, "ppp ip6cp open", "");
        cmds.cfgLine(l, remIfIdCfg == null, cmds.tabulator, "ppp ip6cp peer", "" + remIfIdCfg);
        cmds.cfgLine(l, locIfIdCfg == null, cmds.tabulator, "ppp ip6cp local", "" + locIfIdCfg);
        cmds.cfgLine(l, !keepIpv6addr, cmds.tabulator, "ppp ip6cp keep", "");
        cmds.cfgLine(l, !ctrlBrdg.forced2close(), cmds.tabulator, "ppp bcp close", "");
        cmds.cfgLine(l, !ctrlBrdg.forced2open(), cmds.tabulator, "ppp bcp open", "");
        cmds.cfgLine(l, !ctrlMpls.forced2close(), cmds.tabulator, "ppp mplscp close", "");
        cmds.cfgLine(l, !ctrlMpls.forced2open(), cmds.tabulator, "ppp mplscp open", "");
        cmds.cfgLine(l, !ctrlOsi.forced2close(), cmds.tabulator, "ppp osicp close", "");
        cmds.cfgLine(l, !ctrlOsi.forced2open(), cmds.tabulator, "ppp osicp open", "");
        cmds.cfgLine(l, !ctrlIpx.forced2close(), cmds.tabulator, "ppp ipxcp close", "");
        cmds.cfgLine(l, !ctrlIpx.forced2open(), cmds.tabulator, "ppp ipxcp open", "");
        cmds.cfgLine(l, !ctrlCrypt.forced2close(), cmds.tabulator, "ppp ecp close", "");
        cmds.cfgLine(l, !ctrlCrypt.forced2open(), cmds.tabulator, "ppp ecp open", "");
        cmds.cfgLine(l, !ctrlNsh.forced2close(), cmds.tabulator, "ppp nshcp close", "");
        cmds.cfgLine(l, !ctrlNsh.forced2open(), cmds.tabulator, "ppp nshcp open", "");
        cmds.cfgLine(l, !ctrlPolka.forced2close(), cmds.tabulator, "ppp polkacp close", "");
        cmds.cfgLine(l, !ctrlPolka.forced2open(), cmds.tabulator, "ppp polkacp open", "");
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("refuseauth")) {
            a = cmd.word();
            if (a.equals("pap")) {
                refusePap = true;
                return;
            }
            if (a.equals("chap")) {
                refuseChap = true;
                return;
            }
            if (a.equals("eap")) {
                refuseEap = true;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ip4cp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIp4.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlIp4.forceClose(true);
                return;
            }
            if (a.equals("peer")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                remAddrCfg = adr;
                ctrlIp4.remAddrCur = adr.copyBytes();
                return;
            }
            if (a.equals("local")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                locAddrCfg = adr;
                return;
            }
            if (a.equals("reqaddr")) {
                locAddrReq = true;
                return;
            }
            if (a.equals("dns1")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                dns1addrCfg = adr;
                return;
            }
            if (a.equals("dns2")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                dns2addrCfg = adr;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ip6cp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIp6.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlIp6.forceClose(true);
                return;
            }
            if (a.equals("local")) {
                addrEui adr = new addrEui();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad eui address");
                    return;
                }
                locIfIdCfg = adr;
                return;
            }
            if (a.equals("peer")) {
                addrEui adr = new addrEui();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad eui address");
                    return;
                }
                remIfIdCfg = adr;
                return;
            }
            if (a.equals("keep")) {
                keepIpv6addr = true;
                return;
            }
        }
        if (a.equals("bcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlBrdg.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlBrdg.forceClose(true);
                return;
            }
        }
        if (a.equals("mplscp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlMpls.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlMpls.forceClose(true);
                return;
            }
        }
        if (a.equals("osicp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlOsi.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlOsi.forceClose(true);
                return;
            }
        }
        if (a.equals("ipxcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIpx.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlIpx.forceClose(true);
                return;
            }
        }
        if (a.equals("ecp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlCrypt.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlCrypt.forceClose(true);
                return;
            }
        }
        if (a.equals("nshcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlNsh.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlNsh.forceClose(true);
                return;
            }
        }
        if (a.equals("polkacp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlPolka.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlPolka.forceClose(true);
                return;
            }
        }
        if (a.equals("keepalive")) {
            keepaliveInterval = bits.str2num(cmd.word());
            restartTimer(false);
            return;
        }
        if (a.equals("username")) {
            sentUser = cmd.getRemaining();
            return;
        }
        if (a.equals("password")) {
            sentPass = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (a.equals("accm")) {
            sentAccm = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("mru")) {
            sentMru = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("multilink")) {
            multilinkMrru = bits.str2num(cmd.word());
            a = cmd.word();
            multilinkCfg = 0;
            if (a.equals("short")) {
                multilinkCfg = 1;
            }
            if (a.equals("long")) {
                multilinkCfg = 2;
            }
            return;
        }
        if (a.equals("fragment")) {
            fragLen = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("frgap")) {
            fragGap = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("naktry")) {
            nakRetryLimit = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("authentication")) {
            cfgAuther auth = cfgAll.autherFind(cmd.word(), null);
            if (auth == null) {
                cmd.error("no such authentication list");
                return;
            }
            authenRem = auth.getAuther();
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo configuration
     *
     * @param cmd command
     */
    public void unConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("refuseauth")) {
            a = cmd.word();
            if (a.equals("pap")) {
                refusePap = false;
                return;
            }
            if (a.equals("chap")) {
                refuseChap = false;
                return;
            }
            if (a.equals("eap")) {
                refuseEap = false;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ip4cp")) {
            a = cmd.word();
            if (a.equals("peer")) {
                remAddrCfg = null;
                ctrlIp4.remAddrCur = null;
                return;
            }
            if (a.equals("local")) {
                locAddrCfg = null;
                return;
            }
            if (a.equals("reqaddr")) {
                locAddrReq = false;
                return;
            }
            if (a.equals("dns1")) {
                dns1addrCfg = null;
                return;
            }
            if (a.equals("dns2")) {
                dns2addrCfg = null;
                return;
            }
            if (a.equals("open")) {
                ctrlIp4.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlIp4.forceClose(false);
                return;
            }
            cmd.badCmd();
        }
        if (a.equals("ip6cp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIp6.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlIp6.forceClose(false);
                return;
            }
            if (a.equals("local")) {
                locIfIdCfg = null;
                return;
            }
            if (a.equals("peer")) {
                remIfIdCfg = null;
                return;
            }
            if (a.equals("keep")) {
                keepIpv6addr = false;
                return;
            }
        }
        if (a.equals("bcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlBrdg.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlBrdg.forceClose(false);
                return;
            }
        }
        if (a.equals("mplscp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlMpls.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlMpls.forceClose(false);
                return;
            }
        }
        if (a.equals("osicp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlOsi.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlOsi.forceClose(false);
                return;
            }
        }
        if (a.equals("ipxcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIpx.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlIpx.forceClose(false);
                return;
            }
        }
        if (a.equals("ecp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlCrypt.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlCrypt.forceClose(false);
                return;
            }
        }
        if (a.equals("nshcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlNsh.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlNsh.forceClose(false);
                return;
            }
        }
        if (a.equals("polkacp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlPolka.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlPolka.forceClose(false);
                return;
            }
        }
        if (a.equals("username")) {
            sentUser = null;
            return;
        }
        if (a.equals("password")) {
            sentPass = null;
            return;
        }
        if (a.equals("accm")) {
            sentAccm = 0;
            return;
        }
        if (a.equals("mru")) {
            sentMru = 0;
            return;
        }
        if (a.equals("multilink")) {
            multilinkMrru = 0;
            multilinkCfg = 0;
            return;
        }
        if (a.equals("fragment")) {
            fragLen = 0;
            return;
        }
        if (a.equals("frgap")) {
            fragGap = 0;
            return;
        }
        if (a.equals("authentication")) {
            authenRem = null;
            return;
        }
        cmd.badCmd();
    }

    /**
     * clear state info
     */
    public void clearState() {
        try {
            ctrlAuth.stopThread();
        } catch (Exception e) {
        }
        ctrlAuth = null;
        curMode = modeLcp;
        multilinkRx = 0;
        multilinkTx = 0;
        ctrlLcp.clearState();
        ctrlIp4.clearState();
        ctrlIp6.clearState();
        ctrlBrdg.clearState();
        ctrlMpls.clearState();
        ctrlOsi.clearState();
        ctrlIpx.clearState();
        ctrlCrypt.clearState();
        ctrlNsh.clearState();
        ctrlPolka.clearState();
    }

    /**
     * check peer state from timers
     *
     * @param force try to force line protocol
     * @return true if state changed
     */
    public boolean checkPeerState(state.states force) {
        state.states stat = state.states.down;
        if (ctrlLcp.getReady()) {
            stat = state.states.up;
        }
        if (keepaliveInterval < 1) {
            stat = state.states.up;
        }
        if (state.toForceable(force) == state.states.admin) {
            stat = state.states.admin;
        }
        if (lastState == stat) {
            return false;
        }
        if (stat != state.states.up) {
            clearState();
        }
        lastState = stat;
        if (debugger.ifcPppEvnt) {
            logger.debug("line proto=" + state.conv2string(stat));
        }
        if (stat != state.states.up) {
            lower.flapped();
        } else {
            sendKeepReq();
        }
        cntr.stateChange(stat);
        upper.setState(stat);
        return true;
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (lastState == state.states.admin) {
            return;
        }
        if (shutdown) {
            return;
        }
        if (keepaliveInterval < 1) {
            return;
        }
        keepTimer = new Timer();
        ifcPppTxKeep task = new ifcPppTxKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval);
    }

    /**
     * put address and control bytes
     *
     * @param pck packet to use
     * @param proto protocol number
     */
    public void putAddrCtrlProto(packHolder pck, int proto) {
        pck.putByte(0, 0xff); // address
        pck.putByte(1, 0x03); // control
        pck.msbPutW(2, proto); // protocol
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * send ncp control packet
     *
     * @param pck packet to update
     * @param proto protocol number
     * @param code code number
     * @param id id value
     */
    public void sendNcpCtrl(packHolder pck, int proto, int code, int id) {
        authenHead cis = new authenHead();
        cis.code = code;
        cis.id = id;
        cis.updatePack(pck);
        putAddrCtrlProto(pck, proto);
        lower.sendPack(pck);
        if (debugger.ifcPppEvnt) {
            logger.debug("tx " + ifcPppNcp.code2str(code) + " id=" + id);
        }
    }

    /**
     * received one control packet
     *
     * @param pck packet received
     * @param cp control protocol got it
     * @param proto protocol id
     * @return true went up
     */
    public boolean recvNcpCtrl(packHolder pck, ifcPppNcp cp, int proto) {
        authenHead cis = new authenHead();
        if (cis.parsePack(pck)) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return false;
        }
        if (debugger.ifcPppEvnt) {
            logger.debug("rx " + ifcPppNcp.code2str(cis.code) + " id=" + cis.id);
        }
        boolean b = cp.getReady();
        cp.recvPck(pck, cis.code, cis.id);
        if (b || (!cp.getReady())) {
            return false;
        }
        if (cfger == null) {
            return true;
        }
        switch (proto) {
            case ifcPppIp4.pppCtrl:
                cfger.addr4changed(ctrlIp4.locAddrCur, cfger.mask4, ctrlIp4.remAddrCur);
                break;
            case ifcPppIp6.pppCtrl:
                addrIPv6 saved = null;
                if (keepIpv6addr) {
                    saved = cfger.addr6.copyBytes();
                }
                cfger.addr6changed(ctrlIp6.locAddrCur.toIPv6(null), cfger.mask6, ctrlIp6.remAddrCur.toIPv6(null));
                if (saved != null) {
                    cfger.addr6changed(saved, cfger.mask6, ctrlIp6.remAddrCur.toIPv6(saved));
                }
                break;
        }
        return true;
    }

    /**
     * received authentication packet
     *
     * @param pck received packet
     */
    public void recvAuthPack(packHolder pck) {
        if (ctrlAuth == null) {
            return;
        }
        authenHead cis = new authenHead();
        if (cis.parsePack(pck)) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (debugger.ifcPppEvnt) {
            logger.debug("rx " + cis);
        }
        ctrlAuth.recvPck(pck, cis.code, cis.id);
    }

    /**
     * send authentication packet
     *
     * @param pck packet
     * @param proto protocol
     * @param code code
     * @param id id
     * @param msg message
     */
    public void sendAuthPack(packHolder pck, int proto, int code, int id, String msg) {
        authenHead cis = new authenHead();
        cis.code = code;
        cis.id = id;
        cis.updatePack(pck);
        putAddrCtrlProto(pck, proto);
        lower.sendPack(pck);
        if (debugger.ifcPppEvnt) {
            logger.debug("tx " + msg);
        }
    }

    public void recvAuthPack(String msg) {
        if (debugger.ifcPppEvnt) {
            logger.debug("rx " + msg);
        }
    }

    /**
     * check for authentication up
     */
    public void checkAuthUp() {
        if (ctrlAuth == null) {
            return;
        }
        if (ctrlAuth.working) {
            return;
        }
        boolean b = ctrlAuth.result.result == authResult.authSuccessful;
        ctrlAuth.stopThread();
        if (debugger.ifcPppEvnt) {
            logger.debug("authentication passed=" + b);
        }
        if (!b) {
            clearState();
            return;
        }
        curMode = modeUp;
        if (ctrlAuth.result.ipv4addr != null) {
            remAddrCfg = ctrlAuth.result.ipv4addr;
            ctrlIp4.clearState();
        }
        if (ctrlAuth.result.ipv6ifid != null) {
            remIfIdCfg = ctrlAuth.result.ipv6ifid;
            ctrlIp6.clearState();
        }
        if ((ctrlAuth.result.ipv4route != null) && (cfger.fwdIf4 != null)) {
            cfger.fwdIf4.gatePrfx = authGeneric.route2prefixes(ctrlAuth.result.ipv4route);
        }
        if ((ctrlAuth.result.ipv6route != null) && (cfger.fwdIf6 != null)) {
            cfger.fwdIf6.gatePrfx = authGeneric.route2prefixes(ctrlAuth.result.ipv6route);
        }
        sendKeepReq();
    }

    /**
     * send ncp configure request
     *
     * @param ncp protocol to use
     */
    public void sendConfReq(ifcPppNcp ncp) {
        if (!ncp.getNeeded()) {
            return;
        }
        if (ncp.getReady()) {
            return;
        }
        if (ncp.sawReq3 < 1) {
            reqSent++;
            ncp.sendReq();
        }
        ncp.sawReq3 = 0;
    }

    /**
     * send keepalive over lcp
     */
    public void sendKeepReq() {
        int oldMod = curMode;
        switch (curMode) {
            case modeLcp:
                if (!ctrlLcp.getReady()) {
                    ctrlLcp.sendReq();
                    break;
                }
                multilinkTx = ctrlLcp.multiLoc;
                multilinkRx = ctrlLcp.multiRem;
                ctrlIp4.clearState();
                ctrlIp6.clearState();
                ctrlBrdg.clearState();
                ctrlMpls.clearState();
                ctrlOsi.clearState();
                ctrlIpx.clearState();
                ctrlCrypt.clearState();
                ctrlNsh.clearState();
                ctrlPolka.clearState();
                if (ctrlLcp.authLoc > 0) {
                    ctrlAuth = autherDoer.getWorker(this, ctrlLcp.authLoc);
                    if (ctrlAuth == null) {
                        curMode = modeUp;
                        break;
                    }
                    ctrlAuth.sentUser = sentUser;
                    ctrlAuth.sentPass = sentPass;
                    curMode = modeAuth;
                    break;
                }
                if (authenRem != null) {
                    ctrlAuth = autherDoer.getWorker(this, ctrlLcp.authRem);
                    ctrlAuth.authenRem = authenRem;
                    ctrlAuth.startThread();
                    curMode = modeAuth;
                    break;
                }
                curMode = modeUp;
                break;
            case modeAuth:
                if (debugger.ifcPppEvnt) {
                    logger.debug("sending auth request");
                }
                ctrlAuth.sendReq();
                checkAuthUp();
                break;
            case modeUp:
                reqSent = 0;
                sendConfReq(ctrlLcp);
                sendConfReq(ctrlIp4);
                sendConfReq(ctrlIp6);
                sendConfReq(ctrlBrdg);
                sendConfReq(ctrlMpls);
                sendConfReq(ctrlOsi);
                sendConfReq(ctrlIpx);
                sendConfReq(ctrlCrypt);
                sendConfReq(ctrlNsh);
                sendConfReq(ctrlPolka);
                if (reqSent > 0) {
                    break;
                }
                ctrlLcp.sendEchoReq();
                break;
            default:
                clearState();
                break;
        }
        if (oldMod != curMode) {
            sendKeepReq();
        }
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        // int addr = pck.getByte(0); // address
        // int ctrl = pck.getByte(1); // control
        int prot = pck.msbGetW(2); // protocol
        pck.getSkip(size);
        if (prot == fragType) {
            int i = pck.msbGetW(0);
            pck.getSkip(2);
            int seq;
            if (multilinkRx == 1) {
                seq = i & 0xfff;
            } else {
                seq = ((i & 0xff) << 16) | pck.msbGetW(0);
                pck.getSkip(2);
            }
            if ((i & fragBeg) != 0) {
                fragSeqRx = seq;
                fragReasm.clear();
            }
            if (fragSeqRx != seq) {
                cntr.drop(pck, counter.reasons.badRxSeq);
                return;
            }
            fragSeqRx++;
            if (multilinkRx == 1) {
                fragSeqRx &= 0xfff;
            } else {
                fragSeqRx &= 0xffffff;
            }
            byte[] buf = pck.getCopy();
            fragReasm.putCopy(buf, 0, 0, buf.length);
            fragReasm.putSkip(buf.length);
            fragReasm.merge2end();
            if ((i & fragEnd) == 0) {
                return;
            }
            fragSeqRx = -1;
            pck.copyFrom(fragReasm, true, true);
            prot = pck.msbGetW(0); // protocol
            pck.getSkip(2);
        }
        int newProt = -1;
        switch (prot) {
            case ifcPppMpls.pppDataU:
                newProt = ifcPppMpls.ethTypU;
                break;
            case ifcPppIp4.pppData:
                newProt = ifcPppIp4.ethTyp;
                break;
            case ifcPppIp6.pppData:
                newProt = ifcPppIp6.ethTyp;
                break;
            case ifcPppBrdg.pppData:
                ifcPppBrdg.patchPackRx(pck);
                newProt = ifcPppBrdg.ethTyp;
                break;
            case ifcPppOsi.pppData:
                newProt = ifcPppOsi.ethTyp;
                break;
            case ifcPppIpx.pppData:
                newProt = ifcPppIpx.ethTyp;
                break;
            case ifcPppCrypt.pppData:
                newProt = ifcPppCrypt.ethTyp;
                break;
            case ifcPppNsh.pppData:
                newProt = ifcPppNsh.ethTyp;
                break;
            case ifcPppPolka.pppData:
                newProt = ifcPppPolka.ethTyp;
                break;
            case ifcPppMpls.pppDataM:
                newProt = ifcPppMpls.ethTypM;
                break;
            case ifcPppIp4.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlIp4, prot);
                break;
            case ifcPppIp6.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlIp6, prot);
                break;
            case ifcPppBrdg.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlBrdg, prot);
                break;
            case ifcPppMpls.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlMpls, prot);
                break;
            case ifcPppOsi.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlOsi, prot);
                break;
            case ifcPppIpx.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlIpx, prot);
                break;
            case ifcPppCrypt.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlCrypt, prot);
                break;
            case ifcPppNsh.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlNsh, prot);
                break;
            case ifcPppPolka.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlPolka, prot);
                break;
            case ifcPppLcp.pppCtrl:
                recvNcpCtrl(pck, ctrlLcp, prot);
                checkPeerState(state.states.up);
                break;
            case autherEap.pppCtrl:
            case autherChap.pppCtrl:
            case autherPap.pppCtrl:
                if (debugger.ifcPppEvnt) {
                    logger.debug("got auth request");
                }
                if (!ctrlLcp.getReady()) {
                    break;
                }
                recvAuthPack(pck);
                checkAuthUp();
                break;
            default:
                cntr.drop(pck, counter.reasons.badCmd);
                pck.getSkip(-2);
                sendNcpCtrl(pck, ifcPppLcp.pppCtrl, ifcPppNcp.codeProtRej, bits.randomB());
                break;
        }
        if (newProt < 0) {
            return;
        }
        if (curMode != modeUp) {
            return;
        }
        pck.msbPutW(0, newProt);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        int prot = pck.msbGetW(0); // ethertype
        pck.getSkip(2);
        int newProt = -1;
        switch (prot) {
            case ifcPppMpls.ethTypU:
                newProt = ifcPppMpls.pppDataU;
                break;
            case ifcPppIp4.ethTyp:
                newProt = ifcPppIp4.pppData;
                break;
            case ifcPppIp6.ethTyp:
                newProt = ifcPppIp6.pppData;
                break;
            case ifcPppBrdg.ethTyp:
                ifcPppBrdg.patchPackTx(pck);
                newProt = ifcPppBrdg.pppData;
                break;
            case ifcPppOsi.ethTyp:
                newProt = ifcPppOsi.pppData;
                break;
            case ifcPppIpx.ethTyp:
                newProt = ifcPppIpx.pppData;
                break;
            case ifcPppCrypt.ethTyp:
                newProt = ifcPppCrypt.pppData;
                break;
            case ifcPppNsh.ethTyp:
                newProt = ifcPppNsh.pppData;
                break;
            case ifcPppPolka.ethTyp:
                newProt = ifcPppPolka.pppData;
                break;
            case ifcPppMpls.ethTypM:
                newProt = ifcPppMpls.pppDataM;
                break;
            default:
                cntr.drop(pck, counter.reasons.badEthTyp);
                return;
        }
        if (newProt < 0) {
            return;
        }
        if (curMode != modeUp) {
            return;
        }
        if (multilinkTx < 1) {
            putAddrCtrlProto(pck, newProt);
            lower.sendPack(pck);
            return;
        }
        if ((fragLen < 1) || (pck.dataSize() < fragLen)) {
            putAddrCtrlProto(pck, newProt);
            lower.sendPack(pck);
            return;
        }
        pck.msbPutW(0, newProt); // protocol
        pck.putSkip(2);
        pck.merge2beg();
        byte[] buf = pck.getCopy();
        for (int ofs = 0; ofs < buf.length;) {
            int len = buf.length - ofs;
            if (len > fragLen) {
                len = fragLen;
            }
            pck.clear();
            int i = 0;
            if (ofs == 0) {
                i |= fragBeg;
            }
            if ((ofs + len) >= buf.length) {
                i |= fragEnd;
            }
            if (multilinkTx == 1) {
                fragSeqTx &= 0xfff;
                pck.msbPutW(0, i | fragSeqTx);
                pck.putSkip(2);
            } else {
                fragSeqTx &= 0xffffff;
                pck.msbPutW(0, i | (fragSeqTx >>> 16));
                pck.msbPutW(2, fragSeqTx);
                pck.putSkip(4);
            }
            fragSeqTx++;
            pck.putCopy(buf, ofs, 0, len);
            pck.putSkip(len);
            pck.merge2beg();
            putAddrCtrlProto(pck, fragType);
            lower.sendPack(pck);
            ofs += len;
            if (fragGap < 1) {
                continue;
            }
            bits.sleep(fragGap);
        }
    }

}

class ifcPppTxKeep extends TimerTask {

    private ifcPpp lower;

    public ifcPppTxKeep(ifcPpp parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendKeepReq();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
