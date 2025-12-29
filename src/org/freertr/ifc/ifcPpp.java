package org.freertr.ifc;

import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrEui;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrType;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authLocal;
import org.freertr.auth.authResult;
import org.freertr.auth.authenDown;
import org.freertr.auth.authenHead;
import org.freertr.auth.autherChap;
import org.freertr.auth.autherDoer;
import org.freertr.auth.autherEap;
import org.freertr.auth.autherPap;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.ip.ipMpls;
import org.freertr.ip.ipxIface;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrIsis;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * point to point protocol (rfc1661) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcPpp implements ifcUp, ifcDn, authenDown {

    /**
     * ethertype
     */
    public final static int ethtyp = 0x880b;

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
     * accounting list to use for remote
     */
    public authGeneric accontRem = null;

    /**
     * accounting interval to use
     */
    public int accontInterval;

    /**
     * accounting session to use
     */
    public int accontSession;

    /**
     * accounting last sent
     */
    public long accontLast;

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
     * address, control
     */
    public final static int preamble = 0xff03;

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
     * current keepalive retries
     */
    public int keepaliveRetry = 10;

    /**
     * nak retry limit
     */
    public int nakRetryLimit = 16;

    /**
     * req reset limit
     */
    public int reqResetLimit = 5;

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
     * sgtcp
     */
    public ifcPppSgt ctrlSgt;

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
        ctrlSgt = new ifcPppSgt(this);
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
    public static void getHelp(userHelp l) {
        l.add(null, false, 2, new int[]{3}, "keepalive", "keepalive timer");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "retry", "keepalive retry");
        l.add(null, false, 3, new int[]{-1}, "<num>", "count");
        l.add(null, false, 2, new int[]{3}, "username", "name of user to send");
        l.add(null, false, 3, new int[]{-1}, "<text>", "username");
        l.add(null, false, 2, new int[]{3}, "password", "password of user to send");
        l.add(null, false, 3, new int[]{-1}, "<text>", "password");
        l.add(null, false, 2, new int[]{3}, "accm", "async character map");
        l.add(null, false, 3, new int[]{-1}, "<num>", "accm");
        l.add(null, false, 2, new int[]{3}, "mru", "max receive unit");
        l.add(null, false, 3, new int[]{-1}, "<num>", "mru");
        l.add(null, false, 2, new int[]{3}, "refuseauth", "never use authentication protocol");
        l.add(null, false, 3, new int[]{-1}, "pap", "password authentication protocol");
        l.add(null, false, 3, new int[]{-1}, "chap", "challenge handshake authentication protocol");
        l.add(null, false, 3, new int[]{-1}, "eap", "extensible authentication protocol");
        l.add(null, false, 2, new int[]{3}, "authentication", "set peer authentication list");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of list");
        l.add(null, false, 2, new int[]{3}, "accounting", "set peer accounting list");
        l.add(null, false, 3, new int[]{4}, "<name:aaa>", "name of list");
        l.add(null, false, 4, new int[]{-1}, "<num>", "time in millis");
        l.add(null, false, 2, new int[]{3}, "multilink", "multilink operation");
        l.add(null, false, 3, new int[]{4}, "<num>", "mrru");
        l.add(null, false, 4, new int[]{-1}, "none", "disable operation");
        l.add(null, false, 4, new int[]{-1}, "short", "negotiate short header");
        l.add(null, false, 4, new int[]{-1}, "long", "negotiate long header");
        l.add(null, false, 2, new int[]{3}, "fragment", "set payload size");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of bytes");
        l.add(null, false, 2, new int[]{3}, "frgap", "inter fragment gap");
        l.add(null, false, 3, new int[]{-1}, "<num>", "milliseconds");
        l.add(null, false, 2, new int[]{3}, "naktry", "nak retry limit");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of tries");
        l.add(null, false, 2, new int[]{3}, "reqrst", "req reset limit");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of tries");
        l.add(null, false, 2, new int[]{3}, "ip4cp", "ipv4 control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "reqaddr", "require local address");
        l.add(null, false, 3, new int[]{4}, "peer", "set peer address");
        l.add(null, false, 4, new int[]{-1}, "<text>", "address");
        l.add(null, false, 3, new int[]{4}, "local", "set local address");
        l.add(null, false, 4, new int[]{-1}, "<text>", "address");
        l.add(null, false, 3, new int[]{4}, "dns1", "set dns1 address");
        l.add(null, false, 4, new int[]{-1}, "<text>", "address");
        l.add(null, false, 3, new int[]{4}, "dns2", "set dns1 address");
        l.add(null, false, 4, new int[]{-1}, "<text>", "address");
        l.add(null, false, 2, new int[]{3}, "ip6cp", "ipv6 control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "keep", "keet configured address");
        l.add(null, false, 3, new int[]{4}, "local", "set local address");
        l.add(null, false, 4, new int[]{-1}, "<text>", "address");
        l.add(null, false, 3, new int[]{4}, "peer", "set peer address");
        l.add(null, false, 4, new int[]{-1}, "<text>", "address");
        l.add(null, false, 2, new int[]{3}, "bcp", "bridge control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 2, new int[]{3}, "mplscp", "multiprotocol label switching control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 2, new int[]{3}, "osicp", "open systems interconnect control protocol");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 2, new int[]{3}, "ipxcp", "ipx control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 2, new int[]{3}, "ecp", "encryption control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 2, new int[]{3}, "nshcp", "service control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 2, new int[]{3}, "sgtcp", "sgt control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
        l.add(null, false, 2, new int[]{3}, "polkacp", "polka control protocol");
        l.add(null, false, 3, new int[]{-1}, "open", "force to open state");
        l.add(null, false, 3, new int[]{-1}, "optional", "force to optional state");
        l.add(null, false, 3, new int[]{-1}, "close", "force to close state");
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
        l.add(beg + "retry " + keepaliveRetry);
        cmds.cfgLine(l, authenRem == null, cmds.tabulator, "ppp authentication", "" + authenRem);
        cmds.cfgLine(l, accontRem == null, cmds.tabulator, "ppp accounting", accontRem + " " + accontInterval);
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
        l.add(beg + "reqrst " + reqResetLimit);
        l.add(beg + "accm " + sentAccm);
        l.add(beg + "mru " + sentMru);
        ctrlIp4.getConfig(l, "ppp ip4cp");
        cmds.cfgLine(l, remAddrCfg == null, cmds.tabulator, "ppp ip4cp peer", "" + remAddrCfg);
        cmds.cfgLine(l, locAddrCfg == null, cmds.tabulator, "ppp ip4cp local", "" + locAddrCfg);
        cmds.cfgLine(l, dns1addrCfg == null, cmds.tabulator, "ppp ip4cp dns1", "" + dns1addrCfg);
        cmds.cfgLine(l, dns2addrCfg == null, cmds.tabulator, "ppp ip4cp dns2", "" + dns2addrCfg);
        cmds.cfgLine(l, !locAddrReq, cmds.tabulator, "ppp ip4cp reqaddr", "");
        ctrlIp6.getConfig(l, "ppp ip6cp");
        cmds.cfgLine(l, remIfIdCfg == null, cmds.tabulator, "ppp ip6cp peer", "" + remIfIdCfg);
        cmds.cfgLine(l, locIfIdCfg == null, cmds.tabulator, "ppp ip6cp local", "" + locIfIdCfg);
        cmds.cfgLine(l, !keepIpv6addr, cmds.tabulator, "ppp ip6cp keep", "");
        ctrlBrdg.getConfig(l, "ppp bcp");
        ctrlMpls.getConfig(l, "ppp mplscp");
        ctrlOsi.getConfig(l, "ppp osicp");
        ctrlIpx.getConfig(l, "ppp ipxcp");
        ctrlCrypt.getConfig(l, "ppp ecp");
        ctrlNsh.getConfig(l, "ppp nshcp");
        ctrlSgt.getConfig(l, "ppp sgtcp");
        ctrlPolka.getConfig(l, "ppp polkacp");
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
            ctrlIp4.doConfig(a);
            return;
        }
        if (a.equals("ip6cp")) {
            a = cmd.word();
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
            ctrlIp6.doConfig(a);
            return;
        }
        if (a.equals("bcp")) {
            a = cmd.word();
            ctrlBrdg.doConfig(a);
            return;
        }
        if (a.equals("mplscp")) {
            a = cmd.word();
            ctrlMpls.doConfig(a);
            return;
        }
        if (a.equals("osicp")) {
            a = cmd.word();
            ctrlOsi.doConfig(a);
            return;
        }
        if (a.equals("ipxcp")) {
            a = cmd.word();
            ctrlIpx.doConfig(a);
            return;
        }
        if (a.equals("ecp")) {
            a = cmd.word();
            ctrlCrypt.doConfig(a);
            return;
        }
        if (a.equals("nshcp")) {
            a = cmd.word();
            ctrlNsh.doConfig(a);
            return;
        }
        if (a.equals("sgtcp")) {
            a = cmd.word();
            ctrlSgt.doConfig(a);
            return;
        }
        if (a.equals("polkacp")) {
            a = cmd.word();
            ctrlPolka.doConfig(a);
            return;
        }
        if (a.equals("keepalive")) {
            keepaliveInterval = bits.str2num(cmd.word());
            restartTimer(false);
            return;
        }
        if (a.equals("retry")) {
            keepaliveRetry = bits.str2num(cmd.word());
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
        if (a.equals("reqrst")) {
            reqResetLimit = bits.str2num(cmd.word());
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
        if (a.equals("accounting")) {
            cfgAuther auth = cfgAll.autherFind(cmd.word(), null);
            if (auth == null) {
                cmd.error("no such accounting list");
                return;
            }
            accontRem = auth.getAuther();
            accontInterval = bits.str2num(cmd.word());
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
            ctrlIp4.unConfig(a);
            return;
        }
        if (a.equals("ip6cp")) {
            a = cmd.word();
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
            ctrlIp6.unConfig(a);
            return;
        }
        if (a.equals("bcp")) {
            a = cmd.word();
            ctrlBrdg.unConfig(a);
            return;
        }
        if (a.equals("mplscp")) {
            a = cmd.word();
            ctrlMpls.unConfig(a);
            return;
        }
        if (a.equals("osicp")) {
            a = cmd.word();
            ctrlOsi.unConfig(a);
            return;
        }
        if (a.equals("ipxcp")) {
            a = cmd.word();
            ctrlIpx.unConfig(a);
            return;
        }
        if (a.equals("ecp")) {
            a = cmd.word();
            ctrlCrypt.unConfig(a);
            return;
        }
        if (a.equals("nshcp")) {
            a = cmd.word();
            ctrlNsh.unConfig(a);
            return;
        }
        if (a.equals("sgtcp")) {
            a = cmd.word();
            ctrlSgt.unConfig(a);
            return;
        }
        if (a.equals("polkacp")) {
            a = cmd.word();
            ctrlPolka.unConfig(a);
            return;
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
        if (a.equals("accounting")) {
            accontRem = null;
            accontInterval = 0;
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
        ctrlSgt.clearState();
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
        pck.msbPutW(0, preamble);
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
     * @param auth authenticator
     * @param pck received packet
     */
    public void recvAuthPack(autherDoer auth, packHolder pck) {
        if (auth == null) {
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
        auth.recvPck(pck, cis.code, cis.id);
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
     * send authentication message
     *
     * @param auth authenticator
     */
    public void sendAuthMsg(autherDoer auth) {
        if (auth == null) {
            curMode = modeUp;
            return;
        }
        auth.sendReq();
    }

    /**
     * check for authentication up
     *
     * @param auth authenticator
     */
    public void checkAuthUp(autherDoer auth) {
        if (auth == null) {
            return;
        }
        if (auth.working) {
            return;
        }
        boolean b = auth.result.result == authResult.authSuccessful;
        auth.stopThread();
        if (debugger.ifcPppEvnt) {
            logger.debug("authentication passed=" + b);
        }
        if (!b) {
            clearState();
            return;
        }
        curMode = modeUp;
        if (auth.result.ipv4addr != null) {
            remAddrCfg = auth.result.ipv4addr;
            ctrlIp4.clearState();
        }
        if (auth.result.ipv6ifid != null) {
            remIfIdCfg = auth.result.ipv6ifid;
            ctrlIp6.clearState();
        }
        if ((auth.result.ipv4route != null) && (cfger.fwdIf4 != null)) {
            cfger.fwdIf4.gatePrfx = authGeneric.route2prefixes(auth.result.ipv4route);
        }
        if ((auth.result.ipv6route != null) && (cfger.fwdIf6 != null)) {
            cfger.fwdIf6.gatePrfx = authGeneric.route2prefixes(auth.result.ipv6route);
        }
        if (auth.result.filter != null) {
            String a = auth.result.filter;
            for (;;) {
                cmds cmd;
                int i = a.length();
                if (i < 1) {
                    break;
                }
                i = a.indexOf("/");
                if (i < 0) {
                    cmd = new cmds("fltr", a);
                    a = "";
                } else {
                    cmd = new cmds("fltr", a.substring(0, i));
                    a = a.substring(i + 1, a.length());
                }
                cfger.doCfgStr(cmd);
            }
        }
        sendKeepReq();
        if (accontRem == null) {
            return;
        }
        if (ctrlAuth == null) {
            return;
        }
        accontSession = bits.randomD();
        accontLast = bits.getTime();
        accontRem.acntUserSession(ctrlAuth.result.user, "" + lower, accontSession, cntr, 1);
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
                ctrlSgt.clearState();
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
                sendAuthMsg(ctrlAuth);
                checkAuthUp(ctrlAuth);
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
                sendConfReq(ctrlSgt);
                sendConfReq(ctrlPolka);
                if (reqSent > 0) {
                    break;
                }
                ctrlLcp.sendEchoReq();
                if (accontRem == null) {
                    break;
                }
                if (ctrlAuth == null) {
                    break;
                }
                if (accontLast == 0) {
                    break;
                }
                long tim = bits.getTime();
                if ((tim - accontLast) < accontInterval) {
                    break;
                }
                accontLast = tim;
                if (cfger.ethtyp.hwCntr == null) {
                    accontRem.acntUserSession(ctrlAuth.result.user, "" + lower, accontSession, cntr, 3);
                    break;
                }
                accontRem.acntUserSession(ctrlAuth.result.user, "" + lower, accontSession, cfger.ethtyp.hwCntr, 3);
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
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (pck.msbGetW(0) != preamble) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
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
                newProt = ipMpls.typeU;
                break;
            case ifcPppIp4.pppData:
                newProt = ipIfc4.type;
                break;
            case ifcPppIp6.pppData:
                newProt = ipIfc6.type;
                break;
            case ifcPppBrdg.pppData:
                ifcPppBrdg.patchPackRx(pck);
                newProt = ifcBridge.serialType;
                break;
            case ifcPppOsi.pppData:
                newProt = rtrIsis.ethTyp;
                break;
            case ifcPppIpx.pppData:
                newProt = ipxIface.type;
                break;
            case ifcPppCrypt.pppData:
                newProt = ifcMacSec.ethtyp;
                break;
            case ifcPppNsh.pppData:
                newProt = ifcNshFwd.type;
                break;
            case ifcPppSgt.pppData:
                newProt = ifcSgt.type;
                break;
            case ifcPppPolka.pppData:
                newProt = ifcPolka.type;
                break;
            case ifcPppMpls.pppDataM:
                newProt = ipMpls.typeM;
                break;
            case ifcPppMpls.pppDataB:
                newProt = ipMpls.typeB;
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
            case ifcPppSgt.pppCtrl:
                if (curMode != modeUp) {
                    break;
                }
                recvNcpCtrl(pck, ctrlSgt, prot);
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
                recvAuthPack(ctrlAuth, pck);
                checkAuthUp(ctrlAuth);
                break;
            default:
                cntr.drop(pck, counter.reasons.badCmd);
                pck.getSkip(-2);
                ctrlLcp.cntr.tx(pck);
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
            case ipMpls.typeU:
                newProt = ifcPppMpls.pppDataU;
                break;
            case ipIfc4.type:
                newProt = ifcPppIp4.pppData;
                break;
            case ipIfc6.type:
                newProt = ifcPppIp6.pppData;
                break;
            case ifcBridge.serialType:
                ifcPppBrdg.patchPackTx(pck);
                newProt = ifcPppBrdg.pppData;
                break;
            case rtrIsis.ethTyp:
                newProt = ifcPppOsi.pppData;
                break;
            case ipxIface.type:
                newProt = ifcPppIpx.pppData;
                break;
            case ifcMacSec.ethtyp:
                newProt = ifcPppCrypt.pppData;
                break;
            case ifcNshFwd.type:
                newProt = ifcPppNsh.pppData;
                break;
            case ifcSgt.type:
                newProt = ifcPppSgt.pppData;
                break;
            case ifcPolka.type:
                newProt = ifcPppPolka.pppData;
                break;
            case ipMpls.typeM:
                newProt = ifcPppMpls.pppDataM;
                break;
            case ipMpls.typeB:
                newProt = ifcPppMpls.pppDataB;
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

    private void getShow(userFormat res, ifcPppNcp nc) {
        res.add(nc.getPPPname() + "|" + bits.toHexW(nc.sawBit) + "|" + nc.cntr.getShStat());
    }

    /**
     * get show
     *
     * @return show
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "ncp|state|cntr");
        res.add("lower|-|" + lower);
        res.add("upper|-|" + upper);
        res.add("cfger|-|" + cfger);
        res.add("rtt|-|" + ctrlLcp.lastEchoReply);
        res.add("auth|" + ctrlAuth);
        getShow(res, ctrlLcp);
        getShow(res, ctrlIp4);
        getShow(res, ctrlIp6);
        getShow(res, ctrlBrdg);
        getShow(res, ctrlMpls);
        getShow(res, ctrlOsi);
        getShow(res, ctrlIpx);
        getShow(res, ctrlCrypt);
        getShow(res, ctrlNsh);
        getShow(res, ctrlSgt);
        getShow(res, ctrlPolka);
        return res;
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
