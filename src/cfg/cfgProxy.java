package cfg;

import addr.addrIP;
import auth.authLocal;
import clnt.clntProxy;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pipe.pipeSide;
import serv.servGeneric;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * proxy profile configuration
 *
 * @author matecsaba
 */
public class cfgProxy implements Comparator<cfgProxy>, cfgGeneric {

    /**
     * name of connection map
     */
    public final String name;

    /**
     * proxy configuration
     */
    public final clntProxy proxy;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "proxy-profile .*! protocol local",
        "proxy-profile .*! no security",
        "proxy-profile .*! no username",
        "proxy-profile .*! no password",
        "proxy-profile .*! no recursive",
        "proxy-profile .*! no vrf",
        "proxy-profile .*! no source",
        "proxy-profile .*! no target",
        "proxy-profile .*! no port",
        "proxy-profile .*! prefer none"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgProxy o1, cfgProxy o2) {
        return o1.name.compareTo(o2.name);
    }

    public String toString() {
        return "proxy " + name;
    }

    /**
     * create new profile
     *
     * @param nam name of interface
     */
    public cfgProxy(String nam) {
        name = nam;
        proxy = new clntProxy(nam);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2  protocol                       specify protocol to use");
        l.add("2 .    local                        select local vrf");
        l.add("2 .    socks4                       select socks v4");
        l.add("2 .    socks5                       select socks v5");
        l.add("2 .    http                         select http connect");
        l.add("2 .    hostos                       select host os stack");
        l.add("1 2  security                       select security protocol");
        l.add("2 .    ssh                          use secure shell");
        l.add("2 .    tls                          use transport layer security");
        l.add("2 .    dtls                         use datagram transport layer security");
        l.add("2 .    telnet                       use telnet protocol");
        l.add("1 2  username                       username to send");
        l.add("2 .    <name>                       username");
        l.add("1 2  password                       password to send");
        l.add("2 .    <name>                       username");
        l.add("1 2  recursive                      name of profile to use");
        l.add("2 .    <name>                       profile name");
        l.add("1 2  vrf                            name of vrf to find target in");
        l.add("2 .    <name>                       vrf name");
        l.add("1 2  source                         name of source interface");
        l.add("2 .    <name>                       interface name");
        l.add("1 2  target                         specify address of proxy");
        l.add("2 .    <name>                       name or address");
        l.add("1 2  port                           specify port of proxy");
        l.add("2 .    <num>                        port number");
        l.add("1 2  prefer                         prefer ip protocol");
        l.add("2 .    none                         default");
        l.add("2 .    ipv4                         ipv4");
        l.add("2 .    ipv6                         ipv6");
        return l;
    }

    public String getPrompt() {
        return "proxy";
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("proxy-profile " + name);
        l.add(cmds.tabulator + "protocol " + clntProxy.type2string(proxy.prxProto));
        cmds.cfgLine(l, proxy.secProto == 0, cmds.tabulator, "security", servGeneric.proto2string(proxy.secProto));
        cmds.cfgLine(l, proxy.username == null, cmds.tabulator, "username", proxy.username);
        cmds.cfgLine(l, proxy.password == null, cmds.tabulator, "password", authLocal.passwdEncode(proxy.password));
        if (proxy.lowProxy == null) {
            l.add(cmds.tabulator + "no recursive");
        } else {
            l.add(cmds.tabulator + "recursive " + proxy.lowProxy.name);
        }
        if (proxy.vrf == null) {
            l.add(cmds.tabulator + "no vrf");
        } else {
            l.add(cmds.tabulator + "vrf " + proxy.vrf.name);
        }
        if (proxy.srcIfc == null) {
            l.add(cmds.tabulator + "no source");
        } else {
            l.add(cmds.tabulator + "source " + proxy.srcIfc.name);
        }
        cmds.cfgLine(l, proxy.target == null, cmds.tabulator, "target", proxy.target);
        cmds.cfgLine(l, proxy.port == 0, cmds.tabulator, "port", "" + proxy.port);
        String a;
        if (proxy.prefer == 0) {
            a = "none";
        } else {
            a = "ipv" + proxy.prefer;
        }
        l.add(cmds.tabulator + "prefer " + a);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("protocol")) {
            proxy.prxProto = clntProxy.string2type(cmd.word());
            return;
        }
        if (s.equals("security")) {
            proxy.secProto = servGeneric.string2proto(cmd.word());
            return;
        }
        if (s.equals("username")) {
            proxy.username = cmd.getRemaining();
            return;
        }
        if (s.equals("password")) {
            proxy.password = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (s.equals("vrf")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            proxy.vrf = vrf;
            return;
        }
        if (s.equals("source")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            proxy.srcIfc = ifc;
            return;
        }
        if (s.equals("target")) {
            proxy.target = cmd.getRemaining();
            return;
        }
        if (s.equals("port")) {
            proxy.port = bits.str2num(cmd.getRemaining());
            return;
        }
        if (s.equals("recursive")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such profile");
                return;
            }
            proxy.lowProxy = prx.proxy;
            return;
        }
        if (s.equals("prefer")) {
            s = cmd.word();
            if (s.equals("ipv4")) {
                proxy.prefer = 4;
            }
            if (s.equals("ipv6")) {
                proxy.prefer = 6;
            }
            if (s.equals("none")) {
                proxy.prefer = 0;
            }
            return;
        }
        if (!s.equals("no")) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("protocol")) {
            proxy.prxProto = clntProxy.proxyType.local;
            return;
        }
        if (s.equals("security")) {
            proxy.secProto = 0;
            return;
        }
        if (s.equals("username")) {
            proxy.username = null;
            return;
        }
        if (s.equals("password")) {
            proxy.password = null;
            return;
        }
        if (s.equals("vrf")) {
            proxy.vrf = null;
            return;
        }
        if (s.equals("source")) {
            proxy.srcIfc = null;
            return;
        }
        if (s.equals("target")) {
            proxy.target = null;
            return;
        }
        if (s.equals("port")) {
            proxy.port = 0;
            return;
        }
        if (s.equals("recursive")) {
            proxy.lowProxy = null;
            return;
        }
        cmd.badCmd();
    }

    /**
     * open one connection
     *
     * @param proto protocol to use from servGeneric
     * @param addr target address
     * @param port target port
     * @param name client name
     * @return pipeline, null on error
     */
    public pipeSide doConnect(int proto, addrIP addr, int port, String name) {
        return proxy.doConnect(proto, addr, port, name);
    }

    /**
     * get vrf
     *
     * @return vrf
     */
    public cfgVrf getVrf() {
        return proxy.vrf;
    }

    /**
     * get interface
     *
     * @return interface
     */
    public cfgIfc getIfc() {
        return proxy.srcIfc;
    }

}
