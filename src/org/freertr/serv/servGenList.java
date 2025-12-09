package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgGeneric;
import org.freertr.tab.tabGen;
import org.freertr.user.userHelp;

/**
 * list of generic server
 *
 * @param <T> type of
 * @author matecsaba
 */
public class servGenList<T extends servGeneric> {

    /**
     * list of servers
     */
    public final tabGen<T> lst;

    /**
     * create new list
     */
    public servGenList() {
        lst = new tabGen<T>();
    }

    private boolean updateName(T ntry) {
        ntry.srvName = ntry.srvName.trim();
        return (ntry.srvName.length() < 1);
    }

    /**
     * find one server
     *
     * @param ntry server to add when not found
     * @param create add this on this name if not found
     * @return server, null if not found
     */
    public T find(T ntry, boolean create) {
        if (updateName(ntry)) {
            return null;
        }
        if (!create) {
            return lst.find(ntry);
        }
        T old = lst.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.srvInitialize();
        return ntry;
    }

    /**
     * delete one server
     *
     * @param ntry server to delete
     * @return server, null if not found
     */
    public T del(T ntry) {
        if (updateName(ntry)) {
            return null;
        }
        ntry = lst.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.srvDeinit();
        return ntry;
    }

    /**
     * clear whole list
     */
    public void clear() {
        lst.clear();
    }

    /**
     * get number of list entries
     *
     * @return number of servers
     */
    public int size() {
        return lst.size();
    }

    /**
     * read one server
     *
     * @param i number of server to read
     * @return server readed
     */
    public T get(int i) {
        return lst.get(i);
    }

    /**
     * get running config of this list
     *
     * @param cfg list to append
     * @param filter filter defaults
     */
    public void getShRun(List<String> cfg, int filter) {
        for (int i = 0; i < lst.size(); i++) {
            T ntry = lst.get(i);
            cfg.addAll(ntry.getShRun(filter));
        }
    }

    /**
     * get list of servers
     *
     * @return list
     */
    public List<String> listServers() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            T ntry = lst.get(i);
            l.add(ntry.srvName);
        }
        return l;
    }

    /**
     * get running config of list
     *
     * @param <T> type of server
     * @param cfg list to append
     * @param lst list of elements
     * @param flt true to filter defaults, false to not
     */
    @SuppressWarnings("unchecked")
    public static <T extends cfgGeneric> void listGetRun(List<String> cfg, tabGen<?> lst, int flt) {
        for (int i = 0; i < lst.size(); i++) {
            T ace = (T) lst.get(i);
            if (ace == null) {
                continue;
            }
            cfg.addAll(ace.getShRun(flt));
        }
    }

    /**
     * delete one server
     *
     * @param typ type of server
     * @param nam name of server
     * @return server, null if not found
     */
    public static servGeneric srvDel(String typ, String nam) {
        servGenEntry res = servGenEntry.getDaemon(typ);
        if (res == null) {
            return null;
        }
        res.srv.srvRename(nam);
        return res.lst.del(res.srv);
    }

    /**
     * find one server
     *
     * @param typ type of server
     * @param nam name of server
     * @param create add this on this name if not found
     * @return server, null if not found
     */
    public static servGeneric srvFind(String typ, String nam, boolean create) {
        servGenEntry res = servGenEntry.getDaemon(typ);
        if (res == null) {
            return null;
        }
        res.srv.srvRename(nam);
        return res.lst.find(res.srv, create);
    }

    /**
     * get help string
     *
     * @param l list to update
     * @param p number start
     * @param e ending
     * @param o other
     */
    public static void srvHelp(userHelp l, int p, String e, userHelp o) {
        int[] n;
        if (o == null) {
            n = new int[]{-1};
            o = new userHelp();
        } else {
            n = new int[]{p + 2, -1};
        }
        srvHelp(l, p, n, e, o, "echo", cfgAll.dmnEcho.listServers());
        srvHelp(l, p, n, e, o, "discard", cfgAll.dmnDiscard.listServers());
        srvHelp(l, p, n, e, o, "chargen", cfgAll.dmnCharGen.listServers());
        srvHelp(l, p, n, e, o, "netflow", cfgAll.dmnNetflow.listServers());
        srvHelp(l, p, n, e, o, "udpfwd", cfgAll.dmnUdpFwd.listServers());
        srvHelp(l, p, n, e, o, "upnpfwd", cfgAll.dmnUpnpFwd.listServers());
        srvHelp(l, p, n, e, o, "upnphub", cfgAll.dmnUpnpHub.listServers());
        srvHelp(l, p, n, e, o, "openflow", cfgAll.dmnOpenflow.listServers());
        srvHelp(l, p, n, e, o, "pktmux", cfgAll.dmnPktmux.listServers());
        srvHelp(l, p, n, e, o, "p4lang", cfgAll.dmnP4lang.listServers());
        srvHelp(l, p, n, e, o, "stack", cfgAll.dmnStack.listServers());
        srvHelp(l, p, n, e, o, "quote", cfgAll.dmnQuote.listServers());
        srvHelp(l, p, n, e, o, "forwarder", cfgAll.dmnForwarder.listServers());
        srvHelp(l, p, n, e, o, "syslog", cfgAll.dmnSyslog.listServers());
        srvHelp(l, p, n, e, o, "loadbalancer", cfgAll.dmnLoadBalancer.listServers());
        srvHelp(l, p, n, e, o, "multiplexer", cfgAll.dmnMultiplexer.listServers());
        srvHelp(l, p, n, e, o, "telnet", cfgAll.dmnTelnet.listServers());
        srvHelp(l, p, n, e, o, "xotpad", cfgAll.dmnXotpad.listServers());
        srvHelp(l, p, n, e, o, "rfb", cfgAll.dmnRfb.listServers());
        srvHelp(l, p, n, e, o, "udptn", cfgAll.dmnUdptn.listServers());
        srvHelp(l, p, n, e, o, "http", cfgAll.dmnHttp.listServers());
        srvHelp(l, p, n, e, o, "dhcp4", cfgAll.dmnDhcp4.listServers());
        srvHelp(l, p, n, e, o, "dhcp6", cfgAll.dmnDhcp6.listServers());
        srvHelp(l, p, n, e, o, "dns", cfgAll.dmnDns.listServers());
        srvHelp(l, p, n, e, o, "lpd", cfgAll.dmnLpd.listServers());
        srvHelp(l, p, n, e, o, "honeypot", cfgAll.dmnHoney.listServers());
        srvHelp(l, p, n, e, o, "whois", cfgAll.dmnWhois.listServers());
        srvHelp(l, p, n, e, o, "pop3", cfgAll.dmnPop3.listServers());
        srvHelp(l, p, n, e, o, "imap4", cfgAll.dmnImap4.listServers());
        srvHelp(l, p, n, e, o, "smtp", cfgAll.dmnSmtp.listServers());
        srvHelp(l, p, n, e, o, "modem", cfgAll.dmnModem.listServers());
        srvHelp(l, p, n, e, o, "voice", cfgAll.dmnVoice.listServers());
        srvHelp(l, p, n, e, o, "sip", cfgAll.dmnSip.listServers());
        srvHelp(l, p, n, e, o, "ftp", cfgAll.dmnFtp.listServers());
        srvHelp(l, p, n, e, o, "tftp", cfgAll.dmnTftp.listServers());
        srvHelp(l, p, n, e, o, "gopher", cfgAll.dmnGopher.listServers());
        srvHelp(l, p, n, e, o, "plan9", cfgAll.dmnPlan9.listServers());
        srvHelp(l, p, n, e, o, "iscsi", cfgAll.dmnIscsi.listServers());
        srvHelp(l, p, n, e, o, "bmp2mrt", cfgAll.dmnBmp.listServers());
        srvHelp(l, p, n, e, o, "irc", cfgAll.dmnIrc.listServers());
        srvHelp(l, p, n, e, o, "dcp", cfgAll.dmnDcp.listServers());
        srvHelp(l, p, n, e, o, "sdwan", cfgAll.dmnSdwan.listServers());
        srvHelp(l, p, n, e, o, "pcep", cfgAll.dmnPcep.listServers());
        srvHelp(l, p, n, e, o, "ntp", cfgAll.dmnNtp.listServers());
        srvHelp(l, p, n, e, o, "daytime", cfgAll.dmnDaytime.listServers());
        srvHelp(l, p, n, e, o, "rexec", cfgAll.dmnRexec.listServers());
        srvHelp(l, p, n, e, o, "rtpstat", cfgAll.dmnRtpStat.listServers());
        srvHelp(l, p, n, e, o, "time", cfgAll.dmnTime.listServers());
        srvHelp(l, p, n, e, o, "snmp", cfgAll.dmnSnmp.listServers());
        srvHelp(l, p, n, e, o, "socks", cfgAll.dmnSocks.listServers());
        srvHelp(l, p, n, e, o, "rpki", cfgAll.dmnRpki.listServers());
        srvHelp(l, p, n, e, o, "nrpe", cfgAll.dmnNrpe.listServers());
        srvHelp(l, p, n, e, o, "prometheus", cfgAll.dmnPrometheus.listServers());
        srvHelp(l, p, n, e, o, "streamingmdt", cfgAll.dmnStreamingMdt.listServers());
        srvHelp(l, p, n, e, o, "bstun", cfgAll.dmnBStun.listServers());
        srvHelp(l, p, n, e, o, "mrt2bgp", cfgAll.dmnMrt2bgp.listServers());
        srvHelp(l, p, n, e, o, "stun", cfgAll.dmnStun.listServers());
        srvHelp(l, p, n, e, o, "capwap", cfgAll.dmnCapwap.listServers());
        srvHelp(l, p, n, e, o, "lwapp", cfgAll.dmnLwapp.listServers());
        srvHelp(l, p, n, e, o, "vxlan", cfgAll.dmnVxlan.listServers());
        srvHelp(l, p, n, e, o, "geneve", cfgAll.dmnGeneve.listServers());
        srvHelp(l, p, n, e, o, "l2f", cfgAll.dmnL2f.listServers());
        srvHelp(l, p, n, e, o, "l2tp2", cfgAll.dmnL2tp2.listServers());
        srvHelp(l, p, n, e, o, "l2tp3", cfgAll.dmnL2tp3.listServers());
        srvHelp(l, p, n, e, o, "etherip", cfgAll.dmnEtherIp.listServers());
        srvHelp(l, p, n, e, o, "eoip", cfgAll.dmnEoIp.listServers());
        srvHelp(l, p, n, e, o, "gre", cfgAll.dmnGre.listServers());
        srvHelp(l, p, n, e, o, "erspan", cfgAll.dmnErspan.listServers());
        srvHelp(l, p, n, e, o, "sreth", cfgAll.dmnSrEth.listServers());
        srvHelp(l, p, n, e, o, "mplsip", cfgAll.dmnMplsIp.listServers());
        srvHelp(l, p, n, e, o, "mplsudp", cfgAll.dmnMplsUdp.listServers());
        srvHelp(l, p, n, e, o, "mplsoam", cfgAll.dmnMplsOam.listServers());
        srvHelp(l, p, n, e, o, "twamp", cfgAll.dmnTwamp.listServers());
        srvHelp(l, p, n, e, o, "amt", cfgAll.dmnAmt.listServers());
        srvHelp(l, p, n, e, o, "uni2multi", cfgAll.dmnUni2mul.listServers());
        srvHelp(l, p, n, e, o, "uni2uni", cfgAll.dmnUni2uni.listServers());
        srvHelp(l, p, n, e, o, "pckoudp", cfgAll.dmnPckOudp.listServers());
        srvHelp(l, p, n, e, o, "pckodtls", cfgAll.dmnPckOdtls.listServers());
        srvHelp(l, p, n, e, o, "pckotcp", cfgAll.dmnPckOtcp.listServers());
        srvHelp(l, p, n, e, o, "pckotxt", cfgAll.dmnPckOtxt.listServers());
        srvHelp(l, p, n, e, o, "gtp", cfgAll.dmnGtp.listServers());
        srvHelp(l, p, n, e, o, "pptp", cfgAll.dmnPptp.listServers());
        srvHelp(l, p, n, e, o, "radius", cfgAll.dmnRadius.listServers());
        srvHelp(l, p, n, e, o, "ldap", cfgAll.dmnLdap.listServers());
        srvHelp(l, p, n, e, o, "tacacs", cfgAll.dmnTacacs.listServers());
    }

    private static void srvHelp(userHelp l, int p, int[] n, String e, userHelp o, String k, List<String> s) {
        l.add(null, false, p, new int[]{p + 1}, k, k + " server " + e);
        l.add(s, false, p + 1, n, "<name:loc>", "name of server");
        l.addOther(o);
    }

}

class servGenEntry {

    public final servGeneric srv;

    public final servGenList<servGeneric> lst;

    @SuppressWarnings("unchecked")
    public servGenEntry(servGeneric s, servGenList<? extends servGeneric> l) {
        srv = s;
        lst = (servGenList<servGeneric>) l;
    }

    public static servGenEntry getDaemon(String typ) {
        if (typ.equals("echo")) {
            return new servGenEntry(new servEchoS(), cfgAll.dmnEcho);
        }
        if (typ.equals("discard")) {
            return new servGenEntry(new servDiscard(), cfgAll.dmnDiscard);
        }
        if (typ.equals("quote")) {
            return new servGenEntry(new servQuote(), cfgAll.dmnQuote);
        }
        if (typ.equals("chargen")) {
            return new servGenEntry(new servCharGen(), cfgAll.dmnCharGen);
        }
        if (typ.equals("netflow")) {
            return new servGenEntry(new servNetflow(), cfgAll.dmnNetflow);
        }
        if (typ.equals("udpfwd")) {
            return new servGenEntry(new servUdpFwd(), cfgAll.dmnUdpFwd);
        }
        if (typ.equals("upnpfwd")) {
            return new servGenEntry(new servUpnpFwd(), cfgAll.dmnUpnpFwd);
        }
        if (typ.equals("upnphub")) {
            return new servGenEntry(new servUpnpHub(), cfgAll.dmnUpnpHub);
        }
        if (typ.equals("openflow")) {
            return new servGenEntry(new servOpenflow(), cfgAll.dmnOpenflow);
        }
        if (typ.equals("pktmux")) {
            return new servGenEntry(new servPktmux(), cfgAll.dmnPktmux);
        }
        if (typ.equals("p4lang")) {
            return new servGenEntry(new servP4lang(), cfgAll.dmnP4lang);
        }
        if (typ.equals("stack")) {
            return new servGenEntry(new servStack(), cfgAll.dmnStack);
        }
        if (typ.equals("forwarder")) {
            return new servGenEntry(new servForwarder(), cfgAll.dmnForwarder);
        }
        if (typ.equals("syslog")) {
            return new servGenEntry(new servSyslog(), cfgAll.dmnSyslog);
        }
        if (typ.equals("loadbalancer")) {
            return new servGenEntry(new servLoadBalancer(), cfgAll.dmnLoadBalancer);
        }
        if (typ.equals("multiplexer")) {
            return new servGenEntry(new servMultiplexer(), cfgAll.dmnMultiplexer);
        }
        if (typ.equals("telnet")) {
            return new servGenEntry(new servTelnet(), cfgAll.dmnTelnet);
        }
        if (typ.equals("xotpad")) {
            return new servGenEntry(new servXotPad(), cfgAll.dmnXotpad);
        }
        if (typ.equals("rfb")) {
            return new servGenEntry(new servRfb(), cfgAll.dmnRfb);
        }
        if (typ.equals("udptn")) {
            return new servGenEntry(new servUdptn(), cfgAll.dmnUdptn);
        }
        if (typ.equals("http")) {
            return new servGenEntry(new servHttp(), cfgAll.dmnHttp);
        }
        if (typ.equals("lpd")) {
            return new servGenEntry(new servLpd(), cfgAll.dmnLpd);
        }
        if (typ.equals("honeypot")) {
            return new servGenEntry(new servHoneyPot(), cfgAll.dmnHoney);
        }
        if (typ.equals("whois")) {
            return new servGenEntry(new servWhois(), cfgAll.dmnWhois);
        }
        if (typ.equals("dhcp4")) {
            return new servGenEntry(new servDhcp4(), cfgAll.dmnDhcp4);
        }
        if (typ.equals("dhcp6")) {
            return new servGenEntry(new servDhcp6(), cfgAll.dmnDhcp6);
        }
        if (typ.equals("dns")) {
            return new servGenEntry(new servDns(), cfgAll.dmnDns);
        }
        if (typ.equals("pop3")) {
            return new servGenEntry(new servPop3(), cfgAll.dmnPop3);
        }
        if (typ.equals("imap4")) {
            return new servGenEntry(new servImap4(), cfgAll.dmnImap4);
        }
        if (typ.equals("smtp")) {
            return new servGenEntry(new servSmtp(), cfgAll.dmnSmtp);
        }
        if (typ.equals("modem")) {
            return new servGenEntry(new servModem(), cfgAll.dmnModem);
        }
        if (typ.equals("voice")) {
            return new servGenEntry(new servVoice(), cfgAll.dmnVoice);
        }
        if (typ.equals("sip")) {
            return new servGenEntry(new servSip(), cfgAll.dmnSip);
        }
        if (typ.equals("socks")) {
            return new servGenEntry(new servSocks(), cfgAll.dmnSocks);
        }
        if (typ.equals("ftp")) {
            return new servGenEntry(new servFtp(), cfgAll.dmnFtp);
        }
        if (typ.equals("tftp")) {
            return new servGenEntry(new servTftp(), cfgAll.dmnTftp);
        }
        if (typ.equals("gopher")) {
            return new servGenEntry(new servGopher(), cfgAll.dmnGopher);
        }
        if (typ.equals("plan9")) {
            return new servGenEntry(new servPlan9(), cfgAll.dmnPlan9);
        }
        if (typ.equals("iscsi")) {
            return new servGenEntry(new servIscsi(), cfgAll.dmnIscsi);
        }
        if (typ.equals("bmp2mrt")) {
            return new servGenEntry(new servBmp2mrt(), cfgAll.dmnBmp);
        }
        if (typ.equals("irc")) {
            return new servGenEntry(new servIrc(), cfgAll.dmnIrc);
        }
        if (typ.equals("dcp")) {
            return new servGenEntry(new servDcp(), cfgAll.dmnDcp);
        }
        if (typ.equals("sdwan")) {
            return new servGenEntry(new servSdwan(), cfgAll.dmnSdwan);
        }
        if (typ.equals("pcep")) {
            return new servGenEntry(new servPcep(), cfgAll.dmnPcep);
        }
        if (typ.equals("ntp")) {
            return new servGenEntry(new servNtp(), cfgAll.dmnNtp);
        }
        if (typ.equals("daytime")) {
            return new servGenEntry(new servDaytime(), cfgAll.dmnDaytime);
        }
        if (typ.equals("rexec")) {
            return new servGenEntry(new servRexec(), cfgAll.dmnRexec);
        }
        if (typ.equals("rtpstat")) {
            return new servGenEntry(new servRtpStat(), cfgAll.dmnRtpStat);
        }
        if (typ.equals("time")) {
            return new servGenEntry(new servTime(), cfgAll.dmnTime);
        }
        if (typ.equals("snmp")) {
            return new servGenEntry(new servSnmp(), cfgAll.dmnSnmp);
        }
        if (typ.equals("rpki")) {
            return new servGenEntry(new servRpki(), cfgAll.dmnRpki);
        }
        if (typ.equals("nrpe")) {
            return new servGenEntry(new servNrpe(), cfgAll.dmnNrpe);
        }
        if (typ.equals("prometheus")) {
            return new servGenEntry(new servPrometheus(), cfgAll.dmnPrometheus);
        }
        if (typ.equals("streamingmdt")) {
            return new servGenEntry(new servStreamingMdt(), cfgAll.dmnStreamingMdt);
        }
        if (typ.equals("mrt2bgp")) {
            return new servGenEntry(new servMrt2bgp(), cfgAll.dmnMrt2bgp);
        }
        if (typ.equals("bstun")) {
            return new servGenEntry(new servBstun(), cfgAll.dmnBStun);
        }
        if (typ.equals("stun")) {
            return new servGenEntry(new servStun(), cfgAll.dmnStun);
        }
        if (typ.equals("pckoudp")) {
            return new servGenEntry(new servPckOudp(), cfgAll.dmnPckOudp);
        }
        if (typ.equals("pckodtls")) {
            return new servGenEntry(new servPckOdtls(), cfgAll.dmnPckOdtls);
        }
        if (typ.equals("pckotcp")) {
            return new servGenEntry(new servPckOtcp(), cfgAll.dmnPckOtcp);
        }
        if (typ.equals("pckotxt")) {
            return new servGenEntry(new servPckOtxt(), cfgAll.dmnPckOtxt);
        }
        if (typ.equals("capwap")) {
            return new servGenEntry(new servCapwap(), cfgAll.dmnCapwap);
        }
        if (typ.equals("lwapp")) {
            return new servGenEntry(new servLwapp(), cfgAll.dmnLwapp);
        }
        if (typ.equals("vxlan")) {
            return new servGenEntry(new servVxlan(), cfgAll.dmnVxlan);
        }
        if (typ.equals("geneve")) {
            return new servGenEntry(new servGeneve(), cfgAll.dmnGeneve);
        }
        if (typ.equals("l2f")) {
            return new servGenEntry(new servL2f(), cfgAll.dmnL2f);
        }
        if (typ.equals("l2tp2")) {
            return new servGenEntry(new servL2tp2(), cfgAll.dmnL2tp2);
        }
        if (typ.equals("l2tp3")) {
            return new servGenEntry(new servL2tp3(), cfgAll.dmnL2tp3);
        }
        if (typ.equals("etherip")) {
            return new servGenEntry(new servEtherIp(), cfgAll.dmnEtherIp);
        }
        if (typ.equals("eoip")) {
            return new servGenEntry(new servEoIp(), cfgAll.dmnEoIp);
        }
        if (typ.equals("gre")) {
            return new servGenEntry(new servGre(), cfgAll.dmnGre);
        }
        if (typ.equals("erspan")) {
            return new servGenEntry(new servErspan(), cfgAll.dmnErspan);
        }
        if (typ.equals("sreth")) {
            return new servGenEntry(new servSrEth(), cfgAll.dmnSrEth);
        }
        if (typ.equals("mplsip")) {
            return new servGenEntry(new servMplsIp(), cfgAll.dmnMplsIp);
        }
        if (typ.equals("mplsudp")) {
            return new servGenEntry(new servMplsUdp(), cfgAll.dmnMplsUdp);
        }
        if (typ.equals("mplsoam")) {
            return new servGenEntry(new servMplsOam(), cfgAll.dmnMplsOam);
        }
        if (typ.equals("twamp")) {
            return new servGenEntry(new servTwamp(), cfgAll.dmnTwamp);
        }
        if (typ.equals("amt")) {
            return new servGenEntry(new servAmt(), cfgAll.dmnAmt);
        }
        if (typ.equals("uni2multi")) {
            return new servGenEntry(new servUni2multi(), cfgAll.dmnUni2mul);
        }
        if (typ.equals("uni2uni")) {
            return new servGenEntry(new servUni2uni(), cfgAll.dmnUni2uni);
        }
        if (typ.equals("gtp")) {
            return new servGenEntry(new servGtp(), cfgAll.dmnGtp);
        }
        if (typ.equals("pptp")) {
            return new servGenEntry(new servPptp(), cfgAll.dmnPptp);
        }
        if (typ.equals("radius")) {
            return new servGenEntry(new servRadius(), cfgAll.dmnRadius);
        }
        if (typ.equals("ldap")) {
            return new servGenEntry(new servLdap(), cfgAll.dmnLdap);
        }
        if (typ.equals("tacacs")) {
            return new servGenEntry(new servTacacs(), cfgAll.dmnTacacs);
        }
        return null;
    }

}
