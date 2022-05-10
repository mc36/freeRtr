package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgGeneric;
import net.freertr.tab.tabGen;
import net.freertr.user.userHelping;

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
     */
    public static void srvHelp(userHelping l, int p, String e) {
        l.add(null, p + "  " + (p + 1) + "    echo                         echo server" + e);
        l.add(cfgAll.dmnEcho.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    discard                      discard server" + e);
        l.add(cfgAll.dmnDiscard.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    chargen                      chargen server" + e);
        l.add(cfgAll.dmnCharGen.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    netflow                      netflow server" + e);
        l.add(cfgAll.dmnNetflow.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    udpfwd                       udp forwarder server" + e);
        l.add(cfgAll.dmnUdpFwd.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    upnpfwd                      upnp forwarder server" + e);
        l.add(cfgAll.dmnUpnpFwd.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    upnphub                      upnp hub server" + e);
        l.add(cfgAll.dmnUpnpHub.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    openflow                     openflow server" + e);
        l.add(cfgAll.dmnOpenflow.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pktmux                       p4lang emulator server" + e);
        l.add(cfgAll.dmnPktmux.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    p4lang                       p4lang server" + e);
        l.add(cfgAll.dmnP4lang.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    quote                        quote server" + e);
        l.add(cfgAll.dmnQuote.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    forwarder                    forwarder server" + e);
        l.add(cfgAll.dmnForwarder.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    syslog                       syslog server" + e);
        l.add(cfgAll.dmnSyslog.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    loadbalancer                 loadbalancer server" + e);
        l.add(cfgAll.dmnLoadBalancer.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    multiplexer                  multiplexer server" + e);
        l.add(cfgAll.dmnMultiplexer.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    telnet                       telnet server" + e);
        l.add(cfgAll.dmnTelnet.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    rfb                          rfb server" + e);
        l.add(cfgAll.dmnRfb.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    udptn                        udptn server" + e);
        l.add(cfgAll.dmnUdptn.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    http                         http server" + e);
        l.add(cfgAll.dmnHttp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    dhcp4                        dhcp4 server" + e);
        l.add(cfgAll.dmnDhcp4.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    dhcp6                        dhcp6 server" + e);
        l.add(cfgAll.dmnDhcp6.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    dns                          dns server" + e);
        l.add(cfgAll.dmnDns.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    lpd                          lpd server" + e);
        l.add(cfgAll.dmnLpd.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    honeypot                     honeypot server" + e);
        l.add(cfgAll.dmnHoney.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pop3                         pop3 server" + e);
        l.add(cfgAll.dmnPop3.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    smtp                         smtp server" + e);
        l.add(cfgAll.dmnSmtp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    modem                        modem server" + e);
        l.add(cfgAll.dmnModem.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    voice                        voice server" + e);
        l.add(cfgAll.dmnVoice.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    sip                          sip server" + e);
        l.add(cfgAll.dmnSip.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    ftp                          ftp server" + e);
        l.add(cfgAll.dmnFtp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    tftp                         tftp server" + e);
        l.add(cfgAll.dmnTftp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    gopher                       gopher server" + e);
        l.add(cfgAll.dmnGopher.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    iscsi                        iscsi server" + e);
        l.add(cfgAll.dmnIscsi.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    bmp2mrt                      bmp to mrt server" + e);
        l.add(cfgAll.dmnBmp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    irc                          irc server" + e);
        l.add(cfgAll.dmnIrc.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    dcp                          dcp server" + e);
        l.add(cfgAll.dmnDcp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    sdwan                        sdwan server" + e);
        l.add(cfgAll.dmnSdwan.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pcep                         pcep server" + e);
        l.add(cfgAll.dmnPcep.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    ntp                          ntp server" + e);
        l.add(cfgAll.dmnNtp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    daytime                      daytime server" + e);
        l.add(cfgAll.dmnNtp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    time                         time server" + e);
        l.add(cfgAll.dmnTime.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    snmp                         snmp server" + e);
        l.add(cfgAll.dmnSnmp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    socks                        socks server" + e);
        l.add(cfgAll.dmnSocks.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    rpki                         rpki server" + e);
        l.add(cfgAll.dmnRpki.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    nrpe                         nrpe server" + e);
        l.add(cfgAll.dmnNrpe.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    prometheus                   prometheus server" + e);
        l.add(cfgAll.dmnPrometheus.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    streamingmdt                 streaming telemetry server" + e);
        l.add(cfgAll.dmnStreamingMdt.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    bstun                        bstun server" + e);
        l.add(cfgAll.dmnBStun.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    stun                         stun server" + e);
        l.add(cfgAll.dmnStun.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    vxlan                        vxlan server" + e);
        l.add(cfgAll.dmnVxlan.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    geneve                       geneve server" + e);
        l.add(cfgAll.dmnGeneve.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    l2f                          l2f server" + e);
        l.add(cfgAll.dmnL2f.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    l2tp2                        l2tp v2 server" + e);
        l.add(cfgAll.dmnL2tp2.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    l2tp3                        l2tp v3 server" + e);
        l.add(cfgAll.dmnL2tp3.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    etherip                      etherip server" + e);
        l.add(cfgAll.dmnEtherIp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    gre                          gre server" + e);
        l.add(cfgAll.dmnGre.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    mplsip                       mplsip server" + e);
        l.add(cfgAll.dmnMplsIp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    mplsudp                      mplsudp server" + e);
        l.add(cfgAll.dmnMplsUdp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    mplsoam                      mplsoam server" + e);
        l.add(cfgAll.dmnMplsOam.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    twamp                        twamp server" + e);
        l.add(cfgAll.dmnTwamp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    amt                          amt server" + e);
        l.add(cfgAll.dmnAmt.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    uni2multi                    uni2multi server" + e);
        l.add(cfgAll.dmnUni2mul.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pckoudp                      pckoudp server" + e);
        l.add(cfgAll.dmnPckOudp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pckodtls                     pckodtls server" + e);
        l.add(cfgAll.dmnPckOdtls.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pckotcp                      pckotcp server" + e);
        l.add(cfgAll.dmnPckOtcp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pckotxt                      pckotxt server" + e);
        l.add(cfgAll.dmnPckOtxt.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    gtp                          gtp server" + e);
        l.add(cfgAll.dmnGtp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    pptp                         pptp server" + e);
        l.add(cfgAll.dmnPptp.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    radius                       radius server" + e);
        l.add(cfgAll.dmnRadius.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
        l.add(null, p + "  " + (p + 1) + "    tacacs                       tacacs server" + e);
        l.add(cfgAll.dmnTacacs.listServers(), (p + 1) + "  .        <name:loc>                 name of server");
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
        if (typ.equals("gre")) {
            return new servGenEntry(new servGre(), cfgAll.dmnGre);
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
        if (typ.equals("gtp")) {
            return new servGenEntry(new servGtp(), cfgAll.dmnGtp);
        }
        if (typ.equals("pptp")) {
            return new servGenEntry(new servPptp(), cfgAll.dmnPptp);
        }
        if (typ.equals("radius")) {
            return new servGenEntry(new servRadius(), cfgAll.dmnRadius);
        }
        if (typ.equals("tacacs")) {
            return new servGenEntry(new servTacacs(), cfgAll.dmnTacacs);
        }
        return null;
    }

}
