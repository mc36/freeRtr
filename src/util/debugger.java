package util;

import java.util.List;
import user.userHelping;

/**
 * debugging feature
 *
 * @author matecsaba
 */
public class debugger {

    private debugger() {
    }

    /**
     * userConfig commands
     */
    public static boolean userConfigEvnt = false;

    /**
     * userReader commands
     */
    public static boolean userReaderEvnt = false;

    /**
     * cfgInit hw commands
     */
    public static boolean cfgInitHw = false;

    /**
     * cfgInit sw commands
     */
    public static boolean cfgInitSw = false;

    /**
     * prtRedun commands
     */
    public static boolean prtRedun = false;

    /**
     * prtWatch commands
     */
    public static boolean prtWatchEvnt = false;

    /**
     * lineScript commands
     */
    public static boolean lineScript = false;

    /**
     * lineRunner commands
     */
    public static boolean lineRunnerEvnt = false;

    /**
     * lineThread commands
     */
    public static boolean lineThreadTraf = false;

    /**
     * servDhcp4 traffic
     */
    public static boolean servDhcp4traf = false;

    /**
     * servDhcp6 traffic
     */
    public static boolean servDhcp6traf = false;

    /**
     * servDns traffic
     */
    public static boolean servDnsTraf = false;

    /**
     * servFtp traffic
     */
    public static boolean servFtpTraf = false;

    /**
     * servTftp traffic
     */
    public static boolean servTftpTraf = false;

    /**
     * servGopher traffic
     */
    public static boolean servGopherTraf = false;

    /**
     * servIscsi traffic
     */
    public static boolean servIscsiTraf = false;

    /**
     * servRfb traffic
     */
    public static boolean servRfbTraf = false;

    /**
     * servNtp traffic
     */
    public static boolean servNtpTraf = false;

    /**
     * servSnmp traffic
     */
    public static boolean servSnmpTraf = false;

    /**
     * servScsi traffic
     */
    public static boolean servScsiTraf = false;

    /**
     * servHttp traffic
     */
    public static boolean servHttpTraf = false;

    /**
     * servHttp xml
     */
    public static boolean servHttpXml = false;

    /**
     * servLpd traffic
     */
    public static boolean servLpdTraf = false;

    /**
     * servPop3 traffic
     */
    public static boolean servPop3traf = false;

    /**
     * servSmtp traffic
     */
    public static boolean servSmtpTraf = false;

    /**
     * servIrc traffic
     */
    public static boolean servIrcTraf = false;

    /**
     * servRpki traffic
     */
    public static boolean servRpkiTraf = false;

    /**
     * servNrpe traffic
     */
    public static boolean servNrpeTraf = false;

    /**
     * servDcp traffic
     */
    public static boolean servDcpTraf = false;

    /**
     * servOpenflow traffic
     */
    public static boolean servOpenflowTraf = false;

    /**
     * servP4lang traffic
     */
    public static boolean servP4langTraf = false;

    /**
     * servModem traffic
     */
    public static boolean servModemTraf = false;

    /**
     * servVoice traffic
     */
    public static boolean servVoiceTraf = false;

    /**
     * servSip traffic
     */
    public static boolean servSipTraf = false;

    /**
     * servL2f traffic
     */
    public static boolean servL2fTraf = false;

    /**
     * servL2tp2 traffic
     */
    public static boolean servL2tp2traf = false;

    /**
     * servL2tp3 traffic
     */
    public static boolean servL2tp3traf = false;

    /**
     * servPptp traffic
     */
    public static boolean servPptpTraf = false;

    /**
     * servGtp traffic
     */
    public static boolean servGtpTraf = false;

    /**
     * servRadius traffic
     */
    public static boolean servRadiusTraf = false;

    /**
     * servTacacs traffic
     */
    public static boolean servTacacsTraf = false;

    /**
     * rtrBabel table events
     */
    public static boolean rtrBabelEvnt = false;

    /**
     * rtrBabel traffic
     */
    public static boolean rtrBabelTraf = false;

    /**
     * rtrOlsr table events
     */
    public static boolean rtrOlsrEvnt = false;

    /**
     * rtrOlsr traffic
     */
    public static boolean rtrOlsrTraf = false;

    /**
     * rtrRip4 table events
     */
    public static boolean rtrRip4evnt = false;

    /**
     * rtrRip4 traffic
     */
    public static boolean rtrRip4traf = false;

    /**
     * rtrRip6 table events
     */
    public static boolean rtrRip6evnt = false;

    /**
     * rtrRip6 traffic
     */
    public static boolean rtrRip6traf = false;

    /**
     * rtrOspf4 table events
     */
    public static boolean rtrOspf4evnt = false;

    /**
     * rtrOspf4 traffic
     */
    public static boolean rtrOspf4traf = false;

    /**
     * rtrOspf6 table events
     */
    public static boolean rtrOspf6evnt = false;

    /**
     * rtrOspf6 traffic
     */
    public static boolean rtrOspf6traf = false;

    /**
     * rtrIsis table events
     */
    public static boolean rtrIsisEvnt = false;

    /**
     * rtrIsis traffic
     */
    public static boolean rtrIsisTraf = false;

    /**
     * rtrBgp computation events
     */
    public static boolean rtrBgpComp = false;

    /**
     * rtrBgp incremental events
     */
    public static boolean rtrBgpIncr = false;

    /**
     * rtrBgp table events
     */
    public static boolean rtrBgpEvnt = false;

    /**
     * rtrBgp traffic
     */
    public static boolean rtrBgpTraf = false;

    /**
     * rtrMsdp events
     */
    public static boolean rtrMsdpEvnt = false;

    /**
     * rtrMsdp traffic
     */
    public static boolean rtrMsdpTraf = false;

    /**
     * rtrRsvp event
     */
    public static boolean rtrRsvpEvnt = false;

    /**
     * rtrRsvp traffic
     */
    public static boolean rtrRsvpTraf = false;

    /**
     * rtrLdp event
     */
    public static boolean rtrLdpEvnt = false;

    /**
     * rtrLdp traffic
     */
    public static boolean rtrLdpTraf = false;

    /**
     * rtrPvrp event
     */
    public static boolean rtrPvrpEvnt = false;

    /**
     * rtrPvrp traffic
     */
    public static boolean rtrPvrpTraf = false;

    /**
     * rtrLsrp event
     */
    public static boolean rtrLsrpEvnt = false;

    /**
     * rtrLsrp traffic
     */
    public static boolean rtrLsrpTraf = false;

    /**
     * rtrEigrp event
     */
    public static boolean rtrEigrpEvnt = false;

    /**
     * rtrEigrp traffic
     */
    public static boolean rtrEigrpTraf = false;

    /**
     * pim event
     */
    public static boolean rtrPimEvnt = false;

    /**
     * pim traffic
     */
    public static boolean rtrPimTraf = false;

    /**
     * bfd event
     */
    public static boolean rtrBfdEvnt = false;

    /**
     * bfd traffic
     */
    public static boolean rtrBfdTraf = false;

    /**
     * ptp event
     */
    public static boolean rtrPtpEvnt = false;

    /**
     * ptp traffic
     */
    public static boolean rtrPtpTraf = false;

    /**
     * hsrp event
     */
    public static boolean rtrHsrpEvnt = false;

    /**
     * hsrp traffic
     */
    public static boolean rtrHsrpTraf = false;

    /**
     * vrrp event
     */
    public static boolean rtrVrrpEvnt = false;

    /**
     * vrrp traffic
     */
    public static boolean rtrVrrpTraf = false;

    /**
     * prtGen traffic
     */
    public static boolean prtGenTraf = false;

    /**
     * prtTcp traffic
     */
    public static boolean prtTcpTraf = false;

    /**
     * prtUdp traffic
     */
    public static boolean prtUdpTraf = false;

    /**
     * prtLudp traffic
     */
    public static boolean prtLudpTraf = false;

    /**
     * prtDccp traffic
     */
    public static boolean prtDccpTraf = false;

    /**
     * prtSctp traffic
     */
    public static boolean prtSctpTraf = false;

    /**
     * prtGre traffic
     */
    public static boolean prtGreTraf = false;

    /**
     * prtIpIp traffic
     */
    public static boolean prtIpIpTraf = false;

    /**
     * prtPim traffic
     */
    public static boolean prtPimTraf = false;

    /**
     * tabConnect table events
     */
    public static boolean tabConnectEvnt = false;

    /**
     * tabRoute table events
     */
    public static boolean tabRouteEvnt = false;

    /**
     * tabLabel table events
     */
    public static boolean tabLabelEvnt = false;

    /**
     * tabListing table events
     */
    public static boolean tabListingEvnt = false;

    /**
     * clntNtp traffic
     */
    public static boolean clntNtpTraf = false;

    /**
     * clntSip traffic
     */
    public static boolean clntSipTraf = false;

    /**
     * clntSstp traffic
     */
    public static boolean clntSstpTraf = false;

    /**
     * clntAnyconnect traffic
     */
    public static boolean clntAnyconnTraf = false;

    /**
     * clntSnmp traffic
     */
    public static boolean clntSnmpTraf = false;

    /**
     * clntDhcp4 traffic
     */
    public static boolean clntDhcp4traf = false;

    /**
     * clntDhcp6 traffic
     */
    public static boolean clntDhcp6traf = false;

    /**
     * clntSlaac traffic
     */
    public static boolean clntSlaacTraf = false;

    /**
     * clntDns traffic
     */
    public static boolean clntDnsTraf = false;

    /**
     * clntRadius traffic
     */
    public static boolean clntRadiusTraf = false;

    /**
     * clntTacacs traffic
     */
    public static boolean clntTacacsTraf = false;

    /**
     * clntFtp traffic
     */
    public static boolean clntFtpTraf = false;

    /**
     * clntTftp traffic
     */
    public static boolean clntTftpTraf = false;

    /**
     * clntHttp traffic
     */
    public static boolean clntHttpTraf = false;

    /**
     * clntSmtp traffic
     */
    public static boolean clntSmtpTraf = false;

    /**
     * clntNrpe traffic
     */
    public static boolean clntNrpeTraf = false;

    /**
     * clntProxy traffic
     */
    public static boolean clntProxyTraf = false;

    /**
     * clntL2f traffic
     */
    public static boolean clntL2fTraf = false;

    /**
     * clntL2tp2 traffic
     */
    public static boolean clntL2tp2traf = false;

    /**
     * clntL2tp3 traffic
     */
    public static boolean clntL2tp3traf = false;

    /**
     * clntPptp traffic
     */
    public static boolean clntPptpTraf = false;

    /**
     * clntGtp traffic
     */
    public static boolean clntGtpTraf = false;

    /**
     * clntPwe traffic
     */
    public static boolean clntPweTraf = false;

    /**
     * clntMplsSr traffic
     */
    public static boolean clntMplsSrTraf = false;

    /**
     * clntMplsBier traffic
     */
    public static boolean clntMplsBierTraf = false;

    /**
     * clntMplsTe traffic
     */
    public static boolean clntMplsTeTraf = false;

    /**
     * automesh traffic
     */
    public static boolean clntMplsAutMsh = false;

    /**
     * clntMplsLdp traffic
     */
    public static boolean clntMplsLdpTraf = false;

    /**
     * ipCor4 traffic
     */
    public static boolean ipCor4traf = false;

    /**
     * ipCor6 traffic
     */
    public static boolean ipCor6traf = false;

    /**
     * ipCorSrh traffic
     */
    public static boolean ipCorSrhTraf = false;

    /**
     * ipMpls label traffic
     */
    public static boolean ipMPLStrafL = false;

    /**
     * ipMpls bier traffic
     */
    public static boolean ipMPLStrafB = false;

    /**
     * ipFwd events
     */
    public static boolean ipFwdEvnt = false;

    /**
     * ipFwd traffic
     */
    public static boolean ipFwdTraf = false;

    /**
     * ipIcmp4 traffic
     */
    public static boolean ipIcmp4traf = false;

    /**
     * ipIcmp6 traffic
     */
    public static boolean ipIcmp6traf = false;

    /**
     * ipMhost traffic
     */
    public static boolean ipMhostTraf = false;

    /**
     * ipIfc4arp traffic
     */
    public static boolean ipIfc4arpTraf = false;

    /**
     * ipIfc4arp events
     */
    public static boolean ipIfc4arpEvnt = false;

    /**
     * ipIfc6nei traffic
     */
    public static boolean ipIfc6neiTraf = false;

    /**
     * ipIfc6nei events
     */
    public static boolean ipIfc6neiEvnt = false;

    /**
     * ifcBridge packets
     */
    public static boolean ifcBridgeTraf = false;

    /**
     * ifcBundle packets
     */
    public static boolean ifcBundleTraf = false;

    /**
     * counterTraf events
     */
    public static boolean counterTraf = false;

    /**
     * ifcDot1q packets
     */
    public static boolean ifcDot1qTraf = false;

    /**
     * ifcDot1ad packets
     */
    public static boolean ifcDot1adTraf = false;

    /**
     * ifcDot1ah packets
     */
    public static boolean ifcDot1ahTraf = false;

    /**
     * ifcIsl packets
     */
    public static boolean ifcIslTraf = false;

    /**
     * ifcEthTyp packets
     */
    public static boolean ifcEthTypTraf = false;

    /**
     * ifcHdlc events
     */
    public static boolean ifcHdlcEvnt = false;

    /**
     * ifcIsdn events
     */
    public static boolean ifcIsdnEvnt = false;

    /**
     * ifcNhrp events
     */
    public static boolean ifcNhrpEvnt = false;

    /**
     * ifcFrameRelay events
     */
    public static boolean ifcFrmRlyEvnt = false;

    /**
     * ifcAtmDxi events
     */
    public static boolean ifcAtmDxiEvnt = false;

    /**
     * ifcAtmSar events
     */
    public static boolean ifcAtmSarEvnt = false;

    /**
     * ifcLapb events
     */
    public static boolean ifcLapbEvnt = false;

    /**
     * ifcSyncE events
     */
    public static boolean ifcSynceEvnt = false;

    /**
     * ifcPtp events
     */
    public static boolean ifcPtpEvnt = false;

    /**
     * ifcLacp events
     */
    public static boolean ifcLacpEvnt = false;

    /**
     * ifcPpp events
     */
    public static boolean ifcPppEvnt = false;

    /**
     * ifcSep events
     */
    public static boolean ifcSepEvnt = false;

    /**
     * ifcThread events
     */
    public static boolean ifcThread = false;

    /**
     * ifcP2pOEclnt events
     */
    public static boolean ifcP2pOEclnt = false;

    /**
     * ifcP2pOEserv events
     */
    public static boolean ifcP2pOEserv = false;

    /**
     * ifcP2pOErely events
     */
    public static boolean ifcP2pOErely = false;

    /**
     * ifcEapOLclnt events
     */
    public static boolean ifcEapOLclnt = false;

    /**
     * ifcEapOLserv events
     */
    public static boolean ifcEapOLserv = false;

    /**
     * ifcNsh events
     */
    public static boolean ifcNshEvnt = false;

    /**
     * ifcMacSec packets
     */
    public static boolean ifcMacSecTraf = false;

    /**
     * secSsh packets
     */
    public static boolean secSshTraf = false;

    /**
     * secTls packets
     */
    public static boolean secTlsTraf = false;

    /**
     * secIke packets
     */
    public static boolean secIkeTraf = false;

    /**
     * secTelnet packets
     */
    public static boolean secTelnetTraf = false;

    /**
     * secWebsock packets
     */
    public static boolean secWebsockTraf = false;

    /**
     * get help text for exec commands
     *
     * @param h helping to update
     */
    public static void getHelping(userHelping h) {
        h.add("2 3      line                line specific protocols");
        h.add("3 .        config            configuration commands");
        h.add("3 .        reader            command line reader events");
        h.add("3 .        script            chat script handler");
        h.add("3 .        hwcfg             hardware config");
        h.add("3 .        swcfg             software config");
        h.add("3 .        redundancy        redundancy events");
        h.add("3 .        watchdog          watchdog events");
        h.add("3 .        runner            line runner");
        h.add("3 .        thread            line thread");
        h.add("2 3      server              protocol servers");
        h.add("3 .        dhcp4             ipv4 dynamic host config protocol");
        h.add("3 .        dhcp6             ipv6 dynamic host config protocol");
        h.add("3 .        dns               domain name server");
        h.add("3 .        ftp               file transfer protocol");
        h.add("3 .        tftp              trivival file transfer protocol");
        h.add("3 .        gopher            gopher protocol");
        h.add("3 .        ntp               network time protocol");
        h.add("3 .        irc               internet relay protocol");
        h.add("3 .        rpki              resource public key infrastructure");
        h.add("3 .        nrpe              nagios remote plugin");
        h.add("3 .        dcp               direct connect protocol");
        h.add("3 .        openflow          openflow protocol");
        h.add("3 .        p4lang            p4lang protocol");
        h.add("3 .        snmp              simple network management protocol");
        h.add("3 .        iscsi             iscsi protocol");
        h.add("3 .        rfb               remote frame buffer protocol");
        h.add("3 .        scsi              scsi protocol");
        h.add("3 .        http              hypertext transfer protocol");
        h.add("3 .        lpd               line printer daemon protocol");
        h.add("3 .        pop3              post office protocol");
        h.add("3 .        smtp              simple mail transfer protocol");
        h.add("3 .        modem             modulator demodulator");
        h.add("3 .        voice             voice script");
        h.add("3 .        sip               session initiation protocol");
        h.add("3 .        l2f               layer 2 forwarding protocol");
        h.add("3 .        l2tp2             layer 2 tunneling protocol v2");
        h.add("3 .        l2tp3             layer 2 tunneling protocol v3");
        h.add("3 .        pptp              point to point tunneling protocol");
        h.add("3 .        gtp               gprs tunneling protocol");
        h.add("3 .        radius            radius protocol");
        h.add("3 .        tacacs            tacacs protocol");
        h.add("2 3      proto               transport protocols");
        h.add("3 4        babel             babel routing protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        olsr              optimized link state routing protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        rip4              routing information protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        rip6              routing information protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        ospf4             open shortest path first protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        ospf6             open shortest path first protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        isis              intermediate system intermediate system protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        bgp               border gateway protocol");
        h.add("4 .          computation     computation events");
        h.add("4 .          incremental     incremental events");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        msdp              multicast source discovery protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        rsvp              resource reservation protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        ldp               label distribution protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        pvrp              path vector routing protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        lsrp              link state routing protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        eigrp             enhanced interior gateway routing protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        pim               protocol independent multicast protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        bfd               bidirectional forwarding detection protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        ptp               precision time protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        hsrp              hot standby router protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 4        vrrp              virtual router redundancy protocol");
        h.add("4 .          event           table events");
        h.add("4 .          traffic         interface packets");
        h.add("3 .        mhost             multicast host protocol");
        h.add("3 .        ssh               secure shell protocol");
        h.add("3 .        tls               transport layer security protocol");
        h.add("3 .        telnet            telnet protocol");
        h.add("3 .        websock           websocket protocol");
        h.add("3 .        ike               internet key exchange protocol");
        h.add("3 .        macsec            mac securicy protocol");
        h.add("3 .        gen               generic connections");
        h.add("3 .        tcp               transmission control protocol");
        h.add("3 .        udp               user datagram protocol");
        h.add("3 .        ludp              lightweight user datagram protocol");
        h.add("3 .        dccp              datagram congestion control protocol");
        h.add("3 .        sctp              stream control transmission protocol");
        h.add("3 .        gre               generic routing encapsulation");
        h.add("3 .        ipip              ip in ip encapsulation");
        h.add("2 3      table               table events");
        h.add("3 .        conn              connection table");
        h.add("3 .        route             route table");
        h.add("3 .        label             label table");
        h.add("3 .        list              generic listing");
        h.add("2 3      client              protocol clients");
        h.add("3 .        ntp               network time protocol");
        h.add("3 .        sip               session initiation protocol");
        h.add("3 .        sstp              secure socket tunneling protocol");
        h.add("3 .        anyconn           anyconnect protocol");
        h.add("3 .        snmp              simple network management protocol");
        h.add("3 .        dhcp4             ipv4 dynamic host config protocol");
        h.add("3 .        dhcp6             ipv6 dynamic host config protocol");
        h.add("3 .        slaac             stateless address autoconfiguration protocol");
        h.add("3 .        dns               domain name server");
        h.add("3 .        ftp               file transfer protocol");
        h.add("3 .        http              hypertext transfer protocol");
        h.add("3 .        smtp              simple mail transfer protocol");
        h.add("3 .        nrpe              nagios remote plugin");
        h.add("3 .        proxy             proxy protosols");
        h.add("3 .        tftp              trivival file transfer protocol");
        h.add("3 .        l2f               layer 2 forwarding protocol");
        h.add("3 .        l2tp2             layer 2 tunneling protocol v2");
        h.add("3 .        l2tp3             layer 2 tunneling protocol v3");
        h.add("3 .        pptp              point to point tunneling protocol");
        h.add("3 .        gtp               gprs tunneling protocol");
        h.add("3 .        pwe               pseudowire over mpls protocol");
        h.add("3 .        mplste            mpls traffeng tunnel");
        h.add("3 .        mplsbier          mpls bier tunnel");
        h.add("3 .        mplssr            mpls segrout tunnel");
        h.add("3 .        automesh          mpls automesh tunnel");
        h.add("3 .        mplsldp           mpls ldp tunnel");
        h.add("3 .        radius            radius protocol");
        h.add("3 .        tacacs            tacacs protocol");
        h.add("2 3      fwd                 protocol forwarding");
        h.add("3 .        event             forwarding events");
        h.add("3 .        packet            forwarded packets");
        h.add("3 .        srh               source routed packets");
        h.add("2 3      mpls                multiprotocol label switching");
        h.add("3 .        packet            mpls packet processing");
        h.add("3 .        bier              bier packet processing");
        h.add("2 3      ipv4                internet protocol v4");
        h.add("3 .        packet            ip packet processing");
        h.add("3 .        icmp              internet control messaging protocol");
        h.add("3 4        arp               address resolution protocol");
        h.add("4 .          packet          arp packets");
        h.add("4 .          event           arp events");
        h.add("2 3      ipv6                internet protocol v6");
        h.add("3 .        packet            ip packet processing");
        h.add("3 .        icmp              internet control messaging protocol");
        h.add("3 4        ndp               neighbor discovery protocol");
        h.add("4 .          packet          ndp packets");
        h.add("4 .          event           ndp events");
        h.add("2 3      iface               interface protocols");
        h.add("3 .        drop              dropped packets");
        h.add("3 .        bridge            briding");
        h.add("3 .        bundle            bundlng");
        h.add("3 .        dot1q             802.1q vlan");
        h.add("3 .        dot1ad            802.1ad vlan");
        h.add("3 .        dot1ah            802.1ah vlan");
        h.add("3 .        isl               isl vlan");
        h.add("3 .        ethtyp            ethertypes");
        h.add("3 .        isdn              isdn encapsulation");
        h.add("3 .        hdlc              hdlc encapsulation");
        h.add("3 .        nhrp              nhrp encapsulation");
        h.add("3 .        framerelay        frame relay encapsulation");
        h.add("3 .        atmdxi            atm dxi encapsulation");
        h.add("3 .        atmsar            atm sar encapsulation");
        h.add("3 .        lapb              lapb encapsulation");
        h.add("3 .        ppp               ppp encapsulation");
        h.add("3 .        sep               sep encapsulation");
        h.add("3 .        synce             synchronous ethernet");
        h.add("3 .        ptp               precision time protocol");
        h.add("3 .        lacp              lacp");
        h.add("3 .        thread            interface thread");
        h.add("3 .        p2poec            ppp over ethernet client");
        h.add("3 .        p2poes            ppp over ethernet server");
        h.add("3 .        p2poer            ppp over ethernet relay");
        h.add("3 .        eapolc            eap over lan client");
        h.add("3 .        eapols            eap over lan server");
        h.add("3 .        nsh               nsh encapsulation");
    }

    /**
     * set all values
     *
     * @param v value of event
     */
    public static void setAll(boolean v) {
        userHelping h = new userHelping();
        getHelping(h);
        List<String> r = h.getList();
        for (int i = 0; i < r.size(); i++) {
            cmds c = new cmds("debug", r.get(i));
            setByName(c, v);
        }
    }

    /**
     * set by name
     *
     * @param cmd command to parse
     * @param v value of event
     * @return false on success, true on error
     */
    public static boolean setByName(cmds cmd, boolean v) {
        String s = cmd.word();
        if (s.equals("line")) {
            s = cmd.word();
            if (s.equals("config")) {
                userConfigEvnt = v;
                return false;
            }
            if (s.equals("reader")) {
                userReaderEvnt = v;
                return false;
            }
            if (s.equals("hwcfg")) {
                cfgInitHw = v;
                return false;
            }
            if (s.equals("swcfg")) {
                cfgInitSw = v;
                return false;
            }
            if (s.equals("redundancy")) {
                prtRedun = v;
                return false;
            }
            if (s.equals("watchdog")) {
                prtWatchEvnt = v;
                return false;
            }
            if (s.equals("script")) {
                lineScript = v;
                return false;
            }
            if (s.equals("runner")) {
                lineRunnerEvnt = v;
                return false;
            }
            if (s.equals("thread")) {
                lineThreadTraf = v;
                return false;
            }
            return true;
        }
        if (s.equals("server")) {
            s = cmd.word();
            if (s.equals("dhcp4")) {
                servDhcp4traf = v;
                return false;
            }
            if (s.equals("dhcp6")) {
                servDhcp6traf = v;
                return false;
            }
            if (s.equals("dns")) {
                servDnsTraf = v;
                return false;
            }
            if (s.equals("ftp")) {
                servFtpTraf = v;
                return false;
            }
            if (s.equals("tftp")) {
                servTftpTraf = v;
                return false;
            }
            if (s.equals("gopher")) {
                servGopherTraf = v;
                return false;
            }
            if (s.equals("ntp")) {
                servNtpTraf = v;
                return false;
            }
            if (s.equals("snmp")) {
                servSnmpTraf = v;
                return false;
            }
            if (s.equals("iscsi")) {
                servIscsiTraf = v;
                return false;
            }
            if (s.equals("rfb")) {
                servRfbTraf = v;
                return false;
            }
            if (s.equals("scsi")) {
                servScsiTraf = v;
                return false;
            }
            if (s.equals("http")) {
                servHttpTraf = v;
                return false;
            }
            if (s.equals("lpd")) {
                servLpdTraf = v;
                return false;
            }
            if (s.equals("pop3")) {
                servPop3traf = v;
                return false;
            }
            if (s.equals("smtp")) {
                servSmtpTraf = v;
                return false;
            }
            if (s.equals("irc")) {
                servIrcTraf = v;
                return false;
            }
            if (s.equals("rpki")) {
                servRpkiTraf = v;
                return false;
            }
            if (s.equals("nrpe")) {
                servNrpeTraf = v;
                return false;
            }
            if (s.equals("dcp")) {
                servDcpTraf = v;
                return false;
            }
            if (s.equals("openflow")) {
                servOpenflowTraf = v;
                return false;
            }
            if (s.equals("p4lang")) {
                servP4langTraf = v;
                return false;
            }
            if (s.equals("modem")) {
                servModemTraf = v;
                return false;
            }
            if (s.equals("voice")) {
                servVoiceTraf = v;
                return false;
            }
            if (s.equals("sip")) {
                servSipTraf = v;
                return false;
            }
            if (s.equals("l2f")) {
                servL2fTraf = v;
                return false;
            }
            if (s.equals("l2tp2")) {
                servL2tp2traf = v;
                return false;
            }
            if (s.equals("l2tp3")) {
                servL2tp3traf = v;
                return false;
            }
            if (s.equals("pptp")) {
                servPptpTraf = v;
                return false;
            }
            if (s.equals("gtp")) {
                servGtpTraf = v;
                return false;
            }
            if (s.equals("radius")) {
                servRadiusTraf = v;
                return false;
            }
            if (s.equals("tacacs")) {
                servTacacsTraf = v;
                return false;
            }
            return true;
        }
        if (s.equals("proto")) {
            s = cmd.word();
            if (s.equals("gen")) {
                prtGenTraf = v;
                return false;
            }
            if (s.equals("tcp")) {
                prtTcpTraf = v;
                return false;
            }
            if (s.equals("udp")) {
                prtUdpTraf = v;
                return false;
            }
            if (s.equals("ludp")) {
                prtLudpTraf = v;
                return false;
            }
            if (s.equals("dccp")) {
                prtDccpTraf = v;
                return false;
            }
            if (s.equals("sctp")) {
                prtSctpTraf = v;
                return false;
            }
            if (s.equals("gre")) {
                prtGreTraf = v;
                return false;
            }
            if (s.equals("ipip")) {
                prtIpIpTraf = v;
                return false;
            }
            if (s.equals("babel")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrBabelEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrBabelTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("olsr")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrOlsrEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrOlsrTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("rip4")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrRip4evnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrRip4traf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("rip6")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrRip6evnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrRip6traf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("ospf4")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrOspf4evnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrOspf4traf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("ospf6")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrOspf6evnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrOspf6traf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("isis")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrIsisEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrIsisTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("bgp")) {
                s = cmd.word();
                if (s.equals("computation")) {
                    rtrBgpComp = v;
                    return false;
                }
                if (s.equals("incremental")) {
                    rtrBgpIncr = v;
                    return false;
                }
                if (s.equals("event")) {
                    rtrBgpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrBgpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("msdp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrMsdpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrMsdpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("rsvp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrRsvpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrRsvpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("ldp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrLdpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrLdpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("pvrp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrPvrpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrPvrpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("lsrp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrLsrpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrLsrpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("eigrp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrEigrpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrEigrpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("pim")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrPimEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrPimTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("bfd")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrBfdEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrBfdTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("ptp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrPtpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrPtpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("hsrp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrHsrpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrHsrpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("vrrp")) {
                s = cmd.word();
                if (s.equals("event")) {
                    rtrVrrpEvnt = v;
                    return false;
                }
                if (s.equals("traffic")) {
                    rtrVrrpTraf = v;
                    return false;
                }
                return true;
            }
            if (s.equals("mhost")) {
                ipMhostTraf = v;
                return false;
            }
            if (s.equals("ssh")) {
                secSshTraf = v;
                return false;
            }
            if (s.equals("tls")) {
                secTlsTraf = v;
                return false;
            }
            if (s.contains("ike")) {
                secIkeTraf = v;
                return false;
            }
            if (s.equals("macsec")) {
                ifcMacSecTraf = v;
                return false;
            }
            if (s.equals("telnet")) {
                secTelnetTraf = v;
                return false;
            }
            if (s.equals("websock")) {
                secWebsockTraf = v;
                return false;
            }
            return true;
        }
        if (s.equals("table")) {
            s = cmd.word();
            if (s.equals("conn")) {
                tabConnectEvnt = v;
                return false;
            }
            if (s.equals("route")) {
                tabRouteEvnt = v;
                return false;
            }
            if (s.equals("label")) {
                tabLabelEvnt = v;
                return false;
            }
            if (s.equals("list")) {
                tabListingEvnt = v;
                return false;
            }
            return true;
        }
        if (s.equals("client")) {
            s = cmd.word();
            if (s.equals("ntp")) {
                clntNtpTraf = v;
                return false;
            }
            if (s.equals("sip")) {
                clntSipTraf = v;
                return false;
            }
            if (s.equals("sstp")) {
                clntSstpTraf = v;
                return false;
            }
            if (s.equals("anyconn")) {
                clntAnyconnTraf = v;
                return false;
            }
            if (s.equals("snmp")) {
                clntSnmpTraf = v;
                return false;
            }
            if (s.equals("dhcp4")) {
                clntDhcp4traf = v;
                return false;
            }
            if (s.equals("dhcp6")) {
                clntDhcp6traf = v;
                return false;
            }
            if (s.equals("slaac")) {
                clntSlaacTraf = v;
                return false;
            }
            if (s.equals("dns")) {
                clntDnsTraf = v;
                return false;
            }
            if (s.equals("radius")) {
                clntRadiusTraf = v;
                return false;
            }
            if (s.equals("tacacs")) {
                clntTacacsTraf = v;
                return false;
            }
            if (s.equals("l2f")) {
                clntL2fTraf = v;
                return false;
            }
            if (s.equals("l2tp2")) {
                clntL2tp2traf = v;
                return false;
            }
            if (s.equals("l2tp3")) {
                clntL2tp3traf = v;
                return false;
            }
            if (s.equals("pptp")) {
                clntPptpTraf = v;
                return false;
            }
            if (s.equals("gtp")) {
                clntGtpTraf = v;
                return false;
            }
            if (s.equals("pwe")) {
                clntPweTraf = v;
                return false;
            }
            if (s.equals("mplste")) {
                clntMplsTeTraf = v;
                return false;
            }
            if (s.equals("mplsbier")) {
                clntMplsBierTraf = v;
                return false;
            }
            if (s.equals("mplssr")) {
                clntMplsSrTraf = v;
                return false;
            }
            if (s.equals("automesh")) {
                clntMplsAutMsh = v;
                return false;
            }
            if (s.equals("mplsldp")) {
                clntMplsLdpTraf = v;
                return false;
            }
            if (s.equals("ftp")) {
                clntFtpTraf = v;
                return false;
            }
            if (s.equals("tftp")) {
                clntTftpTraf = v;
                return false;
            }
            if (s.equals("http")) {
                clntHttpTraf = v;
                return false;
            }
            if (s.equals("smtp")) {
                clntSmtpTraf = v;
                return false;
            }
            if (s.equals("nrpe")) {
                clntNrpeTraf = v;
                return false;
            }
            if (s.equals("proxy")) {
                clntProxyTraf = v;
                return false;
            }
            return true;
        }
        if (s.equals("ipv4")) {
            s = cmd.word();
            if (s.equals("packet")) {
                ipCor4traf = v;
                return false;
            }
            if (s.equals("icmp")) {
                ipIcmp4traf = v;
                return false;
            }
            if (s.equals("arp")) {
                s = cmd.word();
                if (s.equals("packet")) {
                    ipIfc4arpTraf = v;
                    return false;
                }
                if (s.equals("event")) {
                    ipIfc4arpEvnt = v;
                    return false;
                }
                return true;
            }
            return true;
        }
        if (s.equals("mpls")) {
            s = cmd.word();
            if (s.equals("packet")) {
                ipMPLStrafL = v;
                return false;
            }
            if (s.equals("bier")) {
                ipMPLStrafB = v;
                return false;
            }
            return true;
        }
        if (s.equals("ipv6")) {
            s = cmd.word();
            if (s.equals("packet")) {
                ipCor6traf = v;
                return false;
            }
            if (s.equals("icmp")) {
                ipIcmp6traf = v;
                return false;
            }
            if (s.equals("ndp")) {
                s = cmd.word();
                if (s.equals("packet")) {
                    ipIfc6neiTraf = v;
                    return false;
                }
                if (s.equals("event")) {
                    ipIfc6neiEvnt = v;
                    return false;
                }
                return true;
            }
            return true;
        }
        if (s.equals("fwd")) {
            s = cmd.word();
            if (s.equals("event")) {
                ipFwdEvnt = v;
                return false;
            }
            if (s.equals("packet")) {
                ipFwdTraf = v;
                return false;
            }
            if (s.equals("srh")) {
                ipCorSrhTraf = v;
                return false;
            }
            return true;
        }
        if (s.equals("iface")) {
            s = cmd.word();
            if (s.equals("drop")) {
                counterTraf = v;
                return false;
            }
            if (s.equals("bridge")) {
                ifcBridgeTraf = v;
                return false;
            }
            if (s.equals("bundle")) {
                ifcBundleTraf = v;
                return false;
            }
            if (s.equals("dot1q")) {
                ifcDot1qTraf = v;
                return false;
            }
            if (s.equals("dot1ad")) {
                ifcDot1adTraf = v;
                return false;
            }
            if (s.equals("dot1ah")) {
                ifcDot1ahTraf = v;
                return false;
            }
            if (s.equals("isl")) {
                ifcIslTraf = v;
                return false;
            }
            if (s.equals("ethtyp")) {
                ifcEthTypTraf = v;
                return false;
            }
            if (s.equals("hdlc")) {
                ifcHdlcEvnt = v;
                return false;
            }
            if (s.equals("isdn")) {
                ifcIsdnEvnt = v;
                return false;
            }
            if (s.equals("nhrp")) {
                ifcNhrpEvnt = v;
                return false;
            }
            if (s.equals("framerelay")) {
                ifcFrmRlyEvnt = v;
                return false;
            }
            if (s.equals("atmdxi")) {
                ifcAtmDxiEvnt = v;
                return false;
            }
            if (s.equals("atmsar")) {
                ifcAtmSarEvnt = v;
                return false;
            }
            if (s.equals("lapb")) {
                ifcLapbEvnt = v;
                return false;
            }
            if (s.equals("ppp")) {
                ifcPppEvnt = v;
                return false;
            }
            if (s.equals("sep")) {
                ifcSepEvnt = v;
                return false;
            }
            if (s.equals("synce")) {
                ifcSynceEvnt = v;
                return false;
            }
            if (s.equals("ptp")) {
                ifcPtpEvnt = v;
                return false;
            }
            if (s.equals("lacp")) {
                ifcLacpEvnt = v;
                return false;
            }
            if (s.equals("thread")) {
                ifcThread = v;
                return false;
            }
            if (s.equals("p2poec")) {
                ifcP2pOEclnt = v;
                return false;
            }
            if (s.equals("p2poes")) {
                ifcP2pOEserv = v;
                return false;
            }
            if (s.equals("p2poer")) {
                ifcP2pOErely = v;
                return false;
            }
            if (s.equals("eapolc")) {
                ifcEapOLclnt = v;
                return false;
            }
            if (s.equals("eapols")) {
                ifcEapOLserv = v;
                return false;
            }
            if (s.equals("nsh")) {
                ifcNshEvnt = v;
                return false;
            }
            return true;
        }
        return true;
    }

}
