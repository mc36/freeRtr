package net.freertr.util;

import java.util.List;
import net.freertr.user.userHelping;

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
     * userExec commands
     */
    public static boolean userExecEvnt = false;

    /**
     * userReader commands
     */
    public static boolean userReaderEvnt = false;

    /**
     * userScreen commands
     */
    public static boolean userScreenEvnt = false;

    /**
     * userNetconf commands
     */
    public static boolean userNetconfEvnt = false;

    /**
     * userXml commands
     */
    public static boolean userXmlEvnt = false;

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
     * servPrometheus traffic
     */
    public static boolean servPrometheusTraf = false;

    /**
     * servDcp traffic
     */
    public static boolean servDcpTraf = false;

    /**
     * servPcep traffic
     */
    public static boolean servPcepTraf = false;

    /**
     * servOpenflow traffic
     */
    public static boolean servOpenflowRx = false;

    /**
     * servOpenflow traffic
     */
    public static boolean servOpenflowTx = false;

    /**
     * servP4lang traffic
     */
    public static boolean servP4langRx = false;

    /**
     * servP4lang traffic
     */
    public static boolean servP4langTx = false;

    /**
     * servP4lang traffic
     */
    public static boolean servP4langErr = false;

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
     * rtrBgp full events
     */
    public static boolean rtrBgpFull = false;

    /**
     * rtrBgp table events
     */
    public static boolean rtrBgpEvnt = false;

    /**
     * rtrBgp traffic
     */
    public static boolean rtrBgpTraf = false;

    /**
     * rtrBgp dampening
     */
    public static boolean rtrBgpDamp = false;

    /**
     * rtrBgp error
     */
    public static boolean rtrBgpError = false;

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
     * srh traffic
     */
    public static boolean rtrSrhTraf = false;

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
     * clntPcep traffic
     */
    public static boolean clntPcepTraf = false;

    /**
     * clntSstp traffic
     */
    public static boolean clntSstpTraf = false;

    /**
     * clntAnyconnect traffic
     */
    public static boolean clntAnyconnTraf = false;

    /**
     * clntForti traffic
     */
    public static boolean clntFortiTraf = false;

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
     * clntWireguard traffic
     */
    public static boolean clntWireguardTraf = false;

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
     * clntPolka traffic
     */
    public static boolean clntPolkaTraf = false;

    /**
     * clntMpolka traffic
     */
    public static boolean clntMpolkaTraf = false;

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
     * ifcSgt packets
     */
    public static boolean ifcSgtTraf = false;

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
     * ifcQinq1 packets
     */
    public static boolean ifcQinq1Traf = false;

    /**
     * ifcQinq2 packets
     */
    public static boolean ifcQinq2Traf = false;

    /**
     * ifcQinq3 packets
     */
    public static boolean ifcQinq3Traf = false;

    /**
     * ifcQinqX packets
     */
    public static boolean ifcQinqXTraf = false;

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
     * ifcMpolka events
     */
    public static boolean ifcMpolkaEvnt = false;

    /**
     * ifcPolka events
     */
    public static boolean ifcPolkaEvnt = false;

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
     * secHttp2 packets
     */
    public static boolean secHttp2traf = false;

    /**
     * get help text for exec commands
     *
     * @param l helping to update
     */
    public static void getHelping(userHelping l) {
        l.add(null, "2 3      line                line specific protocols");
        l.add(null, "3 .        config            configuration commands");
        l.add(null, "3 .        exec              exec commands");
        l.add(null, "3 .        reader            command line reader events");
        l.add(null, "3 .        screen            terminal screen events");
        l.add(null, "3 .        netconf           netconf events");
        l.add(null, "3 .        xml               xml events");
        l.add(null, "3 .        script            chat script handler");
        l.add(null, "3 .        hwcfg             hardware config");
        l.add(null, "3 .        swcfg             software config");
        l.add(null, "3 .        redundancy        redundancy events");
        l.add(null, "3 .        watchdog          watchdog events");
        l.add(null, "3 .        runner            line runner");
        l.add(null, "3 .        thread            line thread");
        l.add(null, "2 3      server              protocol servers");
        l.add(null, "3 .        dhcp4             ipv4 dynamic host config protocol");
        l.add(null, "3 .        dhcp6             ipv6 dynamic host config protocol");
        l.add(null, "3 .        dns               domain name server");
        l.add(null, "3 .        ftp               file transfer protocol");
        l.add(null, "3 .        tftp              trivival file transfer protocol");
        l.add(null, "3 .        gopher            gopher protocol");
        l.add(null, "3 .        ntp               network time protocol");
        l.add(null, "3 .        irc               internet relay protocol");
        l.add(null, "3 .        rpki              resource public key infrastructure");
        l.add(null, "3 .        nrpe              nagios remote plugin");
        l.add(null, "3 .        prometheus        prometheus targets");
        l.add(null, "3 .        dcp               direct connect protocol");
        l.add(null, "3 .        pcep              path computation element protocol");
        l.add(null, "3 4        openflow          openflow protocol");
        l.add(null, "4 .          rx              received packets");
        l.add(null, "4 .          tx              transmitted packets");
        l.add(null, "3 4        p4lang            p4lang protocol");
        l.add(null, "4 .          rx              received packets");
        l.add(null, "4 .          tx              transmitted packets");
        l.add(null, "4 .          err             errored packets");
        l.add(null, "3 .        snmp              simple network management protocol");
        l.add(null, "3 .        iscsi             iscsi protocol");
        l.add(null, "3 .        rfb               remote frame buffer protocol");
        l.add(null, "3 .        scsi              scsi protocol");
        l.add(null, "3 .        http              hypertext transfer protocol");
        l.add(null, "3 .        lpd               line printer daemon protocol");
        l.add(null, "3 .        pop3              post office protocol");
        l.add(null, "3 .        smtp              simple mail transfer protocol");
        l.add(null, "3 .        modem             modulator demodulator");
        l.add(null, "3 .        voice             voice script");
        l.add(null, "3 .        sip               session initiation protocol");
        l.add(null, "3 .        l2f               layer 2 forwarding protocol");
        l.add(null, "3 .        l2tp2             layer 2 tunneling protocol v2");
        l.add(null, "3 .        l2tp3             layer 2 tunneling protocol v3");
        l.add(null, "3 .        pptp              point to point tunneling protocol");
        l.add(null, "3 .        gtp               gprs tunneling protocol");
        l.add(null, "3 .        radius            radius protocol");
        l.add(null, "3 .        tacacs            tacacs protocol");
        l.add(null, "2 3      proto               transport protocols");
        l.add(null, "3 4        babel             babel routing protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        olsr              optimized link state routing protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        rip4              routing information protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        rip6              routing information protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        ospf4             open shortest path first protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        ospf6             open shortest path first protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        isis              intermediate system intermediate system protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        bgp               border gateway protocol");
        l.add(null, "4 .          computation     computation events");
        l.add(null, "4 .          incremental     incremental events");
        l.add(null, "4 .          dampening       dampening events");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          full            full events");
        l.add(null, "4 .          error           error events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        msdp              multicast source discovery protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        rsvp              resource reservation protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        ldp               label distribution protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        pvrp              path vector routing protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        lsrp              link state routing protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        eigrp             enhanced interior gateway routing protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        pim               protocol independent multicast protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        bfd               bidirectional forwarding detection protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        ptp               precision time protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        hsrp              hot standby router protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 4        vrrp              virtual router redundancy protocol");
        l.add(null, "4 .          event           table events");
        l.add(null, "4 .          traffic         interface packets");
        l.add(null, "3 .        mhost             multicast host protocol");
        l.add(null, "3 .        ssh               secure shell protocol");
        l.add(null, "3 .        tls               transport layer security protocol");
        l.add(null, "3 .        telnet            telnet protocol");
        l.add(null, "3 .        websock           websocket protocol");
        l.add(null, "3 .        http2             http2 protocol");
        l.add(null, "3 .        ike               internet key exchange protocol");
        l.add(null, "3 .        macsec            mac securicy protocol");
        l.add(null, "3 .        gen               generic connections");
        l.add(null, "3 .        tcp               transmission control protocol");
        l.add(null, "3 .        udp               user datagram protocol");
        l.add(null, "3 .        ludp              lightweight user datagram protocol");
        l.add(null, "3 .        dccp              datagram congestion control protocol");
        l.add(null, "3 .        sctp              stream control transmission protocol");
        l.add(null, "3 .        gre               generic routing encapsulation");
        l.add(null, "3 .        ipip              ip in ip encapsulation");
        l.add(null, "2 3      table               table events");
        l.add(null, "3 .        conn              connection table");
        l.add(null, "3 .        route             route table");
        l.add(null, "3 .        label             label table");
        l.add(null, "3 .        list              generic listing");
        l.add(null, "2 3      client              protocol clients");
        l.add(null, "3 .        ntp               network time protocol");
        l.add(null, "3 .        sip               session initiation protocol");
        l.add(null, "3 .        pcep              path computation element protocol");
        l.add(null, "3 .        sstp              secure socket tunneling protocol");
        l.add(null, "3 .        anyconn           anyconnect protocol");
        l.add(null, "3 .        forti             fortinet protocol");
        l.add(null, "3 .        snmp              simple network management protocol");
        l.add(null, "3 .        dhcp4             ipv4 dynamic host config protocol");
        l.add(null, "3 .        dhcp6             ipv6 dynamic host config protocol");
        l.add(null, "3 .        slaac             stateless address autoconfiguration protocol");
        l.add(null, "3 .        dns               domain name server");
        l.add(null, "3 .        ftp               file transfer protocol");
        l.add(null, "3 .        http              hypertext transfer protocol");
        l.add(null, "3 .        smtp              simple mail transfer protocol");
        l.add(null, "3 .        nrpe              nagios remote plugin");
        l.add(null, "3 .        proxy             proxy protosols");
        l.add(null, "3 .        tftp              trivival file transfer protocol");
        l.add(null, "3 .        l2f               layer 2 forwarding protocol");
        l.add(null, "3 .        l2tp2             layer 2 tunneling protocol v2");
        l.add(null, "3 .        l2tp3             layer 2 tunneling protocol v3");
        l.add(null, "3 .        pptp              point to point tunneling protocol");
        l.add(null, "3 .        wireguard         wireguard protocol");
        l.add(null, "3 .        gtp               gprs tunneling protocol");
        l.add(null, "3 .        pwe               pseudowire over mpls protocol");
        l.add(null, "3 .        mplste            mpls traffeng tunnel");
        l.add(null, "3 .        mplsbier          mpls bier tunnel");
        l.add(null, "3 .        mplssr            mpls segrout tunnel");
        l.add(null, "3 .        polka             polka tunnel");
        l.add(null, "3 .        mpolka            mpolka tunnel");
        l.add(null, "3 .        automesh          mpls automesh tunnel");
        l.add(null, "3 .        mplsldp           mpls ldp tunnel");
        l.add(null, "3 .        radius            radius protocol");
        l.add(null, "3 .        tacacs            tacacs protocol");
        l.add(null, "2 3      fwd                 protocol forwarding");
        l.add(null, "3 .        event             forwarding events");
        l.add(null, "3 .        packet            forwarded packets");
        l.add(null, "3 .        srh               source routed packets");
        l.add(null, "2 3      mpls                multiprotocol label switching");
        l.add(null, "3 .        packet            mpls packet processing");
        l.add(null, "3 .        bier              bier packet processing");
        l.add(null, "2 3      ipv4                internet protocol v4");
        l.add(null, "3 .        packet            ip packet processing");
        l.add(null, "3 .        icmp              internet control messaging protocol");
        l.add(null, "3 4        arp               address resolution protocol");
        l.add(null, "4 .          packet          arp packets");
        l.add(null, "4 .          event           arp events");
        l.add(null, "2 3      ipv6                internet protocol v6");
        l.add(null, "3 .        packet            ip packet processing");
        l.add(null, "3 .        icmp              internet control messaging protocol");
        l.add(null, "3 4        ndp               neighbor discovery protocol");
        l.add(null, "4 .          packet          ndp packets");
        l.add(null, "4 .          event           ndp events");
        l.add(null, "2 3      iface               interface protocols");
        l.add(null, "3 .        drop              dropped packets");
        l.add(null, "3 .        bridge            briding");
        l.add(null, "3 .        bundle            bundlng");
        l.add(null, "3 .        sgt               sgt encapsulation");
        l.add(null, "3 .        dot1q             802.1q vlan");
        l.add(null, "3 .        dot1ad            802.1ad vlan");
        l.add(null, "3 .        dot1ah            802.1ah vlan");
        l.add(null, "3 .        qinq1             qinq1 vlan");
        l.add(null, "3 .        qinq2             qinq2 vlan");
        l.add(null, "3 .        qinq3             qinq3 vlan");
        l.add(null, "3 .        qinqx             qinqx vlan");
        l.add(null, "3 .        isl               isl vlan");
        l.add(null, "3 .        ethtyp            ethertypes");
        l.add(null, "3 .        isdn              isdn encapsulation");
        l.add(null, "3 .        hdlc              hdlc encapsulation");
        l.add(null, "3 .        nhrp              nhrp encapsulation");
        l.add(null, "3 .        framerelay        frame relay encapsulation");
        l.add(null, "3 .        atmdxi            atm dxi encapsulation");
        l.add(null, "3 .        atmsar            atm sar encapsulation");
        l.add(null, "3 .        lapb              lapb encapsulation");
        l.add(null, "3 .        ppp               ppp encapsulation");
        l.add(null, "3 .        sep               sep encapsulation");
        l.add(null, "3 .        synce             synchronous ethernet");
        l.add(null, "3 .        ptp               precision time protocol");
        l.add(null, "3 .        lacp              lacp");
        l.add(null, "3 .        thread            interface thread");
        l.add(null, "3 .        p2poec            ppp over ethernet client");
        l.add(null, "3 .        p2poes            ppp over ethernet server");
        l.add(null, "3 .        p2poer            ppp over ethernet relay");
        l.add(null, "3 .        eapolc            eap over lan client");
        l.add(null, "3 .        eapols            eap over lan server");
        l.add(null, "3 .        nsh               nsh encapsulation");
        l.add(null, "3 .        polka             polka encapsulation");
        l.add(null, "3 .        mpolka            mpolka encapsulation");
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
            if (s.equals("exec")) {
                userExecEvnt = v;
                return false;
            }
            if (s.equals("reader")) {
                userReaderEvnt = v;
                return false;
            }
            if (s.equals("screen")) {
                userScreenEvnt = v;
                return false;
            }
            if (s.equals("netconf")) {
                userNetconfEvnt = v;
                return false;
            }
            if (s.equals("xml")) {
                userXmlEvnt = v;
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
            if (s.equals("prometheus")) {
                servPrometheusTraf = v;
                return false;
            }
            if (s.equals("dcp")) {
                servDcpTraf = v;
                return false;
            }
            if (s.equals("pcep")) {
                servPcepTraf = v;
                return false;
            }
            if (s.equals("openflow")) {
                s = cmd.word();
                if (s.equals("rx")) {
                    servOpenflowRx = v;
                    return false;
                }
                if (s.equals("tx")) {
                    servOpenflowTx = v;
                    return false;
                }
                return true;
            }
            if (s.equals("p4lang")) {
                s = cmd.word();
                if (s.equals("rx")) {
                    servP4langRx = v;
                    return false;
                }
                if (s.equals("tx")) {
                    servP4langTx = v;
                    return false;
                }
                if (s.equals("err")) {
                    servP4langErr = v;
                    return false;
                }
                return true;
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
                if (s.equals("full")) {
                    rtrBgpFull = v;
                    return false;
                }
                if (s.equals("error")) {
                    rtrBgpError = v;
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
                if (s.equals("dampening")) {
                    rtrBgpDamp = v;
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
            if (s.equals("ike")) {
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
            if (s.equals("http2")) {
                secHttp2traf = v;
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
            if (s.equals("pcep")) {
                clntPcepTraf = v;
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
            if (s.equals("forti")) {
                clntFortiTraf = v;
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
            if (s.equals("wireguard")) {
                clntWireguardTraf = v;
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
            if (s.equals("polka")) {
                clntPolkaTraf = v;
                return false;
            }
            if (s.equals("mpolka")) {
                clntMpolkaTraf = v;
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
                rtrSrhTraf = v;
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
            if (s.equals("sgt")) {
                ifcSgtTraf = v;
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
            if (s.equals("qinq1")) {
                ifcQinq1Traf = v;
                return false;
            }
            if (s.equals("qinq2")) {
                ifcQinq2Traf = v;
                return false;
            }
            if (s.equals("qinq3")) {
                ifcQinq3Traf = v;
                return false;
            }
            if (s.equals("qinqx")) {
                ifcQinqXTraf = v;
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
            if (s.equals("polka")) {
                ifcPolkaEvnt = v;
                return false;
            }
            if (s.equals("mpolka")) {
                ifcMpolkaEvnt = v;
                return false;
            }
            return true;
        }
        return true;
    }

}
