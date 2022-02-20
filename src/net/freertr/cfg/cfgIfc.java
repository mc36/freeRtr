package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrIpx;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.clnt.clntAmt;
import net.freertr.clnt.clntCapwap;
import net.freertr.clnt.clntDhcp4;
import net.freertr.clnt.clntDhcp6;
import net.freertr.clnt.clntDlsw;
import net.freertr.clnt.clntErspan;
import net.freertr.clnt.clntEtherIp;
import net.freertr.clnt.clntGeneve;
import net.freertr.clnt.clntL2tp3;
import net.freertr.clnt.clntLisp;
import net.freertr.clnt.clntLwapp;
import net.freertr.clnt.clntMplsBier;
import net.freertr.clnt.clntMplsExp;
import net.freertr.clnt.clntMplsLdpP2mp;
import net.freertr.clnt.clntMplsLdpP2p;
import net.freertr.clnt.clntMplsPwe;
import net.freertr.clnt.clntMplsSr;
import net.freertr.clnt.clntMplsTeP2mp;
import net.freertr.clnt.clntMplsTeP2p;
import net.freertr.clnt.clntMplsTrg;
import net.freertr.clnt.clntMplsUdp;
import net.freertr.clnt.clntMpolka;
import net.freertr.clnt.clntNvGre;
import net.freertr.clnt.clntOpenvpn;
import net.freertr.clnt.clntPckOudp;
import net.freertr.clnt.clntPolka;
import net.freertr.clnt.clntProxy;
import net.freertr.clnt.clntSatp;
import net.freertr.clnt.clntSlaac;
import net.freertr.clnt.clntSrEth;
import net.freertr.clnt.clntSrExt;
import net.freertr.clnt.clntUdpGre;
import net.freertr.clnt.clntUti;
import net.freertr.clnt.clntVxlan;
import net.freertr.clnt.clntWireguard;
import net.freertr.ifc.ifcArcnet;
import net.freertr.ifc.ifcAtmDxi;
import net.freertr.ifc.ifcAtmSar;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.ifc.ifcBundleIfc;
import net.freertr.ifc.ifcCdp;
import net.freertr.ifc.ifcConnect;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcDot1ad;
import net.freertr.ifc.ifcDot1ah;
import net.freertr.ifc.ifcDot1q;
import net.freertr.ifc.ifcEapOLclnt;
import net.freertr.ifc.ifcEapOLserv;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcFramePpp;
import net.freertr.ifc.ifcFrameRelay;
import net.freertr.ifc.ifcFrameRfc;
import net.freertr.ifc.ifcHdlc;
import net.freertr.ifc.ifcInfiniband;
import net.freertr.ifc.ifcIpOnly;
import net.freertr.ifc.ifcIsdn;
import net.freertr.ifc.ifcIsl;
import net.freertr.ifc.ifcLacp;
import net.freertr.ifc.ifcLapb;
import net.freertr.ifc.ifcLldp;
import net.freertr.ifc.ifcLossDet;
import net.freertr.ifc.ifcMacSec;
import net.freertr.ifc.ifcMpolka;
import net.freertr.ifc.ifcNhrp;
import net.freertr.ifc.ifcNshFwd;
import net.freertr.ifc.ifcNshXcn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcP2pOEclnt;
import net.freertr.ifc.ifcP2pOErely;
import net.freertr.ifc.ifcP2pOEserv;
import net.freertr.ifc.ifcPolka;
import net.freertr.ifc.ifcPpp;
import net.freertr.ifc.ifcPtp;
import net.freertr.ifc.ifcQinq1;
import net.freertr.ifc.ifcQinq2;
import net.freertr.ifc.ifcQinq3;
import net.freertr.ifc.ifcQinqX;
import net.freertr.ifc.ifcRandom;
import net.freertr.ifc.ifcRaw;
import net.freertr.ifc.ifcSep;
import net.freertr.ifc.ifcSgt;
import net.freertr.ifc.ifcSyncE;
import net.freertr.ifc.ifcThread;
import net.freertr.ifc.ifcUdld;
import net.freertr.ifc.ifcUp;
import net.freertr.ifc.ifcVlan;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipIcmp4;
import net.freertr.ip.ipIcmp6;
import net.freertr.ip.ipIfc4;
import net.freertr.ip.ipIfc4arp;
import net.freertr.ip.ipIfc6;
import net.freertr.ip.ipMpls;
import net.freertr.ip.ipProxy;
import net.freertr.ipx.ipxIface;
import net.freertr.pack.packEapOL;
import net.freertr.pack.packLdpPwe;
import net.freertr.pack.packPppOE;
import net.freertr.pack.packPtp;
import net.freertr.prt.prt6to4;
import net.freertr.prt.prtGre;
import net.freertr.prt.prtIcmp;
import net.freertr.prt.prtInlsp;
import net.freertr.prt.prtIpIpTyp;
import net.freertr.prt.prtIpcomp;
import net.freertr.prt.prtIpenc;
import net.freertr.prt.prtMinenc;
import net.freertr.prt.prtMplsIp;
import net.freertr.prt.prtNos;
import net.freertr.prt.prtPckOip;
import net.freertr.prt.prtPim;
import net.freertr.prt.prtPipe;
import net.freertr.prt.prtSkip;
import net.freertr.prt.prtSrv6;
import net.freertr.prt.prtSwipe;
import net.freertr.prt.prtTmux;
import net.freertr.prt.prtUdp;
import net.freertr.rtr.rtrBabelIface;
import net.freertr.rtr.rtrEigrpIface;
import net.freertr.rtr.rtrIsisIface;
import net.freertr.rtr.rtrLdpIface;
import net.freertr.rtr.rtrLsrpIface;
import net.freertr.rtr.rtrOlsrIface;
import net.freertr.rtr.rtrOspf4iface;
import net.freertr.rtr.rtrOspf6iface;
import net.freertr.rtr.rtrPvrpIface;
import net.freertr.rtr.rtrRip4iface;
import net.freertr.rtr.rtrRip6iface;
import net.freertr.rtr.rtrRsvpIface;
import net.freertr.sec.secIke;
import net.freertr.sec.secIsakmp;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabQos;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabSession;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.history;
import net.freertr.util.logger;
import net.freertr.util.state;
import net.freertr.util.verCore;

/**
 * one interface configuration
 *
 * @author matecsaba
 */
public class cfgIfc implements Comparator<cfgIfc>, cfgGeneric {

    /**
     * name of this interface
     */
    public String name = "";

    /**
     * description of this interface
     */
    public String description = "";

    /**
     * hidden interface
     */
    protected boolean hidden = false;

    /**
     * name of followed tracker
     */
    public String followTrack = null;

    /**
     * type of this interface
     */
    public ifaceType type;

    /**
     * where it was cloned, null if not
     */
    public cfgIfc cloned;

    /**
     * truly random variable
     */
    public int carrierDelay;

    /**
     * disable macsec on this interface
     */
    public boolean disableMacsec;

    /**
     * disable sgt on this interface
     */
    public boolean disableSgt;

    /**
     * packet handler
     */
    public ifcDn lower = new ifcNull();

    /**
     * parent interface
     */
    public cfgIfc parent;

    /**
     * template interface
     */
    public cfgIfc template;

    /**
     * ethertype handler
     */
    public final ifcEthTyp ethtyp;

    /**
     * vlan handler
     */
    public ifcVlan vlanHed;

    /**
     * vlan handler
     */
    public int vlanNum;

    /**
     * hdlc handler
     */
    public ifcHdlc hdlc;

    /**
     * isdn handler
     */
    public ifcIsdn isdn;

    /**
     * iponly handler
     */
    public ifcIpOnly ipOnly;

    /**
     * lapb handler
     */
    public ifcLapb lapb;

    /**
     * raw handler
     */
    public ifcRaw raw;

    /**
     * sep handler
     */
    public ifcSep sep;

    /**
     * qinqx handler
     */
    public ifcQinqX qinqx;

    /**
     * frame relay handler
     */
    public ifcFrameRelay frmrly;

    /**
     * ip over frame relay handler
     */
    public ifcFrameRfc frmrfc;

    /**
     * ppp over frame relay handler
     */
    public ifcFramePpp frmppp;

    /**
     * arcnet handler
     */
    public ifcArcnet arcnet;

    /**
     * infiniband handler
     */
    public ifcInfiniband infiniband;

    /**
     * atm sar handler
     */
    public ifcAtmSar atmsar;

    /**
     * atm dxi handler
     */
    public ifcAtmDxi atmdxi;

    /**
     * ppp handler
     */
    public ifcPpp ppp;

    /**
     * lldp handler
     */
    public ifcLldp lldp;

    /**
     * cdp handler
     */
    public ifcCdp cdp;

    /**
     * synce handler
     */
    public ifcSyncE synce;

    /**
     * ptp handler
     */
    public ifcPtp ptp;

    /**
     * lacp handler
     */
    public ifcLacp lacp;

    /**
     * udld handler
     */
    public ifcUdld udld;

    /**
     * nhrp handler
     */
    public ifcNhrp nhrp;

    /**
     * random handler
     */
    public ifcRandom random;

    /**
     * xconnect handler
     */
    public cfgXconnSide xconn;

    /**
     * pseudowire handler
     */
    public cfgXconnSide pwhe;

    /**
     * evc handlers
     */
    public tabGen<cfgIfcEvc> evcs = new tabGen<cfgIfcEvc>();

    /**
     * bridge interface
     */
    public ifcBridgeIfc bridgeIfc;

    /**
     * bridge handler
     */
    public cfgBrdg bridgeHed;

    /**
     * bundle interface
     */
    public ifcBundleIfc bundleIfc;

    /**
     * bundle handler
     */
    public cfgBndl bundleHed;

    /**
     * hairpin handler
     */
    public cfgHrpn hairpinHed;

    /**
     * packet thread
     */
    public ifcThread thread;

    /**
     * auto bandwidth
     */
    public int autoBndWdt;

    /**
     * transparent proxy
     */
    public ipProxy transProxy;

    /**
     * forwarding vrf
     */
    public cfgVrf vrfFor;

    /**
     * ipx address
     */
    public addrIpx ipxAddr;

    /**
     * ipx interface
     */
    public ipxIface ipxIfc;

    /**
     * address pool for remote
     */
    public cfgPool<addrIPv4> ip4polC;

    /**
     * address from pool
     */
    public addrIPv4 ip4polA;

    /**
     * address pool for remote
     */
    public cfgPool<addrIPv6> ip6polC;

    /**
     * address from pool
     */
    public addrIPv6 ip6polA;

    /**
     * ipv4 address
     */
    public addrIPv4 addr4;

    /**
     * ipv4 netmask
     */
    public addrIPv4 mask4;

    /**
     * hide ipv4 address
     */
    public boolean hide4adr;

    /**
     * hide ipv4 netmask
     */
    public boolean hide4msk;

    /**
     * ipc4 packet handler
     */
    public ipIfc4 ipIf4;

    /**
     * ipc4 forwarder handler
     */
    public ipFwdIface fwdIf4;

    /**
     * ipv6 address
     */
    public addrIPv6 addr6;

    /**
     * ipv6 netmask
     */
    public addrIPv6 mask6;

    /**
     * hide ipv6 address
     */
    public boolean hide6adr;

    /**
     * hide ipv6 netmask
     */
    public boolean hide6msk;

    /**
     * ipv6 packet handler
     */
    public ipIfc6 ipIf6;

    /**
     * ipv6 forwarder handler
     */
    public ipFwdIface fwdIf6;

    /**
     * tunnel mode
     */
    public tunnelType tunMode;

    /**
     * tunnel shutdown
     */
    public boolean tunShut;

    /**
     * tunnel vrf where encapsulated goes
     */
    public cfgVrf tunVrf;

    /**
     * tunnel target where encapsulated goes
     */
    public addrIP tunTrg;

    /**
     * tunnel target where encapsulated goes
     */
    public String tunFQDN;

    /**
     * tunnel source to use as source
     */
    public cfgIfc tunSrc;

    /**
     * sending tos value, -1 means maps out
     */
    public int tunTOS;

    /**
     * sending flow value, -1 means maps out
     */
    public int tunFLW;

    /**
     * sending ttl value, -1 means maps out
     */
    public int tunTTL;

    /**
     * tunnel key to use, 0 means disabled
     */
    public int tunKey = 0;

    /**
     * tunnel key to use, 0 means disabled
     */
    public int tunKey2 = 0;

    /**
     * send checksum in packets
     */
    public boolean tunSum = false;

    /**
     * send sequence number in packets
     */
    public boolean tunSeq = false;

    /**
     * association id
     */
    public int tunAscId = 0;

    /**
     * association global id
     */
    public int tunAscId2 = 0;

    /**
     * association address
     */
    public addrIP tunAscAdr = null;

    /**
     * tunnel priority
     */
    public int tunPri = 7;

    /**
     * ipsec profile to use
     */
    public cfgIpsec tunPrt;

    /**
     * gre tunnel handler
     */
    public prtGre tunGRE;

    /**
     * udpgre tunnel handler
     */
    public clntUdpGre tunUdpGre;

    /**
     * amt tunnel handler
     */
    public clntAmt tunAmt;

    /**
     * icmp tunnel handler
     */
    public prtIcmp tunICMP;

    /**
     * pim tunnel handler
     */
    public prtPim tunPIM;

    /**
     * lisp tunnel handler
     */
    public clntLisp tunLisp;

    /**
     * minenc tunnel handler
     */
    public prtMinenc tunMinenc;

    /**
     * pipe tunnel handler
     */
    public prtPipe tunPipe;

    /**
     * nos tunnel handler
     */
    public prtNos tunNos;

    /**
     * ipcomp tunnel handler
     */
    public prtIpcomp tunIpcomp;

    /**
     * ipenc tunnel handler
     */
    public prtIpenc tunIpenc;

    /**
     * tmux tunnel handler
     */
    public prtTmux tunTmux;

    /**
     * ipip tunnel handler
     */
    public prtIpIpTyp tunIPIP;

    /**
     * 6to4 tunnel handler
     */
    public prt6to4 tun6to4;

    /**
     * srv6 tunnel handler
     */
    public prtSrv6 tunSrv6;

    /**
     * ipsec tunnel handler
     */
    public secIsakmp tunIPsec1;

    /**
     * ipsec tunnel handler
     */
    public secIke tunIPsec2;

    /**
     * pckOudp tunnel handler
     */
    public clntPckOudp tunPckOudp;

    /**
     * pckOip tunnel handler
     */
    public prtPckOip tunPckOip;

    /**
     * l2tp3 tunnel handler
     */
    public clntL2tp3 tunL2tp3;

    /**
     * pweOmpls tunnel handler
     */
    public clntMplsPwe tunPweOmpls;

    /**
     * exp bundle tunnel handler
     */
    public clntMplsExp tunExpBun;

    /**
     * sr over mpls tunnel handler
     */
    public clntMplsSr tunSrMpls;

    /**
     * polka tunnel handler
     */
    public clntPolka tunPolka;

    /**
     * mpolka tunnel handler
     */
    public clntMpolka tunMpolka;

    /**
     * sr over srh tunnel handler
     */
    public clntSrExt tunSrExt;

    /**
     * p2p mpls te tunnel handler
     */
    public clntMplsTeP2p tunTeP2p;

    /**
     * p2mp mpls te tunnel handler
     */
    public clntMplsTeP2mp tunTeP2mp;

    /**
     * mpls bier tunnel handler
     */
    public clntMplsBier tunBier;

    /**
     * p2p mpls ldp tunnel handler
     */
    public clntMplsLdpP2p tunLdpP2p;

    /**
     * p2mp mpls ldp tunnel handler
     */
    public clntMplsLdpP2mp tunLdpP2mp;

    /**
     * vxlan tunnel handler
     */
    public clntVxlan tunVxlan;

    /**
     * geneve tunnel handler
     */
    public clntGeneve tunGeneve;

    /**
     * capwap tunnel handler
     */
    public clntCapwap tunCapwap;

    /**
     * lwapp tunnel handler
     */
    public clntLwapp tunLwapp;

    /**
     * erspan tunnel handler
     */
    public clntErspan tunErspan;

    /**
     * dlsw tunnel handler
     */
    public clntDlsw tunDlsw;

    /**
     * etherip tunnel handler
     */
    public clntEtherIp tunEtherip;

    /**
     * sreth tunnel handler
     */
    public clntSrEth tunSreth;

    /**
     * uti tunnel handler
     */
    public clntUti tunUti;

    /**
     * nvgre tunnel handler
     */
    public clntNvGre tunNvgre;

    /**
     * mplsip tunnel handler
     */
    public prtMplsIp tunMplsip;

    /**
     * mplsudp tunnel handler
     */
    public clntMplsUdp tunMplsudp;

    /**
     * swipe tunnel handler
     */
    public prtSwipe tunSwipe;

    /**
     * openvpn tunnel handler
     */
    public clntOpenvpn tunOpenvpn;

    /**
     * wireguard tunnel handler
     */
    public clntWireguard tunWireguard;

    /**
     * satp tunnel handler
     */
    public clntSatp tunSatp;

    /**
     * inlsp tunnel handler
     */
    public prtInlsp tunInlsp;

    /**
     * skip tunnel handler
     */
    public prtSkip tunSkip;

    /**
     * babel4 routing handler
     */
    public cfgRtr rtrBabel4hnd;

    /**
     * babel4 routing interface
     */
    public rtrBabelIface rtrBabel4ifc;

    /**
     * babel6 routing handler
     */
    public cfgRtr rtrBabel6hnd;

    /**
     * babel6 routing interface
     */
    public rtrBabelIface rtrBabel6ifc;

    /**
     * olsr4 routing handler
     */
    public cfgRtr rtrOlsr4hnd;

    /**
     * olsr4 routing interface
     */
    public rtrOlsrIface rtrOlsr4ifc;

    /**
     * olsr6 routing handler
     */
    public cfgRtr rtrOlsr6hnd;

    /**
     * olsr6 routing interface
     */
    public rtrOlsrIface rtrOlsr6ifc;

    /**
     * rip4 routing handler
     */
    public cfgRtr rtrRip4hnd;

    /**
     * rip4 routing interface
     */
    public rtrRip4iface rtrRip4ifc;

    /**
     * rip6 routing handler
     */
    public cfgRtr rtrRip6hnd;

    /**
     * rip6 routing interface
     */
    public rtrRip6iface rtrRip6ifc;

    /**
     * ospf4 routing handler
     */
    public cfgRtr rtrOspf4hnd;

    /**
     * ospf4 routing interface
     */
    public rtrOspf4iface rtrOspf4ifc;

    /**
     * ospf6 routing handler
     */
    public cfgRtr rtrOspf6hnd;

    /**
     * ospf6 routing interface
     */
    public rtrOspf6iface rtrOspf6ifc;

    /**
     * isis routing handler
     */
    public cfgRtr rtrIsisHnd;

    /**
     * isis routing interface
     */
    public rtrIsisIface rtrIsisIfc;

    /**
     * pvrp4 routing interface
     */
    public rtrPvrpIface rtrPvrp4ifc;

    /**
     * pvrp4 routing handler
     */
    public cfgRtr rtrPvrp4hnd;

    /**
     * pvrp4 routing interface
     */
    public rtrPvrpIface rtrPvrp6ifc;

    /**
     * pvrp4 routing handler
     */
    public cfgRtr rtrPvrp6hnd;

    /**
     * lsrp4 routing interface
     */
    public rtrLsrpIface rtrLsrp4ifc;

    /**
     * lsrp4 routing handler
     */
    public cfgRtr rtrLsrp4hnd;

    /**
     * lsrp4 routing interface
     */
    public rtrLsrpIface rtrLsrp6ifc;

    /**
     * lsrp4 routing handler
     */
    public cfgRtr rtrLsrp6hnd;

    /**
     * eigrp4 routing interface
     */
    public rtrEigrpIface rtrEigrp4ifc;

    /**
     * eigrp4 routing handler
     */
    public cfgRtr rtrEigrp4hnd;

    /**
     * eigrp4 routing interface
     */
    public rtrEigrpIface rtrEigrp6ifc;

    /**
     * eigrp4 routing handler
     */
    public cfgRtr rtrEigrp6hnd;

    /**
     * pppoe client handler
     */
    public ifcP2pOEclnt pppoeC;

    /**
     * pppoe server handler
     */
    public ifcP2pOEserv pppoeS;

    /**
     * pppoe relay handler
     */
    public ifcP2pOErely pppoeR;

    /**
     * eapol client handler
     */
    public ifcEapOLclnt eapolC;

    /**
     * eapol server handler
     */
    public ifcEapOLserv eapolS;

    /**
     * dhcp4 client handler
     */
    public clntDhcp4 dhcp4c;

    /**
     * dhcp6 client handler
     */
    public clntDhcp6 dhcp6c;

    /**
     * slaac client handler
     */
    public clntSlaac slaac;

    /**
     * nsh packet processing
     */
    public ifcNshFwd nshPack;

    /**
     * nsh xconnect processing
     */
    public ifcNshXcn nshXcon;

    /**
     * mpls packet processing
     */
    public ipMpls mplsPack;

    /**
     * polka packet processing
     */
    public ifcPolka polkaPack;

    /**
     * mpolka packet processing
     */
    public ifcMpolka mpolkaPack;

    /**
     * mpls ldp ipv4 discovery
     */
    public rtrLdpIface mplsLdp4;

    /**
     * mpls ldp ipv6 discovery
     */
    public rtrLdpIface mplsLdp6;

    /**
     * mpls rsvp ipv4 signaling
     */
    public rtrRsvpIface mplsRsvp4;

    /**
     * mpls rsvp ipv6 signaling
     */
    public rtrRsvpIface mplsRsvp6;

    /**
     * mpls targeted ldp
     */
    public tabGen<clntMplsTrg> mplsTarget = new tabGen<clntMplsTrg>();

    /**
     * interface type
     */
    public enum ifaceType {

        /**
         * null interface
         */
        nul,
        /**
         * loopback interface
         */
        loopback,
        /**
         * template interface
         */
        template,
        /**
         * dialer interface
         */
        dialer,
        /**
         * sdn interface
         */
        sdn,
        /**
         * pw headend interface
         */
        pweth,
        /**
         * virtual ppp interface
         */
        virtppp,
        /**
         * ethernet interface
         */
        ether,
        /**
         * serial interface
         */
        serial,
        /**
         * arcnet interface
         */
        arcnet,
        /**
         * infiniband interface
         */
        infiniband,
        /**
         * atm interface
         */
        atm,
        /**
         * bridged head interface
         */
        bridge,
        /**
         * bundle head interface
         */
        bundle,
        /**
         * hairpin interface
         */
        hairpin,
        /**
         * tunnel interface
         */
        tunnel

    }

    /**
     * tunnel type
     */
    public enum tunnelType {

        /**
         * gre tunnel interface
         */
        gre,
        /**
         * udpgre tunnel interface
         */
        udpgre,
        /**
         * amt tunnel interface
         */
        amt,
        /**
         * icmp tunnel interface
         */
        icmp,
        /**
         * pim tunnel interface
         */
        pim,
        /**
         * lisp tunnel interface
         */
        lisp,
        /**
         * minenc tunnel interface
         */
        minenc,
        /**
         * pipe tunnel interface
         */
        pipe,
        /**
         * nos tunnel interface
         */
        nos,
        /**
         * ipcomp tunnel interface
         */
        ipcomp,
        /**
         * ipenc tunnel interface
         */
        ipenc,
        /**
         * tmux tunnel interface
         */
        tmux,
        /**
         * ipip tunnel interface
         */
        ipip,
        /**
         * 6to4 tunnel interface
         */
        Sto4,
        /**
         * srv6 tunnel interface
         */
        srv6,
        /**
         * ipsec tunnel interface
         */
        ipsec,
        /**
         * pckOudp tunnel interface
         */
        pckOudp,
        /**
         * pckOip tunnel interface
         */
        pckOip,
        /**
         * l2tp3 tunnel interface
         */
        l2tp3,
        /**
         * pweOmpls tunnel interface
         */
        pweOmpls,
        /**
         * polka tunnel interface
         */
        polka,
        /**
         * mpolka tunnel interface
         */
        mpolka,
        /**
         * exp bundle tunnel interface
         */
        expBun,
        /**
         * sr over srmpls tunnel interface
         */
        srMpls,
        /**
         * sr over srh tunnel interface
         */
        srExt,
        /**
         * sr from pcep tunnel interface
         */
        pceSr,
        /**
         * te from pcep tunnel interface
         */
        pceTe,
        /**
         * p2p mpls te tunnel interface
         */
        teP2p,
        /**
         * p2mp mpls te tunnel interface
         */
        teP2mp,
        /**
         * mpls bier tunnel interface
         */
        bier,
        /**
         * p2p ldp tunnel interface
         */
        ldpP2p,
        /**
         * p2mp ldp tunnel interface
         */
        ldpP2mp,
        /**
         * mp2mp ldp tunnel interface
         */
        ldpMp2mp,
        /**
         * vxlan tunnel interface
         */
        vxlan,
        /**
         * geneve tunnel interface
         */
        geneve,
        /**
         * capwap tunnel interface
         */
        capwap,
        /**
         * lwapp tunnel interface
         */
        lwapp,
        /**
         * erspan tunnel interface
         */
        erspan,
        /**
         * dlsw tunnel interface
         */
        dlsw,
        /**
         * etherip tunnel interface
         */
        etherip,
        /**
         * sreth tunnel interface
         */
        sreth,
        /**
         * uti tunnel interface
         */
        uti,
        /**
         * nvgre tunnel interface
         */
        nvgre,
        /**
         * mplsip tunnel interface
         */
        mplsip,
        /**
         * mplsudp tunnel interface
         */
        mplsudp,
        /**
         * swipe tunnel interface
         */
        swipe,
        /**
         * openvpn tunnel interface
         */
        openvpn,
        /**
         * wireguard tunnel interface
         */
        wireguard,
        /**
         * satp tunnel interface
         */
        satp,
        /**
         * inlsp tunnel interface
         */
        inlsp,
        /**
         * skip tunnel interface
         */
        skip

    }

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        // globals
        "interface .*! no mtu",
        "interface .*! no macaddr",
        "interface .*! no template",
        "interface .*! no follow-tracker",
        "interface .*! padup 0 0",
        "interface .*! autostate",
        "interface .*! encapsulation dot1q",
        "interface .*! no bandwidth",
        "interface .*! no lldp enable",
        "interface .*! no cdp enable",
        "interface .*! no cdp odr4",
        "interface .*! no cdp odr6",
        "interface .*! no synceth enable",
        "interface .*! no ptp enable",
        "interface .*! no ptp receive",
        "interface .*! no lacp",
        "interface .*! no carrier-delay",
        "interface .*! no udld enable",
        "interface .*! no random",
        "interface .*! enforce-mtu none",
        "interface .*! no macsec",
        "interface .*! no disable-macsec",
        "interface .*! no disable-sgt",
        "interface .*! no loss-detection",
        "interface .*! no sgt enable",
        "interface .*! no sgt assign",
        "interface .*! monitor-direction both",
        "interface .*! monitor-truncate 0",
        "interface .*! monitor-sample 0",
        "interface .*! no monitor-filter",
        "interface .*! no monitor-session",
        "interface .*! no monitor-buffer",
        "interface .*! no eapol client",
        "interface .*! no eapol server",
        "interface .*! no bridge-group",
        "interface .*! no bridge-filter ipv4in",
        "interface .*! no bridge-filter ipv4out",
        "interface .*! no bridge-filter ipv6in",
        "interface .*! no bridge-filter ipv6out",
        "interface .*! no bridge-macrewrite",
        "interface .*! no bridge-portsecurity",
        "interface .*! no bridge-staticaddr",
        "interface .*! no bundle-group",
        "interface .*! bundle-priority 0",
        "interface .*! no service-policy-in",
        "interface .*! no service-policy-out",
        // forwarding
        "interface .*! no transproxy",
        "interface .*! no p2poe client",
        "interface .*! no p2poe server",
        "interface .*! no p2poe relay",
        "interface .*! no vrf forwarding",
        "interface .*! no nhrp ipv4",
        "interface .*! no nhrp ipv6",
        "interface .*! no ipx network",
        "interface .*! no nsh enable",
        "interface .*! no nsh xconnect",
        "interface .*! no polka enable",
        "interface .*! no mpolka enable",
        // mpls
        "interface .*! no mpls enable",
        "interface .*! no mpls access-group-in",
        "interface .*! no mpls access-group-out",
        "interface .*! no mpls access-group-common-in",
        "interface .*! no mpls access-group-common-out",
        "interface .*! no mpls inspect",
        "interface .*! no mpls label-security",
        "interface .*! no mpls netflow-rx",
        "interface .*! no mpls netflow-tx",
        "interface .*! no mpls redirection",
        "interface .*! no mpls ldp4",
        "interface .*! no mpls ldp6",
        "interface .*! no mpls label4pop",
        "interface .*! no mpls label6pop",
        "interface .*! no mpls label4in",
        "interface .*! no mpls label4out",
        "interface .*! no mpls label6in",
        "interface .*! no mpls label6out",
        "interface .*! no mpls rsvp4",
        "interface .*! no mpls rsvp6",
        // ip
        "interface .*! no ipv[4|6] address",
        "interface .*! no ipv[4|6] enable",
        "interface .*! no ipv[4|6] reassembly",
        "interface .*! no ipv[4|6] fragmentation",
        "interface .*! no ipv[4|6] pmtud-reply",
        "interface .*! no ipv[4|6] redirection",
        "interface .*! ipv[4|6] unreachables",
        "interface .*! no ipv[4|6] unreach-source",
        "interface .*! no ipv[4|6] propagate-ttl-always",
        "interface .*! ipv[4|6] propagate-ttl-allow",
        "interface .*! no ipv[4|6] resend-packet",
        "interface .*! no ipv[4|6] dapp-disable",
        "interface .*! no ipv[4|6] flowspec-disable",
        "interface .*! no ipv[4|6] directed-broadcast",
        "interface .*! no ipv[4|6] broadcast-multicast",
        "interface .*! no ipv[4|6] verify-source",
        "interface .*! no ipv[4|6] srh enable",
        "interface .*! ipv[4|6] gateway-local",
        "interface .*! ipv[4|6] gateway-remote",
        "interface .*! no ipv[4|6] gateway-prefix",
        "interface .*! no ipv[4|6] gateway-routemap",
        "interface .*! no ipv[4|6] access-group-in",
        "interface .*! no ipv[4|6] access-group-out",
        "interface .*! no ipv[4|6] access-group-common-in",
        "interface .*! no ipv[4|6] access-group-common-out",
        "interface .*! no ipv[4|6] bfd",
        "interface .*! no ipv[4|6] autoroute",
        "interface .*! no ipv[4|6] host-watch",
        "interface .*! ipv[4|6] host-reach 360000",
        "interface .*! ipv[4|6] host-retry 180000",
        "interface .*! no ipv[4|6] proxy-remote",
        "interface .*! no ipv[4|6] proxy-local",
        "interface .*! no ipv[4|6] proxy-filter",
        "interface .*! no ipv[4|6] tcp-mss-in",
        "interface .*! no ipv[4|6] tcp-mss-out",
        "interface .*! no ipv[4|6] hsrp address",
        "interface .*! no ipv[4|6] netflow-rx",
        "interface .*! no ipv[4|6] netflow-tx",
        "interface .*! ipv[4|6] hsrp group 0",
        "interface .*! ipv[4|6] hsrp password \\$v10\\$Y2lzY28=",
        "interface .*! ipv4 hsrp mac-address 0000.0c9f.f000",
        "interface .*! ipv6 hsrp mac-address 0005.73a0.0000",
        "interface .*! ipv[4|6] hsrp version 2",
        "interface .*! ipv[4|6] hsrp timer 3000 10000",
        "interface .*! ipv[4|6] hsrp priority 100",
        "interface .*! no ipv[4|6] hsrp preempt",
        "interface .*! no ipv[4|6] hsrp bfd",
        "interface .*! no ipv[4|6] hsrp tracker",
        "interface .*! no ipv[4|6] vrrp address",
        "interface .*! ipv[4|6] vrrp group 0",
        "interface .*! ipv4 vrrp mac-address 0000.5e00.0100",
        "interface .*! ipv6 vrrp mac-address 0000.5e00.0200",
        "interface .*! ipv[4|6] vrrp version 3",
        "interface .*! ipv[4|6] vrrp timer 1000 3000",
        "interface .*! ipv[4|6] vrrp priority 100",
        "interface .*! no ipv[4|6] vrrp tracker",
        "interface .*! no ipv[4|6] vrrp bfd",
        "interface .*! no ipv[4|6] pool",
        "interface .*! no ipv[4|6] inspect",
        "interface .*! no ipv[4|6] ptp enable",
        "interface .*! no ipv[4|6] ptp receive",
        "interface .*! ipv4 dhcp-client broadcast",
        "interface .*! no ipv6 dhcp-client prefix",
        "interface .*! no ipv[4|6] dhcp-client early",
        "interface .*! ipv[4|6] dhcp-client renew-min 60",
        "interface .*! ipv[4|6] dhcp-client renew-max 7200",
        "interface .*! no ipv[4|6] dhcp-client enable",
        "interface .*! no ipv6 prefix-suppress",
        "interface .*! no ipv6 slaac",
        "interface .*! no ipv6 prefix-dns",
        "interface .*! no ipv6 prefix-domain",
        "interface .*! ipv6 prefix-interval 120000",
        "interface .*! ipv6 prefix-validity 604800000",
        // multicast
        "interface .*! no ipv[4|6] multicast source-override-in",
        "interface .*! no ipv[4|6] multicast source-override-out",
        "interface .*! ipv[4|6] multicast ttl-threshold 0",
        "interface .*! no ipv[4|6] pim enable",
        "interface .*! no ipv[4|6] pim bfd",
        "interface .*! no ipv[4|6] pim join-source",
        "interface .*! ipv[4|6] pim allow-rx",
        "interface .*! ipv[4|6] pim allow-tx",
        "interface .*! ipv[4|6] pim bier-tunnel 0",
        "interface .*! ipv[4|6] pim packet-timer 20",
        "interface .*! ipv[4|6] pim priority 1",
        "interface .*! ipv[4|6] pim hello-time 30000",
        "interface .*! no ipv[4|6] multicast mldp-enable",
        "interface .*! no ipv[4|6] multicast host-enable",
        "interface .*! no ipv[4|6] multicast host-proxy",
        "interface .*! ipv[4|6] multicast host-query 60000",
        // babel
        "interface .*! no router babel[4|6] .* bfd",
        "interface .*! router babel[4|6] .* split-horizon",
        "interface .*! no router babel[4|6] .* default-originate",
        "interface .*! no router babel[4|6] .* suppress-prefix",
        "interface .*! no router babel[4|6] .* unsuppress-prefix",
        "interface .*! router babel[4|6] .* verify-source",
        "interface .*! router babel[4|6] .* distance 130",
        "interface .*! router babel[4|6] .* metric-in 100",
        "interface .*! router babel[4|6] .* metric-out 0",
        "interface .*! router babel[4|6] .* packet-timer 20",
        "interface .*! router babel[4|6] .* update-timer 20000",
        "interface .*! no router babel[4|6] .* prefix-list-in",
        "interface .*! no router babel[4|6] .* prefix-list-out",
        "interface .*! no router babel[4|6] .* route-map-in",
        "interface .*! no router babel[4|6] .* route-map-out",
        "interface .*! no router babel[4|6] .* route-policy-in",
        "interface .*! no router babel[4|6] .* route-policy-out",
        // olsr
        "interface .*! no router olsr[4|6] .* bfd",
        "interface .*! router olsr[4|6] .* split-horizon",
        "interface .*! router olsr[4|6] .* lq-mode",
        "interface .*! no router olsr[4|6] .* default-originate",
        "interface .*! no router olsr[4|6] .* suppress-prefix",
        "interface .*! no router olsr[4|6] .* unsuppress-prefix",
        "interface .*! router olsr[4|6] .* verify-source",
        "interface .*! router olsr[4|6] .* distance 140",
        "interface .*! router olsr[4|6] .* willingness 7",
        "interface .*! router olsr[4|6] .* metric-in 1",
        "interface .*! router olsr[4|6] .* metric-out 0",
        "interface .*! router olsr[4|6] .* packet-timer 20",
        "interface .*! router olsr[4|6] .* hello-timer 5000",
        "interface .*! router olsr[4|6] .* hello-hold 15000",
        "interface .*! router olsr[4|6] .* advertise-timer 30000",
        "interface .*! router olsr[4|6] .* advertise-hold 90000",
        "interface .*! no router olsr[4|6] .* prefix-list-in",
        "interface .*! no router olsr[4|6] .* prefix-list-out",
        "interface .*! no router olsr[4|6] .* route-map-in",
        "interface .*! no router olsr[4|6] .* route-map-out",
        "interface .*! no router olsr[4|6] .* route-policy-in",
        "interface .*! no router olsr[4|6] .* route-policy-out",
        // rip
        "interface .*! router rip[4|6] .* allow-rx",
        "interface .*! router rip[4|6] .* allow-tx",
        "interface .*! no router rip[4|6] .* bfd",
        "interface .*! router rip[4|6] .* verify-source",
        "interface .*! router rip[4|6] .* poison-reverse",
        "interface .*! router rip[4|6] .* split-horizon",
        "interface .*! no router rip[4|6] .* default-originate",
        "interface .*! no router rip[4|6] .* suppress-prefix",
        "interface .*! no router rip[4|6] .* unsuppress-prefix",
        "interface .*! router rip[4|6] .* distance 120",
        "interface .*! router rip[4|6] .* metric-in 0",
        "interface .*! router rip[4|6] .* metric-out 1",
        "interface .*! router rip[4|6] .* packet-timer 20",
        "interface .*! router rip[4|6] .* update-timer 30000",
        "interface .*! router rip[4|6] .* hold-time 180000",
        "interface .*! router rip[4|6] .* flush-time 240000",
        "interface .*! no router rip[4|6] .* password",
        "interface .*! no router rip[4|6] .* prefix-list-in",
        "interface .*! no router rip[4|6] .* prefix-list-out",
        "interface .*! no router rip[4|6] .* route-map-in",
        "interface .*! no router rip[4|6] .* route-map-out",
        "interface .*! no router rip[4|6] .* route-policy-in",
        "interface .*! no router rip[4|6] .* route-policy-out",
        // ospf
        "interface .*! no router ospf[4|6] .* passive",
        "interface .*! router ospf[4|6] .* network point2point",
        "interface .*! no router ospf[4|6] .* bfd",
        "interface .*! no router ospf[4|6] .* suppress-prefix",
        "interface .*! no router ospf[4|6] .* unsuppress-prefix",
        "interface .*! router ospf[4|6] .* verify-source",
        "interface .*! no router ospf[4|6] .* password",
        "interface .*! router ospf[4|6] .* authen-type clear",
        "interface .*! router ospf[4|6] .* authen-id 0",
        "interface .*! router ospf[4|6] .* instance 0",
        "interface .*! router ospf[4|6] .* cost 10",
        "interface .*! router ospf[4|6] .* priority 0",
        "interface .*! router ospf[4|6] .* hello-time 10000",
        "interface .*! router ospf[4|6] .* dead-time 40000",
        "interface .*! router ospf[4|6] .* retransmit-time 3000",
        "interface .*! no router ospf[4|6] .* traffeng suppress",
        "interface .*! router ospf[4|6] .* traffeng metric 10",
        "interface .*! router ospf[4|6] .* traffeng bandwidth 100000000",
        "interface .*! router ospf[4|6] .* traffeng affinity 0",
        "interface .*! router ospf[4|6] .* traffeng srlg 0",
        "interface .*! no router ospf[4|6] .* segrout index",
        "interface .*! no router ospf[4|6] .* segrout node",
        "interface .*! no router ospf[4|6] .* segrout pop",
        "interface .*! no router ospf[4|6] .* bier index",
        "interface .*! no router ospf[4|6] .* dynamic-metric",
        "interface .*! router ospf[4|6] .* dynamic-time 60000",
        "interface .*! router ospf[4|6] .* dynamic-size 5",
        "interface .*! router ospf[4|6] .* dynamic-minimum 1",
        "interface .*! router ospf[4|6] .* dynamic-maximum 65530",
        "interface .*! router ospf[4|6] .* dynamic-divisor 1",
        "interface .*! router ospf[4|6] .* dynamic-multiply 1",
        "interface .*! router ospf[4|6] .* dynamic-ignore 0",
        "interface .*! router ospf[4|6] .* dynamic-skip-min 0",
        "interface .*! router ospf[4|6] .* dynamic-skip-max 0",
        "interface .*! router ospf[4|6] .* dynamic-algo minimum",
        // pvrp
        "interface .*! router pvrp[4|6] .* split-horizon",
        "interface .*! no router pvrp[4|6] .* passive",
        "interface .*! no router pvrp[4|6] .* dump",
        "interface .*! no router pvrp[4|6] .* accept-metric",
        "interface .*! no router pvrp[4|6] .* dynamic-metric",
        "interface .*! no router pvrp[4|6] .* bfd",
        "interface .*! no router pvrp[4|6] .* default-originate",
        "interface .*! no router pvrp[4|6] .* label-pop",
        "interface .*! no router pvrp[4|6] .* stub",
        "interface .*! no router pvrp[4|6] .* unstub",
        "interface .*! no router pvrp[4|6] .* suppress-prefix",
        "interface .*! no router pvrp[4|6] .* unsuppress-prefix",
        "interface .*! router pvrp[4|6] .* verify-source",
        "interface .*! no router pvrp[4|6] .* password",
        "interface .*! no router pvrp[4|6] .* disable-password",
        "interface .*! no router pvrp[4|6] .* encryption",
        "interface .*! router pvrp[4|6] .* distance 80",
        "interface .*! router pvrp[4|6] .* metric-in 10",
        "interface .*! router pvrp[4|6] .* metric-out 0",
        "interface .*! router pvrp[4|6] .* hello-time 5000",
        "interface .*! router pvrp[4|6] .* dead-time 15000",
        "interface .*! router pvrp[4|6] .* dynamic-time 60000",
        "interface .*! router pvrp[4|6] .* dynamic-size 5",
        "interface .*! router pvrp[4|6] .* dynamic-minimum 1",
        "interface .*! router pvrp[4|6] .* dynamic-maximum 100000",
        "interface .*! router pvrp[4|6] .* dynamic-divisor 1",
        "interface .*! router pvrp[4|6] .* dynamic-multiply 1",
        "interface .*! router pvrp[4|6] .* dynamic-ignore 0",
        "interface .*! router pvrp[4|6] .* dynamic-skip-min 0",
        "interface .*! router pvrp[4|6] .* dynamic-skip-max 0",
        "interface .*! router pvrp[4|6] .* dynamic-algo minimum",
        "interface .*! no router pvrp[4|6] .* label-in",
        "interface .*! no router pvrp[4|6] .* label-out",
        "interface .*! no router pvrp[4|6] .* prefix-list-in",
        "interface .*! no router pvrp[4|6] .* prefix-list-out",
        "interface .*! no router pvrp[4|6] .* route-map-in",
        "interface .*! no router pvrp[4|6] .* route-map-out",
        "interface .*! no router pvrp[4|6] .* route-policy-in",
        "interface .*! no router pvrp[4|6] .* route-policy-out",
        // lsrp
        "interface .*! no router lsrp[4|6] .* passive",
        "interface .*! no router lsrp[4|6] .* dump",
        "interface .*! no router lsrp[4|6] .* accept-metric",
        "interface .*! no router lsrp[4|6] .* dynamic-metric",
        "interface .*! no router lsrp[4|6] .* bfd",
        "interface .*! no router lsrp[4|6] .* segrout",
        "interface .*! no router lsrp[4|6] .* bier",
        "interface .*! no router lsrp[4|6] .* stub",
        "interface .*! no router lsrp[4|6] .* unstub",
        "interface .*! no router lsrp[4|6] .* suppress-prefix",
        "interface .*! no router lsrp[4|6] .* unsuppress-prefix",
        "interface .*! router lsrp[4|6] .* verify-source",
        "interface .*! no router lsrp[4|6] .* password",
        "interface .*! no router lsrp[4|6] .* disable-password",
        "interface .*! no router lsrp[4|6] .* encryption",
        "interface .*! no router lsrp[4|6] .* database-filter",
        "interface .*! router lsrp[4|6] .* split-horizon",
        "interface .*! router lsrp[4|6] .* metric 10",
        "interface .*! router lsrp[4|6] .* affinity 0",
        "interface .*! router lsrp[4|6] .* srlg 0",
        "interface .*! router lsrp[4|6] .* hello-time 5000",
        "interface .*! router lsrp[4|6] .* dead-time 15000",
        "interface .*! router lsrp[4|6] .* dynamic-time 60000",
        "interface .*! router lsrp[4|6] .* dynamic-size 5",
        "interface .*! router lsrp[4|6] .* dynamic-minimum 1",
        "interface .*! router lsrp[4|6] .* dynamic-maximum 100000",
        "interface .*! router lsrp[4|6] .* dynamic-divisor 1",
        "interface .*! router lsrp[4|6] .* dynamic-multiply 1",
        "interface .*! router lsrp[4|6] .* dynamic-ignore 0",
        "interface .*! router lsrp[4|6] .* dynamic-skip-min 0",
        "interface .*! router lsrp[4|6] .* dynamic-skip-max 0",
        "interface .*! router lsrp[4|6] .* dynamic-algo minimum",
        // eigrp
        "interface .*! router eigrp[4|6] .* split-horizon",
        "interface .*! no router eigrp[4|6] .* passive",
        "interface .*! no router eigrp[4|6] .* bfd",
        "interface .*! no router eigrp[4|6] .* default-originate",
        "interface .*! no router eigrp[4|6] .* suppress-prefix",
        "interface .*! no router eigrp[4|6] .* unsuppress-prefix",
        "interface .*! router eigrp[4|6] .* verify-source",
        "interface .*! router eigrp[4|6] .* delay-in 10",
        "interface .*! router eigrp[4|6] .* delay-out 0",
        "interface .*! router eigrp[4|6] .* distance 90",
        "interface .*! router eigrp[4|6] .* hello-time 5000",
        "interface .*! router eigrp[4|6] .* dead-time 15000",
        "interface .*! no router eigrp[4|6] .* prefix-list-in",
        "interface .*! no router eigrp[4|6] .* prefix-list-out",
        "interface .*! no router eigrp[4|6] .* route-map-in",
        "interface .*! no router eigrp[4|6] .* route-map-out",
        "interface .*! no router eigrp[4|6] .* route-policy-in",
        "interface .*! no router eigrp[4|6] .* route-policy-out",
        // isis
        "interface .*! no router isis[4|6] .* passive",
        "interface .*! no router isis[4|6] .* other-enable",
        "interface .*! no router isis[4|6] .* raw-encapsulation",
        "interface .*! router isis[4|6] .* network point2point",
        "interface .*! no router isis[4|6] .* bfd",
        "interface .*! router isis[4|6] .* suppress-address",
        "interface .*! no router isis[4|6] .* suppress-prefix",
        "interface .*! no router isis[4|6] .* unsuppress-prefix",
        "interface .*! router isis[4|6] .* other-suppress-address",
        "interface .*! no router isis[4|6] .* other-suppress-prefix",
        "interface .*! no router isis[4|6] .* other-unsuppress-prefix",
        "interface .*! router isis[4|6] .* verify-source",
        "interface .*! no router isis[4|6] .* send-csnp",
        "interface .*! no router isis[4|6] .* password",
        "interface .*! router isis[4|6] .* authen-type clear",
        "interface .*! router isis[4|6] .* metric 10",
        "interface .*! router isis[4|6] .* priority 64",
        "interface .*! router isis[4|6] .* hello-time 10000",
        "interface .*! router isis[4|6] .* dead-time 30000",
        "interface .*! router isis[4|6] .* retransmit-time 3000",
        "interface .*! no router isis[4|6] .* traffeng suppress",
        "interface .*! router isis[4|6] .* traffeng metric 10",
        "interface .*! router isis[4|6] .* traffeng bandwidth 100000000",
        "interface .*! router isis[4|6] .* traffeng affinity 0",
        "interface .*! router isis[4|6] .* traffeng srlg 0",
        "interface .*! no router isis[4|6] .* segrout index",
        "interface .*! no router isis[4|6] .* segrout other-index",
        "interface .*! no router isis[4|6] .* segrout node",
        "interface .*! no router isis[4|6] .* segrout pop",
        "interface .*! no router isis[4|6] .* bier index",
        "interface .*! no router isis[4|6] .* bier other-index",
        "interface .*! no router isis[4|6] .* dynamic-metric",
        "interface .*! router isis[4|6] .* dynamic-time 60000",
        "interface .*! router isis[4|6] .* dynamic-size 5",
        "interface .*! router isis[4|6] .* dynamic-minimum 1",
        "interface .*! router isis[4|6] .* dynamic-maximum 16777210",
        "interface .*! router isis[4|6] .* dynamic-divisor 1",
        "interface .*! router isis[4|6] .* dynamic-multiply 1",
        "interface .*! router isis[4|6] .* dynamic-ignore 0",
        "interface .*! router isis[4|6] .* dynamic-skip-min 0",
        "interface .*! router isis[4|6] .* dynamic-skip-max 0",
        "interface .*! router isis[4|6] .* dynamic-algo minimum",
        // vlan
        "interface .*! no vlan subif-macs",
        // qinqx
        "interface .*! qinqx ethertype fa52",
        // sep
        "interface .*! sep mode peer",
        "interface .*! sep keepalive 5000",
        // ppp
        "interface .*! ppp multilink 0 none",
        "interface .*! ppp fragment 0",
        "interface .*! ppp frgap 0",
        "interface .*! ppp keepalive 5000",
        "interface .*! ppp naktry 16",
        "interface .*! no ppp username",
        "interface .*! no ppp password",
        "interface .*! no ppp refuseauth pap",
        "interface .*! no ppp refuseauth chap",
        "interface .*! no ppp refuseauth eap",
        "interface .*! ppp accm 0",
        "interface .*! ppp mru 0",
        "interface .*! no ppp ip4cp close",
        "interface .*! no ppp ip4cp open",
        "interface .*! no ppp ip4cp peer",
        "interface .*! no ppp ip4cp local",
        "interface .*! no ppp ip4cp dns1",
        "interface .*! no ppp ip4cp dns2",
        "interface .*! no ppp ip4cp reqaddr",
        "interface .*! no ppp ip6cp close",
        "interface .*! no ppp ip6cp open",
        "interface .*! no ppp ip6cp local",
        "interface .*! no ppp ip6cp peer",
        "interface .*! no ppp ip6cp keep",
        "interface .*! no ppp bcp close",
        "interface .*! no ppp bcp open",
        "interface .*! no ppp mplscp close",
        "interface .*! no ppp mplscp open",
        "interface .*! no ppp osicp close",
        "interface .*! no ppp osicp open",
        "interface .*! no ppp ipxcp close",
        "interface .*! no ppp ipxcp open",
        "interface .*! no ppp ecp close",
        "interface .*! no ppp ecp open",
        "interface .*! no ppp nshcp close",
        "interface .*! no ppp nshcp open",
        "interface .*! no ppp polkacp close",
        "interface .*! no ppp polkacp open",
        "interface .*! no ppp mpolkacp close",
        "interface .*! no ppp mpolkacp open",
        "interface .*! no ppp authentication",
        // hdlc
        "interface .*! hdlc keepalive 5000",
        // isdn
        "interface .*! isdn keepalive 5000",
        // labp
        "interface .*! lapb keepalive 5000",
        "interface .*! lapb modulus 8",
        // framerelay
        "interface .*! framerelay keepalive 5000",
        "interface .*! framerelay lmi ansi",
        "interface .*! framerelay fragment 0",
        "interface .*! framerelay frgap 0",
        // tunnel
        "interface .*! no tunnel sequence-datagrams",
        "interface .*! no tunnel checksum",
        "interface .*! no tunnel shutdown",
        "interface .*! tunnel key 0",
        "interface .*! tunnel flow -1",
        "interface .*! tunnel tos -1",
        "interface .*! tunnel ttl 255",
        "interface .*! tunnel priority 7",
        "interface .*! no tunnel association",
        "interface .*! no tunnel protection",
        "interface .*! no tunnel domain-name"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * template text
     */
    public final static String[] notemplL = {
        "interface .*! description.*",
        "interface .*! no description.*",
        "interface .*! vrf forwarding.*",
        "interface .*! no vrf forwarding.*",
        "interface .*! ipv[4|6] address.*",
        "interface .*! no ipv[4|6] address.*",
        "interface .*! ipx network.*",
        "interface .*! no ipx network.*",
        "interface .*! autostate.*",
        "interface .*! no autostate.*",
        "interface .*! shutdown.*",
        "interface .*! no shutdown.*"
    };

    /**
     * template filter
     */
    public static tabGen<userFilter> notemplF;

    /**
     * clone text
     */
    public final static String[] nocloneL = {
        "interface .*! autostate.*",
        "interface .*! no autostate.*",
        "interface .*! shutdown.*",
        "interface .*! no shutdown.*"
    };

    /**
     * clone filter
     */
    public static tabGen<userFilter> nocloneF;

    /**
     * convert interface name to type
     *
     * @param s name of interface
     * @return type
     */
    public static ifaceType string2type(String s) {
        if (s == null) {
            return null;
        }
        if (s.length() < 3) {
            return null;
        }
        s = s.substring(0, 3);
        cfgIfc.ifaceType typ = null;
        if (s.equals("eth")) {
            typ = cfgIfc.ifaceType.ether;
        }
        if (s.equals("ser")) {
            typ = cfgIfc.ifaceType.serial;
        }
        if (s.equals("atm")) {
            typ = cfgIfc.ifaceType.atm;
        }
        if (s.equals("arc")) {
            typ = cfgIfc.ifaceType.arcnet;
        }
        if (s.equals("inf")) {
            typ = cfgIfc.ifaceType.infiniband;
        }
        return typ;
    }

    /**
     * flap this interface
     *
     * @param tim length of flap
     */
    public synchronized void flapNow(int tim) {
        if (ethtyp.forcedDN != 0) {
            return;
        }
        ethtyp.forcedDN |= 2;
        ethtyp.propagateState();
        propagateEthtypState();
        bits.sleep(tim);
        ethtyp.forcedDN &= ~2;
        ethtyp.propagateState();
        propagateEthtypState();
    }

    /**
     * clone this interface
     *
     * @param lower lower layer
     * @return cloned interface
     */
    public synchronized cfgIfc cloneStart(ifcDn lower) {
        addrIPv4 peer4 = null;
        addrIPv6 peer6 = null;
        if (ip4polC != null) {
            peer4 = ip4polC.pool.addrAlloc();
            if (peer4 == null) {
                logger.info("got no address from pool " + ip4polC.name);
                return null;
            }
        }
        if (ip6polC != null) {
            peer6 = ip6polC.pool.addrAlloc();
            if (peer6 == null) {
                logger.info("got no address from pool " + ip6polC.name);
                return null;
            }
        }
        List<String> l = getShRun(0);
        l = userFilter.filterText(l, nocloneF);
        cfgIfc res;
        for (;;) {
            res = cfgAll.ifcFind("access" + bits.randomD(), true);
            if (res.cloned != null) {
                continue;
            }
            break;
        }
        res.cloned = this;
        res.lower = lower;
        for (int i = 1; i < l.size() - 2; i++) {
            res.doCfgStr(new cmds("clone", l.get(i).trim()));
        }
        if (peer4 != null) {
            res.ip4polA = peer4;
            if (res.ppp != null) {
                res.ppp.remAddrCfg = peer4.copyBytes();
                res.ppp.ctrlIp4.remAddrCur = peer4.copyBytes();
            }
            if (res.sep != null) {
                res.sep.loc4addr = addr4.copyBytes();
                res.sep.msk4addr = mask4.copyBytes();
                res.sep.rem4addr = peer4.copyBytes();
            }
        }
        if (peer6 != null) {
            res.ip6polA = peer6;
            if (res.sep != null) {
                res.sep.loc6addr = addr6.copyBytes();
                res.sep.msk6addr = mask6.copyBytes();
                res.sep.rem6addr = peer6.copyBytes();
            }
        }
        return res;
    }

    /**
     * stop this cloned interface
     */
    public synchronized void cloneStop() {
        if (cloned == null) {
            return;
        }
        ethtyp.forcedDN |= 4;
        ethtyp.propagateState();
        ethtyp.logStateChg = false;
        if (ip4polA != null) {
            ip4polC.pool.addrRelease(ip4polA);
            ip4polA = null;
        }
        if (ip6polA != null) {
            ip6polC.pool.addrRelease(ip6polA);
            ip6polA = null;
        }
        cfgAll.ifcDel(name, false);
        initEncap(" x ");
        ethtyp.closeUp();
    }

    /**
     * state changed
     *
     * @param s new state
     */
    public void stateChanged(state.states s) {
        if (slaac != null) {
            slaac.clearState();
        }
        if (dhcp4c != null) {
            dhcp4c.clearState();
        }
        if (dhcp6c != null) {
            dhcp6c.clearState();
        }
    }

    /**
     * get forwarder interface
     *
     * @param adr address to test
     * @return forwarder interface
     */
    public ipFwdIface getFwdIfc(addrIP adr) {
        if (adr.isIPv4()) {
            return fwdIf4;
        } else {
            return fwdIf6;
        }
    }

    /**
     * get local address
     *
     * @param adr address to test
     * @return local address
     */
    public addrIP getLocAddr(addrIP adr) {
        addrIP res = new addrIP();
        if (adr.isIPv4()) {
            if (addr4 == null) {
                return null;
            }
            res.fromIPv4addr(addr4);
        } else {
            if (addr6 == null) {
                return null;
            }
            res.fromIPv6addr(addr6);
        }
        return res;
    }

    public int compare(cfgIfc o1, cfgIfc o2) {
        String s1 = o1.name.toLowerCase();
        String s2 = o2.name.toLowerCase();
        boolean l1 = s1.startsWith("template");
        boolean l2 = s2.startsWith("template");
        if (l1 != l2) {
            return l1 ? -1 : +1;
        }
        l1 = s1.startsWith("loopback");
        l2 = s2.startsWith("loopback");
        if (l1 != l2) {
            return l1 ? -1 : +1;
        }
        l1 = s1.startsWith("access");
        l2 = s2.startsWith("access");
        if (l1 != l2) {
            return l1 ? +1 : -1;
        }
        l1 = s1.startsWith("tunnel");
        l2 = s2.startsWith("tunnel");
        if (l1 != l2) {
            return l1 ? +1 : -1;
        }
        return s1.compareTo(s2);
    }

    /**
     * convert tunnel type to displayable string
     *
     * @param badEmpty decode bad entry
     * @return string showing type
     */
    public String tunnelMode(boolean badEmpty) {
        if (tunMode == null) {
            if (badEmpty) {
                return "";
            } else {
                return "unknown tunnel";
            }
        }
        switch (tunMode) {
            case gre:
                return "gre";
            case udpgre:
                return "udpgre";
            case amt:
                return "amt";
            case icmp:
                return "icmp";
            case pim:
                return "pim";
            case lisp:
                return "lisp";
            case minenc:
                return "minenc";
            case pipe:
                return "pipe";
            case nos:
                return "nos";
            case ipcomp:
                return "ipcomp";
            case ipenc:
                return "ipenc";
            case tmux:
                return "tmux";
            case ipip:
                return "ipip";
            case Sto4:
                return "6to4";
            case srv6:
                return "srv6";
            case ipsec:
                return "ipsec";
            case pckOudp:
                return "pckoudp";
            case pckOip:
                return "pckoip";
            case l2tp3:
                return "l2tp3";
            case pweOmpls:
                return "pweompls";
            case polka:
                return "polka";
            case mpolka:
                return "mpolka";
            case expBun:
                return "expbun";
            case srMpls:
                return "srmpls";
            case srExt:
                return "srext";
            case pceSr:
                return "pcesr";
            case pceTe:
                return "pcete";
            case teP2p:
                return "p2pte";
            case teP2mp:
                return "p2mpte";
            case bier:
                return "bier";
            case ldpP2p:
                return "p2pldp";
            case ldpP2mp:
                return "p2mpldp";
            case ldpMp2mp:
                return "mp2mpldp";
            case vxlan:
                return "vxlan";
            case geneve:
                return "geneve";
            case capwap:
                return "capwap";
            case lwapp:
                return "lwapp";
            case erspan:
                return "erspan";
            case dlsw:
                return "dlsw";
            case etherip:
                return "etherip";
            case sreth:
                return "sreth";
            case uti:
                return "uti";
            case nvgre:
                return "nvgre";
            case mplsip:
                return "mplsip";
            case mplsudp:
                return "mplsudp";
            case swipe:
                return "swipe";
            case openvpn:
                return "openvpn";
            case wireguard:
                return "wireguard";
            case satp:
                return "satp";
            case inlsp:
                return "inlsp";
            case skip:
                return "skip";
            default:
                return null;
        }
    }

    /**
     * string to tunnel mode
     *
     * @param s string
     * @return tunnel mode
     */
    public static tunnelType string2tunnelMode(String s) {
        if (s.equals("gre")) {
            return tunnelType.gre;
        }
        if (s.equals("udpgre")) {
            return tunnelType.udpgre;
        }
        if (s.equals("amt")) {
            return tunnelType.amt;
        }
        if (s.equals("icmp")) {
            return tunnelType.icmp;
        }
        if (s.equals("pim")) {
            return tunnelType.pim;
        }
        if (s.equals("lisp")) {
            return tunnelType.lisp;
        }
        if (s.equals("minenc")) {
            return tunnelType.minenc;
        }
        if (s.equals("pipe")) {
            return tunnelType.pipe;
        }
        if (s.equals("nos")) {
            return tunnelType.nos;
        }
        if (s.equals("ipcomp")) {
            return tunnelType.ipcomp;
        }
        if (s.equals("ipenc")) {
            return tunnelType.ipenc;
        }
        if (s.equals("tmux")) {
            return tunnelType.tmux;
        }
        if (s.equals("ipip")) {
            return tunnelType.ipip;
        }
        if (s.equals("6to4")) {
            return tunnelType.Sto4;
        }
        if (s.equals("srv6")) {
            return tunnelType.srv6;
        }
        if (s.equals("ipsec")) {
            return tunnelType.ipsec;
        }
        if (s.equals("pckoudp")) {
            return tunnelType.pckOudp;
        }
        if (s.equals("pckoip")) {
            return tunnelType.pckOip;
        }
        if (s.equals("l2tp3")) {
            return tunnelType.l2tp3;
        }
        if (s.equals("pweompls")) {
            return tunnelType.pweOmpls;
        }
        if (s.equals("polka")) {
            return tunnelType.polka;
        }
        if (s.equals("mpolka")) {
            return tunnelType.mpolka;
        }
        if (s.equals("expbun")) {
            return tunnelType.expBun;
        }
        if (s.equals("srmpls")) {
            return tunnelType.srMpls;
        }
        if (s.equals("srext")) {
            return tunnelType.srExt;
        }
        if (s.equals("pcesr")) {
            return tunnelType.pceSr;
        }
        if (s.equals("pcete")) {
            return tunnelType.pceTe;
        }
        if (s.equals("p2pte")) {
            return tunnelType.teP2p;
        }
        if (s.equals("p2mpte")) {
            return tunnelType.teP2mp;
        }
        if (s.equals("bier")) {
            return tunnelType.bier;
        }
        if (s.equals("p2pldp")) {
            return tunnelType.ldpP2p;
        }
        if (s.equals("p2mpldp")) {
            return tunnelType.ldpP2mp;
        }
        if (s.equals("mp2mpldp")) {
            return tunnelType.ldpMp2mp;
        }
        if (s.equals("vxlan")) {
            return tunnelType.vxlan;
        }
        if (s.equals("geneve")) {
            return tunnelType.geneve;
        }
        if (s.equals("capwap")) {
            return tunnelType.capwap;
        }
        if (s.equals("lwapp")) {
            return tunnelType.lwapp;
        }
        if (s.equals("erspan")) {
            return tunnelType.erspan;
        }
        if (s.equals("dlsw")) {
            return tunnelType.dlsw;
        }
        if (s.equals("etherip")) {
            return tunnelType.etherip;
        }
        if (s.equals("sreth")) {
            return tunnelType.sreth;
        }
        if (s.equals("uti")) {
            return tunnelType.uti;
        }
        if (s.equals("nvgre")) {
            return tunnelType.nvgre;
        }
        if (s.equals("mplsip")) {
            return tunnelType.mplsip;
        }
        if (s.equals("mplsudp")) {
            return tunnelType.mplsudp;
        }
        if (s.equals("swipe")) {
            return tunnelType.swipe;
        }
        if (s.equals("openvpn")) {
            return tunnelType.openvpn;
        }
        if (s.equals("wireguard")) {
            return tunnelType.wireguard;
        }
        if (s.equals("satp")) {
            return tunnelType.satp;
        }
        if (s.equals("inlsp")) {
            return tunnelType.inlsp;
        }
        if (s.equals("skip")) {
            return tunnelType.skip;
        }
        return null;
    }

    /**
     * convert type to displayable string
     *
     * @return string showing type
     */
    public String type2string() {
        switch (type) {
            case ether:
                return "ethernet";
            case serial:
                return "serial";
            case atm:
                return "atm";
            case arcnet:
                return "arcnet";
            case infiniband:
                return "infiniband";
            case bridge:
                return "bridged";
            case bundle:
                return "bundle";
            case hairpin:
                return "hairpin";
            case tunnel:
                return tunnelMode(false);
            case loopback:
                return "loopback";
            case nul:
                return "null";
            case template:
                return "template";
            case dialer:
                return "dialer";
            case sdn:
                return "sdn";
            case pweth:
                return "pwether";
            case virtppp:
                return "virtualppp";
        }
        return "unknown";
    }

    /**
     * normalize one interface name
     *
     * @param s string to normalize
     * @param subi set true to get subinterface if, false to get main interface
     * id
     * @return normalized name, "" if failed
     */
    public static String normName(String s, boolean subi) {
        s = s.trim().toLowerCase();
        int p = s.length();
        for (int i = 0; i < 10; i++) {
            int o = s.indexOf("" + i);
            if (o < 0) {
                continue;
            }
            if (o < p) {
                p = o;
            }
        }
        if (p >= s.length()) {
            return "";
        }
        String b = s.substring(0, p);
        s = s.substring(p, s.length());
        userHelping hl = new userHelping();
        hl.add(null, "1 . loopback     ifc");
        hl.add(null, "1 . null         ifc");
        hl.add(null, "1 . template     ifc");
        hl.add(null, "1 . dialer       ifc");
        hl.add(null, "1 . sdn          ifc");
        hl.add(null, "1 . pwether      ifc");
        hl.add(null, "1 . virtualppp   ifc");
        hl.add(null, "1 . access       ifc");
        hl.add(null, "1 . bvi          ifc");
        hl.add(null, "1 . bundle       ifc");
        hl.add(null, "1 . tunnel       ifc");
        hl.add(null, "1 . hairpin      ifc");
        for (int i = 0; i < verCore.ifaces.length; i++) {
            hl.add(null, "1 . " + verCore.ifaces[i] + " ifc");
        }
        b = hl.repairLine(b).trim();
        if (b.length() < 1) {
            return "";
        }
        p = s.indexOf(".");
        if (p < 0) {
            p = bits.str2num(s);
            b += p;
            s = "";
        } else {
            b += bits.str2num(s.substring(0, p));
            s = s.substring(p + 1, s.length());
        }
        if (subi) {
            return s.trim();
        } else {
            return b.trim();
        }
    }

    /**
     * normalize one interface name
     *
     * @param nam name to process
     * @return normalized name, "" if failed
     */
    public static String normName(String nam) {
        String s = normName(nam, true);
        if (s.length() > 0) {
            s = "." + s;
        }
        nam = normName(nam, false);
        if (nam.length() < 1) {
            return "";
        }
        return nam + s;
    }

    public String toString() {
        return "ifc " + name;
    }

    /**
     * create new interface
     *
     * @param nam name of interface
     */
    public cfgIfc(String nam) {
        name = nam.trim();
        ethtyp = new ifcEthTyp(name, this);
        lower = new ifcNull();
        lower.setUpper(ethtyp);
        ethtyp.setState(state.states.down);
    }

    /**
     * notified when new address negotiated
     *
     * @param adr address
     * @param msk netmask
     * @param gw gateway
     */
    public void addr4changed(addrIPv4 adr, addrIPv4 msk, addrIPv4 gw) {
        if (addr4 == null) {
            return;
        }
        if (adr == null) {
            fwdIf4.gateAddr = null;
            return;
        }
        addr4 = adr.copyBytes();
        mask4 = msk.copyBytes();
        if (gw != null) {
            if (gw.isEmpty()) {
                gw = null;
            }
        }
        if (gw != null) {
            if (gw.isBroadcast()) {
                gw = null;
            }
        }
        if (gw == null) {
            fwdIf4.gateAddr = null;
        } else {
            fwdIf4.gateAddr = new addrIP();
            fwdIf4.gateAddr.fromIPv4addr(gw);
        }
        ipIf4.setIPv4addr(adr, mask4.toNetmask());
        vrfFor.fwd4.routerStaticChg();
    }

    /**
     * notified when new address negotiated
     *
     * @param adr address
     * @param msk netmask
     * @param gw gateway
     */
    public void addr6changed(addrIPv6 adr, addrIPv6 msk, addrIPv6 gw) {
        if (addr6 == null) {
            return;
        }
        if (adr == null) {
            fwdIf6.gateAddr = null;
            return;
        }
        addr6 = adr.copyBytes();
        mask6 = msk.copyBytes();
        if (adr.isLinkLocal()) {
            addrIP ad = new addrIP();
            ad.fromIPv6addr(adr);
            ipIf6.setLinkLocalAddr(ad);
        }
        if (gw != null) {
            if (gw.isEmpty()) {
                gw = null;
            }
        }
        if (gw != null) {
            if (gw.isBroadcast()) {
                gw = null;
            }
        }
        if (gw == null) {
            fwdIf6.gateAddr = null;
        } else {
            fwdIf6.gateAddr = new addrIP();
            fwdIf6.gateAddr.fromIPv6addr(gw);
        }
        ipIf6.setIPv6addr(adr, mask6.toNetmask());
        vrfFor.fwd6.routerStaticChg();
    }

    /**
     * test if interface need arp/nd
     *
     * @return false on no, true on yes
     */
    public boolean ifaceNeedArp() {
        if (parent != null) {
            return parent.ifaceNeedArp();
        }
        switch (type) {
            case serial:
            case atm:
            case arcnet:
            case infiniband:
            case tunnel:
            case dialer:
            case virtppp:
            case template:
            case loopback:
            case nul:
                return false;
            case bundle:
                return !bundleHed.bundleHed.notEther;
            case hairpin:
                return !hairpinHed.hairpinHed.notEther;
            default:
                return true;
        }
    }

    /**
     * test if interface need mac payload for bridging
     *
     * @return false on no, true on yes
     */
    public boolean ifaceNeedMacs() {
        if (parent != null) {
            return parent.ifaceNeedMacs();
        }
        switch (type) {
            case ether:
            case bridge:
            case pweth:
            case sdn:
            case template:
            case loopback:
            case nul:
                return false;
            case bundle:
                return bundleHed.bundleHed.notEther;
            case hairpin:
                return hairpinHed.hairpinHed.notEther;
            default:
                return true;
        }
    }

    /**
     * setup to routing process
     *
     * @param rtr router process to use
     */
    public synchronized void setup2router(cfgRtr rtr) {
        if (vrfFor == null) {
            return;
        }
        if (rtr.vrf == null) {
            return;
        }
        if (!vrfFor.name.equals(rtr.vrf.name)) {
            return;
        }
        switch (rtr.type) {
            case babel4:
                if (rtrBabel4ifc != null) {
                    break;
                }
                rtrBabel4ifc = rtr.babel.addInterface(fwdIf4);
                if (rtrBabel4ifc == null) {
                    break;
                }
                rtrBabel4hnd = rtr;
                break;
            case babel6:
                if (rtrBabel6ifc != null) {
                    break;
                }
                rtrBabel6ifc = rtr.babel.addInterface(fwdIf6);
                if (rtrBabel6ifc == null) {
                    break;
                }
                rtrBabel6hnd = rtr;
                break;
            case olsr4:
                if (rtrOlsr4ifc != null) {
                    break;
                }
                rtrOlsr4ifc = rtr.olsr.addInterface(fwdIf4);
                if (rtrOlsr4ifc == null) {
                    break;
                }
                rtrOlsr4hnd = rtr;
                break;
            case olsr6:
                if (rtrOlsr6ifc != null) {
                    break;
                }
                rtrOlsr6ifc = rtr.olsr.addInterface(fwdIf6);
                if (rtrOlsr6ifc == null) {
                    break;
                }
                rtrOlsr6hnd = rtr;
                break;
            case rip4:
                if (rtrRip4ifc != null) {
                    break;
                }
                rtrRip4ifc = rtr.rip4.addInterface(fwdIf4);
                if (rtrRip4ifc == null) {
                    break;
                }
                rtrRip4hnd = rtr;
                break;
            case rip6:
                if (rtrRip6ifc != null) {
                    break;
                }
                rtrRip6ifc = rtr.rip6.addInterface(fwdIf6);
                if (rtrRip6ifc == null) {
                    break;
                }
                rtrRip6hnd = rtr;
                break;
            case ospf4:
                if (rtrOspf4ifc != null) {
                    break;
                }
                rtrOspf4ifc = rtr.ospf4.addInterface(fwdIf4);
                if (rtrOspf4ifc == null) {
                    break;
                }
                rtrOspf4hnd = rtr;
                break;
            case ospf6:
                if (rtrOspf6ifc != null) {
                    break;
                }
                rtrOspf6ifc = rtr.ospf6.addInterface(fwdIf6);
                if (rtrOspf6ifc == null) {
                    break;
                }
                rtrOspf6hnd = rtr;
                break;
            case isis4:
                if (rtrIsisIfc != null) {
                    break;
                }
                rtrIsisIfc = rtr.isis.addInterface(fwdIf4, fwdIf6, ethtyp);
                if (rtrIsisIfc == null) {
                    break;
                }
                rtrIsisHnd = rtr;
                break;
            case isis6:
                if (rtrIsisIfc != null) {
                    break;
                }
                rtrIsisIfc = rtr.isis.addInterface(fwdIf6, fwdIf4, ethtyp);
                if (rtrIsisIfc == null) {
                    break;
                }
                rtrIsisHnd = rtr;
                break;
            case pvrp4:
                if (rtrPvrp4ifc != null) {
                    break;
                }
                rtrPvrp4ifc = rtr.pvrp.addInterface(fwdIf4);
                if (rtrPvrp4ifc == null) {
                    break;
                }
                rtrPvrp4hnd = rtr;
                break;
            case pvrp6:
                if (rtrPvrp6ifc != null) {
                    break;
                }
                rtrPvrp6ifc = rtr.pvrp.addInterface(fwdIf6);
                if (rtrPvrp6ifc == null) {
                    break;
                }
                rtrPvrp6hnd = rtr;
                break;
            case lsrp4:
                if (rtrLsrp4ifc != null) {
                    break;
                }
                rtrLsrp4ifc = rtr.lsrp.addInterface(fwdIf4);
                if (rtrLsrp4ifc == null) {
                    break;
                }
                rtrLsrp4hnd = rtr;
                break;
            case lsrp6:
                if (rtrLsrp6ifc != null) {
                    break;
                }
                rtrLsrp6ifc = rtr.lsrp.addInterface(fwdIf6);
                if (rtrLsrp6ifc == null) {
                    break;
                }
                rtrLsrp6hnd = rtr;
                break;
            case eigrp4:
                if (rtrEigrp4ifc != null) {
                    break;
                }
                rtrEigrp4ifc = rtr.eigrp.addInterface(fwdIf4);
                if (rtrEigrp4ifc == null) {
                    break;
                }
                rtrEigrp4hnd = rtr;
                break;
            case eigrp6:
                if (rtrEigrp6ifc != null) {
                    break;
                }
                rtrEigrp6ifc = rtr.eigrp.addInterface(fwdIf6);
                if (rtrEigrp6ifc == null) {
                    break;
                }
                rtrEigrp6hnd = rtr;
                break;
            default:
                break;
        }
    }

    /**
     * clear from routing process
     *
     * @param ip4 ipv4
     * @param ip6 ipv6
     */
    public void clear2routing(boolean ip4, boolean ip6) {
        if (ip4) {
            clear2router(rtrRip4hnd);
        }
        if (ip6) {
            clear2router(rtrRip6hnd);
        }
        if (ip4) {
            clear2router(rtrBabel4hnd);
        }
        if (ip6) {
            clear2router(rtrBabel6hnd);
        }
        if (ip4) {
            clear2router(rtrOlsr4hnd);
        }
        if (ip6) {
            clear2router(rtrOlsr6hnd);
        }
        if (ip4) {
            clear2router(rtrOspf4hnd);
        }
        if (ip6) {
            clear2router(rtrOspf6hnd);
        }
        if (ip4 || ip6) {
            clear2router(rtrIsisHnd);
        }
        if (ip4) {
            clear2router(rtrPvrp4hnd);
        }
        if (ip6) {
            clear2router(rtrPvrp6hnd);
        }
        if (ip4) {
            clear2router(rtrLsrp4hnd);
        }
        if (ip6) {
            clear2router(rtrLsrp6hnd);
        }
        if (ip4) {
            clear2router(rtrEigrp4hnd);
        }
        if (ip6) {
            clear2router(rtrEigrp6hnd);
        }
    }

    /**
     * clear from routing process
     *
     * @param rtr router process to use
     */
    public synchronized void clear2router(cfgRtr rtr) {
        if (rtr == null) {
            return;
        }
        switch (rtr.type) {
            case babel4:
                if (rtrBabel4hnd == null) {
                    return;
                }
                rtrBabel4hnd = null;
                rtrBabel4ifc = null;
                rtr.babel.closedInterface(fwdIf4);
                return;
            case babel6:
                if (rtrBabel6hnd == null) {
                    return;
                }
                rtrBabel6hnd = null;
                rtrBabel6ifc = null;
                rtr.babel.closedInterface(fwdIf6);
                return;
            case olsr4:
                if (rtrOlsr4hnd == null) {
                    return;
                }
                rtrOlsr4hnd = null;
                rtrOlsr4ifc = null;
                rtr.olsr.closedInterface(fwdIf4);
                return;
            case olsr6:
                if (rtrOlsr6hnd == null) {
                    return;
                }
                rtrOlsr6hnd = null;
                rtrOlsr6ifc = null;
                rtr.olsr.closedInterface(fwdIf6);
                return;
            case rip4:
                if (rtrRip4hnd == null) {
                    return;
                }
                rtrRip4hnd = null;
                rtrRip4ifc = null;
                rtr.rip4.closedInterface(fwdIf4);
                return;
            case rip6:
                if (rtrRip6hnd == null) {
                    return;
                }
                rtrRip6hnd = null;
                rtrRip6ifc = null;
                rtr.rip6.closedInterface(fwdIf6);
                return;
            case ospf4:
                if (rtrOspf4hnd == null) {
                    return;
                }
                rtrOspf4hnd = null;
                rtrOspf4ifc = null;
                rtr.ospf4.delInterface(fwdIf4);
                return;
            case ospf6:
                if (rtrOspf6hnd == null) {
                    return;
                }
                rtrOspf6hnd = null;
                rtrOspf6ifc = null;
                rtr.ospf6.delInterface(fwdIf6);
                return;
            case isis4:
                if (rtrIsisHnd == null) {
                    return;
                }
                rtrIsisHnd = null;
                rtrIsisIfc = null;
                rtr.isis.delInterface(fwdIf4, fwdIf6);
                return;
            case isis6:
                if (rtrIsisHnd == null) {
                    return;
                }
                rtrIsisHnd = null;
                rtrIsisIfc = null;
                rtr.isis.delInterface(fwdIf6, fwdIf4);
                return;
            case pvrp4:
                if (rtrPvrp4hnd == null) {
                    return;
                }
                rtrPvrp4hnd = null;
                rtrPvrp4ifc = null;
                rtr.pvrp.delInterface(fwdIf4);
                return;
            case pvrp6:
                if (rtrPvrp6hnd == null) {
                    return;
                }
                rtrPvrp6hnd = null;
                rtrPvrp6ifc = null;
                rtr.pvrp.delInterface(fwdIf6);
                return;
            case lsrp4:
                if (rtrLsrp4hnd == null) {
                    return;
                }
                rtrLsrp4hnd = null;
                rtrLsrp4ifc = null;
                rtr.lsrp.delInterface(fwdIf4);
                return;
            case lsrp6:
                if (rtrLsrp6hnd == null) {
                    return;
                }
                rtrLsrp6hnd = null;
                rtrLsrp6ifc = null;
                rtr.lsrp.delInterface(fwdIf6);
                return;
            case eigrp4:
                if (rtrEigrp4hnd == null) {
                    return;
                }
                rtrEigrp4hnd = null;
                rtrEigrp4ifc = null;
                rtr.eigrp.delInterface(fwdIf4);
                return;
            case eigrp6:
                if (rtrEigrp6hnd == null) {
                    return;
                }
                rtrEigrp6hnd = null;
                rtrEigrp6ifc = null;
                rtr.eigrp.delInterface(fwdIf6);
                return;
            default:
                break;
        }
    }

    /**
     * setup this bridge interface
     */
    public synchronized void initBridge() {
        lower = bridgeHed.bridgeHed;
        bridgeHed.bridgeHed.setFilter(false);
        bridgeHed.bridgeHed.setUpper(ethtyp);
    }

    /**
     * setup this bundle interface
     */
    public synchronized void initBundle() {
        lower = bundleHed.bundleHed;
        bundleHed.bundleHed.setFilter(false);
        bundleHed.bundleHed.setUpper(ethtyp);
    }

    /**
     * setup this hairpin interface
     *
     * @param side side to initialize
     */
    public synchronized void initHairpin(boolean side) {
        if (side) {
            lower = hairpinHed.hairpinHed.getSide1();
        } else {
            lower = hairpinHed.hairpinHed.getSide2();
        }
        lower.setFilter(false);
        lower.setUpper(ethtyp);
    }

    private synchronized boolean initEncap(String a) {
        int enc = 0;
        if (a.equals("hdlc")) {
            enc = 1;
        }
        if (a.equals("ppp")) {
            enc = 2;
        }
        if (a.equals("lapb")) {
            enc = 3;
        }
        if (a.equals("framerelay")) {
            enc = 4;
        }
        if (a.equals("atmdxi")) {
            enc = 5;
        }
        if (a.equals("frppp")) {
            enc = 6;
        }
        if (a.equals("raw")) {
            enc = 7;
        }
        if (a.equals("isdn")) {
            enc = 8;
        }
        if (a.equals("sep")) {
            enc = 9;
        }
        if (a.equals("iponly")) {
            enc = 10;
        }
        if (a.equals("frrfc")) {
            enc = 11;
        }
        if (ifaceNeedArp()) {
            enc = 0;
        }
        if (a.equals("dot1q")) {
            initVlan(new ifcDot1q());
            return false;
        }
        if (a.equals("dot1ad")) {
            initVlan(new ifcDot1ad());
            return false;
        }
        if (a.equals("dot1ah")) {
            initVlan(new ifcDot1ah());
            return false;
        }
        if (a.equals("qinq1")) {
            initVlan(new ifcQinq1());
            return false;
        }
        if (a.equals("qinq2")) {
            initVlan(new ifcQinq2());
            return false;
        }
        if (a.equals("qinq3")) {
            initVlan(new ifcQinq3());
            return false;
        }
        if (a.equals("qinqx")) {
            qinqx = new ifcQinqX(null);
            initVlan(qinqx);
            return false;
        }
        if (a.equals("isl")) {
            initVlan(new ifcIsl());
            return false;
        }
        if (a.equals(" x ")) {
            enc = 1000;
        }
        if (enc <= 0) {
            return true;
        }
        if (hdlc != null) {
            hdlc.restartTimer(true);
            hdlc = null;
        }
        if (ppp != null) {
            ppp.restartTimer(true);
            ppp = null;
        }
        if (lapb != null) {
            lapb.restartTimer(true);
            lapb = null;
        }
        if (raw != null) {
            raw = null;
        }
        if (sep != null) {
            sep.restartTimer(true);
            sep = null;
        }
        if (isdn != null) {
            isdn.restartTimer(true);
            isdn = null;
        }
        if (frmrly != null) {
            frmrly.restartTimer(true);
            frmrly = null;
        }
        if (frmppp != null) {
            frmppp = null;
        }
        if (frmrfc != null) {
            frmrfc = null;
        }
        if (atmdxi != null) {
            atmdxi = null;
        }
        switch (enc) {
            case 1:
                hdlc = new ifcHdlc();
                lower.setUpper(hdlc);
                hdlc.setUpper(ethtyp);
                break;
            case 2:
                ppp = new ifcPpp();
                ppp.cfger = this;
                lower.setUpper(ppp);
                ppp.setUpper(ethtyp);
                break;
            case 3:
                lapb = new ifcLapb();
                lower.setUpper(lapb);
                lapb.setUpper(ethtyp);
                break;
            case 4:
                frmrly = new ifcFrameRelay();
                lower.setUpper(frmrly);
                frmrly.setUpper(ethtyp);
                break;
            case 5:
                atmdxi = new ifcAtmDxi();
                lower.setUpper(atmdxi);
                atmdxi.setUpper(ethtyp);
                break;
            case 6:
                frmppp = new ifcFramePpp();
                frmrly = new ifcFrameRelay();
                lower.setUpper(frmrly);
                frmrly.setUpper(frmppp);
                ppp = new ifcPpp();
                ppp.cfger = this;
                frmppp.setUpper(ppp);
                ppp.setUpper(ethtyp);
                break;
            case 7:
                raw = new ifcRaw();
                lower.setUpper(raw);
                raw.setUpper(ethtyp);
                break;
            case 8:
                isdn = new ifcIsdn();
                lower.setUpper(isdn);
                isdn.setUpper(ethtyp);
                break;
            case 9:
                sep = new ifcSep();
                lower.setUpper(sep);
                sep.cfger = this;
                sep.setUpper(ethtyp);
                break;
            case 10:
                ipOnly = new ifcIpOnly();
                lower.setUpper(ipOnly);
                ipOnly.setUpper(ethtyp);
                break;
            case 11:
                frmrfc = new ifcFrameRfc();
                frmrly = new ifcFrameRelay();
                lower.setUpper(frmrly);
                frmrly.setUpper(frmrfc);
                frmrfc.setUpper(ethtyp);
                break;
            default:
                ifcNull nul = new ifcNull();
                nul.setUpper(ethtyp);
                break;
        }
        ethtyp.propagateState();
        return true;
    }

    /**
     * setup this physical interface
     */
    public synchronized void initPhysical() {
        switch (type) {
            case atm:
                lower = thread;
                atmsar = new ifcAtmSar();
                thread.setUpper(atmsar);
                atmsar.setUpper(ethtyp);
                break;
            case arcnet:
                lower = thread;
                arcnet = new ifcArcnet();
                thread.setUpper(arcnet);
                arcnet.setUpper(ethtyp);
                break;
            case infiniband:
                lower = thread;
                infiniband = new ifcInfiniband();
                thread.setUpper(infiniband);
                infiniband.setUpper(ethtyp);
                break;
            case serial:
                lower = thread;
                initEncap("hdlc");
                break;
            case dialer:
            case virtppp:
                lower = new ifcNull();
                initEncap("ppp");
                break;
            case tunnel:
            case bundle:
            case hairpin:
            case bridge:
            case template:
            case loopback:
            case nul:
                lower = new ifcNull();
                break;
            case pweth:
                ethtyp.forcedMac = addrMac.getRandom();
                ethtyp.forcedMTU = 1500;
                lower = new ifcNull();
                break;
            case sdn:
                ethtyp.forcedMac = addrMac.getRandom();
                ethtyp.forcedMTU = 1500;
                lower = new ifcNull();
                break;
            case ether:
                lower = thread;
                thread.setFilter(false);
                thread.setUpper(ethtyp);
                break;
        }
    }

    /**
     * setup this subinterface
     *
     * @param prnt parent to use
     */
    public synchronized void initSubiface(cfgIfc prnt) {
        parent = prnt;
        type = parent.type;
        parent.initVlan();
        parent.vlanHed.addVlan(vlanNum, ethtyp);
        lower = parent.vlanHed.updateVlan(vlanNum, ethtyp);
    }

    private synchronized void initVlan() {
        if (vlanHed != null) {
            return;
        }
        vlanHed = new ifcDot1q();
        vlanHed.reg2ethTyp(ethtyp);
    }

    private synchronized void initVlan(ifcVlan vlan) {
        if (vlanHed != null) {
            vlan.subMacs = vlanHed.subMacs;
            vlanHed.unreg2ethTyp(ethtyp);
        }
        vlanHed = vlan;
        vlanHed.reg2ethTyp(ethtyp);
        cfgAll.regSubifaces(this);
    }

    /**
     * setup this loopback
     */
    public synchronized void initLoopback() {
        lower = new ifcNull();
        ethtyp.setParent(new ifcNull());
    }

    /**
     * setup this template
     */
    public synchronized void initTemplate() {
        lower = new ifcNull();
        ethtyp.setParent(new ifcNull());
    }

    /**
     * setup this interface for bridging
     *
     * @param brdg bridging to set up
     */
    public synchronized void setup2bridge(cfgBrdg brdg) {
        bridgeHed = brdg;
        if (bridgeIfc == null) {
            bridgeIfc = bridgeHed.bridgeHed.newIface(true, ifaceNeedMacs(), ifaceNeedMacs());
        }
        ethtyp.addET(-1, "bridging", bridgeIfc);
        ethtyp.updateET(-1, bridgeIfc);
    }

    /**
     * clear bridging
     */
    public synchronized void clear2bridge() {
        if (bridgeIfc == null) {
            return;
        }
        ethtyp.delET(-1);
        bridgeIfc.closeUp();
        bridgeHed.bridgeHed.delIface(bridgeIfc.ifcNum);
        bridgeIfc = null;
        bridgeHed = null;
    }

    /**
     * setup this interface for bundling
     *
     * @param bndl bundling to set up
     */
    public synchronized void setup2bundle(cfgBndl bndl) {
        bundleHed = bndl;
        if (bundleIfc == null) {
            bundleIfc = bundleHed.bundleHed.newIface();
        }
        ethtyp.addET(-1, "bundling", bundleIfc);
        ethtyp.updateET(-1, bundleIfc);
        bundleHed.bundleHed.propagateState();
    }

    /**
     * clear bundling
     */
    public synchronized void clear2bundle() {
        if (bundleIfc == null) {
            return;
        }
        ethtyp.delET(-1);
        bundleIfc.closeUp();
        bundleHed.bundleHed.delIface(bundleIfc.ifcNum);
        bundleIfc = null;
        bundleHed = null;
    }

    /**
     * clear evcs
     */
    public synchronized void clear2evcs() {
        for (int i = 0; i < evcs.size(); i++) {
            cfgIfcEvc ntry = evcs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopWork();
        }
        evcs.clear();
    }

    /**
     * clear xconnect
     */
    public synchronized void clear2xconnect() {
        if (xconn == null) {
            return;
        }
        ethtyp.delET(-1);
        xconn.stop2run();
        xconn = null;
    }

    /**
     * clear pseudowire
     */
    public synchronized void clear2pseudowire() {
        if (pwhe == null) {
            return;
        }
        ifcUp upp = null;
        switch (type) {
            case virtppp:
                upp = getEncapProto();
                upp.setState(state.states.close);
                break;
            case pweth:
                upp = ethtyp;
                break;
            default:
                return;
        }
        ifcNull nul = new ifcNull();
        nul.setUpper(upp);
        pwhe.stop2run();
        pwhe = null;
        lower = nul;
    }

    /**
     * clear vrf membership
     */
    public synchronized void clear2vrf() {
        addr4 = null;
        mask4 = null;
        setup2vrf(true, false, false);
        addr6 = null;
        mask6 = null;
        setup2vrf(false, true, false);
        ipxAddr = null;
        setup2vrf(false, false, true);
        vrfFor = null;
    }

    /**
     * set interface to vrf
     *
     * @param ip4 set true to process this protocol
     * @param ip6 set true to process this protocol
     * @param ipx set true to process this protocol
     */
    public synchronized void setup2vrf(boolean ip4, boolean ip6, boolean ipx) {
        if (ip4 && (ipIf4 != null)) {
            ipIf4.closeUp();
            vrfFor.fwd4.ifaceDel(fwdIf4);
            fwdIf4 = null;
            ipIf4 = null;
            ethtyp.delET(ipIfc4.type);
            ethtyp.delET(ipIfc4arp.type);
        }
        if (ip6 && (ipIf6 != null)) {
            ipIf6.closeUp();
            vrfFor.fwd6.ifaceDel(fwdIf6);
            fwdIf6 = null;
            ipIf6 = null;
            ethtyp.delET(ipIfc6.type);
        }
        if (ipx && (ipxIfc != null)) {
            ipxIfc.closeUp();
            vrfFor.ipx.ifaceDel(ipxIfc);
            ipxIfc = null;
            ethtyp.delET(ipxIface.type);
        }
        if (vrfFor == null) {
            return;
        }
        if (ip4 && (addr4 != null)) {
            ipIf4 = new ipIfc4(ifaceNeedArp());
            fwdIf4 = vrfFor.fwd4.ifaceAdd(ipIf4);
            ipIf4.setIPv4addr(addr4, mask4.toNetmask());
            ethtyp.addET(ipIfc4.type, "ip4", ipIf4);
            ethtyp.updateET(ipIfc4.type, ipIf4);
            ifcUp arp = ipIf4.getPeerHdr();
            if (arp != null) {
                ethtyp.addET(ipIfc4arp.type, "arp4", arp);
                ethtyp.updateET(ipIfc4arp.type, arp);
            }
            ethtyp.propagateState();
            vrfFor.fwd4.routerStaticChg();
        }
        if (ip6 && (addr6 != null)) {
            ipIf6 = new ipIfc6(ifaceNeedArp());
            fwdIf6 = vrfFor.fwd6.ifaceAdd(ipIf6);
            ipIf6.setIPv6addr(addr6, mask6.toNetmask());
            ethtyp.addET(ipIfc6.type, "ip6", ipIf6);
            ethtyp.updateET(ipIfc6.type, ipIf6);
            if (addr6.isLinkLocal()) {
                addrIP ad = new addrIP();
                ad.fromIPv6addr(addr6);
                ipIf6.setLinkLocalAddr(ad);
            }
            ethtyp.propagateState();
            vrfFor.fwd6.routerStaticChg();
        }
        if (ipx && (ipxAddr != null)) {
            ipxIfc = vrfFor.ipx.ifaceAdd(ethtyp);
            vrfFor.ipx.ifaceAddr(ipxIfc, ipxAddr);
            ethtyp.addET(ipxIface.type, "ipx", ipxIfc);
            ethtyp.updateET(ipxIface.type, ipxIfc);
            ipxAddr.putMac(ipxIfc.hwaddr);
        }
        if (fwdIf4 != null) {
            fwdIf4.otherHandler = fwdIf6;
        }
        if (fwdIf6 != null) {
            fwdIf6.otherHandler = fwdIf4;
        }
        update2mpls();
        update2polka();
    }

    /**
     * clear tunnel interface
     *
     * @param justWorker set true to just protocol handler
     */
    public synchronized void clear2tunnel(boolean justWorker) {
        lower = new ifcNull();
        lower.setUpper(ethtyp);
        ethtyp.setState(state.states.down);
        if (tunGRE != null) {
            tunGRE.closeDn();
            tunGRE = null;
        }
        if (tunUdpGre != null) {
            tunUdpGre.closeDn();
            tunUdpGre = null;
        }
        if (tunAmt != null) {
            tunAmt.closeDn();
            tunAmt = null;
        }
        if (tunICMP != null) {
            tunICMP.closeDn();
            tunICMP = null;
        }
        if (tunPIM != null) {
            tunPIM.closeDn();
            tunPIM = null;
        }
        if (tunLisp != null) {
            tunLisp.closeDn();
            tunLisp = null;
        }
        if (tunMinenc != null) {
            tunMinenc.closeDn();
            tunMinenc = null;
        }
        if (tunPipe != null) {
            tunPipe.closeDn();
            tunPipe = null;
        }
        if (tunNos != null) {
            tunNos.closeDn();
            tunNos = null;
        }
        if (tunIpcomp != null) {
            tunIpcomp.closeDn();
            tunIpcomp = null;
        }
        if (tunIpenc != null) {
            tunIpenc.closeDn();
            tunIpenc = null;
        }
        if (tunTmux != null) {
            tunTmux.closeDn();
            tunTmux = null;
        }
        if (tunMplsip != null) {
            tunMplsip.closeDn();
            tunMplsip = null;
        }
        if (tunMplsudp != null) {
            tunMplsudp.closeDn();
            tunMplsudp = null;
        }
        if (tunSwipe != null) {
            tunSwipe.closeDn();
            tunSwipe = null;
        }
        if (tunOpenvpn != null) {
            tunOpenvpn.workStop();
            tunOpenvpn = null;
        }
        if (tunWireguard != null) {
            tunWireguard.workStop();
            tunWireguard = null;
        }
        if (tunSatp != null) {
            tunSatp.workStop();
            tunSatp = null;
        }
        if (tunInlsp != null) {
            tunInlsp.closeDn();
            tunInlsp = null;
        }
        if (tunSkip != null) {
            tunSkip.closeDn();
            tunSkip = null;
        }
        if (tunIPIP != null) {
            tunIPIP.closeDn();
            tunIPIP = null;
        }
        if (tun6to4 != null) {
            tun6to4.closeDn();
            tun6to4 = null;
        }
        if (tunSrv6 != null) {
            tunSrv6.closeDn();
            tunSrv6 = null;
        }
        if (tunIPsec1 != null) {
            tunIPsec1.workStop();
            tunIPsec1 = null;
        }
        if (tunIPsec2 != null) {
            tunIPsec2.workStop();
            tunIPsec2 = null;
        }
        if (tunPckOudp != null) {
            tunPckOudp.workStop();
            tunPckOudp = null;
        }
        if (tunPckOip != null) {
            tunPckOip.closeDn();
            tunPckOip = null;
        }
        if (tunL2tp3 != null) {
            tunL2tp3.workStop();
            tunL2tp3 = null;
        }
        if (tunPweOmpls != null) {
            tunPweOmpls.workStop();
            tunPweOmpls = null;
        }
        if (tunPolka != null) {
            tunPolka.workStop();
            tunPolka = null;
        }
        if (tunMpolka != null) {
            tunMpolka.workStop();
            tunMpolka = null;
        }
        if (tunExpBun != null) {
            tunExpBun.workStop();
            tunExpBun = null;
        }
        if (tunSrMpls != null) {
            tunSrMpls.workStop();
            tunSrMpls = null;
        }
        if (tunSrExt != null) {
            tunSrExt.workStop();
            tunSrExt = null;
        }
        if (tunTeP2p != null) {
            tunTeP2p.workStop();
            tunTeP2p = null;
        }
        if (tunTeP2mp != null) {
            tunTeP2mp.workStop();
            tunTeP2mp = null;
        }
        if (tunBier != null) {
            tunBier.workStop();
            tunBier = null;
        }
        if (tunLdpP2p != null) {
            tunLdpP2p.workStop();
            tunLdpP2p = null;
        }
        if (tunLdpP2mp != null) {
            tunLdpP2mp.workStop();
            tunLdpP2mp = null;
        }
        if (tunVxlan != null) {
            tunVxlan.workStop();
            tunVxlan = null;
        }
        if (tunGeneve != null) {
            tunGeneve.workStop();
            tunGeneve = null;
        }
        if (tunCapwap != null) {
            tunCapwap.workStop();
            tunCapwap = null;
        }
        if (tunLwapp != null) {
            tunLwapp.workStop();
            tunLwapp = null;
        }
        if (tunErspan != null) {
            tunErspan.workStop();
            tunErspan = null;
        }
        if (tunDlsw != null) {
            tunDlsw.workStop();
            tunDlsw = null;
        }
        if (tunEtherip != null) {
            tunEtherip.workStop();
            tunEtherip = null;
        }
        if (tunSreth != null) {
            tunSreth.workStop();
            tunSreth = null;
        }
        if (tunUti != null) {
            tunUti.workStop();
            tunUti = null;
        }
        if (tunNvgre != null) {
            tunNvgre.workStop();
            tunNvgre = null;
        }
        if (justWorker) {
            return;
        }
        tunTOS = -1;
        tunFLW = -1;
        tunTTL = 255;
        tunKey = 0;
        tunKey2 = 0;
        tunSum = false;
        tunSeq = false;
        tunPri = 7;
        tunAscId = 0;
        tunAscId2 = 0;
        tunAscAdr = null;
        tunVrf = null;
        tunTrg = null;
        tunSrc = null;
        tunPrt = null;
    }

    /**
     * setup interface for tunneling
     *
     * @return false on success, true on error
     */
    public synchronized boolean setup2tunnel() {
        clear2tunnel(true);
        if (tunShut) {
            return true;
        }
        if (tunVrf == null) {
            return true;
        }
        if (tunTrg == null) {
            return true;
        }
        if (tunSrc == null) {
            return true;
        }
        ipFwd fwd;
        ipFwdIface ifc;
        prtUdp udp;
        if (tunTrg.isIPv4()) {
            fwd = tunVrf.fwd4;
            udp = tunVrf.udp4;
            ifc = tunSrc.fwdIf4;
        } else {
            fwd = tunVrf.fwd6;
            udp = tunVrf.udp6;
            ifc = tunSrc.fwdIf6;
        }
        if (ifc == null) {
            return true;
        }
        if (tunMode == null) {
            return true;
        }
        switch (tunMode) {
            case gre:
                tunGRE = new prtGre(fwd);
                tunGRE.setEndpoints(ifc, tunTrg, true);
                tunGRE.setUpper(ethtyp);
                tunGRE.sendingTOS = tunTOS;
                tunGRE.sendingFLW = tunFLW;
                tunGRE.sendingTTL = tunTTL;
                tunGRE.tunnelKey = tunKey;
                tunGRE.tunnelSum = tunSum;
                tunGRE.tunnelSeq = tunSeq;
                lower = tunGRE;
                break;
            case udpgre:
                tunUdpGre = new clntUdpGre();
                tunUdpGre.vrf = tunVrf;
                tunUdpGre.srcIfc = tunSrc;
                tunUdpGre.target = "" + tunTrg;
                tunUdpGre.sendingTOS = tunTOS;
                tunUdpGre.sendingFLW = tunFLW;
                tunUdpGre.sendingTTL = tunTTL;
                tunUdpGre.tunnelKey = tunKey;
                tunUdpGre.tunnelSum = tunSum;
                tunUdpGre.tunnelSeq = tunSeq;
                tunUdpGre.setUpper(ethtyp);
                tunUdpGre.workStart();
                lower = tunUdpGre;
                break;
            case amt:
                tunAmt = new clntAmt();
                tunAmt.vrf = tunVrf;
                tunAmt.srcIfc = tunSrc;
                tunAmt.target = "" + tunTrg;
                tunAmt.prtR = tunKey;
                tunAmt.prtL = tunKey2;
                tunAmt.negotiate = tunSeq;
                tunAmt.sendingTOS = tunTOS;
                tunAmt.sendingFLW = tunFLW;
                tunAmt.sendingTTL = tunTTL;
                tunAmt.setUpper(ethtyp);
                tunAmt.workStart();
                lower = tunAmt;
                break;
            case icmp:
                tunICMP = new prtIcmp(fwd);
                tunICMP.setEndpoints(ifc, tunTrg);
                tunICMP.setUpper(ethtyp);
                tunICMP.sendingTOS = tunTOS;
                tunICMP.sendingFLW = tunFLW;
                tunICMP.sendingTTL = tunTTL;
                tunICMP.tunnelKey = tunKey;
                lower = tunICMP;
                break;
            case pim:
                tunPIM = new prtPim(fwd);
                tunPIM.setEndpoints(ifc, tunTrg);
                tunPIM.setUpper(ethtyp);
                tunPIM.sendingTOS = tunTOS;
                tunPIM.sendingFLW = tunFLW;
                tunPIM.sendingTTL = tunTTL;
                lower = tunPIM;
                break;
            case lisp:
                tunLisp = new clntLisp();
                tunLisp.udp = tunVrf.getUdp(tunTrg);
                tunLisp.fwdIfc = tunSrc.getFwdIfc(tunTrg);
                tunLisp.target = tunTrg.copyBytes();
                tunLisp.prtR = tunKey;
                tunLisp.prtL = tunKey2;
                tunLisp.sendingTOS = tunTOS;
                tunLisp.sendingFLW = tunFLW;
                tunLisp.sendingTTL = tunTTL;
                tunLisp.setUpper(ethtyp);
                tunLisp.workStart();
                lower = tunLisp;
                break;
            case minenc:
                tunMinenc = new prtMinenc(fwd);
                tunMinenc.setEndpoints(ifc, tunTrg);
                tunMinenc.setUpper(ethtyp);
                tunMinenc.sendingTOS = tunTOS;
                tunMinenc.sendingFLW = tunFLW;
                tunMinenc.sendingTTL = tunTTL;
                lower = tunMinenc;
                break;
            case pipe:
                tunPipe = new prtPipe(fwd);
                tunPipe.setEndpoints(ifc, tunTrg);
                tunPipe.setUpper(ethtyp);
                tunPipe.sendingTOS = tunTOS;
                tunPipe.sendingFLW = tunFLW;
                tunPipe.sendingTTL = tunTTL;
                tunPipe.vpnId = tunKey;
                lower = tunPipe;
                break;
            case nos:
                tunNos = new prtNos(fwd);
                tunNos.setEndpoints(ifc, tunTrg);
                tunNos.setUpper(ethtyp);
                tunNos.sendingTOS = tunTOS;
                tunNos.sendingFLW = tunFLW;
                tunNos.sendingTTL = tunTTL;
                lower = tunNos;
                break;
            case ipcomp:
                tunIpcomp = new prtIpcomp(fwd);
                tunIpcomp.setEndpoints(ifc, tunTrg);
                tunIpcomp.setUpper(ethtyp);
                tunIpcomp.sendingTOS = tunTOS;
                tunIpcomp.sendingFLW = tunFLW;
                tunIpcomp.sendingTTL = tunTTL;
                lower = tunIpcomp;
                break;
            case ipenc:
                tunIpenc = new prtIpenc(fwd);
                tunIpenc.setEndpoints(ifc, tunTrg);
                tunIpenc.setUpper(ethtyp);
                tunIpenc.sendingTOS = tunTOS;
                tunIpenc.sendingFLW = tunFLW;
                tunIpenc.sendingTTL = tunTTL;
                tunIpenc.flowId = tunKey;
                lower = tunIpenc;
                break;
            case tmux:
                tunTmux = new prtTmux(fwd);
                tunTmux.setEndpoints(ifc, tunTrg);
                tunTmux.setUpper(ethtyp);
                tunTmux.sendingTOS = tunTOS;
                tunTmux.sendingFLW = tunFLW;
                tunTmux.sendingTTL = tunTTL;
                lower = tunTmux;
                break;
            case mplsip:
                tunMplsip = new prtMplsIp(fwd);
                tunMplsip.setEndpoints(ifc, tunTrg, true);
                tunMplsip.setUpper(ethtyp);
                tunMplsip.sendingTOS = tunTOS;
                tunMplsip.sendingFLW = tunFLW;
                tunMplsip.sendingTTL = tunTTL;
                lower = tunMplsip;
                break;
            case mplsudp:
                tunMplsudp = new clntMplsUdp();
                tunMplsudp.udp = tunVrf.getUdp(tunTrg);
                tunMplsudp.fwdIfc = tunSrc.getFwdIfc(tunTrg);
                tunMplsudp.target = tunTrg.copyBytes();
                tunMplsudp.prtR = tunKey;
                tunMplsudp.prtL = tunKey2;
                tunMplsudp.sendingTOS = tunTOS;
                tunMplsudp.sendingFLW = tunFLW;
                tunMplsudp.sendingTTL = tunTTL;
                tunMplsudp.setUpper(ethtyp);
                tunMplsudp.workStart();
                lower = tunMplsudp;
                break;
            case swipe:
                if (tunPrt == null) {
                    return true;
                }
                tunSwipe = new prtSwipe(fwd);
                tunSwipe.preshared = tunPrt.preshared;
                tunSwipe.transform = tunPrt.trans;
                tunSwipe.setEndpoints(ifc, tunTrg);
                tunSwipe.setUpper(ethtyp);
                tunSwipe.sendingTOS = tunTOS;
                tunSwipe.sendingFLW = tunFLW;
                tunSwipe.sendingTTL = tunTTL;
                lower = tunSwipe;
                break;
            case openvpn:
                if (tunPrt == null) {
                    return true;
                }
                tunOpenvpn = new clntOpenvpn();
                tunOpenvpn.preshared = tunPrt.preshared;
                tunOpenvpn.transform = tunPrt.trans;
                tunOpenvpn.replayCheck = tunPrt.replay;
                tunOpenvpn.sendingTOS = tunTOS;
                tunOpenvpn.sendingFLW = tunFLW;
                tunOpenvpn.sendingTTL = tunTTL;
                tunOpenvpn.vrf = tunVrf;
                tunOpenvpn.srcIfc = tunSrc;
                tunOpenvpn.target = "" + tunTrg;
                tunOpenvpn.prtR = tunKey;
                tunOpenvpn.prtL = tunKey2;
                tunOpenvpn.workStart();
                tunOpenvpn.setUpper(ethtyp);
                lower = tunOpenvpn;
                break;
            case wireguard:
                if (tunPrt == null) {
                    return true;
                }
                tunWireguard = new clntWireguard();
                tunWireguard.preshared = tunPrt.preshared;
                tunWireguard.replayCheck = tunPrt.replay;
                tunWireguard.sendingTOS = tunTOS;
                tunWireguard.sendingFLW = tunFLW;
                tunWireguard.sendingTTL = tunTTL;
                tunWireguard.vrf = tunVrf;
                tunWireguard.srcIfc = tunSrc;
                tunWireguard.target = "" + tunTrg;
                tunWireguard.prtR = tunKey;
                tunWireguard.prtL = tunKey2;
                tunWireguard.workStart();
                tunWireguard.setUpper(ethtyp);
                lower = tunWireguard;
                break;
            case satp:
                if (tunPrt == null) {
                    return true;
                }
                tunSatp = new clntSatp();
                tunSatp.preshared = tunPrt.preshared;
                tunSatp.transform = tunPrt.trans;
                tunSatp.sendingTOS = tunTOS;
                tunSatp.sendingFLW = tunFLW;
                tunSatp.sendingTTL = tunTTL;
                tunSatp.vrf = tunVrf;
                tunSatp.srcIfc = tunSrc;
                tunSatp.target = "" + tunTrg;
                tunSatp.prtR = tunKey;
                tunSatp.prtL = tunKey2;
                tunSatp.workStart();
                tunSatp.setUpper(ethtyp);
                lower = tunSatp;
                break;
            case inlsp:
                if (tunPrt == null) {
                    return true;
                }
                tunInlsp = new prtInlsp(fwd);
                tunInlsp.preshared = tunPrt.preshared;
                tunInlsp.transform = tunPrt.trans;
                tunInlsp.setEndpoints(ifc, tunTrg);
                tunInlsp.setUpper(ethtyp);
                tunInlsp.sendingTOS = tunTOS;
                tunInlsp.sendingFLW = tunFLW;
                tunInlsp.sendingTTL = tunTTL;
                tunInlsp.said = tunKey;
                lower = tunInlsp;
                break;
            case skip:
                if (tunPrt == null) {
                    return true;
                }
                tunSkip = new prtSkip(fwd);
                tunSkip.preshared = tunPrt.preshared;
                tunSkip.transform = tunPrt.trans;
                tunSkip.setEndpoints(ifc, tunTrg);
                tunSkip.setUpper(ethtyp);
                tunSkip.sendingTOS = tunTOS;
                tunSkip.sendingFLW = tunFLW;
                tunSkip.sendingTTL = tunTTL;
                lower = tunSkip;
                break;
            case ipip:
                tunIPIP = new prtIpIpTyp(fwd);
                tunIPIP.setEndpoints(ifc, tunTrg);
                tunIPIP.setUpper(ethtyp);
                tunIPIP.setTxTOS(tunTOS);
                tunIPIP.setTxFLW(tunFLW);
                tunIPIP.setTxTTL(tunTTL);
                lower = tunIPIP;
                break;
            case Sto4:
                tun6to4 = new prt6to4(tunTrg, tunKey);
                tun6to4.setUpper(ethtyp);
                lower = tun6to4;
                break;
            case srv6:
                tunSrv6 = new prtSrv6(tunTrg, ethtyp, tunVrf.fwd4, tunVrf.fwd6);
                tunSrv6.setUpper(ethtyp);
                lower = tunSrv6;
                break;
            case ipsec:
                if (tunPrt == null) {
                    return true;
                }
                if (tunPrt.ikeVer == 1) {
                    tunIPsec1 = new secIsakmp();
                    tunIPsec1.role = tunPrt.role;
                    tunIPsec1.preshared = tunPrt.preshared;
                    tunIPsec1.sendingTTL = tunTTL;
                    tunIPsec1.sendingTOS = tunTOS;
                    tunIPsec1.sendingFLW = tunFLW;
                    tunIPsec1.transform = tunPrt.trans;
                    tunIPsec1.ipv6 = tunPrt.ipv6;
                    tunIPsec1.replayCheck = tunPrt.replay;
                    tunIPsec1.workStart(fwd, udp, ifc, tunTrg);
                    tunIPsec1.setUpper(ethtyp);
                    lower = tunIPsec1;
                } else {
                    tunIPsec2 = new secIke();
                    tunIPsec2.role = tunPrt.role;
                    tunIPsec2.preshared = tunPrt.preshared;
                    tunIPsec2.sendingTTL = tunTTL;
                    tunIPsec2.sendingTOS = tunTOS;
                    tunIPsec2.sendingFLW = tunFLW;
                    tunIPsec2.transform = tunPrt.trans;
                    tunIPsec2.ipv6 = tunPrt.ipv6;
                    tunIPsec2.replayCheck = tunPrt.replay;
                    tunIPsec2.workStart(fwd, udp, ifc, tunTrg);
                    tunIPsec2.setUpper(ethtyp);
                    lower = tunIPsec2;
                }
                break;
            case pckOudp:
                if (tunKey < 1) {
                    return true;
                }
                tunPckOudp = new clntPckOudp();
                tunPckOudp.vrf = tunVrf;
                tunPckOudp.srcIfc = tunSrc;
                tunPckOudp.target = "" + tunTrg;
                tunPckOudp.prtR = tunKey;
                tunPckOudp.prtL = tunKey2;
                tunPckOudp.sendingTTL = tunTTL;
                tunPckOudp.sendingTOS = tunTOS;
                tunPckOudp.sendingFLW = tunFLW;
                tunPckOudp.setUpper(ethtyp);
                tunPckOudp.workStart();
                lower = tunPckOudp;
                break;
            case pckOip:
                if (tunKey < 1) {
                    return true;
                }
                tunPckOip = new prtPckOip(fwd);
                tunPckOip.setEndpoints(ifc, tunTrg, tunKey);
                tunPckOip.setUpper(ethtyp);
                tunPckOip.sendingTOS = tunTOS;
                tunPckOip.sendingFLW = tunFLW;
                tunPckOip.sendingTTL = tunTTL;
                lower = tunPckOip;
                break;
            case l2tp3:
                tunL2tp3 = new clntL2tp3();
                tunL2tp3.pwType = packLdpPwe.pwtIp;
                tunL2tp3.vrf = tunVrf;
                tunL2tp3.srcIfc = tunSrc;
                tunL2tp3.target = "" + tunTrg;
                tunL2tp3.vcid = "" + tunKey;
                tunL2tp3.direction = tunTrg.compare(tunTrg, ifc.addr) < 0;
                tunL2tp3.sendingTOS = tunTOS;
                tunL2tp3.sendingFLW = tunFLW;
                tunL2tp3.sendingTTL = tunTTL;
                tunL2tp3.setUpper(ethtyp);
                tunL2tp3.workStart();
                lower = tunL2tp3;
                break;
            case polka:
                tunPolka = new clntPolka();
                tunPolka.fwdCor = tunVrf.getFwd(tunTrg);
                tunPolka.fwdIfc = tunSrc.getFwdIfc(tunTrg);
                tunPolka.target = tunTrg.copyBytes();
                tunPolka.setTargets(tunFQDN);
                tunFQDN = tunPolka.getTargets();
                tunPolka.ttl = tunTTL;
                tunPolka.verify = tunSum;
                tunPolka.setUpper(ethtyp);
                tunPolka.workStart();
                lower = tunPolka;
                break;
            case mpolka:
                tunMpolka = new clntMpolka();
                tunMpolka.fwdCor = tunVrf.getFwd(tunTrg);
                tunMpolka.setTargets(tunFQDN);
                tunFQDN = tunMpolka.getTargets();
                tunMpolka.ttl = tunTTL;
                tunMpolka.verify = tunSum;
                tunMpolka.setUpper(ethtyp);
                tunMpolka.workStart();
                lower = tunMpolka;
                break;
            case pweOmpls:
                tunPweOmpls = new clntMplsPwe();
                tunPweOmpls.pwType = packLdpPwe.pwtIp;
                tunPweOmpls.vrf = tunVrf;
                tunPweOmpls.srcIfc = tunSrc;
                tunPweOmpls.target = "" + tunTrg;
                tunPweOmpls.vcid = tunKey;
                tunPweOmpls.ctrlWrd = tunSeq;
                tunPweOmpls.descr = description.length() > 0 ? description : name;
                tunPweOmpls.setUpper(ethtyp);
                tunPweOmpls.workStart();
                lower = tunPweOmpls;
                break;
            case expBun:
                if (tunFQDN == null) {
                    return true;
                }
                tunExpBun = new clntMplsExp();
                tunExpBun.setTargets(tunFQDN);
                tunFQDN = tunExpBun.getTargets();
                tunExpBun.expr = tunTOS;
                tunExpBun.entr = tunFLW;
                tunExpBun.ttl = tunTTL;
                tunExpBun.setUpper(ethtyp);
                tunExpBun.workStart();
                lower = tunExpBun;
                break;
            case srMpls:
                if (tunFQDN == null) {
                    return true;
                }
                tunSrMpls = new clntMplsSr();
                tunSrMpls.fwdCor = tunVrf.getFwd(tunTrg);
                tunSrMpls.fwdIfc = tunSrc.getFwdIfc(tunTrg);
                tunSrMpls.target = tunTrg.copyBytes();
                tunSrMpls.setTargets(tunFQDN);
                tunFQDN = tunSrMpls.getTargets();
                tunSrMpls.expr = tunTOS;
                tunSrMpls.entr = tunFLW;
                tunSrMpls.ttl = tunTTL;
                tunSrMpls.prioS = tunPri;
                tunSrMpls.prioH = tunPri;
                tunSrMpls.bndwdt = ethtyp.getBandwidth();
                tunSrMpls.setUpper(ethtyp);
                tunSrMpls.workStart();
                lower = tunSrMpls;
                break;
            case srExt:
                if (tunFQDN == null) {
                    return true;
                }
                tunSrExt = new clntSrExt();
                tunSrExt.fwdCor = tunVrf.getFwd(tunTrg);
                tunSrExt.target = tunTrg.copyBytes();
                tunSrExt.setTargets(tunFQDN);
                tunFQDN = tunSrExt.getTargets();
                tunSrExt.tos = tunTOS;
                tunSrExt.flw = tunFLW;
                tunSrExt.ttl = tunTTL;
                tunSrExt.setUpper(ethtyp);
                tunSrExt.workStart();
                lower = tunSrExt;
                break;
            case pceSr:
                if (tunFQDN == null) {
                    return true;
                }
                tunSrMpls = new clntMplsSr();
                tunSrMpls.fwdCor = tunVrf.getFwd(tunTrg);
                tunSrMpls.fwdIfc = tunSrc.getFwdIfc(tunTrg);
                tunSrMpls.target = tunTrg.copyBytes();
                tunSrMpls.setTargets("" + tunTrg);
                tunSrMpls.pcep = tunFQDN;
                tunSrMpls.expr = tunTOS;
                tunSrMpls.entr = tunFLW;
                tunSrMpls.ttl = tunTTL;
                tunSrMpls.prioS = tunPri;
                tunSrMpls.prioH = tunPri;
                tunSrMpls.bndwdt = ethtyp.getBandwidth();
                tunSrMpls.setUpper(ethtyp);
                tunSrMpls.workStart();
                lower = tunSrMpls;
                break;
            case pceTe:
                if (tunFQDN == null) {
                    return true;
                }
                tunTeP2p = new clntMplsTeP2p();
                tunTeP2p.fwdCor = tunVrf.getFwd(tunTrg);
                tunTeP2p.fwdIfc = tunSrc.getFwdIfc(tunTrg);
                tunTeP2p.target = tunTrg.copyBytes();
                tunTeP2p.descr = cfgAll.hostName + ":" + name;
                tunTeP2p.pcep = tunFQDN;
                tunTeP2p.expr = tunTOS;
                tunTeP2p.entr = tunFLW;
                tunTeP2p.ttl = tunTTL;
                tunTeP2p.ascId = tunAscId;
                tunTeP2p.ascId2 = tunAscId2;
                tunTeP2p.ascAdr = tunAscAdr;
                tunTeP2p.prioS = tunPri;
                tunTeP2p.prioH = tunPri;
                tunTeP2p.bndwdt = ethtyp.getBandwidth();
                tunTeP2p.recRou = tunSeq;
                tunTeP2p.setUpper(ethtyp);
                tunTeP2p.workStart();
                lower = tunTeP2p;
                break;
            case teP2p:
                tunTeP2p = new clntMplsTeP2p();
                tunTeP2p.fwdCor = tunVrf.getFwd(tunTrg);
                tunTeP2p.fwdIfc = tunSrc.getFwdIfc(tunTrg);
                tunTeP2p.target = tunTrg.copyBytes();
                tunTeP2p.setMiddles(tunFQDN);
                tunTeP2p.descr = cfgAll.hostName + ":" + name;
                tunTeP2p.expr = tunTOS;
                tunTeP2p.entr = tunFLW;
                tunTeP2p.ttl = tunTTL;
                tunTeP2p.ascId = tunAscId;
                tunTeP2p.ascId2 = tunAscId2;
                tunTeP2p.ascAdr = tunAscAdr;
                tunTeP2p.prioS = tunPri;
                tunTeP2p.prioH = tunPri;
                tunTeP2p.bndwdt = ethtyp.getBandwidth();
                tunTeP2p.recRou = tunSeq;
                tunTeP2p.setUpper(ethtyp);
                tunTeP2p.workStart();
                lower = tunTeP2p;
                break;
            case teP2mp:
                if (tunFQDN == null) {
                    return true;
                }
                tunTeP2mp = new clntMplsTeP2mp();
                tunTeP2mp.fwdCor = tunVrf.getFwd(tunTrg);
                tunTeP2mp.srcIfc = tunSrc;
                tunTeP2mp.trgId = tunTrg.copyBytes();
                tunTeP2mp.setTargets(tunFQDN);
                tunFQDN = tunTeP2mp.getTargets();
                tunTeP2mp.descr = cfgAll.hostName + ":" + name;
                tunTeP2mp.expr = tunTOS;
                tunTeP2mp.entr = tunFLW;
                tunTeP2mp.ttl = tunTTL;
                tunTeP2mp.prioS = tunPri;
                tunTeP2mp.prioH = tunPri;
                tunTeP2mp.bndwdt = ethtyp.getBandwidth();
                tunTeP2mp.recRou = tunSeq;
                tunTeP2mp.setUpper(ethtyp);
                tunTeP2mp.workStart();
                lower = tunTeP2mp;
                break;
            case bier:
                if (tunFQDN == null) {
                    return true;
                }
                tunBier = new clntMplsBier();
                tunBier.fwdCor = tunVrf.getFwd(tunTrg);
                tunBier.srcId = tunKey;
                tunBier.setTargets(tunFQDN);
                tunFQDN = tunBier.getTargets();
                tunBier.expr = tunTOS;
                tunBier.entr = tunFLW;
                tunBier.ttl = tunTTL;
                tunBier.setUpper(ethtyp);
                tunBier.workStart();
                lower = tunBier;
                break;
            case ldpP2p:
                tunLdpP2p = new clntMplsLdpP2p();
                tunLdpP2p.fwdCor = tunVrf.getFwd(tunTrg);
                tunLdpP2p.trgId = tunKey;
                tunLdpP2p.target = tunTrg.copyBytes();
                tunLdpP2p.expr = tunTOS;
                tunLdpP2p.entr = tunFLW;
                tunLdpP2p.ttl = tunTTL;
                tunLdpP2p.setUpper(ethtyp);
                tunLdpP2p.workStart();
                lower = tunLdpP2p;
                break;
            case ldpP2mp:
                tunLdpP2mp = new clntMplsLdpP2mp();
                tunLdpP2mp.vrf = tunVrf;
                tunLdpP2mp.mp2mp = false;
                tunLdpP2mp.trgId = tunKey;
                tunLdpP2mp.target = "" + tunTrg;
                tunLdpP2mp.expr = tunTOS;
                tunLdpP2mp.entr = tunFLW;
                tunLdpP2mp.ttl = tunTTL;
                tunLdpP2mp.setUpper(ethtyp);
                tunLdpP2mp.workStart();
                lower = tunLdpP2mp;
                break;
            case ldpMp2mp:
                tunLdpP2mp = new clntMplsLdpP2mp();
                tunLdpP2mp.vrf = tunVrf;
                tunLdpP2mp.mp2mp = true;
                tunLdpP2mp.trgId = tunKey;
                tunLdpP2mp.target = "" + tunTrg;
                tunLdpP2mp.expr = tunTOS;
                tunLdpP2mp.entr = tunFLW;
                tunLdpP2mp.ttl = tunTTL;
                tunLdpP2mp.setUpper(ethtyp);
                tunLdpP2mp.workStart();
                lower = tunLdpP2mp;
                break;
            case vxlan:
                if (tunKey < 1) {
                    return true;
                }
                tunVxlan = new clntVxlan();
                tunVxlan.target = "" + tunTrg;
                tunVxlan.vrf = tunVrf;
                tunVxlan.srcIfc = tunSrc;
                tunVxlan.inst = tunKey;
                tunVxlan.prot = tunPri;
                tunVxlan.sendingTOS = tunTOS;
                tunVxlan.sendingFLW = tunFLW;
                tunVxlan.sendingTTL = tunTTL;
                tunVxlan.wildcard = tunSeq;
                tunVxlan.setUpper(ethtyp);
                tunVxlan.workStart();
                lower = tunVxlan;
                break;
            case geneve:
                if (tunKey < 1) {
                    return true;
                }
                tunGeneve = new clntGeneve();
                tunGeneve.target = "" + tunTrg;
                tunGeneve.vrf = tunVrf;
                tunGeneve.srcIfc = tunSrc;
                tunGeneve.vni = tunKey;
                tunGeneve.sendingTOS = tunTOS;
                tunGeneve.sendingFLW = tunFLW;
                tunGeneve.sendingTTL = tunTTL;
                tunGeneve.setUpper(ethtyp);
                tunGeneve.workStart();
                lower = tunGeneve;
                break;
            case capwap:
                tunCapwap = new clntCapwap();
                tunCapwap.target = "" + tunTrg;
                tunCapwap.vrf = tunVrf;
                tunCapwap.srcIfc = tunSrc;
                tunCapwap.sendingTOS = tunTOS;
                tunCapwap.sendingFLW = tunFLW;
                tunCapwap.sendingTTL = tunTTL;
                tunCapwap.setUpper(ethtyp);
                tunCapwap.workStart();
                lower = tunCapwap;
                break;
            case lwapp:
                tunLwapp = new clntLwapp();
                tunLwapp.target = "" + tunTrg;
                tunLwapp.vrf = tunVrf;
                tunLwapp.srcIfc = tunSrc;
                tunLwapp.sendingTOS = tunTOS;
                tunLwapp.sendingFLW = tunFLW;
                tunLwapp.sendingTTL = tunTTL;
                tunLwapp.setUpper(ethtyp);
                tunLwapp.workStart();
                lower = tunLwapp;
                break;
            case erspan:
                if (tunKey < 1) {
                    return true;
                }
                tunErspan = new clntErspan();
                tunErspan.target = "" + tunTrg;
                tunErspan.vrf = tunVrf;
                tunErspan.srcIfc = tunSrc;
                tunErspan.spnid = tunKey;
                tunErspan.vlnid = tunKey;
                tunErspan.sendingTOS = tunTOS;
                tunErspan.sendingFLW = tunFLW;
                tunErspan.sendingTTL = tunTTL;
                tunErspan.setUpper(ethtyp);
                tunErspan.workStart();
                lower = tunErspan;
                break;
            case dlsw:
                tunDlsw = new clntDlsw();
                tunDlsw.target = "" + tunTrg;
                tunDlsw.vrf = tunVrf;
                tunDlsw.srcIfc = tunSrc;
                tunDlsw.sendingTOS = tunTOS;
                tunDlsw.sendingFLW = tunFLW;
                tunDlsw.sendingTTL = tunTTL;
                tunDlsw.setUpper(ethtyp);
                tunDlsw.workStart();
                lower = tunDlsw;
                break;
            case etherip:
                tunEtherip = new clntEtherIp();
                tunEtherip.target = "" + tunTrg;
                tunEtherip.vrf = tunVrf;
                tunEtherip.srcIfc = tunSrc;
                tunEtherip.sendingTOS = tunTOS;
                tunEtherip.sendingFLW = tunFLW;
                tunEtherip.sendingTTL = tunTTL;
                tunEtherip.setUpper(ethtyp);
                tunEtherip.workStart();
                lower = tunEtherip;
                break;
            case sreth:
                tunSreth = new clntSrEth();
                tunSreth.target = "" + tunTrg;
                tunSreth.vrf = tunVrf;
                tunSreth.srcIfc = tunSrc;
                tunSreth.sendingTOS = tunTOS;
                tunSreth.sendingFLW = tunFLW;
                tunSreth.sendingTTL = tunTTL;
                tunSreth.setUpper(ethtyp);
                tunSreth.workStart();
                lower = tunSreth;
                break;
            case uti:
                tunUti = new clntUti();
                tunUti.target = "" + tunTrg;
                tunUti.vrf = tunVrf;
                tunUti.srcIfc = tunSrc;
                tunUti.tunKey = tunKey;
                tunUti.sendingTOS = tunTOS;
                tunUti.sendingFLW = tunFLW;
                tunUti.sendingTTL = tunTTL;
                tunUti.setUpper(ethtyp);
                tunUti.workStart();
                lower = tunUti;
                break;
            case nvgre:
                tunNvgre = new clntNvGre();
                tunNvgre.target = "" + tunTrg;
                tunNvgre.vrf = tunVrf;
                tunNvgre.srcIfc = tunSrc;
                tunNvgre.vsid = tunKey;
                tunNvgre.sendingTOS = tunTOS;
                tunNvgre.sendingFLW = tunFLW;
                tunNvgre.sendingTTL = tunTTL;
                tunNvgre.setUpper(ethtyp);
                tunNvgre.workStart();
                lower = tunNvgre;
                break;
            default:
                lower = new ifcNull();
                return true;
        }
        ethtyp.setState(state.states.up);
        return false;
    }

    /**
     * setup template usage
     *
     * @param ifc source interface
     */
    public synchronized void setup2template(cfgIfc ifc) {
        if (type == ifaceType.template) {
            return;
        }
        if (ifc.type != ifaceType.template) {
            return;
        }
        template = ifc;
        List<String> l = ifc.getShRun(1);
        l = userFilter.filterText(l, notemplF);
        for (int i = 1; i < l.size() - 2; i++) {
            doCfgStr(new cmds("template", l.get(i).trim()));
        }
    }

    /**
     * setup for transparent proxy
     *
     * @param clnt client to use
     */
    public synchronized void setup2transproxy(clntProxy clnt) {
        clear2transproxy();
        transProxy = new ipProxy(clnt);
        ethtyp.addET(ipIfc4.type, "ip4", transProxy);
        ethtyp.updateET(ipIfc4.type, transProxy);
        ethtyp.addET(ipIfc4arp.type, "arp4", transProxy);
        ethtyp.updateET(ipIfc4arp.type, transProxy);
        ethtyp.addET(ipIfc6.type, "ip6", transProxy);
        ethtyp.updateET(ipIfc6.type, transProxy);
    }

    /**
     * clear transparent proxy
     */
    public synchronized void clear2transproxy() {
        if (transProxy == null) {
            return;
        }
        transProxy.closeUp();
        transProxy = null;
        ethtyp.delET(ipIfc4.type);
        ethtyp.delET(ipIfc4arp.type);
        ethtyp.delET(ipIfc6.type);
    }

    /**
     * propagate ethertype state
     */
    public void propagateEthtypState() {
        if ((bundleIfc != null) || (bridgeIfc != null)) {
            return;
        }
        if ((bundleHed == null) && (bridgeHed == null)) {
            return;
        }
        for (int i = 0; i < cfgAll.ifaces.size(); i++) {
            cfgIfc ntry = cfgAll.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if ((ntry.bundleIfc == null) && (ntry.bridgeIfc == null)) {
                continue;
            }
            if ((ntry.bundleHed != bundleHed) || (ntry.bridgeHed != bridgeHed)) {
                continue;
            }
            ntry.ethtyp.forcedDN = ethtyp.forcedDN;
            ntry.ethtyp.forcedUP = ethtyp.forcedUP;
            ntry.ethtyp.forcedMTU = ethtyp.forcedMTU;
            ntry.ethtyp.propagateState();
        }
    }

    /**
     * get encapsulator protocol handler
     *
     * @return handler
     */
    public synchronized ifcUp getEncapProto() {
        if (hdlc != null) {
            return hdlc;
        }
        if (isdn != null) {
            return isdn;
        }
        if (ipOnly != null) {
            return ipOnly;
        }
        if (raw != null) {
            return raw;
        }
        if (sep != null) {
            return sep;
        }
        if (frmrly != null) {
            return frmrly;
        }
        if (ppp != null) {
            return ppp;
        }
        if (lapb != null) {
            return lapb;
        }
        if (atmdxi != null) {
            return atmdxi;
        }
        if (atmsar != null) {
            return atmsar;
        }
        if (arcnet != null) {
            return arcnet;
        }
        if (infiniband != null) {
            return infiniband;
        }
        return null;
    }

    /**
     * set lower layer
     *
     * @param l lower handler
     */
    public void setLowerHandler(ifcDn l) {
        lower = l;
        ifcUp e = getEncapProto();
        if (e == null) {
            return;
        }
        lower.setUpper(e);
    }

    /**
     * setup interface pppoe client
     *
     * @param dialer dialer interface to use
     * @return false on success, true on error
     */
    public synchronized boolean setup2pppoeClnt(cfgIfc dialer) {
        if (pppoeC != null) {
            pppoeC.clnIfc.getEncapProto().setState(state.states.close);
            pppoeC.clnIfc.lower = new ifcNull();
            pppoeC.restartTimer(true);
            pppoeC = null;
            ethtyp.delET(packPppOE.typeCtr);
            ethtyp.delET(packPppOE.typeDat);
        }
        if (dialer == null) {
            return true;
        }
        if (dialer.type != ifaceType.dialer) {
            return true;
        }
        ifcUp enc = dialer.getEncapProto();
        if (enc == null) {
            return true;
        }
        pppoeC = new ifcP2pOEclnt();
        pppoeC.clnIfc = dialer;
        dialer.lower = pppoeC;
        pppoeC.setUpper(enc);
        ethtyp.addET(packPppOE.typeCtr, "pppoeCctrl", pppoeC);
        ethtyp.updateET(packPppOE.typeCtr, pppoeC);
        ethtyp.addET(packPppOE.typeDat, "pppoeCdata", pppoeC);
        ethtyp.updateET(packPppOE.typeDat, pppoeC);
        enc.setState(state.states.up);
        return false;
    }

    /**
     * setup interface pppoe server
     *
     * @param dialer dialer interface to use
     * @param cmd optional parameters
     * @return false on success, true on error
     */
    public synchronized boolean setup2pppoeServ(cfgIfc dialer, cmds cmd) {
        if (pppoeS != null) {
            pppoeS.closeUp();
            pppoeS = null;
            ethtyp.delET(packPppOE.typeCtr);
            ethtyp.delET(packPppOE.typeDat);
        }
        if (dialer == null) {
            return true;
        }
        if (dialer.type != ifaceType.dialer) {
            return true;
        }
        dialer.lower = new ifcNull();
        pppoeS = new ifcP2pOEserv();
        pppoeS.clnIfc = dialer;
        pppoeS.pktIfc = this;
        ethtyp.addET(packPppOE.typeCtr, "pppoeSctrl", pppoeS);
        ethtyp.updateET(packPppOE.typeCtr, pppoeS);
        ethtyp.addET(packPppOE.typeDat, "pppoeSdata", pppoeS);
        ethtyp.updateET(packPppOE.typeDat, pppoeS);
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("name")) {
                pppoeS.serviceNam = cmd.word();
                continue;
            }
            if (a.equals("delay")) {
                pppoeS.serviceDly = bits.str2num(cmd.word());
                continue;
            }
        }
        return false;
    }

    /**
     * setup interface pppoe relay
     *
     * @param serial serial interface to use
     * @param cmd optional parameters
     * @return false on success, true on error
     */
    public synchronized boolean setup2pppoeRely(cfgIfc serial, cmds cmd) {
        if (pppoeR != null) {
            if (pppoeR.ser != null) {
                pppoeR.clnIfc.ethtyp.delET(-1);
            } else {
                pppoeR.clnIfc.getEncapProto().setState(state.states.close);
                pppoeR.clnIfc.lower = new ifcNull();
            }
            pppoeR.closeUp();
            pppoeR = null;
            ethtyp.delET(packPppOE.typeCtr);
            ethtyp.delET(packPppOE.typeDat);
        }
        if (serial == null) {
            return true;
        }
        switch (serial.type) {
            case serial:
            case virtppp:
                pppoeR = new ifcP2pOErely(true);
                serial.ethtyp.addET(-1, "pppoeR", pppoeR.ser);
                serial.ethtyp.updateET(-1, pppoeR.ser);
                break;
            case dialer:
                ifcUp enc = serial.getEncapProto();
                if (enc == null) {
                    return true;
                }
                pppoeR = new ifcP2pOErely(false);
                pppoeR.diaI.setUpper(enc);
                enc.setState(state.states.up);
                serial.lower = pppoeR.diaI;
                break;
            default:
                return true;
        }
        pppoeR.clnIfc = serial;
        ethtyp.addET(packPppOE.typeCtr, "pppoeRctrl", pppoeR);
        ethtyp.updateET(packPppOE.typeCtr, pppoeR);
        ethtyp.addET(packPppOE.typeDat, "pppoeRdata", pppoeR);
        ethtyp.updateET(packPppOE.typeDat, pppoeR);
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("name")) {
                pppoeR.serviceNam = cmd.word();
                continue;
            }
            if (a.equals("delay")) {
                pppoeR.serviceDly = bits.str2num(cmd.word());
                continue;
            }
        }
        return false;
    }

    /**
     * setup interface eapol client
     *
     * @param user username to use
     * @param pass password to use
     * @return false on success, true on error
     */
    public synchronized boolean setup2eapolClnt(String user, String pass) {
        if (eapolC != null) {
            eapolC.restartTimer(true);
            eapolC = null;
            ethtyp.delET(packEapOL.type);
        }
        if (user == null) {
            return true;
        }
        if (pass == null) {
            return true;
        }
        eapolC = new ifcEapOLclnt();
        eapolC.username = user;
        eapolC.password = pass;
        ethtyp.addET(packEapOL.type, "eapolC", eapolC);
        ethtyp.updateET(packEapOL.type, eapolC);
        return false;
    }

    /**
     * setup interface eapol server
     *
     * @param auth authentication list
     * @return false on success, true on error
     */
    public synchronized boolean setup2eapolServ(cfgAuther auth) {
        if (eapolS != null) {
            eapolS = null;
        }
        if (auth == null) {
            return true;
        }
        eapolS = new ifcEapOLserv(auth.getAuther());
        ethtyp.addET(packEapOL.type, "eapolS", eapolS);
        ethtyp.updateET(packEapOL.type, eapolS);
        return false;
    }

    /**
     * update ip for mpls packet
     */
    public synchronized void update2mpls() {
        if (ipIf4 != null) {
            ipIf4.setMpls(mplsPack);
        }
        if (ipIf6 != null) {
            ipIf6.setMpls(mplsPack);
        }
    }

    /**
     * setup interface mpls packet
     */
    public synchronized void setup2mpls() {
        if (vrfFor == null) {
            return;
        }
        clear2mpls();
        mplsPack = new ipMpls(vrfFor.fwd4, vrfFor.fwd6, ethtyp);
        mplsPack.register2eth();
        update2mpls();
    }

    /**
     * clear interface mpls packet
     */
    public synchronized void clear2mpls() {
        ipMpls hnd = new ipMpls(null, null, ethtyp);
        hnd.unregister2eth();
        mplsPack = null;
        update2mpls();
    }

    /**
     * update ip for mpls packet
     */
    public synchronized void update2polka() {
        if (ipIf4 != null) {
            ipIf4.setPolka(polkaPack);
            ipIf4.setMpolka(mpolkaPack);
        }
        if (ipIf6 != null) {
            ipIf6.setPolka(polkaPack);
            ipIf6.setMpolka(mpolkaPack);
        }
        if (polkaPack != null) {
            polkaPack.fwd4 = vrfFor.fwd4;
            polkaPack.fwd6 = vrfFor.fwd6;
        }
        if (mpolkaPack != null) {
            mpolkaPack.fwd4 = vrfFor.fwd4;
            mpolkaPack.fwd6 = vrfFor.fwd6;
        }
    }

    /**
     * setup interface polka packet
     *
     * @param id local id
     * @param bas crc base
     * @param max crc max
     */
    public synchronized void setup2polka(int id, int bas, int max) {
        if (vrfFor == null) {
            return;
        }
        clear2polka();
        polkaPack = new ifcPolka(id, bas, max);
        ethtyp.addET(ifcPolka.type, "polka", polkaPack);
        ethtyp.updateET(ifcPolka.type, polkaPack);
        update2polka();
    }

    /**
     * clear interface polka packet
     */
    public synchronized void clear2polka() {
        polkaPack = null;
        ethtyp.delET(ifcPolka.type);
        update2polka();
    }

    /**
     * setup interface mpolka packet
     *
     * @param id local id
     * @param bas crc base
     * @param max crc max
     */
    public synchronized void setup2mpolka(int id, int bas, int max) {
        if (vrfFor == null) {
            return;
        }
        clear2mpolka();
        mpolkaPack = new ifcMpolka(id, bas, max);
        ethtyp.addET(ifcMpolka.type, "mpolka", mpolkaPack);
        ethtyp.updateET(ifcMpolka.type, mpolkaPack);
        update2polka();
    }

    /**
     * clear interface polka packet
     */
    public synchronized void clear2mpolka() {
        mpolkaPack = null;
        ethtyp.delET(ifcMpolka.type);
        update2polka();
    }

    /**
     * setup interface nsh packet
     */
    public synchronized void setup2nshFwd() {
        clear2nshFwd();
        nshPack = new ifcNshFwd();
        ethtyp.nshFwd = nshPack;
        ethtyp.addET(ifcNshFwd.type, "nsh", nshPack);
        ethtyp.updateET(ifcNshFwd.type, nshPack);
    }

    /**
     * clear interface nsh packet
     */
    public synchronized void clear2nshFwd() {
        nshPack = null;
        ethtyp.nshFwd = null;
        ethtyp.delET(ifcNshFwd.type);
    }

    /**
     * setup interface nsh packet
     *
     * @param p service path
     * @param i service index
     */
    public synchronized void setup2nshXcn(int p, int i) {
        nshXcon = new ifcNshXcn();
        nshXcon.sp = p;
        nshXcon.si = i;
        ethtyp.addET(-1, "nshx", nshXcon);
        ethtyp.updateET(-1, nshXcon);
        nshXcon.setPromiscous(true);
    }

    /**
     * clear interface nsh packet
     */
    public synchronized void clear2nshXcn() {
        if (nshXcon == null) {
            return;
        }
        ethtyp.delET(-1);
        nshXcon = null;
    }

    /**
     * setup interface ldp processing
     *
     * @param ver ip version
     * @param cmd parameters
     */
    public synchronized void setup2ldp(int ver, cmds cmd) {
        clear2ldp(ver);
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
        if (ifc == null) {
            ifc = this;
        }
        if (ver == 4) {
            if (fwdIf4 == null) {
                return;
            }
            if (ifc.fwdIf4 == null) {
                return;
            }
            mplsLdp4 = new rtrLdpIface(vrfFor.fwd4, vrfFor.udp4, vrfFor.tcp4, fwdIf4, ifc.fwdIf4, ifc);
            mplsLdp4.register2udp();
        } else {
            if (fwdIf6 == null) {
                return;
            }
            if (ifc.fwdIf6 == null) {
                return;
            }
            mplsLdp6 = new rtrLdpIface(vrfFor.fwd6, vrfFor.udp6, vrfFor.tcp6, fwdIf6, ifc.fwdIf6, ifc);
            mplsLdp6.register2udp();
        }
    }

    /**
     * clear interface ldp processing
     *
     * @param ver ip version
     */
    public synchronized void clear2ldp(int ver) {
        if (ver == 4) {
            if (mplsLdp4 == null) {
                return;
            }
            mplsLdp4.unregister2udp();
            mplsLdp4 = null;
        } else {
            if (mplsLdp6 == null) {
                return;
            }
            mplsLdp6.unregister2udp();
            mplsLdp6 = null;
        }
    }

    /**
     * setup targeted ldp session
     *
     * @param trg peer address
     */
    public synchronized void setup2ldptrg(String trg) {
        clntMplsTrg clnt = new clntMplsTrg();
        clnt.target = new addrIP();
        if (clnt.target.fromString(trg)) {
            return;
        }
        clnt.vrf = vrfFor;
        clnt.srcIfc = this;
        if (clnt.target.isIPv4()) {
            clnt.ldpIfc = mplsLdp4;
        } else {
            clnt.ldpIfc = mplsLdp6;
        }
        if (mplsTarget.add(clnt) != null) {
            return;
        }
        clnt.workStart();
    }

    /**
     * clear targeted ldp session
     *
     * @param trg peer address
     */
    public synchronized void clear2ldptrg(String trg) {
        clntMplsTrg clnt = new clntMplsTrg();
        clnt.target = new addrIP();
        if (clnt.target.fromString(trg)) {
            return;
        }
        clnt = mplsTarget.del(clnt);
        if (clnt == null) {
            return;
        }
        clnt.workStop();
    }

    /**
     * setup ldp password
     *
     * @param trg peer address
     * @param pwd password
     */
    public synchronized void setup2ldppwd(String trg, String pwd) {
        addrIP adr = new addrIP();
        if (adr.fromString(trg)) {
            return;
        }
        ipFwdIface ifc;
        if (adr.isIPv4()) {
            ifc = fwdIf4;
        } else {
            ifc = fwdIf6;
        }
        if (ifc == null) {
            return;
        }
        ifc.ldpasPut(adr, pwd);
    }

    /**
     * clear ldp password
     *
     * @param trg peer address
     */
    public synchronized void clear2ldppwd(String trg) {
        addrIP adr = new addrIP();
        if (adr.fromString(trg)) {
            return;
        }
        ipFwdIface ifc;
        if (adr.isIPv4()) {
            ifc = fwdIf4;
        } else {
            ifc = fwdIf6;
        }
        if (ifc == null) {
            return;
        }
        ifc.ldpasDel(adr);
    }

    /**
     * setup interface rsvp processing
     *
     * @param ver ip version
     */
    public synchronized void setup2rsvp(int ver) {
        clear2rsvp(ver);
        if (ver == 4) {
            if (fwdIf4 == null) {
                return;
            }
            mplsRsvp4 = new rtrRsvpIface(vrfFor.fwd4, fwdIf4);
            mplsRsvp4.register2ip();
        } else {
            if (fwdIf6 == null) {
                return;
            }
            mplsRsvp6 = new rtrRsvpIface(vrfFor.fwd6, fwdIf6);
            mplsRsvp6.register2ip();
        }
    }

    /**
     * clear interface rsvp processing
     *
     * @param ver ip version
     */
    public synchronized void clear2rsvp(int ver) {
        if (ver == 4) {
            if (mplsRsvp4 == null) {
                return;
            }
            mplsRsvp4.unregister2ip();
            mplsRsvp4 = null;
        } else {
            if (mplsRsvp6 == null) {
                return;
            }
            mplsRsvp6.unregister2ip();
            mplsRsvp6 = null;
        }
    }

    /**
     * look up tunnel domain name
     */
    public void tunnelDomainName() {
        if (type != ifaceType.tunnel) {
            return;
        }
        if (tunFQDN == null) {
            return;
        }
        addrIP adr = userTerminal.justResolv(tunFQDN, 0);
        if (adr == null) {
            return;
        }
        if (adr.compare(adr, tunTrg) == 0) {
            return;
        }
        tunTrg = adr;
        setup2tunnel();
    }

    /**
     * set bandwidth based on traffic
     */
    public void autoBandwidth() {
        if (autoBndWdt == 0) {
            return;
        }
        history hst;
        if (ethtyp.hwCntr == null) {
            hst = ethtyp.getHistory();
        } else {
            hst = ethtyp.hwHstry;
        }
        long i = hst.getAutoBw(autoBndWdt);
        if (i < 1) {
            i = 1;
        }
        ethtyp.forcedBW = i * 8;
        if (bundleHed != null) {
            bundleHed.bundleHed.propagateState();
        }
    }

    /**
     * update interface history
     */
    public void updateHistory() {
        ethtyp.getHistory().update(ethtyp.getCounter());
    }

    /**
     * follow tracker
     */
    public void followTracker() {
        if (followTrack == null) {
            return;
        }
        cfgTrack trck = cfgAll.trackFind(followTrack, false);
        if (trck == null) {
            logger.warn("interface " + name + " cannot to find configured tracker");
            return;
        }
        final int myVal = 8;
        int res = 0;
        if (!trck.worker.getStatus()) {
            res = myVal;
        }
        if ((ethtyp.forcedDN & myVal) == res) {
            return;
        }
        ethtyp.forcedDN = (ethtyp.forcedDN & (~myVal)) | res;
        ethtyp.propagateState();
        propagateEthtypState();
    }

    /**
     * get interface statistics
     *
     * @param mode mode to use: 1=counters, 2..10=history, 11=hwcounters,
     * 12..20=hwhistory, 21..26-swbwmon, 27..32-hwbwmon
     * @return string list
     */
    public List<String> getShIntTxt(int mode) {
        List<String> l = new ArrayList<String>();
        switch (mode) {
            case 1:
                l.add(ethtyp.getShHeads());
                l.add(cmds.tabulator + "description: " + description);
                String a = ", hwaddr=" + ethtyp.getHwAddr() + ", mtu=" + ethtyp.getMTUsize() + ", bw=" + bits.bandwidth(ethtyp.getBandwidth());
                if (vrfFor != null) {
                    a += ", vrf=" + vrfFor.name;
                }
                if (bridgeIfc != null) {
                    a += ", bridge=" + bridgeHed.name;
                }
                if (bundleIfc != null) {
                    a += ", bundle=" + bundleHed.name;
                }
                if (carrierDelay != 0) {
                    a += ", carrdel=" + carrierDelay;
                }
                l.add(cmds.tabulator + "type is " + type2string() + a);
                if (fwdIf4 != null) {
                    l.add(cmds.tabulator + "ip4 address=" + addr4 + "/" + mask4.toNetmask() + ", netmask=" + mask4 + ", ifcid=" + fwdIf4.ifwNum);
                }
                if (fwdIf6 != null) {
                    l.add(cmds.tabulator + "ip6 address=" + addr6 + "/" + mask6.toNetmask() + ", netmask=" + mask6 + ", ifcid=" + fwdIf6.ifwNum);
                }
                if (ipxIfc != null) {
                    l.add(cmds.tabulator + "ipx address=" + ipxAddr + ", ifcid=" + ipxIfc.ifwNum);
                }
                l.addAll(ethtyp.getCounter().getShFull(ethtyp.getPromisc(), ethtyp.getMacsec()));
                break;
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
            case 8:
            case 9:
            case 10:
                l.addAll(ethtyp.getHistory().show(mode - 1));
                break;
            case 11:
                if (ethtyp.hwCntr == null) {
                    return null;
                }
                l.addAll(ethtyp.hwCntr.getShFull(ethtyp.getPromisc(), ethtyp.getMacsec()));
                break;
            case 12:
            case 13:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
                if (ethtyp.hwHstry == null) {
                    return null;
                }
                l.addAll(ethtyp.hwHstry.show(mode - 11));
                break;
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
                l.add(name + " - " + description);
                l.addAll(ethtyp.getHistory().show(mode - 11));
                break;
            case 27:
            case 28:
            case 29:
            case 30:
            case 31:
            case 32:
                l.add(name + " - " + description);
                if (ethtyp.hwHstry == null) {
                    return l;
                }
                l.addAll(ethtyp.hwHstry.show(mode - 17));
                break;
        }
        return l;
    }

    /**
     * get interface statistics
     *
     * @param l list to update
     * @param mode mode to use: 1=descr, 2=sumary, 3=vrf, 4=ip4, 5=ip6, 6=cdp,
     * 7=lldp, 8=udld, 9=trafic, 10=total, 11=psumary, 12=ptrafic, 13=ptotal,
     * 14=lacp, 15=hwsum, 16=hwpsum, 17=hwtrafic, 18=hwptrafic, 19=swsum,
     * 20=swpsum, 21=swtrafic, 22=swptrafic, 23=hwtot, 24=hwptot, 25=swtot,
     * 26=swptot
     */
    public void getShIntTab(userFormat l, int mode) {
        switch (mode) {
            case 1:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + description);
                break;
            case 2:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getCounter().getShHwBsum(ethtyp.hwCntr));
                break;
            case 3:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getBandwidth() + "|" + (vrfFor == null ? "n/a" : vrfFor.name));
                break;
            case 4:
                if (addr4 == null) {
                    break;
                }
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + addr4 + "|" + mask4);
                break;
            case 5:
                if (addr6 == null) {
                    break;
                }
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + addr6 + "|" + mask6);
                break;
            case 6:
                if (cdp == null) {
                    break;
                }
                l.add(cdp.getShNeigh(false));
                break;
            case 7:
                if (lldp == null) {
                    break;
                }
                l.add(lldp.getShNeigh(false));
                break;
            case 8:
                if (udld == null) {
                    break;
                }
                l.add(udld.getShNeigh(false));
                break;
            case 9:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getHistory().getShHwSum(ethtyp.hwHstry));
                break;
            case 10:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getTotalCntr().getShHwBsum(ethtyp.getHwTotalCntr()));
                break;
            case 11:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getCounter().getShHwPsum(ethtyp.hwCntr));
                break;
            case 12:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getHistory().getShHwPSum(ethtyp.hwHstry));
                break;
            case 13:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getTotalCntr().getShHwPsum(ethtyp.getHwTotalCntr()));
                break;
            case 14:
                if (lacp == null) {
                    break;
                }
                l.add(lacp.getShNeigh(false));
                break;
            case 15:
                if (ethtyp.hwCntr == null) {
                    break;
                }
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.hwCntr.getShBsum());
                break;
            case 16:
                if (ethtyp.hwCntr == null) {
                    break;
                }
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.hwCntr.getShPsum());
                break;
            case 17:
                if (ethtyp.hwHstry == null) {
                    break;
                }
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.hwHstry.getShSum());
                break;
            case 18:
                if (ethtyp.hwHstry == null) {
                    break;
                }
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.hwHstry.getShPSum());
                break;
            case 19:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getCounter().getShBsum());
                break;
            case 20:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getCounter().getShPsum());
                break;
            case 21:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getHistory().getShSum());
                break;
            case 22:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getHistory().getShPSum());
                break;
            case 23:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + counter.getShBsum(ethtyp.getHwTotalCntr()));
                break;
            case 24:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + counter.getShPsum(ethtyp.getHwTotalCntr()));
                break;
            case 25:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getTotalCntr().getShBsum());
                break;
            case 26:
                l.add(name + "|" + state.conv2string(ethtyp.getState()) + "|" + ethtyp.getTotalCntr().getShPsum());
                break;
        }
    }

    public String getPrompt() {
        if (parent != null) {
            return "subif";
        }
        return "if";
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (hidden) {
            return l;
        }
        if (cloned != null) {
            return l;
        }
        l.add("interface " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, ethtyp.forcedMTU < 1, cmds.tabulator, "mtu", "" + ethtyp.forcedMTU);
        if (autoBndWdt == 0) {
            cmds.cfgLine(l, ethtyp.forcedBW < 1, cmds.tabulator, "bandwidth", "" + (ethtyp.forcedBW / 1000));
        } else {
            String s = "";
            switch (autoBndWdt & 0xf) {
                case 0x1:
                    s += " rx";
                    break;
                case 0x2:
                    s += " tx";
                    break;
                case 0x3:
                    s += " both";
                    break;
            }
            switch (autoBndWdt & 0xf0) {
                case 0x10:
                    s += " second";
                    break;
                case 0x20:
                    s += " minute-average";
                    break;
                case 0x30:
                    s += " minute-maximum";
                    break;
                case 0x40:
                    s += " hour-average";
                    break;
                case 0x50:
                    s += " hour-maximum";
                    break;
            }
            l.add(cmds.tabulator + "bandwidth auto" + s);
        }
        cmds.cfgLine(l, ethtyp.forcedMac == null, cmds.tabulator, "macaddr", "" + ethtyp.forcedMac);
        l.add(cmds.tabulator + "padup " + ethtyp.padupMin + " " + ethtyp.padupMod);
        String s;
        switch (ethtyp.monDir) {
            case 0x0:
                s = "none";
                break;
            case 0x1:
                s = "rx";
                break;
            case 0x2:
                s = "tx";
                break;
            case 0x3:
                s = "both";
                break;
            default:
                s = "unknown";
                break;
        }
        l.add(cmds.tabulator + "monitor-direction " + s);
        l.add(cmds.tabulator + "monitor-truncate " + ethtyp.monTrnc);
        l.add(cmds.tabulator + "monitor-sample " + ethtyp.monSmpN);
        cmds.cfgLine(l, ethtyp.monFlt == null, cmds.tabulator, "monitor-filter", "" + ethtyp.monFlt);
        cmds.cfgLine(l, ethtyp.monSes == null, cmds.tabulator, "monitor-session", "" + ethtyp.monSes);
        cmds.cfgLine(l, ethtyp.monBufD == null, cmds.tabulator, "monitor-buffer", "" + ethtyp.getMonBufSize());
        cmds.cfgLine(l, lldp == null, cmds.tabulator, "lldp enable", "");
        cmds.cfgLine(l, cdp == null, cmds.tabulator, "cdp enable", "");
        if (cdp != null) {
            cmds.cfgLine(l, cdp.odr4 == null, cmds.tabulator, "cdp odr4", "" + cdp.odr4);
            cmds.cfgLine(l, cdp.odr6 == null, cmds.tabulator, "cdp odr6", "" + cdp.odr6);
        }
        cmds.cfgLine(l, synce == null, cmds.tabulator, "synceth enable", "");
        if (ptp == null) {
            l.add(cmds.tabulator + "no ptp enable");
        } else {
            l.add(cmds.tabulator + "ptp enable");
            cmds.cfgLine(l, !ptp.receive, cmds.tabulator, "ptp receive", "");
        }
        if (carrierDelay != 0) {
            l.add(cmds.tabulator + "carrier-delay " + carrierDelay);
        } else {
            l.add(cmds.tabulator + "no carrier-delay");
        }
        cmds.cfgLine(l, lacp == null, cmds.tabulator, "lacp", ifcLacp.getCfg(lacp));
        cmds.cfgLine(l, udld == null, cmds.tabulator, "udld enable", "");
        if (nhrp != null) {
            cmds.cfgLine(l, nhrp.ip4 == null, cmds.tabulator, "nhrp ipv4", "" + nhrp.ip4);
            cmds.cfgLine(l, nhrp.ip6 == null, cmds.tabulator, "nhrp ipv6", "" + nhrp.ip6);
        }
        s = null;
        if (vlanHed != null) {
            s = "" + vlanHed;
            int i = s.indexOf(" ");
            s = s.substring(0, i);
        }
        if (s != null) {
            l.add(cmds.tabulator + "encapsulation " + s);
        }
        switch (type) {
            case serial:
            case dialer:
            case virtppp:
                if (vlanNum != 0) {
                    break;
                }
                s = "none";
                if (hdlc != null) {
                    s = "hdlc";
                }
                if (isdn != null) {
                    s = "isdn";
                }
                if (ipOnly != null) {
                    s = "iponly";
                }
                if (ppp != null) {
                    s = "ppp";
                }
                if (lapb != null) {
                    s = "lapb";
                }
                if (raw != null) {
                    s = "raw";
                }
                if (sep != null) {
                    s = "sep";
                }
                if (frmrly != null) {
                    s = "framerelay";
                }
                if (frmppp != null) {
                    s = "frppp";
                }
                if (frmrfc != null) {
                    s = "frrfc";
                }
                if (atmdxi != null) {
                    s = "atmdxi";
                }
                l.add(cmds.tabulator + "encapsulation " + s);
                break;
            case tunnel:
                if (vlanNum != 0) {
                    break;
                }
                cmds.cfgLine(l, !tunSeq, cmds.tabulator, "tunnel sequence-datagrams", "");
                cmds.cfgLine(l, !tunSum, cmds.tabulator, "tunnel checksum", "");
                cmds.cfgLine(l, !tunShut, cmds.tabulator, "tunnel shutdown", "");
                s = "";
                if (tunKey2 != 0) {
                    s = " " + tunKey2;
                }
                l.add(cmds.tabulator + "tunnel key " + tunKey + s);
                l.add(cmds.tabulator + "tunnel tos " + tunTOS);
                l.add(cmds.tabulator + "tunnel flow " + tunFLW);
                l.add(cmds.tabulator + "tunnel ttl " + tunTTL);
                l.add(cmds.tabulator + "tunnel priority " + tunPri);
                if (tunAscAdr == null) {
                    l.add(cmds.tabulator + "no tunnel association");
                } else {
                    s = "";
                    if (tunAscId2 != 0) {
                        s = " " + tunAscId2;
                    }
                    l.add(cmds.tabulator + "tunnel association " + tunAscAdr + " " + tunAscId + s);
                }
                if (tunVrf == null) {
                    l.add(cmds.tabulator + "no tunnel vrf");
                } else {
                    l.add(cmds.tabulator + "tunnel vrf " + tunVrf.name);
                }
                if (tunPrt == null) {
                    l.add(cmds.tabulator + "no tunnel protection");
                } else {
                    l.add(cmds.tabulator + "tunnel protection " + tunPrt.name);
                }
                if (tunSrc == null) {
                    l.add(cmds.tabulator + "no tunnel source");
                } else {
                    l.add(cmds.tabulator + "tunnel source " + tunSrc.name);
                }
                cmds.cfgLine(l, tunTrg == null, cmds.tabulator, "tunnel destination", "" + tunTrg);
                cmds.cfgLine(l, tunFQDN == null, cmds.tabulator, "tunnel domain-name", "" + tunFQDN);
                s = tunnelMode(true);
                cmds.cfgLine(l, s.length() < 1, cmds.tabulator, "tunnel mode", s);
                break;
            default:
                break;
        }
        if (hdlc != null) {
            hdlc.getConfig(l, cmds.tabulator + "hdlc ");
        }
        if (isdn != null) {
            isdn.getConfig(l, cmds.tabulator + "isdn ");
        }
        if (ppp != null) {
            ppp.getConfig(l, cmds.tabulator + "ppp ", filter);
        }
        if (sep != null) {
            sep.getConfig(l, cmds.tabulator + "sep ");
        }
        if (vlanHed != null) {
            vlanHed.vlnGetConfig(l, cmds.tabulator);
        }
        if (qinqx != null) {
            qinqx.getConfig(l, cmds.tabulator + "qinqx ");
        }
        if (atmdxi != null) {
            atmdxi.getConfig(l, cmds.tabulator + "atmdxi ");
        }
        if (atmsar != null) {
            atmsar.getConfig(l, cmds.tabulator + "atmsar ");
        }
        if (frmrly != null) {
            frmrly.getConfig(l, cmds.tabulator + "framerelay ");
        }
        if (lapb != null) {
            lapb.getConfig(l, cmds.tabulator + "lapb ");
        }
        cmds.cfgLine(l, random == null, cmds.tabulator, "random", "" + ifcRandom.getCfg(random));
        cmds.cfgLine(l, !disableMacsec, cmds.tabulator, "disable-macsec", "");
        cmds.cfgLine(l, !disableSgt, cmds.tabulator, "disable-sgt", "");
        cmds.cfgLine(l, ethtyp.macSec == null, cmds.tabulator, "macsec", "" + ethtyp.macSec);
        cmds.cfgLine(l, ethtyp.lossDet == null, cmds.tabulator, "loss-detection", "" + ethtyp.lossDet);
        cmds.cfgLine(l, ethtyp.sgtHnd == null, cmds.tabulator, "sgt enable", "");
        cmds.cfgLine(l, ethtyp.sgtSet < 1, cmds.tabulator, "sgt assign", "" + ethtyp.sgtSet);
        s = "none";
        if (ethtyp.mtuCheckRx) {
            s = "in";
        }
        if (ethtyp.mtuCheckTx) {
            s = "out";
        }
        if (ethtyp.mtuCheckRx && ethtyp.mtuCheckTx) {
            s = "both";
        }
        l.add(cmds.tabulator + "enforce-mtu " + s);
        if (pppoeC == null) {
            l.add(cmds.tabulator + "no p2poe client");
        } else {
            l.add(cmds.tabulator + "p2poe client " + pppoeC.clnIfc.name);
        }
        cmds.cfgLine(l, pppoeS == null, cmds.tabulator, "p2poe server", "" + pppoeS);
        cmds.cfgLine(l, pppoeR == null, cmds.tabulator, "p2poe relay", "" + pppoeR);
        if (eapolC == null) {
            l.add(cmds.tabulator + "no eapol client");
        } else {
            l.add(cmds.tabulator + "eapol client " + eapolC.username + " " + eapolC.password);
        }
        if (eapolS == null) {
            l.add(cmds.tabulator + "no eapol server");
        } else {
            l.add(cmds.tabulator + "eapol server " + eapolS.auther.autName);
        }
        if (bridgeIfc == null) {
            l.add(cmds.tabulator + "no bridge-group");
        } else {
            l.add(cmds.tabulator + "bridge-group " + bridgeHed.name);
            cmds.cfgLine(l, bridgeIfc.filter4in == null, cmds.tabulator, "bridge-filter ipv4in", "" + bridgeIfc.filter4in);
            cmds.cfgLine(l, bridgeIfc.filter4out == null, cmds.tabulator, "bridge-filter ipv4out", "" + bridgeIfc.filter4out);
            cmds.cfgLine(l, bridgeIfc.filter6in == null, cmds.tabulator, "bridge-filter ipv6in", "" + bridgeIfc.filter6in);
            cmds.cfgLine(l, bridgeIfc.filter6out == null, cmds.tabulator, "bridge-filter ipv6out", "" + bridgeIfc.filter6out);
            cmds.cfgLine(l, bridgeIfc.macRewrite == null, cmds.tabulator, "bridge-macrewrite", "" + bridgeIfc.macRewrite);
            if (bridgeIfc.statAddr == null) {
                l.add(cmds.tabulator + "no bridge-staticaddr");
            } else {
                s = "";
                for (int i = 0; i < bridgeIfc.statAddr.size(); i++) {
                    s += " " + bridgeIfc.statAddr.get(i);
                }
                l.add(cmds.tabulator + "bridge-staticaddr" + s);
            }
            if (bridgeIfc.portSec == null) {
                l.add(cmds.tabulator + "no bridge-portsecurity");
            } else {
                s = "";
                for (int i = 0; i < bridgeIfc.portSec.size(); i++) {
                    s += " " + bridgeIfc.portSec.get(i);
                }
                l.add(cmds.tabulator + "bridge-portsecurity" + s);
            }
        }
        if (bundleIfc == null) {
            l.add(cmds.tabulator + "no bundle-group");
        } else {
            l.add(cmds.tabulator + "bundle-group " + bundleHed.name);
            l.add(cmds.tabulator + "bundle-priority " + bundleIfc.priority);
        }
        if (ethtyp.qosIn == null) {
            l.add(cmds.tabulator + "no service-policy-in");
        } else {
            l.add(cmds.tabulator + "service-policy-in " + ethtyp.qosIn.policyName);
        }
        if (ethtyp.qosOut == null) {
            l.add(cmds.tabulator + "no service-policy-out");
        } else {
            l.add(cmds.tabulator + "service-policy-out " + ethtyp.qosOut.policyName);
        }
        if (transProxy == null) {
            l.add(cmds.tabulator + "no transproxy");
        } else {
            l.add(cmds.tabulator + "transproxy " + transProxy.upper.name);
        }
        if (vrfFor == null) {
            l.add(cmds.tabulator + "no vrf forwarding");
        } else {
            l.add(cmds.tabulator + "vrf forwarding " + vrfFor.name);
            String a = "";
            if (hide4adr) {
                a += "dynamic";
            } else {
                a += "" + addr4;
            }
            if (hide4msk) {
                a += " dynamic";
            } else {
                a += " " + mask4;
            }
            cmds.cfgLine(l, addr4 == null, cmds.tabulator, "ipv4 address", a);
            if (fwdIf4 != null) {
                fwdIf4.getConfig(l, vrfFor.fwd4, "ipv4 ", filter);
                cmds.cfgLine(l, ipIf4.redirect == null, cmds.tabulator, "ipv4 redirection", "" + ipIf4.redirect);
                cmds.cfgLine(l, dhcp4c == null, cmds.tabulator, "ipv4 dhcp-client enable", "");
                if (dhcp4c != null) {
                    dhcp4c.getConfig(l, cmds.tabulator, "ipv4 dhcp-client ");
                }
                cmds.cfgLine(l, ip4polC == null, cmds.tabulator, "ipv4 pool", "" + ip4polC);
            }
            a = "";
            if (hide6adr) {
                a += "dynamic";
            } else {
                a += "" + addr6;
            }
            if (hide6msk) {
                a += " dynamic";
            } else {
                a += " " + mask6;
            }
            cmds.cfgLine(l, addr6 == null, cmds.tabulator, "ipv6 address", a);
            if (fwdIf6 != null) {
                fwdIf6.getConfig(l, vrfFor.fwd6, "ipv6 ", filter);
                cmds.cfgLine(l, ipIf6.redirect == null, cmds.tabulator, "ipv6 redirection", "" + ipIf6.redirect);
                cmds.cfgLine(l, slaac == null, cmds.tabulator, "ipv6 slaac", "");
                cmds.cfgLine(l, dhcp6c == null, cmds.tabulator, "ipv6 dhcp-client enable", "");
                if (dhcp6c != null) {
                    dhcp6c.getConfig(l, cmds.tabulator, "ipv6 dhcp-client ");
                }
                cmds.cfgLine(l, ip6polC == null, cmds.tabulator, "ipv6 pool", "" + ip6polC);
                cmds.cfgLine(l, !ipIf6.rtrAdvSuppress, cmds.tabulator, "ipv6 prefix-suppress", "");
                cmds.cfgLine(l, ipIf6.rtrAdvDns == null, cmds.tabulator, "ipv6 prefix-dns", "" + ipIf6.rtrAdvDns);
                cmds.cfgLine(l, ipIf6.rtrAdvDom == null, cmds.tabulator, "ipv6 prefix-domain", "" + ipIf6.rtrAdvDom);
                l.add(cmds.tabulator + "ipv6 prefix-interval " + ipIf6.rtrAdvInterval);
                l.add(cmds.tabulator + "ipv6 prefix-validity " + ipIf6.rtrAdvValidity);
            }
            if (ipxAddr != null) {
                l.add(cmds.tabulator + "ipx network " + bits.toHexD(ipxAddr.getNet()));
            } else {
                l.add(cmds.tabulator + "no ipx network");
            }
        }
        if (template == null) {
            l.add(cmds.tabulator + "no template");
        } else {
            l.add(cmds.tabulator + "template " + template.name);
        }
        cmds.cfgLine(l, nshPack == null, cmds.tabulator, "nsh enable", "");
        cmds.cfgLine(l, nshXcon == null, cmds.tabulator, "nsh xconnect", ifcNshXcn.getCfg(nshXcon));
        String a = "";
        if (polkaPack != null) {
            a = polkaPack.localId + " " + polkaPack.crcBase + " " + polkaPack.crcMax;
        }
        cmds.cfgLine(l, polkaPack == null, cmds.tabulator, "polka enable", a);
        a = "";
        if (mpolkaPack != null) {
            a = mpolkaPack.localId + " " + mpolkaPack.crcBase + " " + mpolkaPack.crcMax;
        }
        cmds.cfgLine(l, mpolkaPack == null, cmds.tabulator, "mpolka enable", a);
        cmds.cfgLine(l, mplsPack == null, cmds.tabulator, "mpls enable", "");
        if (mplsPack != null) {
            cmds.cfgLine(l, !mplsPack.security, cmds.tabulator, "mpls label-security", "");
            cmds.cfgLine(l, !mplsPack.netflowRx, cmds.tabulator, "mpls netflow-rx", "");
            cmds.cfgLine(l, !mplsPack.netflowTx, cmds.tabulator, "mpls netflow-tx", "");
            cmds.cfgLine(l, mplsPack.redirect == null, cmds.tabulator, "mpls redirection", "" + mplsPack.redirect);
            cmds.cfgLine(l, mplsPack.cfilterIn == null, cmds.tabulator, "mpls access-group-common-in", "" + mplsPack.cfilterIn);
            cmds.cfgLine(l, mplsPack.cfilterOut == null, cmds.tabulator, "mpls access-group-common-out", "" + mplsPack.cfilterOut);
            cmds.cfgLine(l, mplsPack.filterIn == null, cmds.tabulator, "mpls access-group-in", "" + mplsPack.filterIn);
            cmds.cfgLine(l, mplsPack.filterOut == null, cmds.tabulator, "mpls access-group-out", "" + mplsPack.filterOut);
            cmds.cfgLine(l, mplsPack.inspect == null, cmds.tabulator, "mpls inspect", "" + mplsPack.inspect);
        }
        cmds.cfgLine(l, mplsLdp4 == null, cmds.tabulator, "mpls ldp4", rtrLdpIface.getLdpCfg(mplsLdp4, this));
        cmds.cfgLine(l, mplsLdp6 == null, cmds.tabulator, "mpls ldp6", rtrLdpIface.getLdpCfg(mplsLdp6, this));
        rtrLdpIface.getGenCfg(mplsLdp4, 4, l);
        rtrLdpIface.getGenCfg(mplsLdp6, 6, l);
        cmds.cfgLine(l, mplsRsvp4 == null, cmds.tabulator, "mpls rsvp4", "");
        cmds.cfgLine(l, mplsRsvp6 == null, cmds.tabulator, "mpls rsvp6", "");
        for (int i = 0; i < mplsTarget.size(); i++) {
            l.add(cmds.tabulator + "mpls ldptarget " + mplsTarget.get(i).target);
        }
        if (fwdIf4 != null) {
            fwdIf4.ldpasCfg(l, cmds.tabulator + "mpls ldppassword", filter);
        }
        if (fwdIf6 != null) {
            fwdIf6.ldpasCfg(l, cmds.tabulator + "mpls ldppassword", filter);
        }
        if (rtrBabel4hnd != null) {
            s = "router babel4 " + rtrBabel4hnd.number + " ";
            rtrBabel4ifc.routerGetConfig(l, s);
        }
        if (rtrBabel6hnd != null) {
            s = "router babel6 " + rtrBabel6hnd.number + " ";
            rtrBabel6ifc.routerGetConfig(l, s);
        }
        if (rtrOlsr4hnd != null) {
            s = "router olsr4 " + rtrOlsr4hnd.number + " ";
            rtrOlsr4ifc.routerGetConfig(l, s);
        }
        if (rtrOlsr6hnd != null) {
            s = "router olsr6 " + rtrOlsr6hnd.number + " ";
            rtrOlsr6ifc.routerGetConfig(l, s);
        }
        if (rtrRip4hnd != null) {
            s = "router rip4 " + rtrRip4hnd.number + " ";
            rtrRip4ifc.routerGetConfig(l, s, filter);
        }
        if (rtrRip6hnd != null) {
            s = "router rip6 " + rtrRip6hnd.number + " ";
            rtrRip6ifc.routerGetConfig(l, s);
        }
        if (rtrOspf4hnd != null) {
            s = "router ospf4 " + rtrOspf4hnd.number + " ";
            rtrOspf4ifc.routerGetConfig(l, s, filter);
        }
        if (rtrOspf6hnd != null) {
            s = "router ospf6 " + rtrOspf6hnd.number + " ";
            rtrOspf6ifc.routerGetConfig(l, s);
        }
        if (rtrIsisHnd != null) {
            s = "router isis" + rtrIsisHnd.isis.getProtoVer() + " " + rtrIsisHnd.number + " ";
            rtrIsisIfc.routerGetConfig(l, s, filter);
        }
        if (rtrPvrp4hnd != null) {
            s = "router pvrp4 " + rtrPvrp4hnd.number + " ";
            rtrPvrp4ifc.routerGetConfig(l, s, filter);
        }
        if (rtrPvrp6hnd != null) {
            s = "router pvrp6 " + rtrPvrp6hnd.number + " ";
            rtrPvrp6ifc.routerGetConfig(l, s, filter);
        }
        if (rtrLsrp4hnd != null) {
            s = "router lsrp4 " + rtrLsrp4hnd.number + " ";
            rtrLsrp4ifc.routerGetConfig(l, s, filter);
        }
        if (rtrLsrp6hnd != null) {
            s = "router lsrp6 " + rtrLsrp6hnd.number + " ";
            rtrLsrp6ifc.routerGetConfig(l, s, filter);
        }
        if (rtrEigrp4hnd != null) {
            s = "router eigrp4 " + rtrEigrp4hnd.number + " ";
            rtrEigrp4ifc.routerGetConfig(l, s);
        }
        if (rtrEigrp6hnd != null) {
            s = "router eigrp6 " + rtrEigrp6hnd.number + " ";
            rtrEigrp6ifc.routerGetConfig(l, s);
        }
        for (int i = 0; i < evcs.size(); i++) {
            cfgIfcEvc ntry = evcs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(cmds.tabulator + "service-instance " + ntry.getCfg());
        }
        if (xconn != null) {
            l.add(cmds.tabulator + "xconnect " + xconn.getCfg());
        }
        if (pwhe != null) {
            l.add(cmds.tabulator + "pseudowire " + pwhe.getCfg());
        }
        cmds.cfgLine(l, followTrack == null, cmds.tabulator, "follow-tracker", "" + followTrack);
        cmds.cfgLine(l, ethtyp.forcedUP, cmds.tabulator, "autostate", "");
        cmds.cfgLine(l, (ethtyp.forcedDN & 1) == 0, cmds.tabulator, "shutdown", "");
        cmds.cfgLine(l, !ethtyp.logStateChg, cmds.tabulator, "log-link-change", "");
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        l = userFilter.filterText(l, defaultF);
        if (template == null) {
            return l;
        }
        List<userFilter> ls = userFilter.text2section(l);
        l = template.getShRun(filter);
        l = userFilter.filterText(l, notemplF);
        for (int i = 1; i < l.size() - 2; i++) {
            userFilter ntry = new userFilter("interface " + name, l.get(i), null);
            ntry = userFilter.findText(ntry, ls);
            if (ntry == null) {
                continue;
            }
            ntry.used = true;
        }
        userFilter.delUsed(ls, true);
        l = userFilter.section2text(ls, true);
        l.add(cmds.comment);
        return l;
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 2,. description                   description of this interface");
        l.add(null, "2 2,.   [text]                      text describing this interface");
        l.add(null, "1 .   log-link-change               log link state changes");
        l.add(null, "1 2   carrier-delay                 log link state changes");
        l.add(null, "2 .     <num>                       time before bringing link up");
        l.add(null, "1 .   shutdown                      administratively disable interface");
        l.add(null, "1 .   autostate                     administratively enable interface");
        l.add(null, "1 2   mtu                           change interface maximum transmission unit");
        l.add(null, "2 .     <num>                       physical layer bytes allowed");
        l.add(null, "1 2   follow-tracker                set administrative state based on a tracker");
        l.add(null, "2 .     <name:trk>                  name of the tracker");
        l.add(null, "1 2   padup                         change interface padding");
        l.add(null, "2 3     <num>                       minimum bytes");
        l.add(null, "3 .       <num>                     modulo bytes");
        l.add(null, "1 2   macaddr                       change interface mac address");
        l.add(null, "2 .     <addr>                      physical layer address");
        l.add(null, "1 2   bandwidth                     change interface bandwidth");
        l.add(null, "2 .     <num>                       kilobits per second");
        l.add(null, "2 3     auto                        calculate automatically");
        l.add(null, "3 4       rx                        received amount");
        l.add(null, "3 4       tx                        transmitted amount");
        l.add(null, "3 4       both                      total amount");
        l.add(null, "4 .         second                  last second");
        l.add(null, "4 .         minute-average          last minute average");
        l.add(null, "4 .         minute-maximum          last minute maximum");
        l.add(null, "4 .         hour-average            last hour average");
        l.add(null, "4 .         hour-maximum            last hour maximum");
        l.add(null, "1 2   template                      get configuration from template");
        l.add(null, "2 .     <name:ifc>                  name of source interface");
        l.add(null, "1 2   monitor-direction             specify monitored direction");
        l.add(null, "2 .     rx                          only received packets");
        l.add(null, "2 .     tx                          only transmitted packets");
        l.add(null, "2 .     both                        both directions");
        l.add(null, "2 .     none                        pause packet mirroring");
        l.add(null, "1 2   monitor-truncate              truncate monitored packets");
        l.add(null, "2 .     <num>                       maximum packet size");
        l.add(null, "1 2   monitor-sample                specify sampled monitoring");
        l.add(null, "2 .     <num>                       one of every n packet");
        l.add(null, "1 2   monitor-filter                specify filtered monitoring");
        l.add(null, "2 .     <name:acl>                  name of access list");
        l.add(null, "1 2   monitor-session               set monitor session");
        l.add(null, "2 .     <name:ifc>                  name of target interface");
        l.add(null, "1 2   monitor-buffer                set monitor buffer");
        l.add(null, "2 .     <num>                       number of bytes");
        l.add(null, "1 2   encapsulation                 change encapsulation");
        l.add(null, "2 .     hdlc                        set to hdlc encapsulation");
        l.add(null, "2 .     isdn                        set to isdn encapsulation");
        l.add(null, "2 .     iponly                      set to iponly encapsulation");
        l.add(null, "2 .     ppp                         set to ppp encapsulation");
        l.add(null, "2 .     lapb                        set to lapb encapsulation");
        l.add(null, "2 .     framerelay                  set to frame relay encapsulation");
        l.add(null, "2 .     frppp                       set to ppp over frame relay encapsulation");
        l.add(null, "2 .     frrfc                       set to ip over frame relay encapsulation");
        l.add(null, "2 .     atmdxi                      set to atm dxi encapsulation");
        l.add(null, "2 .     raw                         set to raw encapsulation");
        l.add(null, "2 .     sep                         set to sep encapsulation");
        l.add(null, "2 .     isl                         set to isl encapsulation");
        l.add(null, "2 .     dot1q                       set to 802.1q encapsulation");
        l.add(null, "2 .     dot1ad                      set to 802.1ad encapsulation");
        l.add(null, "2 .     dot1ah                      set to 802.1ah encapsulation");
        l.add(null, "2 .     qinq1                       set to qinq1 encapsulation");
        l.add(null, "2 .     qinq2                       set to qinq2 encapsulation");
        l.add(null, "2 .     qinq3                       set to qinq3 encapsulation");
        l.add(null, "2 .     qinqx                       set to qinqx encapsulation");
        l.add(null, "1 2   hdlc                          hdlc parameters on the interface");
        ifcHdlc.getHelp(l);
        l.add(null, "1 2   isdn                          isdn parameters on the interface");
        ifcIsdn.getHelp(l);
        l.add(null, "1 2   sep                           sep parameters on the interface");
        ifcSep.getHelp(l);
        l.add(null, "1 2   qinqx                         qinqx parameters on the interface");
        ifcQinqX.getHelp(l);
        l.add(null, "1 2   vlan                          vlan parameters on the interface");
        ifcVlan.vlnGetHelp(l);
        l.add(null, "1 2   p2poe                         pppoe parameters on the interface");
        l.add(null, "2 3     client                      start pppoe client");
        l.add(null, "3 .       <name:ifc>                name of dialer interface");
        l.add(null, "2 3     server                      start pppoe server");
        l.add(null, "3 4,.     <name:ifc>                name of dialer interface");
        l.add(null, "4 5         name                    set service name");
        l.add(null, "5 4,.         <name>                text");
        l.add(null, "4 5         delay                   set pado delay");
        l.add(null, "5 4,.         <num>                 time");
        l.add(null, "2 3     relay                       start pppoe relay");
        l.add(null, "3 4,.     <name:ifc>                name of dialer interface");
        l.add(null, "4 5         name                    set service name");
        l.add(null, "5 4,.         <name>                text");
        l.add(null, "4 5         delay                   set pado delay");
        l.add(null, "5 4,.         <num>                 time");
        l.add(null, "1 2   eapol                         eapol parameters on the interface");
        l.add(null, "2 3     client                      start pppoe client");
        l.add(null, "3 4       <text>                    username");
        l.add(null, "4 .         <text>                  password");
        l.add(null, "2 3     server                      start pppoe server");
        l.add(null, "3 .       <text>                    authentication list");
        l.add(null, "1 2   ppp                           ppp parameters on the interface");
        ifcPpp.getHelp(l);
        l.add(null, "1 2   atmdxi                        atm dxi parameters on the interface");
        ifcAtmDxi.getHelp(l);
        l.add(null, "1 2   atmsar                        atm sar parameters on the interface");
        ifcAtmSar.getHelp(l);
        l.add(null, "1 2   framerelay                    frame relay parameters on the interface");
        ifcFrameRelay.getHelp(l);
        l.add(null, "1 2   lapb                          lapb parameters on the interface");
        ifcLapb.getHelp(l);
        l.add(null, "1 2   bundle-group                  bundling interface parameters");
        l.add(null, "2 .     <num>                       number of bundle group");
        l.add(null, "1 2   bundle-priority               bundling priority parameter");
        l.add(null, "2 .     <num>                       priroty of link");
        l.add(null, "1 2   bridge-group                  transparent bridging interface parameters");
        l.add(null, "2 .     <num>                       number of bridge group");
        l.add(null, "1 2   bridge-portsecurity           transparent bridging interface parameters");
        l.add(null, "2 2,.   <addr>                      address to allow");
        l.add(null, "1 2   bridge-staticaddr             transparent bridging interface parameters");
        l.add(null, "2 2,.   <addr>                      address to forward");
        l.add(null, "1 2   bridge-macrewrite             transparent bridging interface parameters");
        l.add(null, "2 .     <addr>                      address to use");
        l.add(null, "1 2   bridge-filter                 transparent bridging filtering parameters");
        l.add(null, "2 3     ipv4in                      ipv4 ingress filter");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     ipv4out                     ipv4 egress filter");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     ipv6in                      ipv6 ingress filter");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     ipv6out                     ipv6 egress filter");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "1 2   vrf                           vrf parameters on the interface");
        l.add(null, "2 3     forwarding                  configure forwarding table");
        l.add(null, "3 .       <name:vrf>                name of table");
        l.add(null, "1 2   transproxy                    transparent proxy on the interface");
        l.add(null, "2 .     <name:prx>                  name of proxy profile");
        l.add(null, "1 2   ipx                           interface ipx config commands");
        l.add(null, "2 3     network                     configure network");
        l.add(null, "3 .       <num>                     network number");
        l.add(null, "1 2   ipv4                          interface internet protocol config commands");
        ipFwdIface.getHelp(l);
        l.add(null, "2 3     dhcp-client                 acquire address by dhcp");
        l.add(null, "3 .       enable                    start address acquision");
        l.add(null, "3 .       broadcast                 set broadcast flag");
        l.add(null, "3 .       early                     pick up address early");
        l.add(null, "3 4       renew-min                 minimum renew time");
        l.add(null, "4 .         <num>                   time in seconds");
        l.add(null, "3 4       renew-max                 maximum renew time");
        l.add(null, "4 .         <num>                   time in seconds");
        l.add(null, "2 3     pool                        peer address pool");
        l.add(null, "3 .       <name:pl4>                name of address pool");
        l.add(null, "1 2   ipv6                          interface internet protocol config commands");
        ipFwdIface.getHelp(l);
        l.add(null, "2 .     slaac                       stateless address autoconfiguration");
        l.add(null, "2 .     enable                      link local address routing");
        l.add(null, "2 3     dhcp-client                 acquire address by dhcp");
        l.add(null, "3 .       enable                    start address acquision");
        l.add(null, "3 .       prefix                    request prefix");
        l.add(null, "3 .       early                     pick up address early");
        l.add(null, "3 4       renew-min                 minimum renew time");
        l.add(null, "4 .         <num>                   time in seconds");
        l.add(null, "3 4       renew-max                 maximum renew time");
        l.add(null, "4 .         <num>                   time in seconds");
        l.add(null, "2 .     prefix-suppress             suppress router advertisements");
        l.add(null, "2 3     prefix-dns                  name server in router advertisements");
        l.add(null, "3 .       <addr>                    name server address");
        l.add(null, "2 3     prefix-domain               domain name in router advertisements");
        l.add(null, "3 .       <name>                    domain name");
        l.add(null, "2 3     prefix-interval             time between router advertisements");
        l.add(null, "3 .       <num>                     time in milliseconds");
        l.add(null, "2 3     prefix-validity             prefix validity in router advertisements");
        l.add(null, "3 .       <num>                     time in milliseconds");
        l.add(null, "2 3     pool                        peer address pool");
        l.add(null, "3 .       <name:pl6>                name of address pool");
        l.add(null, "1 2   tunnel                        protocol-over-protocol tunneling");
        l.add(null, "2 3     vrf                         set encapsulated vrf membership");
        l.add(null, "3 .       <name:vrf>                name of vrf where encapsulated packets");
        l.add(null, "2 3     mode                        set encapsulation method");
        l.add(null, "3 .       gre                       generic route encapsulation protocol");
        l.add(null, "3 .       udpgre                    generic route encapsulation in udp");
        l.add(null, "3 .       amt                       automatic multicast tunneling protocol");
        l.add(null, "3 .       icmp                      internet control message protocol");
        l.add(null, "3 .       pim                       protocol independent multicast");
        l.add(null, "3 .       lisp                      locator id separation protocol");
        l.add(null, "3 .       minenc                    minimal encapsulation protocol");
        l.add(null, "3 .       pipe                      private ipip encapsulation");
        l.add(null, "3 .       nos                       nos ipip encapsulation");
        l.add(null, "3 .       ipcomp                    ip compression");
        l.add(null, "3 .       ipenc                     ip encapsulation protocol");
        l.add(null, "3 .       tmux                      transport multiplexing protocol");
        l.add(null, "3 .       6to4                      ipv6 to ipv4 protocol translator");
        l.add(null, "3 .       srv6                      segment routing v6 protocol translator");
        l.add(null, "3 .       ipip                      ip over ip encapsulation");
        l.add(null, "3 .       ipsec                     ip security encapsulation");
        l.add(null, "3 .       pckoudp                   packet over udp encapsulation");
        l.add(null, "3 .       pckoip                    packet over raw ip protocol");
        l.add(null, "3 .       l2tp3                     l2tp v3 encapsulation");
        l.add(null, "3 .       vxlan                     vxlan encapsulation");
        l.add(null, "3 .       geneve                    geneve encapsulation");
        l.add(null, "3 .       capwap                    capwap encapsulation");
        l.add(null, "3 .       lwapp                     lwapp encapsulation");
        l.add(null, "3 .       erspan                    erspan encapsulation");
        l.add(null, "3 .       dlsw                      dlsw encapsulation");
        l.add(null, "3 .       etherip                   etherip encapsulation");
        l.add(null, "3 .       sreth                     segment routing ethernet encapsulation");
        l.add(null, "3 .       uti                       universal transport interface");
        l.add(null, "3 .       nvgre                     nvgre encapsulation");
        l.add(null, "3 .       mplsip                    mplsip encapsulation");
        l.add(null, "3 .       mplsudp                   mplsudp encapsulation");
        l.add(null, "3 .       swipe                     swipe encapsulation");
        l.add(null, "3 .       openvpn                   openvpn encapsulation");
        l.add(null, "3 .       wireguard                 wireguard encapsulation");
        l.add(null, "3 .       satp                      satp encapsulation");
        l.add(null, "3 .       inlsp                     inlsp encapsulation");
        l.add(null, "3 .       skip                      skip encapsulation");
        l.add(null, "3 .       pweompls                  pseudowire over mpls encapsulation");
        l.add(null, "3 .       expbun                    mpls exp bundle tunnel");
        l.add(null, "3 .       srmpls                    segment routing te over mpls tunnel");
        l.add(null, "3 .       polka                     polinomial key architecture tunnel");
        l.add(null, "3 .       mpolka                    multipath polinomial key architecture tunnel");
        l.add(null, "3 .       srext                     segment routing te over exthdr tunnel");
        l.add(null, "3 .       pcesr                     mpls sr tunnel from pcep");
        l.add(null, "3 .       pcete                     mpls te tunnel from pcep");
        l.add(null, "3 .       p2pte                     point to point mpls te tunnel");
        l.add(null, "3 .       p2mpte                    point to multipoint mpls te tunnel");
        l.add(null, "3 .       bier                      mpls bier tunnel");
        l.add(null, "3 .       p2pldp                    point to point mpls ldp tunnel");
        l.add(null, "3 .       p2mpldp                   point to multipoint mpls ldp tunnel");
        l.add(null, "3 .       mp2mpldp                  multipoint to multipoint mpls ldp tunnel");
        l.add(null, "2 3     source                      source of encapsulated packets");
        l.add(null, "3 .       <name:ifc>                name of interface where from send");
        l.add(null, "2 3     destination                 destination of encapsulated packets");
        l.add(null, "3 .       <addr>                    ip address where to send");
        l.add(null, "2 3     domain-name                 destination of encapsulated packets");
        l.add(null, "3 3,.     <name>                    domain name where to send");
        l.add(null, "2 3     tos                         set type of service, -1 to map out");
        l.add(null, "3 .       <num>                     value of tos field");
        l.add(null, "2 3     ttl                         set time to live, -1 to map out");
        l.add(null, "3 .       <num>                     value of ttl field");
        l.add(null, "2 3     priority                    set tunnel priority");
        l.add(null, "3 .       <num>                     value of ttl field");
        l.add(null, "2 3     association                 set tunnel association");
        l.add(null, "3 4       <addr>                    source address");
        l.add(null, "4 5,.       <num>                   unique id");
        l.add(null, "5 .           <num>                 global id");
        l.add(null, "2 3     flow                        set flow label, -1 to map out");
        l.add(null, "3 .       <num>                     value of flow label");
        l.add(null, "2 3     key                         set security key, 0 to disable");
        l.add(null, "3 4,.     <num>                     value of key field");
        l.add(null, "4 .         <num>                   value of local key field");
        l.add(null, "2 .     checksum                    enable checksumming of packets");
        l.add(null, "2 .     shutdown                    shutdown tunnel protocol");
        l.add(null, "2 .     sequence-datagrams          drop datagrams arriving out of order");
        l.add(null, "2 3     protection                  set ipsec profile to use");
        l.add(null, "3 .       <name:ips>                name of ipsec profile");
        l.add(null, "1 2   router                        interface routing protocol config commands");
        l.add(null, "2 3     babel4                      babel routing protocol for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrBabelIface.routerGetHelp(l);
        l.add(null, "2 3     babel6                      babel routing protocol for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrBabelIface.routerGetHelp(l);
        l.add(null, "2 3     olsr4                       optimized link state routing protocol for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrOlsrIface.routerGetHelp(l);
        l.add(null, "2 3     olsr6                       optimized link state routing protocol for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrOlsrIface.routerGetHelp(l);
        l.add(null, "2 3     rip4                        routing information protocol for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrRip4iface.routerGetHelp(l);
        l.add(null, "2 3     rip6                        routing information protocol for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrRip6iface.routerGetHelp(l);
        l.add(null, "2 3     ospf4                       open shortest path first for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrOspf4iface.routerGetHelp(l);
        l.add(null, "2 3     ospf6                       open shortest path first for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrOspf6iface.routerGetHelp(l);
        l.add(null, "2 3     isis4                       intermediate system intermediate system for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrIsisIface.routerGetHelp(l);
        l.add(null, "2 3     isis6                       intermediate system intermediate system for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrIsisIface.routerGetHelp(l);
        l.add(null, "2 3     pvrp4                       path vector routing protocol for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrPvrpIface.routerGetHelp(l);
        l.add(null, "2 3     pvrp6                       path vector routing protocol for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrPvrpIface.routerGetHelp(l);
        l.add(null, "2 3     lsrp4                       link state routing protocol for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrLsrpIface.routerGetHelp(l);
        l.add(null, "2 3     lsrp6                       link state routing protocol for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrLsrpIface.routerGetHelp(l);
        l.add(null, "2 3     eigrp4                      enhanced interior gateway routing protocol for ipv4");
        l.add(null, "3 4       <num>                     process id");
        rtrEigrpIface.routerGetHelp(l);
        l.add(null, "2 3     eigrp6                      enhanced interior gateway routing protocol for ipv6");
        l.add(null, "3 4       <num>                     process id");
        rtrEigrpIface.routerGetHelp(l);
        l.add(null, "1 2   nsh                           network service header config commands");
        l.add(null, "2 .     enable                      enable/disable packet processing");
        l.add(null, "2 3     xconnect                    enable/disable packet forwarding");
        l.add(null, "3 4       <num>                     service path");
        l.add(null, "4 .         <num>                   service index");
        l.add(null, "1 2   polka                         polynominal key architecture commands");
        l.add(null, "2 3     enable                      enable/disable packet processing");
        l.add(null, "3 4       <num>                     local node id");
        l.add(null, "4 5         <num>                   coefficient base");
        l.add(null, "5 .           <num>                 number of coefficients");
        l.add(null, "1 2   mpolka                        multipath polynominal key architecture commands");
        l.add(null, "2 3     enable                      enable/disable packet processing");
        l.add(null, "3 4       <num>                     local node id");
        l.add(null, "4 5         <num>                   coefficient base");
        l.add(null, "5 .           <num>                 number of coefficients");
        l.add(null, "1 2   mpls                          multiprotocol label switching config commands");
        l.add(null, "2 .     enable                      enable/disable packet processing");
        l.add(null, "2 .     label-security              enable/disable security checks");
        l.add(null, "2 .     netflow-rx                  netflow received packets");
        l.add(null, "2 .     netflow-tx                  netflow transmitted packets");
        l.add(null, "2 3     access-group-in             access list to apply to ingress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     access-group-out            access list to apply to egress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     access-group-common-in      common access list to apply to ingress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     access-group-common-out     common access list to apply to egress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3,.   inspect                     enable/disable inspection");
        l.add(null, "3 4       timeout                   set timeout");
        l.add(null, "4 3,.       <num>                   timeout in ms");
        l.add(null, "3 3,.     mac                       log mac addresses");
        l.add(null, "3 3,.     before                    log on session start");
        l.add(null, "3 3,.     after                     log on session stop");
        l.add(null, "3 3,.     dropped                   log dropped sessions");
        l.add(null, "3 3,.     drop-rx                   drop sessions ingress");
        l.add(null, "3 3,.     drop-tx                   drop sessions egress");
        l.add(null, "3 3,.     drop-frg                  drop fragmented packets");
        l.add(null, "3 3,.     allow-routing             allow control multicast traffic");
        l.add(null, "3 3,.     allow-linklocal           allow link local traffic");
        l.add(null, "3 3,.     allow-multicast           allow multicast traffic");
        l.add(null, "3 3,.     allow-broadcast           allow broadcast traffic");
        l.add(null, "3 4       allow-list                allow specific traffic");
        l.add(null, "4 3,.       <name:acl>              name of access list");
        l.add(null, "3 4       allow-url                 allow specific traffic");
        l.add(null, "4 3,.       <num>                   translation rule");
        l.add(null, "3 4       member                    member of inspection");
        l.add(null, "4 5         <name:ses>              name of session group");
        l.add(null, "5 3,.         <name>                local identifier in the group");
        l.add(null, "2 3     redirection                 send packets out on different interface");
        l.add(null, "3 .       <name:ifc>                name of interface");
        l.add(null, "2 3,.   ldp4                        enable/disable ldp ipv4 discovery");
        l.add(null, "3 .       [name:ifc]                name of interface");
        l.add(null, "2 3,.   ldp6                        enable/disable ldp ipv6 discovery");
        l.add(null, "3 .       [name:ifc]                name of interface");
        l.add(null, "2 3     ldptarget                   set targeted ldp peer");
        l.add(null, "3 .       <addr>                    address of peer");
        l.add(null, "2 .     label4pop                   advertise php");
        l.add(null, "2 .     label6pop                   advertise php");
        l.add(null, "2 3     label4in                    set label filter");
        l.add(null, "3 .       <addr>                    name of prefix list");
        l.add(null, "2 3     label4out                   set label filter");
        l.add(null, "3 .       <addr>                    name of prefix list");
        l.add(null, "2 3     label6in                    set label filter");
        l.add(null, "3 .       <addr>                    name of prefix list");
        l.add(null, "2 3     label6out                   set label filter");
        l.add(null, "3 .       <addr>                    name of prefix list");
        l.add(null, "2 3     ldppassword                 set ldp password for peer");
        l.add(null, "3 4       <addr>                    address of peer");
        l.add(null, "4 .         <text>                  password");
        l.add(null, "2 .     rsvp4                       enable/disable rsvp-te ipv4 signaling");
        l.add(null, "2 .     rsvp6                       enable/disable rsvp-te ipv6 signaling");
        l.add(null, "1 2   lldp                          link layer discovery protocol commands");
        l.add(null, "2 .     enable                      enable/disable processing");
        l.add(null, "1 2   cdp                           cisco discovery protocol commands");
        l.add(null, "2 .     enable                      enable/disable processing");
        l.add(null, "2 3     odr4                        send on demand routing gateway");
        l.add(null, "3 .       <addr>                    address to send");
        l.add(null, "2 3     odr6                        send on demand routing gateway");
        l.add(null, "3 .       <addr>                    address to send");
        l.add(null, "1 2   lacp                          link aggregation control protocol commands");
        l.add(null, "2 3     <addr>                      system id");
        l.add(null, "3 4       <num>                     system key");
        l.add(null, "4 .         <num>                   port number");
        l.add(null, "1 2   synceth                       synchronous ethernet commands");
        l.add(null, "2 .     enable                      enable/disable processing");
        l.add(null, "1 2   ptp                           precision time protococol commands");
        l.add(null, "2 .     enable                      enable/disable processing");
        l.add(null, "2 .     receive                     allow clock adjustment");
        l.add(null, "1 2   udld                          unidirectional link detection commands");
        l.add(null, "2 .     enable                      enable/disable processing");
        l.add(null, "1 2   nhrp                          next hop resolution protocol commands");
        l.add(null, "2 3     ipv4                        enable for ipv4");
        l.add(null, "3 .       <addr>                    target to register");
        l.add(null, "2 3     ipv6                        enable for ipv6");
        l.add(null, "3 .       <addr>                    target to register");
        l.add(null, "1 2   random                        random packet injector");
        l.add(null, "2 3     <num>                       ethertype to use");
        l.add(null, "3 4       <num>                     minimum packet size");
        l.add(null, "4 5         <num>                   maximum packet size");
        l.add(null, "5 6           <num>                 minimum interval");
        l.add(null, "6 .             <num>               maximum interval");
        l.add(null, "1 2   enforce-mtu                   enfore mtu on packets");
        l.add(null, "2 .     in                          only in ingress");
        l.add(null, "2 .     out                         only in egress");
        l.add(null, "2 .     both                        check in both directions");
        l.add(null, "2 .     none                        not check at all");
        l.add(null, "1 2   sgt                           security group tag commands");
        l.add(null, "2 .     enable                      enable tagging");
        l.add(null, "2 3     assign                      assign tag");
        l.add(null, "3 .       <num>                     tag");
        l.add(null, "1 2,. loss-detection                loss detection commands");
        l.add(null, "2 3     <num>                       packet loss to block");
        l.add(null, "3 .       <num>                     time to block");
        l.add(null, "1 .   disable-macsec                disable macsec");
        l.add(null, "1 .   disable-sgt                   disable sgt");
        l.add(null, "1 2   macsec                        mac security protocol commands");
        l.add(null, "2 3,.   <name:ips>                  name of ipsec profile");
        l.add(null, "3 .       <num>                     ethertype to use");
        l.add(null, "1 2   xconnect                      cross connect interface");
        cfgXconnSide.getHelp(l, 2);
        l.add(null, "1 2   pseudowire                    pseudowire of interface");
        cfgXconnSide.getHelp(l, 2);
        l.add(null, "1 2   service-instance              configure ethernet services");
        l.add(null, "2 3     <num>                       vlan id");
        l.add(null, "3 .       shutdown                  drop frames unconditionally");
        l.add(null, "3 4       bridge-group              transparent bridging interface parameters");
        l.add(null, "4 .         <num>                   number of bridge group");
        l.add(null, "3 4       xconnect                  cross connect vlan");
        cfgXconnSide.getHelp(l, 4);
        l.add(null, "1 2   service-policy-in             policy map to apply to ingress packets");
        l.add(null, "2 .     <name:pm>                   name of policy map");
        l.add(null, "1 2   service-policy-out            policy map to apply to egress packets");
        l.add(null, "2 .     <name:pm>                   name of policy map");
    }

    public synchronized void doCfgStr(cmds cmd) {
        if (type == ifaceType.template) {
            cfgAll.templateConfig(this, cmd);
        }
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("log-link-change")) {
            ethtyp.logStateChg = true;
            return;
        }
        if (a.equals("macaddr")) {
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                return;
            }
            ethtyp.forcedMac = mac;
            return;
        }
        if (a.equals("shutdown")) {
            ethtyp.forcedDN |= 1;
            ethtyp.propagateState();
            propagateEthtypState();
            return;
        }
        if (a.equals("autostate")) {
            ethtyp.forcedUP = false;
            ethtyp.propagateState();
            propagateEthtypState();
            return;
        }
        if (a.equals("template")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            setup2template(ifc);
            return;
        }
        if (a.equals("transproxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy profile");
                return;
            }
            setup2transproxy(prx.proxy);
            return;
        }
        if (a.equals("monitor-buffer")) {
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                return;
            }
            ethtyp.monBufP = 0;
            ethtyp.monBufD = new byte[i];
            return;
        }
        if (a.equals("monitor-filter")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such list");
                return;
            }
            ethtyp.ip4cor = new ipCor4();
            ethtyp.ip6cor = new ipCor6();
            ethtyp.monFlt = acl.aceslst;
            return;
        }
        if (a.equals("monitor-direction")) {
            a = cmd.word();
            ethtyp.monDir = 0;
            if (a.equals("rx")) {
                ethtyp.monDir = 1;
            }
            if (a.equals("tx")) {
                ethtyp.monDir = 2;
            }
            if (a.equals("both")) {
                ethtyp.monDir = 3;
            }
            return;
        }
        if (a.equals("monitor-truncate")) {
            ethtyp.monTrnc = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("monitor-sample")) {
            ethtyp.monSmpN = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("monitor-session")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            ethtyp.monHdr = ifc.ifaceNeedMacs();
            ethtyp.monSes = ifc.ethtyp;
            return;
        }
        if (a.equals("follow-tracker")) {
            followTrack = cmd.word();
            return;
        }
        if (a.equals("padup")) {
            ethtyp.padupMin = bits.str2num(cmd.word());
            ethtyp.padupMod = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("mtu")) {
            ethtyp.forcedMTU = bits.str2num(cmd.word());
            propagateEthtypState();
            return;
        }
        if (a.equals("bandwidth")) {
            autoBndWdt = 0;
            a = cmd.word();
            if (!a.equals("auto")) {
                ethtyp.forcedBW = bits.str2long(a) * 1000;
                if (bundleHed != null) {
                    bundleHed.bundleHed.propagateState();
                }
                return;
            }
            a = cmd.word();
            if (a.equals("rx")) {
                autoBndWdt |= 0x1;
            }
            if (a.equals("tx")) {
                autoBndWdt |= 0x2;
            }
            if (a.equals("both")) {
                autoBndWdt |= 0x3;
            }
            a = cmd.word();
            if (a.equals("second")) {
                autoBndWdt |= 0x10;
            }
            if (a.equals("minute-average")) {
                autoBndWdt |= 0x20;
            }
            if (a.equals("minute-maximum")) {
                autoBndWdt |= 0x30;
            }
            if (a.equals("hour-average")) {
                autoBndWdt |= 0x40;
            }
            if (a.equals("hour-maximum")) {
                autoBndWdt |= 0x50;
            }
            return;
        }
        if (a.equals("encapsulation")) {
            initEncap(cmd.word());
            return;
        }
        if (a.equals("carrier-delay")) {
            carrierDelay = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("p2poe")) {
            a = cmd.word();
            if (a.equals("client")) {
                if (setup2pppoeClnt(cfgAll.ifcFind(cmd.word(), false))) {
                    cmd.error("failed to setup encapsulation");
                    return;
                }
                return;
            }
            if (a.equals("server")) {
                if (setup2pppoeServ(cfgAll.ifcFind(cmd.word(), false), cmd)) {
                    cmd.error("failed to setup encapsulation");
                    return;
                }
                return;
            }
            if (a.equals("relay")) {
                if (setup2pppoeRely(cfgAll.ifcFind(cmd.word(), false), cmd)) {
                    cmd.error("failed to setup encapsulation");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("eapol")) {
            a = cmd.word();
            if (a.equals("client")) {
                a = cmd.word();
                if (setup2eapolClnt(a, cmd.word())) {
                    cmd.error("failed to setup encapsulation");
                    return;
                }
                return;
            }
            if (a.equals("server")) {
                if (setup2eapolServ(cfgAll.autherFind(cmd.word(), null))) {
                    cmd.error("failed to setup encapsulation");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("hdlc")) {
            if (hdlc == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            hdlc.doConfig(cmd);
            return;
        }
        if (a.equals("isdn")) {
            if (isdn == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            isdn.doConfig(cmd);
            return;
        }
        if (a.equals("sep")) {
            if (sep == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            sep.doConfig(cmd);
            return;
        }
        if (a.equals("qinqx")) {
            if (qinqx == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            qinqx = new ifcQinqX(qinqx);
            qinqx.doConfig(cmd);
            initVlan(qinqx);
            return;
        }
        if (a.equals("vlan")) {
            if (vlanHed == null) {
                initVlan();
            }
            vlanHed.vlnDoConfig(cmd);
            return;
        }
        if (a.equals("lapb")) {
            if (lapb == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            lapb.doConfig(cmd);
            return;
        }
        if (a.equals("framerelay")) {
            if (frmrly == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            frmrly.doConfig(cmd);
            return;
        }
        if (a.equals("atmdxi")) {
            if (atmdxi == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            atmdxi.doConfig(cmd);
            return;
        }
        if (a.equals("atmsar")) {
            if (atmsar == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            atmsar.doConfig(cmd);
            return;
        }
        if (a.equals("ppp")) {
            if (ppp == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            ppp.doConfig(cmd);
            ppp.clearState();
            return;
        }
        if (a.equals("bridge-group")) {
            cfgBrdg brdg = cfgAll.brdgFind(cmd.word(), false);
            if (brdg == null) {
                cmd.error("invalid bridge number");
                return;
            }
            setup2bridge(brdg);
            return;
        }
        if (a.equals("bridge-staticaddr")) {
            if (bridgeIfc == null) {
                cmd.error("not bridged");
                return;
            }
            bridgeIfc.statAddr = new tabGen<addrMac>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                addrMac adr = new addrMac();
                adr.fromString(a);
                bridgeIfc.statAddr.add(adr);
            }
            bridgeHed.bridgeHed.delMacs(bridgeIfc);
            bridgeHed.bridgeHed.addMacs(bridgeIfc, bridgeIfc.statAddr);
            return;
        }
        if (a.equals("bridge-portsecurity")) {
            if (bridgeIfc == null) {
                cmd.error("not bridged");
                return;
            }
            bridgeIfc.portSec = new tabGen<addrMac>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                addrMac adr = new addrMac();
                adr.fromString(a);
                bridgeIfc.portSec.add(adr);
            }
            return;
        }
        if (a.equals("bridge-macrewrite")) {
            a = cmd.word();
            if (bridgeIfc == null) {
                cmd.error("not bridged");
                return;
            }
            bridgeIfc.macRewrite = new addrMac();
            bridgeIfc.macRewrite.fromString(a);
            return;
        }
        if (a.equals("bridge-filter")) {
            a = cmd.word();
            if (bridgeIfc == null) {
                cmd.error("not bridged");
                return;
            }
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return;
            }
            if (a.equals("ipv4in")) {
                ntry.aceslst.myCor = new ipCor4();
                ntry.aceslst.myIcmp = new ipIcmp4();
                bridgeIfc.filter4in = ntry.aceslst;
                return;
            }
            if (a.equals("ipv4out")) {
                ntry.aceslst.myCor = new ipCor4();
                ntry.aceslst.myIcmp = new ipIcmp4();
                bridgeIfc.filter4out = ntry.aceslst;
                return;
            }
            if (a.equals("ipv6in")) {
                ntry.aceslst.myCor = new ipCor6();
                ntry.aceslst.myIcmp = new ipIcmp6();
                bridgeIfc.filter6in = ntry.aceslst;
                return;
            }
            if (a.equals("ipv6out")) {
                ntry.aceslst.myCor = new ipCor6();
                ntry.aceslst.myIcmp = new ipIcmp6();
                bridgeIfc.filter6out = ntry.aceslst;
                return;
            }
            return;
        }
        if (a.equals("bundle-group")) {
            cfgBndl bndl = cfgAll.bndlFind(cmd.word(), false);
            if (bndl == null) {
                cmd.error("invalid bundle number");
                return;
            }
            setup2bundle(bndl);
            return;
        }
        if (a.equals("bundle-priority")) {
            if (bundleIfc == null) {
                cmd.error("not enabled");
                return;
            }
            bundleIfc.priority = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("random")) {
            if (random != null) {
                random.stopWork();
            }
            random = new ifcRandom();
            random.ethtyp = bits.fromHex(cmd.word());
            random.sizMin = bits.str2num(cmd.word());
            random.sizMax = bits.str2num(cmd.word());
            random.intMin = bits.str2num(cmd.word());
            random.intMax = bits.str2num(cmd.word());
            ethtyp.addET(random.ethtyp, "random", random);
            ethtyp.updateET(random.ethtyp, random);
            random.startWork();
            return;
        }
        if (a.equals("enforce-mtu")) {
            a = cmd.word();
            ethtyp.mtuCheckRx = false;
            ethtyp.mtuCheckTx = false;
            if (a.equals("in")) {
                ethtyp.mtuCheckRx = true;
                return;
            }
            if (a.equals("out")) {
                ethtyp.mtuCheckTx = true;
                return;
            }
            if (a.equals("both")) {
                ethtyp.mtuCheckRx = true;
                ethtyp.mtuCheckTx = true;
                return;
            }
            return;
        }
        if (a.equals("sgt")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (disableSgt) {
                    ethtyp.sgtHnd = null;
                    return;
                }
                ethtyp.sgtHnd = new ifcSgt();
                return;
            }
            if (a.equals("assign")) {
                ethtyp.sgtSet = bits.str2num(cmd.word());
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("loss-detection")) {
            ifcLossDet sec = new ifcLossDet();
            sec.doInit(ethtyp);
            sec.packets = bits.str2num(cmd.word());
            sec.blocking = bits.str2num(cmd.word());
            ethtyp.lossDet = sec;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("disable-sgt")) {
            disableSgt = true;
            ethtyp.sgtHnd = null;
            return;
        }
        if (a.equals("disable-macsec")) {
            disableMacsec = true;
            ethtyp.macSec = null;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("macsec")) {
            cfgIpsec prf = cfgAll.ipsecFind(cmd.word(), false);
            if (prf == null) {
                cmd.error("no such profile");
                return;
            }
            if (disableMacsec) {
                ethtyp.macSec = null;
                ethtyp.timerUpdate();
                return;
            }
            ifcMacSec sec = new ifcMacSec();
            sec.doInit(prf, ethtyp, bits.fromHex(cmd.word()));
            sec.needLayer2 = ifaceNeedArp();
            ethtyp.macSec = sec;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("service-policy-in")) {
            cfgPlymp plcy = cfgAll.plmpFind(cmd.word(), false);
            if (plcy == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(plcy.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            ethtyp.qosIn = wrkr;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("service-policy-out")) {
            cfgPlymp plcy = cfgAll.plmpFind(cmd.word(), false);
            if (plcy == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(plcy.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            ethtyp.qosOut = wrkr;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("vrf")) {
            a = cmd.word();
            if (a.equals("forwarding")) {
                a = cmd.word();
                cfgVrf v = cfgAll.vrfFind(a, false);
                if (v == null) {
                    cmd.error("no such vrf exists");
                    return;
                }
                if (vrfFor == v) {
                    return;
                }
                clear2routing(true, true);
                clear2vrf();
                vrfFor = v;
                setup2vrf(true, true, true);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("lldp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (lldp != null) {
                    lldp.restartTimer(true);
                }
                lldp = new ifcLldp(this);
                ethtyp.addET(ifcLldp.ethtyp, "lldp", lldp);
                ethtyp.updateET(ifcLldp.ethtyp, lldp);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("nhrp")) {
            a = cmd.word();
            if (nhrp == null) {
                nhrp = new ifcNhrp(this);
                ethtyp.addET(ifcNhrp.ethtyp, "nhrp", nhrp);
                ethtyp.updateET(ifcNhrp.ethtyp, nhrp);
            }
            if (a.equals("ipv4")) {
                nhrp.ip4 = new addrIPv4();
                nhrp.ip4.fromString(cmd.word());
                return;
            }
            if (a.equals("ipv6")) {
                nhrp.ip6 = new addrIPv6();
                nhrp.ip6.fromString(cmd.word());
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("cdp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (cdp != null) {
                    cdp.restartTimer(true);
                }
                cdp = new ifcCdp(this);
                ethtyp.addSNAP(ifcCdp.orgid, "cdp", cdp);
                ethtyp.updateSNAP(ifcCdp.orgid, cdp);
                return;
            }
            if (a.equals("odr4")) {
                if (cdp == null) {
                    return;
                }
                cdp.odr4 = new addrIPv4();
                cdp.odr4.fromString(cmd.word());
                return;
            }
            if (a.equals("odr6")) {
                if (cdp == null) {
                    return;
                }
                cdp.odr6 = new addrIPv6();
                cdp.odr6.fromString(cmd.word());
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("synceth")) {
            a = cmd.word();
            if (a.equals("enable")) {
                synce = new ifcSyncE();
                ethtyp.addET(ifcSyncE.ethtyp, "synceth", synce);
                ethtyp.updateET(ifcSyncE.ethtyp, synce);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ptp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                ptp = new ifcPtp();
                ethtyp.addET(packPtp.ethtyp, "ptp", ptp);
                ethtyp.updateET(packPtp.ethtyp, ptp);
                return;
            }
            if (ptp == null) {
                return;
            }
            if (a.equals("receive")) {
                ptp.receive = true;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("lacp")) {
            lacp = new ifcLacp(this);
            lacp.sysMac.fromString(cmd.word());
            lacp.sysKey = bits.str2num(cmd.word());
            lacp.portNum = bits.str2num(cmd.word());
            ethtyp.addET(ifcLacp.ethtyp, "lacp", lacp);
            ethtyp.updateET(ifcLacp.ethtyp, lacp);
            return;
        }
        if (a.equals("udld")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (udld != null) {
                    udld.restartTimer(true);
                }
                udld = new ifcUdld(this);
                ethtyp.addSNAP(ifcUdld.orgid, "udld", udld);
                ethtyp.updateSNAP(ifcUdld.orgid, udld);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("service-instance")) {
            initVlan();
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                cmd.error("invalid evc id");
                return;
            }
            cfgIfcEvc ntry = new cfgIfcEvc(i, this);
            cfgIfcEvc old = evcs.find(ntry);
            if (old != null) {
                old.stopWork();
            }
            ntry.doCfg(cmd);
            ntry.startWork();
            evcs.put(ntry);
            return;
        }
        if (a.equals("xconnect")) {
            clear2xconnect();
            ifcEther eth = new ifcEther(ifaceNeedMacs());
            ifcConnect con = new ifcConnect();
            xconn = new cfgXconnSide();
            xconn.name = description.length() > 0 ? description : name;
            xconn.upper = con.getSide1();
            ifcUp upp = con.getSide2();
            switch (type) {
                case serial:
                case dialer:
                case virtppp:
                    xconn.pwtype = packLdpPwe.pwtPpp;
                    break;
                case tunnel:
                    xconn.pwtype = packLdpPwe.pwtIp;
                    break;
                case atm:
                    xconn.pwtype = packLdpPwe.pwtAtmAal5;
                    break;
                default:
                    xconn.pwtype = packLdpPwe.pwtEthPort;
                    xconn.upper = eth.getSideEth();
                    upp = eth.getSideTyp();
                    break;
            }
            xconn.pwmtu = ethtyp.getMTUsize();
            xconn.doCfg(cmd);
            if (!xconn.ready2run()) {
                xconn = null;
                return;
            }
            xconn.start2run();
            ethtyp.addET(-1, "xconn", upp);
            ethtyp.updateET(-1, upp);
            eth.setPromiscous(true);
            con.setPromiscous(true);
            return;
        }
        if (a.equals("pseudowire")) {
            clear2pseudowire();
            ifcUp upp = null;
            int pwt = 0;
            switch (type) {
                case virtppp:
                    upp = getEncapProto();
                    pwt = packLdpPwe.pwtPpp;
                    upp.setState(state.states.up);
                    break;
                case pweth:
                    upp = new ifcEther(false, ethtyp);
                    pwt = packLdpPwe.pwtEthPort;
                    break;
                default:
                    return;
            }
            pwhe = new cfgXconnSide();
            pwhe.upper = upp;
            pwhe.name = description.length() > 0 ? description : name;
            pwhe.pwtype = pwt;
            pwhe.pwmtu = ethtyp.getMTUsize();
            pwhe.doCfg(cmd);
            if (!pwhe.ready2run()) {
                pwhe = null;
                return;
            }
            pwhe.start2run();
            lower = pwhe.lower;
            return;
        }
        if (a.equals("ipx")) {
            doCfgIpx(cmd);
            return;
        }
        if (a.equals("ipv4")) {
            doCfgIp4(cmd);
            return;
        }
        if (a.equals("ipv6")) {
            doCfgIp6(cmd);
            return;
        }
        if (a.equals("router")) {
            doCfgRouter(cmd);
            return;
        }
        if (a.equals("tunnel")) {
            doCfgTunnel(cmd);
            return;
        }
        if (a.equals("polka")) {
            doCfgPolka(cmd);
            return;
        }
        if (a.equals("mpolka")) {
            doCfgMpolka(cmd);
            return;
        }
        if (a.equals("mpls")) {
            doCfgMpls(cmd);
            return;
        }
        if (a.equals("nsh")) {
            doCfgNsh(cmd);
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("log-link-change")) {
            ethtyp.logStateChg = false;
            return;
        }
        if (a.equals("macaddr")) {
            ethtyp.forcedMac = null;
            return;
        }
        if (a.equals("shutdown")) {
            ethtyp.forcedDN &= ~1;
            ethtyp.propagateState();
            propagateEthtypState();
            return;
        }
        if (a.equals("autostate")) {
            ethtyp.forcedUP = true;
            ethtyp.propagateState();
            propagateEthtypState();
            return;
        }
        if (a.equals("template")) {
            template = null;
            return;
        }
        if (a.equals("transproxy")) {
            clear2transproxy();
            return;
        }
        if (a.equals("monitor-filter")) {
            ethtyp.monFlt = null;
            ethtyp.ip4cor = null;
            ethtyp.ip6cor = null;
            return;
        }
        if (a.equals("monitor-buffer")) {
            ethtyp.monBufD = null;
            return;
        }
        if (a.equals("monitor-direction")) {
            ethtyp.monDir = 3;
            return;
        }
        if (a.equals("monitor-truncate")) {
            ethtyp.monTrnc = 0;
            return;
        }
        if (a.equals("monitor-sample")) {
            ethtyp.monSmpN = 0;
            return;
        }
        if (a.equals("monitor-session")) {
            ethtyp.monSes = null;
            ethtyp.monHdr = false;
            return;
        }
        if (a.equals("follow-tracker")) {
            followTrack = null;
            return;
        }
        if (a.equals("padup")) {
            ethtyp.padupMin = 0;
            ethtyp.padupMod = 0;
            return;
        }
        if (a.equals("mtu")) {
            ethtyp.forcedMTU = 0;
            propagateEthtypState();
            return;
        }
        if (a.equals("bandwidth")) {
            autoBndWdt = 0;
            ethtyp.forcedBW = 0;
            if (bundleHed != null) {
                bundleHed.bundleHed.propagateState();
            }
            return;
        }
        if (a.equals("eapol")) {
            a = cmd.word();
            if (a.equals("client")) {
                setup2eapolClnt(null, null);
                return;
            }
            if (a.equals("server")) {
                setup2eapolServ(null);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("carrier-delay")) {
            carrierDelay = 0;
            return;
        }
        if (a.equals("p2poe")) {
            a = cmd.word();
            if (a.equals("client")) {
                setup2pppoeClnt(null);
                return;
            }
            if (a.equals("server")) {
                setup2pppoeServ(null, null);
                return;
            }
            if (a.equals("relay")) {
                setup2pppoeRely(null, null);
                return;
            }
            cmd.badCmd();
        }
        if (a.equals("vlan")) {
            if (vlanHed == null) {
                initVlan();
            }
            vlanHed.vlnUnConfig(cmd);
            return;
        }
        if (a.equals("ppp")) {
            if (ppp == null) {
                cmd.error("encapsulation not in effect");
                return;
            }
            ppp.unConfig(cmd);
            ppp.clearState();
            return;
        }
        if (a.equals("bridge-group")) {
            clear2bridge();
            return;
        }
        if (a.equals("bridge-staticaddr")) {
            if (bridgeIfc == null) {
                cmd.error("not bridged");
                return;
            }
            bridgeIfc.statAddr = null;
            bridgeHed.bridgeHed.delMacs(bridgeIfc);
            return;
        }
        if (a.equals("bridge-portsecurity")) {
            if (bridgeIfc == null) {
                cmd.error("not bridged");
                return;
            }
            bridgeIfc.portSec = null;
            return;
        }
        if (a.equals("bridge-macrewrite")) {
            a = cmd.word();
            if (bridgeIfc == null) {
                cmd.error("not bridged");
                return;
            }
            bridgeIfc.macRewrite = null;
            return;
        }
        if (a.equals("bridge-filter")) {
            a = cmd.word();
            if (bridgeIfc == null) {
                return;
            }
            if (a.equals("ipv4in")) {
                bridgeIfc.filter4in = null;
                return;
            }
            if (a.equals("ipv4out")) {
                bridgeIfc.filter4out = null;
                return;
            }
            if (a.equals("ipv6in")) {
                bridgeIfc.filter6in = null;
                return;
            }
            if (a.equals("ipv6out")) {
                bridgeIfc.filter6out = null;
                return;
            }
            return;
        }
        if (a.equals("bundle-group")) {
            clear2bundle();
            return;
        }
        if (a.equals("random")) {
            if (random == null) {
                return;
            }
            ethtyp.delET(random.ethtyp);
            random.stopWork();
            random = null;
            return;
        }
        if (a.equals("enforce-mtu")) {
            ethtyp.mtuCheckRx = false;
            ethtyp.mtuCheckTx = false;
            return;
        }
        if (a.equals("sgt")) {
            a = cmd.word();
            if (a.equals("enable")) {
                ethtyp.sgtHnd = null;
                return;
            }
            if (a.equals("assign")) {
                ethtyp.sgtSet = 0;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("loss-detection")) {
            ethtyp.lossDet = null;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("disable-sgt")) {
            disableSgt = false;
            return;
        }
        if (a.equals("disable-macsec")) {
            disableMacsec = false;
            return;
        }
        if (a.equals("macsec")) {
            ethtyp.macSec = null;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("service-policy-in")) {
            ethtyp.qosIn = null;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("service-policy-out")) {
            ethtyp.qosOut = null;
            ethtyp.timerUpdate();
            return;
        }
        if (a.equals("vrf")) {
            a = cmd.word();
            if (a.equals("forwarding")) {
                clear2routing(true, true);
                clear2vrf();
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("lldp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (lldp == null) {
                    return;
                }
                lldp.restartTimer(true);
                lldp = null;
                ethtyp.delET(ifcLldp.ethtyp);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("nhrp")) {
            a = cmd.word();
            if (nhrp == null) {
                return;
            }
            if (a.equals("ipv4")) {
                nhrp.ip4 = null;
            }
            if (a.equals("ipv6")) {
                nhrp.ip6 = null;
            }
            if ((nhrp.ip4 != null) || (nhrp.ip6 != null)) {
                return;
            }
            nhrp.restartTimer(true);
            nhrp = null;
            ethtyp.delET(ifcNhrp.ethtyp);
            return;
        }
        if (a.equals("cdp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (cdp == null) {
                    return;
                }
                cdp.restartTimer(true);
                cdp = null;
                ethtyp.delSNAP(ifcCdp.orgid);
                return;
            }
            if (a.equals("odr4")) {
                if (cdp == null) {
                    return;
                }
                cdp.odr4 = null;
                return;
            }
            if (a.equals("odr6")) {
                if (cdp == null) {
                    return;
                }
                cdp.odr6 = null;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("synceth")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (synce == null) {
                    return;
                }
                synce.stopWork();
                synce = null;
                ethtyp.delET(ifcSyncE.ethtyp);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ptp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (ptp == null) {
                    return;
                }
                ptp.stopWork();
                ptp = null;
                ethtyp.delET(packPtp.ethtyp);
                return;
            }
            if (ptp == null) {
                return;
            }
            if (a.equals("receive")) {
                ptp.receive = false;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("lacp")) {
            if (lacp == null) {
                return;
            }
            lacp.stopWork();
            lacp = null;
            ethtyp.delET(ifcLacp.ethtyp);
            return;
        }
        if (a.equals("udld")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (udld == null) {
                    return;
                }
                udld.restartTimer(true);
                udld = null;
                ethtyp.delSNAP(ifcUdld.orgid);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("service-instance")) {
            initVlan();
            cfgIfcEvc ntry = evcs.find(new cfgIfcEvc(bits.str2num(cmd.word()), this));
            if (ntry == null) {
                cmd.error("no such evc");
                return;
            }
            evcs.del(ntry);
            ntry.stopWork();
            return;
        }
        if (a.equals("xconnect")) {
            clear2xconnect();
            return;
        }
        if (a.equals("pseudowire")) {
            clear2pseudowire();
            return;
        }
        if (a.equals("ipx")) {
            doCfgNoIpx(cmd);
            return;
        }
        if (a.equals("ipv4")) {
            doCfgNoIp4(cmd);
            return;
        }
        if (a.equals("ipv6")) {
            doCfgNoIp6(cmd);
            return;
        }
        if (a.equals("router")) {
            doCfgNoRouter(cmd);
            return;
        }
        if (a.equals("tunnel")) {
            doCfgNoTunnel(cmd);
            return;
        }
        if (a.equals("polka")) {
            doCfgNoPolka(cmd);
            return;
        }
        if (a.equals("mpolka")) {
            doCfgNoMpolka(cmd);
            return;
        }
        if (a.equals("mpls")) {
            doCfgNoMpls(cmd);
            return;
        }
        if (a.equals("nsh")) {
            doCfgNoNsh(cmd);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgIpx(cmds cmd) {
        if (vrfFor == null) {
            cmd.error("no vrf membership selected");
            return;
        }
        String a = cmd.word();
        if (a.equals("network")) {
            addrIpx adr = new addrIpx();
            if (adr.fromString(cmd.word() + "." + addrMac.getRandom())) {
                cmd.error("bad network format");
                return;
            }
            ipxAddr = adr;
            setup2vrf(false, false, true);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoIpx(cmds cmd) {
        String a = cmd.word();
        if (a.equals("network")) {
            ipxAddr = null;
            setup2vrf(false, false, true);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgIp4(cmds cmd) {
        if (vrfFor == null) {
            cmd.error("no vrf membership selected");
            return;
        }
        String a = cmd.word();
        if (a.equals("address")) {
            addrIPv4 adr = new addrIPv4();
            addrIPv4 msk = new addrIPv4();
            a = cmd.word();
            hide4adr = a.equals("dynamic");
            if (hide4adr) {
                a = "" + addrIPv4.getEmpty();
            }
            if (adr.fromString(a)) {
                cmd.error("invalid address");
                return;
            }
            a = cmd.word();
            hide4msk = a.equals("dynamic");
            if (hide4msk) {
                a = "/" + new addrIPv4().maxBits();
            }
            if (a.startsWith("/")) {
                addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(adr, bits.str2num(a.substring(1, a.length())));
                msk = prf.mask;
            } else {
                if (msk.fromString(a)) {
                    cmd.error("invalid netmask");
                    return;
                }
            }
            addr4 = adr;
            mask4 = msk;
            clear2routing(true, false);
            setup2vrf(true, false, false);
            return;
        }
        if (addr4 == null) {
            cmd.error("protocol not enabled");
            return;
        }
        if (a.equals("redirection")) {
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such interface");
                return;
            }
            ipIf4.redirect = ntry.ipIf4;
            return;
        }
        if (a.equals("dhcp-client")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (dhcp4c != null) {
                    return;
                }
                dhcp4c = new clntDhcp4(vrfFor.udp4, fwdIf4, ethtyp, this);
                return;
            }
            if (dhcp4c == null) {
                cmd.error("protocol not enabled");
                return;
            }
            if (!dhcp4c.doConfig(a, cmd)) {
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("pool")) {
            ip4polC = cfgAll.poolFind(cfgAll.ip4pool, cmd.word(), false);
            return;
        }
        if (!fwdIf4.doConfig(a, cmd, vrfFor.core4, vrfFor.fwd4, vrfFor.udp4)) {
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoIp4(cmds cmd) {
        if (vrfFor == null) {
            cmd.error("no vrf membership selected");
            return;
        }
        String a = cmd.word();
        if (a.equals("address")) {
            addr4 = null;
            mask4 = null;
            hide4adr = false;
            hide4msk = false;
            clear2routing(true, false);
            setup2vrf(true, false, false);
            return;
        }
        if (addr4 == null) {
            cmd.error("protocol not enabled");
            return;
        }
        if (a.equals("redirection")) {
            ipIf4.redirect = null;
            return;
        }
        if (a.equals("dhcp-client")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (dhcp4c == null) {
                    return;
                }
                dhcp4c.closeClient();
                dhcp4c = null;
                return;
            }
            if (dhcp4c == null) {
                cmd.error("protocol not enabled");
                return;
            }
            if (!dhcp4c.unConfig(a)) {
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("pool")) {
            ip4polC = null;
            return;
        }
        if (!fwdIf4.unConfig(a, cmd, vrfFor.fwd4)) {
            return;
        }
        cmd.badCmd();
    }

    private void doCfgIp6(cmds cmd) {
        if (vrfFor == null) {
            cmd.error("no vrf membership selected");
            return;
        }
        String a = cmd.word();
        if (a.equals("address")) {
            addrIPv6 adr = new addrIPv6();
            addrIPv6 msk = new addrIPv6();
            a = cmd.word();
            hide6adr = a.equals("dynamic");
            if (hide6adr) {
                a = "" + addrIPv6.getEmpty();
            }
            if (adr.fromString(a)) {
                cmd.error("invalid address");
                return;
            }
            a = cmd.word();
            hide6msk = a.equals("dynamic");
            if (hide6msk) {
                a = "/" + new addrIPv6().maxBits();
            }
            if (a.startsWith("/")) {
                addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr, bits.str2num(a.substring(1, a.length())));
                msk = prf.mask;
            } else {
                if (msk.fromString(a)) {
                    cmd.error("invalid netmask");
                    return;
                }
            }
            addr6 = adr;
            mask6 = msk;
            clear2routing(false, true);
            setup2vrf(false, true, false);
            return;
        }
        if (addr6 == null) {
            cmd.error("protocol not enabled");
            return;
        }
        if (a.equals("redirection")) {
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such interface");
                return;
            }
            ipIf6.redirect = ntry.ipIf6;
            return;
        }
        if (a.equals("dhcp-client")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (dhcp6c != null) {
                    return;
                }
                dhcp6c = new clntDhcp6(vrfFor.udp6, fwdIf6, ipIf6, ethtyp, this);
                return;
            }
            if (dhcp6c == null) {
                cmd.error("protocol not enabled");
                return;
            }
            if (!dhcp6c.doConfig(a, cmd)) {
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("pool")) {
            ip6polC = cfgAll.poolFind(cfgAll.ip6pool, cmd.word(), false);
            return;
        }
        if (a.equals("slaac")) {
            slaac = new clntSlaac(vrfFor.fwd6, fwdIf6, ipIf6, ethtyp, this);
            return;
        }
        if (a.equals("prefix-suppress")) {
            ipIf6.rtrAdvSuppress = true;
            return;
        }
        if (a.equals("prefix-interval")) {
            ipIf6.rtrAdvInterval = bits.str2num(cmd.word());
            ipIf6.resetTimer(true);
            return;
        }
        if (a.equals("prefix-validity")) {
            ipIf6.rtrAdvValidity = bits.str2num(cmd.word());
            ipIf6.resetTimer(true);
            return;
        }
        if (a.equals("prefix-dns")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                return;
            }
            ipIf6.rtrAdvDns = adr;
            return;
        }
        if (a.equals("prefix-domain")) {
            ipIf6.rtrAdvDom = cmd.word();
            return;
        }
        if (!fwdIf6.doConfig(a, cmd, vrfFor.core6, vrfFor.fwd6, vrfFor.udp6)) {
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoIp6(cmds cmd) {
        if (vrfFor == null) {
            cmd.error("no vrf membership selected");
            return;
        }
        String a = cmd.word();
        if (a.equals("address")) {
            addr6 = null;
            mask6 = null;
            hide6adr = false;
            hide6msk = false;
            clear2routing(false, true);
            setup2vrf(false, true, false);
            return;
        }
        if (addr6 == null) {
            cmd.error("protocol not enabled");
            return;
        }
        if (a.equals("redirection")) {
            ipIf6.redirect = null;
            return;
        }
        if (a.equals("dhcp-client")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (dhcp6c == null) {
                    return;
                }
                dhcp6c.closeClient();
                dhcp6c = null;
                return;
            }
            if (dhcp6c == null) {
                cmd.error("protocol not enabled");
                return;
            }
            if (!dhcp6c.unConfig(a)) {
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("pool")) {
            ip6polC = null;
            return;
        }
        if (a.equals("slaac")) {
            if (slaac == null) {
                return;
            }
            slaac.closeClient();
            slaac = null;
            return;
        }
        if (a.equals("prefix-suppress")) {
            ipIf6.rtrAdvSuppress = false;
            return;
        }
        if (a.equals("prefix-dns")) {
            ipIf6.rtrAdvDns = null;
            return;
        }
        if (a.equals("prefix-domain")) {
            ipIf6.rtrAdvDom = null;
            return;
        }
        if (!fwdIf6.unConfig(a, cmd, vrfFor.fwd6)) {
            return;
        }
        cmd.badCmd();
    }

    private void doCfgTunnel(cmds cmd) {
        if (type != ifaceType.tunnel) {
            cmd.error("not a tunnel interface");
            return;
        }
        String a = cmd.word();
        if (a.equals("ttl")) {
            tunTTL = bits.str2num(cmd.word());
            setup2tunnel();
            return;
        }
        if (a.equals("tos")) {
            tunTOS = bits.str2num(cmd.word());
            setup2tunnel();
            return;
        }
        if (a.equals("flow")) {
            tunFLW = bits.str2num(cmd.word());
            setup2tunnel();
            return;
        }
        if (a.equals("sequence-datagrams")) {
            tunSeq = true;
            setup2tunnel();
            return;
        }
        if (a.equals("association")) {
            tunAscAdr = new addrIP();
            tunAscAdr.fromString(cmd.word());
            tunAscId = bits.str2num(cmd.word());
            tunAscId2 = bits.str2num(cmd.word());
            setup2tunnel();
            return;
        }
        if (a.equals("priority")) {
            tunPri = bits.str2num(cmd.word());
            setup2tunnel();
            return;
        }
        if (a.equals("checksum")) {
            tunSum = true;
            setup2tunnel();
            return;
        }
        if (a.equals("key")) {
            tunKey = bits.str2num(cmd.word());
            tunKey2 = bits.str2num(cmd.word());
            setup2tunnel();
            return;
        }
        if (a.equals("vrf")) {
            cfgVrf v = cfgAll.vrfFind(cmd.word(), false);
            if (v == null) {
                cmd.error("invalid vrf");
                return;
            }
            tunVrf = v;
            setup2tunnel();
            return;
        }
        if (a.equals("shutdown")) {
            tunShut = true;
            setup2tunnel();
            return;
        }
        if (a.equals("source")) {
            cfgIfc i = cfgAll.ifcFind(cmd.word(), false);
            if (i == null) {
                cmd.error("no such interface");
                return;
            }
            tunSrc = i;
            setup2tunnel();
            return;
        }
        if (a.equals("destination")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("invalid address");
                return;
            }
            tunTrg = adr;
            setup2tunnel();
            return;
        }
        if (a.equals("domain-name")) {
            tunFQDN = cmd.getRemaining();
            return;
        }
        if (a.equals("mode")) {
            tunnelType i = string2tunnelMode(cmd.word());
            if (i == null) {
                cmd.error("invalid mode");
                return;
            }
            tunMode = i;
            setup2tunnel();
            return;
        }
        if (a.equals("protection")) {
            cfgIpsec ips = cfgAll.ipsecFind(cmd.word(), false);
            if (ips == null) {
                cmd.error("no such profile");
                return;
            }
            tunPrt = ips;
            setup2tunnel();
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoTunnel(cmds cmd) {
        if (type != ifaceType.tunnel) {
            cmd.error("not a tunnel interface");
            return;
        }
        String a = cmd.word();
        if (a.equals("ttl")) {
            tunTTL = 255;
            setup2tunnel();
            return;
        }
        if (a.equals("tos")) {
            tunTOS = -1;
            setup2tunnel();
            return;
        }
        if (a.equals("flow")) {
            tunFLW = -1;
            setup2tunnel();
            return;
        }
        if (a.equals("sequence-datagrams")) {
            tunSeq = false;
            setup2tunnel();
            return;
        }
        if (a.equals("association")) {
            tunAscAdr = null;
            tunAscId = 0;
            tunAscId2 = 0;
            setup2tunnel();
            return;
        }
        if (a.equals("priority")) {
            tunPri = 7;
            setup2tunnel();
            return;
        }
        if (a.equals("checksum")) {
            tunSum = false;
            setup2tunnel();
            return;
        }
        if (a.equals("key")) {
            tunKey = 0;
            tunKey2 = 0;
            setup2tunnel();
            return;
        }
        if (a.equals("vrf")) {
            tunVrf = null;
            setup2tunnel();
            return;
        }
        if (a.equals("shutdown")) {
            tunShut = false;
            setup2tunnel();
            return;
        }
        if (a.equals("source")) {
            tunSrc = null;
            setup2tunnel();
            return;
        }
        if (a.equals("destination")) {
            tunTrg = null;
            setup2tunnel();
            return;
        }
        if (a.equals("domain-name")) {
            tunFQDN = null;
            return;
        }
        if (a.equals("mode")) {
            tunMode = null;
            setup2tunnel();
            return;
        }
        if (a.equals("protection")) {
            setup2tunnel();
            tunPrt = null;
            return;
        }
        cmd.badCmd();
    }

    private void doCfgRouter(cmds cmd) {
        tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
        int i = bits.str2num(cmd.word());
        cfgRtr rtr = cfgAll.rtrFind(o, i, false);
        if (rtr == null) {
            cmd.error("no such router process");
            return;
        }
        String a = cmd.word();
        if (o == tabRouteAttr.routeType.babel4) {
            if (a.equals("enable")) {
                clear2router(rtrBabel4hnd);
                setup2router(rtr);
                return;
            }
            if (rtrBabel4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrBabel4ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.babel6) {
            if (a.equals("enable")) {
                clear2router(rtrBabel6hnd);
                setup2router(rtr);
                return;
            }
            if (rtrBabel6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrBabel6ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.olsr4) {
            if (a.equals("enable")) {
                clear2router(rtrOlsr4hnd);
                setup2router(rtr);
                return;
            }
            if (rtrOlsr4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrOlsr4ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.olsr6) {
            if (a.equals("enable")) {
                clear2router(rtrOlsr6hnd);
                setup2router(rtr);
                return;
            }
            if (rtrOlsr6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrOlsr6ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.rip4) {
            if (a.equals("enable")) {
                clear2router(rtrRip4hnd);
                setup2router(rtr);
                return;
            }
            if (rtrRip4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrRip4ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.rip6) {
            if (a.equals("enable")) {
                clear2router(rtrRip6hnd);
                setup2router(rtr);
                return;
            }
            if (rtrRip6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrRip6ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.ospf4) {
            if (a.equals("enable")) {
                clear2router(rtrOspf4hnd);
                setup2router(rtr);
                return;
            }
            if (rtrOspf4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrOspf4ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.ospf6) {
            if (a.equals("enable")) {
                clear2router(rtrOspf6hnd);
                setup2router(rtr);
                return;
            }
            if (rtrOspf6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrOspf6ifc.routerDoConfig(a, cmd);
            return;
        }
        if ((o == tabRouteAttr.routeType.isis4) || (o == tabRouteAttr.routeType.isis6)) {
            if (a.equals("enable")) {
                clear2router(rtrIsisHnd);
                setup2router(rtr);
                return;
            }
            if (rtrIsisHnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrIsisIfc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.pvrp4) {
            if (a.equals("enable")) {
                clear2router(rtrPvrp4hnd);
                setup2router(rtr);
                return;
            }
            if (rtrPvrp4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrPvrp4ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.pvrp6) {
            if (a.equals("enable")) {
                clear2router(rtrPvrp6hnd);
                setup2router(rtr);
                return;
            }
            if (rtrPvrp6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrPvrp6ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.lsrp4) {
            if (a.equals("enable")) {
                clear2router(rtrLsrp4hnd);
                setup2router(rtr);
                return;
            }
            if (rtrLsrp4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrLsrp4ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.lsrp6) {
            if (a.equals("enable")) {
                clear2router(rtrLsrp6hnd);
                setup2router(rtr);
                return;
            }
            if (rtrLsrp6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrLsrp6ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.eigrp4) {
            if (a.equals("enable")) {
                clear2router(rtrEigrp4hnd);
                setup2router(rtr);
                return;
            }
            if (rtrEigrp4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrEigrp4ifc.routerDoConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.eigrp6) {
            if (a.equals("enable")) {
                clear2router(rtrEigrp6hnd);
                setup2router(rtr);
                return;
            }
            if (rtrEigrp6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            rtrEigrp6ifc.routerDoConfig(a, cmd);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoRouter(cmds cmd) {
        tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
        int i = bits.str2num(cmd.word());
        cfgRtr rtr = cfgAll.rtrFind(o, i, false);
        if (rtr == null) {
            cmd.error("no such router process");
            return;
        }
        String a = cmd.word();
        if (o == tabRouteAttr.routeType.babel4) {
            if (rtrBabel4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrBabel4ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.babel6) {
            if (rtrBabel6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrBabel6ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.olsr4) {
            if (rtrOlsr4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrOlsr4ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.olsr6) {
            if (rtrOlsr6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrOlsr6ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.rip4) {
            if (rtrRip4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrRip4ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.rip6) {
            if (rtrRip6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrRip6ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.ospf4) {
            if (rtrOspf4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrOspf4ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.ospf6) {
            if (rtrOspf6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrOspf6ifc.routerUnConfig(a, cmd);
            return;
        }
        if ((o == tabRouteAttr.routeType.isis4) || (o == tabRouteAttr.routeType.isis6)) {
            if (rtrIsisHnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrIsisIfc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.pvrp4) {
            if (rtrPvrp4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrPvrp4ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.pvrp6) {
            if (rtrPvrp6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrPvrp6ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.lsrp4) {
            if (rtrLsrp4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrLsrp4ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.lsrp6) {
            if (rtrLsrp6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrLsrp6ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.eigrp4) {
            if (rtrEigrp4hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrEigrp4ifc.routerUnConfig(a, cmd);
            return;
        }
        if (o == tabRouteAttr.routeType.eigrp6) {
            if (rtrEigrp6hnd == null) {
                cmd.error("process not enabled on interface");
                return;
            }
            if (a.equals("enable")) {
                clear2router(rtr);
                return;
            }
            rtrEigrp6ifc.routerUnConfig(a, cmd);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgMpls(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            setup2mpls();
            return;
        }
        if (s.equals("access-group-in")) {
            if (mplsPack == null) {
                return;
            }
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return;
            }
            mplsPack.filterIn = ntry.aceslst;
            return;
        }
        if (s.equals("access-group-out")) {
            if (mplsPack == null) {
                return;
            }
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return;
            }
            mplsPack.filterOut = ntry.aceslst;
            return;
        }
        if (s.equals("access-group-common-in")) {
            if (mplsPack == null) {
                return;
            }
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return;
            }
            mplsPack.cfilterIn = ntry.aceslst;
            return;
        }
        if (s.equals("access-group-common-out")) {
            if (mplsPack == null) {
                return;
            }
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return;
            }
            mplsPack.cfilterOut = ntry.aceslst;
            return;
        }
        if (s.equals("inspect")) {
            if (mplsPack == null) {
                return;
            }
            if (mplsPack.inspect != null) {
                mplsPack.inspect.stopTimer();
            }
            mplsPack.inspect = new tabSession(true, 180000);
            mplsPack.inspect.fromString(cmd);
            mplsPack.inspect.startTimer();
            return;
        }
        if (s.equals("label-security")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.security = true;
            return;
        }
        if (s.equals("netflow-rx")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.netflowRx = true;
            return;
        }
        if (s.equals("netflow-tx")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.netflowTx = true;
            return;
        }
        if (s.equals("redirection")) {
            if (mplsPack == null) {
                return;
            }
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such interface");
                return;
            }
            mplsPack.redirect = ntry.mplsPack;
            return;
        }
        if (s.equals("ldp4")) {
            setup2ldp(4, cmd);
            return;
        }
        if (s.equals("ldp6")) {
            setup2ldp(6, cmd);
            return;
        }
        if (s.equals("label4pop")) {
            if (mplsLdp4 == null) {
                return;
            }
            mplsLdp4.labelPop = true;
            return;
        }
        if (s.equals("label6pop")) {
            if (mplsLdp6 == null) {
                return;
            }
            mplsLdp6.labelPop = true;
            return;
        }
        if (s.equals("label4in")) {
            if (mplsLdp4 == null) {
                return;
            }
            cfgPrfxlst res = cfgAll.prfxFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such prefix list");
                return;
            }
            mplsLdp4.filterIn = res.prflst;
            return;
        }
        if (s.equals("label4out")) {
            if (mplsLdp4 == null) {
                return;
            }
            cfgPrfxlst res = cfgAll.prfxFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such prefix list");
                return;
            }
            mplsLdp4.filterOut = res.prflst;
            return;
        }
        if (s.equals("label6in")) {
            if (mplsLdp6 == null) {
                return;
            }
            cfgPrfxlst res = cfgAll.prfxFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such prefix list");
                return;
            }
            mplsLdp6.filterIn = res.prflst;
            return;
        }
        if (s.equals("label6out")) {
            if (mplsLdp6 == null) {
                return;
            }
            cfgPrfxlst res = cfgAll.prfxFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such prefix list");
                return;
            }
            mplsLdp6.filterOut = res.prflst;
            return;
        }
        if (s.equals("ldptarget")) {
            setup2ldptrg(cmd.word());
            return;
        }
        if (s.equals("ldppassword")) {
            s = cmd.word();
            setup2ldppwd(s, cmd.word());
            return;
        }
        if (s.equals("rsvp4")) {
            setup2rsvp(4);
            return;
        }
        if (s.equals("rsvp6")) {
            setup2rsvp(6);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoMpls(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            clear2mpls();
            return;
        }
        if (s.equals("access-group-in")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.filterIn = null;
            return;
        }
        if (s.equals("access-group-out")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.filterOut = null;
            return;
        }
        if (s.equals("access-group-common-in")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.cfilterIn = null;
            return;
        }
        if (s.equals("access-group-common-out")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.cfilterOut = null;
            return;
        }
        if (s.equals("inspect")) {
            if (mplsPack == null) {
                return;
            }
            if (mplsPack.inspect != null) {
                mplsPack.inspect.stopTimer();
            }
            mplsPack.inspect = null;
            return;
        }
        if (s.equals("netflow-rx")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.netflowRx = false;
            return;
        }
        if (s.equals("netflow-tx")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.netflowTx = false;
            return;
        }
        if (s.equals("label-security")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.security = false;
            return;
        }
        if (s.equals("redirection")) {
            if (mplsPack == null) {
                return;
            }
            mplsPack.redirect = null;
            return;
        }
        if (s.equals("ldp4")) {
            clear2ldp(4);
            return;
        }
        if (s.equals("ldp6")) {
            clear2ldp(6);
            return;
        }
        if (s.equals("label4pop")) {
            if (mplsLdp4 == null) {
                return;
            }
            mplsLdp4.labelPop = false;
            return;
        }
        if (s.equals("label6pop")) {
            if (mplsLdp6 == null) {
                return;
            }
            mplsLdp6.labelPop = false;
            return;
        }
        if (s.equals("label4in")) {
            if (mplsLdp4 == null) {
                return;
            }
            mplsLdp4.filterIn = null;
            return;
        }
        if (s.equals("label4out")) {
            if (mplsLdp4 == null) {
                return;
            }
            mplsLdp4.filterOut = null;
            return;
        }
        if (s.equals("label6in")) {
            if (mplsLdp6 == null) {
                return;
            }
            mplsLdp6.filterIn = null;
            return;
        }
        if (s.equals("label6out")) {
            if (mplsLdp6 == null) {
                return;
            }
            mplsLdp6.filterOut = null;
            return;
        }
        if (s.equals("ldptarget")) {
            clear2ldptrg(cmd.word());
            return;
        }
        if (s.equals("ldppassword")) {
            clear2ldppwd(cmd.word());
            return;
        }
        if (s.equals("rsvp4")) {
            clear2rsvp(4);
            return;
        }
        if (s.equals("rsvp6")) {
            clear2rsvp(6);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNsh(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            setup2nshFwd();
            return;
        }
        if (s.equals("xconnect")) {
            int p = bits.str2num(cmd.word());
            int i = bits.str2num(cmd.word());
            setup2nshXcn(p, i);
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoNsh(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            clear2nshFwd();
            return;
        }
        if (s.equals("xconnect")) {
            clear2nshXcn();
            return;
        }
        cmd.badCmd();
    }

    private void doCfgPolka(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            int i = bits.str2num(cmd.word());
            int o = bits.str2num(cmd.word());
            setup2polka(i, o, bits.str2num(cmd.word()));
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoPolka(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            clear2polka();
            return;
        }
        cmd.badCmd();
    }

    private void doCfgMpolka(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            int i = bits.str2num(cmd.word());
            int o = bits.str2num(cmd.word());
            setup2mpolka(i, o, bits.str2num(cmd.word()));
            return;
        }
        cmd.badCmd();
    }

    private void doCfgNoMpolka(cmds cmd) {
        String s = cmd.word();
        if (s.equals("enable")) {
            clear2mpolka();
            return;
        }
        cmd.badCmd();
    }

}
