package org.freertr.user;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authLocal;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAlias;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgChat;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgLin;
import org.freertr.cfg.cfgMenuK;
import org.freertr.cfg.cfgMenuT;
import org.freertr.cfg.cfgPrcss;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgSched;
import org.freertr.cfg.cfgScrpt;
import org.freertr.cfg.cfgTrack;
import org.freertr.cfg.cfgVdc;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntCurl;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntPorts;
import org.freertr.clnt.clntProxy;
import org.freertr.prt.prtTraceroute;
import org.freertr.clnt.clntWhois;
import org.freertr.enc.encBase64;
import org.freertr.ifc.ifcNull;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdEcho;
import org.freertr.ip.ipFwdEchod;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipRtr;
import org.freertr.line.lineHdlc;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packDnsZone;
import org.freertr.pack.packText;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeShell;
import org.freertr.pipe.pipeSide;
import org.freertr.pipe.pipeTerm;
import org.freertr.prt.prtAccept;
import org.freertr.prt.prtGen;
import org.freertr.prt.prtRedun;
import org.freertr.rtr.rtrBgpParam;
import org.freertr.serv.servGenList;
import org.freertr.serv.servGeneric;
import org.freertr.enc.encUrl;
import org.freertr.prt.prtIcmptun;
import org.freertr.tab.tabRouteAttr;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.differ;
import org.freertr.util.logger;
import org.freertr.util.version;

/**
 * process exec commands
 *
 * @author matecsaba
 */
public class userExec {

    /**
     * pipe to use
     */
    protected final pipeSide pipe;

    /**
     * time of last command
     */
    public long last;

    /**
     * reader of user
     */
    protected final userReader reader;

    /**
     * currently processed string
     */
    public cmds cmd;

    /**
     * privileged commands allowed
     */
    public boolean privileged;

    /**
     * authenticated username
     */
    public String username = "<nobody>";

    /**
     * authorization list
     */
    public authGeneric authorization;

    /**
     * expand variables
     */
    public boolean needExpand;

    /**
     * framed interface handler
     */
    public cfgIfc framedIface;

    /**
     * physical line, no clone
     */
    public boolean physicalLin;

    /**
     * rollback protected configuration session requested
     */
    public boolean rollback;

    /**
     * command results
     */
    protected enum cmdRes {

        /**
         * continue command processing
         */
        command,
        /**
         * logout requested
         */
        logout,
        /**
         * configuration requested
         */
        config

    }

    /**
     * constructs new reader for a pipeline
     *
     * @param pip pipeline to use as input
     * @param rdr reader to use as input
     */
    public userExec(pipeSide pip, userReader rdr) {
        pipe = pip;
        reader = rdr;
    }

    /**
     * copy credentinals to exec
     *
     * @param o other
     */
    public void copy2exec(userExec o) {
        o.needExpand = needExpand;
        o.username = username;
        o.privileged = privileged;
        o.authorization = authorization;
    }

    /**
     * copy credentinals to config
     *
     * @param o other
     */
    public void copy2cfg(userConfig o) {
        o.needExpand = needExpand;
        o.username = username;
        o.authorization = authorization;
    }

    /**
     * update last command time
     *
     * @param o other
     */
    public void updateLast(userExec o) {
        if (o.last < last) {
            return;
        }
        last = o.last;
    }

    private static void getSecretHelp(userHelping hl, int beg) {
        for (int i = 0; i < version.secrets.length; i++) {
            String a = version.secrets[i][0];
            hl.add(null, beg + " .  " + a + "   sh0w m30www s0m30www " + a);
        }
    }

    private void getHelpClearIpX(userHelping hl) {
        hl.add(null, "3 4        savemrt               dump routing table");
        hl.add(null, "4 5          <name:vrf>          vrf name");
        hl.add(null, "5 .            <name>            name of file");
        hl.add(null, "3 4        route                 routing table");
        hl.add(null, "4 .          <name:vrf>          vrf name");
        hl.add(null, "3 4        nat                   address translation table");
        hl.add(null, "4 .          <name:vrf>          vrf name");
        hl.add(null, "3 4        babel                 babel routing protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        bfd                   bidirectional forwarding detection");
        hl.add(null, "4 5          <name:vrf>          vrf name");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        bgp                   border gateway protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 6            peer              select address");
        hl.add(null, "6 7              <addr>          neighbor address regexp");
        hl.add(null, "7 .                hard          flap session");
        hl.add(null, "7 8                save          export an afi");
        rtrBgpParam.getAfiList(hl, "8 9", "clear", false);
        hl.add(null, "9 .                  <name>      name of file");
        hl.add(null, "7 8                in            send route refresh");
        hl.add(null, "7 8                out           resend prefixes");
        hl.add(null, "7 8                add           add an afi");
        hl.add(null, "7 8                del           delete an afi");
        rtrBgpParam.getAfiList(hl, "8 .", "clear", false);
        hl.add(null, "5 6            asn               select asn");
        hl.add(null, "6 7              <num>           neighbor asn regexp");
        hl.add(null, "7 .                hard          flap session");
        hl.add(null, "7 8                save          export an afi");
        rtrBgpParam.getAfiList(hl, "8 9", "clear", false);
        hl.add(null, "9 .                  <name>      name of file");
        hl.add(null, "7 8                in            send route refresh");
        hl.add(null, "7 8                out           resend prefixes");
        hl.add(null, "7 8                add           add an afi");
        hl.add(null, "7 8                del           delete an afi");
        rtrBgpParam.getAfiList(hl, "8 .", "clear", false);
        hl.add(null, "5 7            ibgp              select ibgp peers");
        hl.add(null, "7 .                hard          flap session");
        hl.add(null, "7 8                save          export an afi");
        rtrBgpParam.getAfiList(hl, "8 9", "clear", false);
        hl.add(null, "9 .                  <name>      name of file");
        hl.add(null, "7 8                in            send route refresh");
        hl.add(null, "7 8                out           resend prefixes");
        hl.add(null, "7 8                add           add an afi");
        hl.add(null, "7 8                del           delete an afi");
        rtrBgpParam.getAfiList(hl, "8 .", "clear", false);
        hl.add(null, "5 7            ebgp              select ebgp peers");
        hl.add(null, "7 .                hard          flap session");
        hl.add(null, "7 8                save          export an afi");
        rtrBgpParam.getAfiList(hl, "8 9", "clear", false);
        hl.add(null, "9 .                  <name>      name of file");
        hl.add(null, "7 8                in            send route refresh");
        hl.add(null, "7 8                out           resend prefixes");
        hl.add(null, "7 8                add           add an afi");
        hl.add(null, "7 8                del           delete an afi");
        rtrBgpParam.getAfiList(hl, "8 .", "clear", false);
        hl.add(null, "5 7            all               select every peer");
        hl.add(null, "7 .                hard          flap session");
        hl.add(null, "7 8                save          export an afi");
        rtrBgpParam.getAfiList(hl, "8 9", "clear", false);
        hl.add(null, "9 .                  <name>      name of file");
        hl.add(null, "7 8                in            send route refresh");
        hl.add(null, "7 8                out           resend prefixes");
        hl.add(null, "7 8                add           add an afi");
        hl.add(null, "7 8                del           delete an afi");
        rtrBgpParam.getAfiList(hl, "8 .", "clear", false);
        hl.add(null, "5 .            recompute         trigger full compute round");
        hl.add(null, "5 .            flaps             collected flap statistics");
        hl.add(null, "5 .            peaks             collected peak statistics");
        hl.add(null, "5 .            tinys             collected tiny statistics");
        hl.add(null, "5 .            attribs           collected attributes statistics");
        hl.add(null, "5 .            messages          collected message statistics");
        hl.add(null, "3 4        eigrp                 enhanced interior gateway routing protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        isis                  intermediate system to intermediate system");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 6            <addr>            neighbor address");
        hl.add(null, "6 .              <num>           level");
        hl.add(null, "3 4        ldp                   label distribution protocol");
        hl.add(null, "4 5          <name:vrf>          vrf name");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        lsrp                  link state routing protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        msdp                  multicast source discovery protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        rpki                  resource public key infra protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        olsr                  optimized link state routing");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        ospf                  open shortest path first");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 6            <num>             area id");
        hl.add(null, "6 .              <addr>          neighbor address");
        hl.add(null, "3 4        rift                  routing in fat trees");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        pvrp                  path vector routing protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        rip                   routing information protocol");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            <addr>            neighbor address");
        hl.add(null, "3 4        logger                route flap collector");
        hl.add(null, "4 .          <num:rtr>           process id");
        hl.add(null, "3 4        ghosthunt             ghost hunter");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 .            stop              stop processing");
        hl.add(null, "5 .            start             start processing");
    }

    private static void getHelpShowIpX(userHelping hl) {
        hl.add(null, "3 4,.      interface                interface information");
        hl.add(null, "4 .          [name:ifc]             name of interface");
        hl.add(null, "3 4,.      vrf                      vrf information");
        hl.add(null, "4 5          [name:vrf]             name of vrf");
        hl.add(null, "5 .            full                 full listing");
        hl.add(null, "5 .            interface            interface list");
        hl.add(null, "5 .            rates                traffic rates");
        hl.add(null, "5 .            realtime             realtime counters");
        hl.add(null, "5 .            history              historic byte counters");
        hl.add(null, "5 .            rxhistory            historic rx byte counters");
        hl.add(null, "5 .            txhistory            historic tx byte counters");
        hl.add(null, "5 .            drhistory            historic drop byte counters");
        hl.add(null, "5 .            phistory             historic packet counters");
        hl.add(null, "5 .            rxphistory           historic rx packet counters");
        hl.add(null, "5 .            txphistory           historic tx packet counters");
        hl.add(null, "5 .            drphistory           historic drop packet counters");
        hl.add(null, "5 .            numhist              numeric historic byte counters");
        hl.add(null, "5 .            numphist             numeric historic packet counters");
        hl.add(null, "3 4        counter                  unicast routing table traffic");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        compress                 compressed unicast routing table");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        unused                   unused unicast routing table");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        changes                  changed unicast routing table");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        just-network             unicast routing table by network");
        hl.add(null, "4 5          <name:vrf>             name of routing table");
        hl.add(null, "5 5,.          <str>                prefix matcher");
        hl.add(null, "3 4        just-interface           unicast routing table by target");
        hl.add(null, "4 5          <name:vrf>             name of routing table");
        hl.add(null, "5 .            <name:ifc>           name of interface");
        hl.add(null, "3 4        just-nexthop             unicast routing table by target");
        hl.add(null, "4 5          <name:vrf>             name of routing table");
        hl.add(null, "5 .            <addr>               address of next hop");
        hl.add(null, "3 4        just-recursive           unicast routing table by target");
        hl.add(null, "4 5          <name:vrf>             name of routing table");
        hl.add(null, "5 .            <addr>               address of original next hop");
        hl.add(null, "3 4        just-protocol            unicast routing table by source");
        hl.add(null, "4 5          <name:vrf>             name of routing table");
        cfgRtr.getRouterList(hl, "5 .", " to filter");
        cfgRtr.getRouterList(hl, 3, " to filter");
        hl.add(null, "6 .            <num:rtr>            process id");
        hl.add(null, "3 4        ecmp                     unicast routing table ecmp");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        labels                   unicast routing table labels");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        prefix-lengths           prefix length distribution");
        hl.add(null, "4 .          <name:vrf>             vrf name");
        hl.add(null, "3 4        out-interfaces           egress interface distribution");
        hl.add(null, "4 .          <name:vrf>             vrf name");
        hl.add(null, "3 4        hostwatch                peer host table");
        hl.add(null, "4 .          <name:ifc>             name of interface");
        hl.add(null, "3 4        nexthops                 egress router distribution");
        hl.add(null, "4 .          <name:vrf>             vrf name");
        hl.add(null, "3 4        route                    unicast routing table entries");
        hl.add(null, "4 5,.        <name:vrf>             name of routing table");
        hl.add(null, "5 .            [addr]               prefix to view");
        hl.add(null, "3 4        rpf                      multicast routing table entries");
        hl.add(null, "4 5,.        <name:vrf>             name of routing table");
        hl.add(null, "5 .            [addr]               prefix to view");
        hl.add(null, "3 4        flwspc                   flowspec routing table entries");
        hl.add(null, "4 5,.        <name:vrf>             name of routing table");
        hl.add(null, "5 6,.          [addr]               prefix to view");
        hl.add(null, "6 .              [rd]               route distinguisher");
        hl.add(null, "3 4        protocol                 routing protocol summary");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        mroute                   multicast forwarding table entries");
        hl.add(null, "4 5,.        <name:vrf>             name of routing table");
        hl.add(null, "5 6            [addr]               source");
        hl.add(null, "6 .              <addr>             group");
        hl.add(null, "3 4        segrout                  segment routing forwarding table entries");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        srindex                  segment routing index table entries");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        bier                     bier forwarding table entries");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        inspect                  upper layer inspection");
        hl.add(null, "4 .          <name:ifc>             name of interface");
        hl.add(null, "3 4        toptalk                  top talker list");
        hl.add(null, "4 .          <name:ifc>             name of interface");
        hl.add(null, "3 4        flow                     netflow table entries");
        hl.add(null, "4 5          <name:vrf>             name of routing table");
        hl.add(null, "5 .            session              list of sessions");
        hl.add(null, "5 .            toptalk              top talker list");
        hl.add(null, "3 4        pbr                      pbr table entries");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        nat                      nat table entries");
        hl.add(null, "4 5          <name:vrf>             name of routing table");
        hl.add(null, "5 .            statistics           list of configuration entries");
        hl.add(null, "5 .            translations         list of translation entries");
        hl.add(null, "3 4        sockets                  socket table entries");
        hl.add(null, "4 .          <name:vrf>             name of routing table");
        hl.add(null, "3 4        bfd                      bidirectional forwarding detection protocol");
        hl.add(null, "4 5          <name:vrf>             specify routing table");
        hl.add(null, "5 .            neighbor             list of neighbors");
        hl.add(null, "3 4        dlep                     hot dynamic link exchange neighbors");
        hl.add(null, "4 5          <name:ifc>             specify routing table");
        hl.add(null, "5 .            neighbor             list of neighbors");
        hl.add(null, "5 .            clients              list of clients");
        hl.add(null, "3 4        hsrp                     hot standby router protocol");
        hl.add(null, "4 5          <name:vrf>             specify routing table");
        hl.add(null, "5 .            neighbor             list of neighbors");
        hl.add(null, "3 4        vrrp                     virtual router redundancy protocol");
        hl.add(null, "4 5          <name:vrf>             specify routing table");
        hl.add(null, "5 .            neighbor             list of neighbors");
        hl.add(null, "3 4        pim                      protocol independent multicast");
        hl.add(null, "4 5          <name:vrf>             specify routing table");
        hl.add(null, "5 .            neighbor             list of neighbors");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "3 4        msdp                     multicast source discovert protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 .            neighbor             list of neighbors");
        hl.add(null, "5 .            database             list of sources");
        hl.add(null, "3 4        rpki                     resource public key infra protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 .            status               status of the process");
        hl.add(null, "5 .            summary              list of neighbors");
        hl.add(null, "5 .            aspagraph            graph of neighbors");
        hl.add(null, "5 6            prefixes4            list ipv4 prefixes of asn");
        hl.add(null, "6 .              <num>              as number");
        hl.add(null, "5 6            prefixes6            list ipv6 prefixes of asn");
        hl.add(null, "6 .              <num>              as number");
        hl.add(null, "5 6            provider             list customers of asn");
        hl.add(null, "6 .              <num>              as number");
        hl.add(null, "5 6            pubkey               list public keys of asn");
        hl.add(null, "6 .              <num>              as number");
        hl.add(null, "5 6            learned4             list ipv4 feeds from neighbor");
        hl.add(null, "6 .              <addr>             peer address");
        hl.add(null, "5 6            learned6             list ipv4 feeds from neighbor");
        hl.add(null, "6 .              <addr>             peer address");
        hl.add(null, "5 6            learnedp             list aspa feeds from neighbor");
        hl.add(null, "6 .              <addr>             peer address");
        hl.add(null, "5 6            learnedk             list key feeds from neighbor");
        hl.add(null, "6 .              <addr>             peer address");
        hl.add(null, "5 6            compare4             compare ipv4 feeds from neighbors");
        hl.add(null, "6 7              <addr>             peer1 address");
        hl.add(null, "7 .                <addr>           peer2 address");
        hl.add(null, "5 6            compare6             compare ipv6 feeds from neighbors");
        hl.add(null, "6 7              <addr>             peer1 address");
        hl.add(null, "7 .                <addr>           peer2 address");
        hl.add(null, "5 6            comparep             compare aspa feeds from neighbors");
        hl.add(null, "6 7              <addr>             peer1 address");
        hl.add(null, "7 .                <addr>           peer2 address");
        hl.add(null, "5 6            comparek             compare key feeds from neighbors");
        hl.add(null, "6 7              <addr>             peer1 address");
        hl.add(null, "7 .                <addr>           peer2 address");
        hl.add(null, "5 .            connection           list of neighbor connections");
        hl.add(null, "5 .            database4            list of ipv4 roas");
        hl.add(null, "5 .            database6            list of ipv6 roas");
        hl.add(null, "5 .            databasep            list of aspas");
        hl.add(null, "5 .            databasek            list of keys");
        hl.add(null, "5 6            evaluate             evaluate prefix");
        hl.add(null, "6 7              <num>              local as");
        hl.add(null, "7 8                <addr>           prefix");
        hl.add(null, "8 8,.                <num>          as path");
        hl.add(null, "5 6            lookup4              lookup ipv4 roa for prefix");
        hl.add(null, "6 .              <addr>             prefix to lookup");
        hl.add(null, "5 6            lookup6              lookup ipv6 roa for prefix");
        hl.add(null, "6 .              <addr>             prefix to lookup");
        hl.add(null, "5 6            lookupp              lookup aspa for asn");
        hl.add(null, "6 .              <num>              as number");
        hl.add(null, "5 6            lookupk              lookup key for asn");
        hl.add(null, "6 7              <num>              as number");
        hl.add(null, "7 .                <str>            subject key identifier");
        hl.add(null, "3 4        rsvp                     resource reservation protocol");
        hl.add(null, "4 5          <name:vrf>             specify routing table");
        hl.add(null, "5 .            summary              list of tunnels in database");
        hl.add(null, "5 6            detail               display a specific tunnel");
        hl.add(null, "6 7              <addr>             source address");
        hl.add(null, "7 8                <num>            source id");
        hl.add(null, "8 9                  <addr>         subgroup address");
        hl.add(null, "9 .                    <num>        subgroup id");
        hl.add(null, "3 4        ldp                      label distribution protocol");
        hl.add(null, "4 5          <name:vrf>             specify routing table");
        hl.add(null, "5 6            neighbor             information about neighbor");
        hl.add(null, "6 7              <addr>             neighbor address");
        hl.add(null, "7 8,.              nulled           null labels learned from neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "7 8,.              learned          labels learned from neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "7 8,.              advertised       labels advertised to neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "7 .                l2learned        pseudowires learned from neighbor");
        hl.add(null, "7 .                l2advertised     pseudowires advertised to neighbor");
        hl.add(null, "7 .                l2needed         pseudowires needed to neighbor");
        hl.add(null, "7 .                mplearned        multipoints learned from neighbor");
        hl.add(null, "7 .                mpadvertised     multipoints advertised to neighbor");
        hl.add(null, "7 .                status           status of neighbor");
        hl.add(null, "5 .            summary              list of neighbors");
        hl.add(null, "5 .            nulled-summary       list of neighbors null label counts");
        hl.add(null, "5 6,.          database             list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 .            mpdatabase           list of multipoint sessions");
        hl.add(null, "3 4        babel                    babel routing protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6            neighbor             information about neighbor");
        hl.add(null, "6 7              <addr>             neighbor address");
        hl.add(null, "7 8,.              learned          routes learned from neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "5 .            summary              list of neighbors");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 6,.          database             list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        olsr                     optimized link state routing protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6            neighbor             information about neighbor");
        hl.add(null, "6 7              <addr>             neighbor address");
        hl.add(null, "7 8,.              learned          routes learned from neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "5 .            summary              list of neighbors");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 6,.          database             list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        rip                      routing information protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6            neighbor             information about neighbor");
        hl.add(null, "6 7              <addr>             neighbor address");
        hl.add(null, "7 8,.              learned          routes learned from neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "5 .            summary              list of neighbors");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 6,.          database             list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        rift                     routing in fat trees");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6,.          neighbor             list of neighbors");
        hl.add(null, "6 .              brief              only brief listing");
        hl.add(null, "5 6,.          database             list of database");
        hl.add(null, "6 7              [dir]              entry to view");
        hl.add(null, "7 8                [addr]           entry to view");
        hl.add(null, "8 .                  [num]          entry to view");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 .            metric               list of metrics");
        hl.add(null, "5 6            spf                  information about last spf");
        hl.add(null, "6 .              <dir>              north or south spf");
        hl.add(null, "5 6            hostnames            hostnames from database");
        hl.add(null, "6 .              <dir>              north or south spf");
        hl.add(null, "5 6            tree                 tree about last spf");
        hl.add(null, "6 .              <dir>              north or south spf");
        hl.add(null, "5 6            othertree            tree of other node");
        hl.add(null, "6 7              <dir>              north or south spf");
        hl.add(null, "7 .                <addr>           other node to view");
        hl.add(null, "5 6            othertopology        topology of other node");
        hl.add(null, "6 7              <dir>              north or south spf");
        hl.add(null, "7 8,.              <addr>           other node to view");
        hl.add(null, "8 .                  [addr]         node to view");
        hl.add(null, "5 6            graph                graph about last spf");
        hl.add(null, "6 7,.            <dir>              north or south spf");
        hl.add(null, "7 7,.              cli              cli commands");
        hl.add(null, "7 7,.              svg              svg commands");
        hl.add(null, "7 7,.              nets             connected networks");
        hl.add(null, "7 7,.              ints             connected interfaces");
        hl.add(null, "7 8                locs             resolve locations");
        hl.add(null, "8 7,.                <str>          domain part of txt");
        hl.add(null, "7 8                defl             default location");
        hl.add(null, "8 9                  <str>          default x coordinate");
        hl.add(null, "9 7,.                  <str>        default y coordinate");
        hl.add(null, "7 8                dns              resolve addresses");
        hl.add(null, "8 7,.                <str>          domain part of ptr");
        hl.add(null, "7 8                rect             limit locations");
        hl.add(null, "8 9                  <num>          begin x coordinate");
        hl.add(null, "9 10                   <num>        begin y coordinate");
        hl.add(null, "10 11                    <num>      end x coordinate");
        hl.add(null, "11 7,.                     <num>    end y coordinate");
        hl.add(null, "7 8                scal             scale locations");
        hl.add(null, "8 9                  <num>          multiply x coordinate");
        hl.add(null, "9 10                   <num>        multiply y coordinate");
        hl.add(null, "10 11                    <num>      substract x coordinate");
        hl.add(null, "11 7,.                     <num>    substract y coordinate");
        hl.add(null, "5 7            lnkinconsistent      inconsistent advertisements of metrics");
        hl.add(null, "6 7,.            <dir>              north or south spf");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "5 6            nhinconsistent       inconsistent advertisements of next hops");
        hl.add(null, "6 7,.            <dir>              north or south spf");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "5 6            topology             topology about last spf");
        hl.add(null, "6 7,.            <dir>              north or south spf");
        hl.add(null, "7 .                [addr]           node to view");
        hl.add(null, "5 6,.          route                list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        pvrp                     path vector routing protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6            neighbor             information about neighbor");
        hl.add(null, "6 7              <addr>             neighbor address");
        hl.add(null, "7 8,.              learned          routes learned from neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "7 8,.              adverted         routes advertised to neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "5 6,.          summary              list of neighbors");
        hl.add(null, "6 .              brief              only brief listing");
        hl.add(null, "5 .            metric               list of metrics");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 6,.          route                list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        lsrp                     link state routing protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6,.          neighbor             list of neighbors");
        hl.add(null, "6 .              brief              only brief listing");
        hl.add(null, "5 .            flexalgo             list of algorithms");
        hl.add(null, "5 .            metric               list of metrics");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 .            software             list of software");
        hl.add(null, "5 .            hardware             list of hardware");
        hl.add(null, "5 .            forwarder            list of forwarder");
        hl.add(null, "5 .            middleware           list of middleware");
        hl.add(null, "5 .            kernel               list of kernel");
        hl.add(null, "5 6            zone-rev             list of reverse zone");
        hl.add(null, "6 7              <str>              name of zone");
        hl.add(null, "7 8,.              <str>            separator");
        hl.add(null, "8 9                  <str>          replace from");
        hl.add(null, "9 8,.                  <str>        replace to");
        hl.add(null, "5 6            zone-fwd             list of forward zone");
        hl.add(null, "6 7              <str>              name of zone");
        hl.add(null, "7 8,.              <str>            separator");
        hl.add(null, "8 9                  <str>          replace from");
        hl.add(null, "9 8,.                  <str>        replace to");
        hl.add(null, "5 .            uptime               list of uptime");
        hl.add(null, "5 6,.          database             list of database");
        hl.add(null, "6 .              [addr]             entry to view");
        hl.add(null, "5 .            spf                  information about last spf");
        hl.add(null, "5 .            hostnames            hostnames from database");
        hl.add(null, "5 .            tree                 tree about last spf");
        hl.add(null, "5 6            othertree            tree of other node");
        hl.add(null, "6 .              <addr>             other node to view");
        hl.add(null, "5 6            othertopology        topology of other node");
        hl.add(null, "6 7,.            <addr>             other node to view");
        hl.add(null, "7 .                [addr]           node to view");
        hl.add(null, "5 6,.          graph                graph about last spf");
        hl.add(null, "6 6,.            cli                cli commands");
        hl.add(null, "6 6,.            svg                svg commands");
        hl.add(null, "6 6,.            nets               connected networks");
        hl.add(null, "6 6,.            ints               connected interfaces");
        hl.add(null, "6 7              locs               resolve locations");
        hl.add(null, "7 6,.              <str>            domain part of txt");
        hl.add(null, "6 7              defl               default location");
        hl.add(null, "7 8                <str>            default x coordinate");
        hl.add(null, "8 6,.                <str>          default y coordinate");
        hl.add(null, "6 7              dns                resolve addresses");
        hl.add(null, "7 6,.              <str>            domain part of ptr");
        hl.add(null, "6 7              rect               limit locations");
        hl.add(null, "7 8                <num>            begin x coordinate");
        hl.add(null, "8 9                  <num>          begin y coordinate");
        hl.add(null, "9 10                   <num>        end x coordinate");
        hl.add(null, "10 6,.                   <num>      end y coordinate");
        hl.add(null, "6 7              scal               scale locations");
        hl.add(null, "7 8                <num>            multiply x coordinate");
        hl.add(null, "8 9                  <num>          multiply y coordinate");
        hl.add(null, "9 10                   <num>        substract x coordinate");
        hl.add(null, "10 6,.                   <num>      substract y coordinate");
        hl.add(null, "5 6,.          lnkinconsistent      inconsistent advertisements of metrics");
        hl.add(null, "6 .              [str]              int matcher");
        hl.add(null, "5 6,.          nhinconsistent       inconsistent advertisements of next hops");
        hl.add(null, "6 .              [str]              int matcher");
        hl.add(null, "5 6,.          topology             topology about last spf");
        hl.add(null, "6 .              [addr]             node to view");
        hl.add(null, "5 6,.          route                list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        eigrp                    enhanced interior gateway routing protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6            neighbor             information about neighbor");
        hl.add(null, "6 7              <addr>             neighbor address");
        hl.add(null, "7 8,.              learned          routes learned from neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "7 8,.              adverted         routes advertised to neighbor");
        hl.add(null, "8 .                  [addr]         prefix to view");
        hl.add(null, "5 .            summary              list of neighbors");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 6,.          route                list of routes in database");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        ospf                     open shortest path first protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6,.          neighbor             list of neighbors");
        hl.add(null, "6 .              brief              only brief listing");
        hl.add(null, "5 .            metric               list of metrics");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 6            database             list of lsas in area");
        hl.add(null, "6 7,.            <num>              area number");
        hl.add(null, "7 8                [addr]           entry to view");
        hl.add(null, "8 .                  [addr]         entry to view");
        hl.add(null, "5 6            flexalgo             list of algorithms");
        hl.add(null, "6 .              <num>              area number");
        hl.add(null, "5 6            spf                  information about last spf");
        hl.add(null, "6 .              <num>              area number");
        hl.add(null, "5 6            hostnames            hostnames from database");
        hl.add(null, "6 .              <num>              area number");
        hl.add(null, "5 6            tree                 tree about last spf");
        hl.add(null, "6 .              <num>              area number");
        hl.add(null, "5 6            othertree            tree of other node");
        hl.add(null, "6 7              <num>              area number");
        hl.add(null, "7 .                <addr>           other node to view");
        hl.add(null, "5 6            othertopology        topology of other node");
        hl.add(null, "6 7              <num>              area number");
        hl.add(null, "7 8,.              <addr>           other node to view");
        hl.add(null, "8 .                  [addr]         node to view");
        hl.add(null, "5 6            graph                graph about last spf");
        hl.add(null, "6 7,.            <num>              area number");
        hl.add(null, "7 7,.              cli              cli commands");
        hl.add(null, "7 7,.              svg              svg commands");
        hl.add(null, "7 7,.              nets             connected networks");
        hl.add(null, "7 7,.              ints             connected interfaces");
        hl.add(null, "7 8                locs             resolve locations");
        hl.add(null, "8 7,.                <str>          domain part of txt");
        hl.add(null, "7 8                defl             default location");
        hl.add(null, "8 9                  <str>          default x coordinate");
        hl.add(null, "9 7,.                  <str>        default y coordinate");
        hl.add(null, "7 8                dns              resolve addresses");
        hl.add(null, "8 7,.                <str>          domain part of ptr");
        hl.add(null, "7 8                rect             limit locations");
        hl.add(null, "8 9                  <num>          begin x coordinate");
        hl.add(null, "9 10                   <num>        begin y coordinate");
        hl.add(null, "10 11                    <num>      end x coordinate");
        hl.add(null, "11 7,.                     <num>    end y coordinate");
        hl.add(null, "7 8                scal             scale locations");
        hl.add(null, "8 9                  <num>          multiply x coordinate");
        hl.add(null, "9 10                   <num>        multiply y coordinate");
        hl.add(null, "10 11                    <num>      substract x coordinate");
        hl.add(null, "11 7,.                     <num>    substract y coordinate");
        hl.add(null, "5 6            lnkinconsistent      inconsistent advertisements of metrics");
        hl.add(null, "6 7,.            <num>              area number");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "5 6            nhinconsistent       inconsistent advertisements of next hops");
        hl.add(null, "6 7,.            <num>              area number");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "5 6            topology             topology about last spf");
        hl.add(null, "6 7,.            <num>              area number");
        hl.add(null, "7 .                [addr]           node to view");
        hl.add(null, "5 6            route                list of routes in area");
        hl.add(null, "6 7,.            <num>              area number");
        hl.add(null, "7 .                [addr]           prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        isis                     intermediate system intermediate system protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6,.          neighbor             list of neighbors");
        hl.add(null, "6 .              brief              only brief listing");
        hl.add(null, "5 .            metric               list of metrics");
        hl.add(null, "5 .            interface            list of interfaces");
        hl.add(null, "5 6            database             list of lsas in area");
        hl.add(null, "6 7,.            <num>              level number");
        hl.add(null, "7 .                [addr]           entry to view");
        hl.add(null, "5 6            flexalgo             list of algorithms");
        hl.add(null, "6 .              <num>              level number");
        hl.add(null, "5 6            spf                  information about last spf");
        hl.add(null, "6 .              <num>              level number");
        hl.add(null, "5 6            hostnames            hostnames from database");
        hl.add(null, "6 .              <num>              level number");
        hl.add(null, "5 6            tree                 tree about last spf");
        hl.add(null, "6 .              <num>              level number");
        hl.add(null, "5 6            othertree            tree of other node");
        hl.add(null, "6 7              <num>              level number");
        hl.add(null, "7 .                <addr>           other node to view");
        hl.add(null, "5 6            othertopology        topology of other node");
        hl.add(null, "6 7              <num>              level number");
        hl.add(null, "7 8,.              <addr>           other node to view");
        hl.add(null, "8 .                  [addr]         node to view");
        hl.add(null, "5 6            graph                graph about last spf");
        hl.add(null, "6 7,.            <num>              level number");
        hl.add(null, "7 7,.              cli              cli commands");
        hl.add(null, "7 7,.              svg              svg commands");
        hl.add(null, "7 7,.              nets             connected networks");
        hl.add(null, "7 7,.              ints             connected interfaces");
        hl.add(null, "7 8                locs             resolve locations");
        hl.add(null, "8 7,.                <str>          domain part of txt");
        hl.add(null, "7 8                defl             default location");
        hl.add(null, "8 9                  <str>          default x coordinate");
        hl.add(null, "9 7,.                  <str>        default y coordinate");
        hl.add(null, "7 8                dns              resolve addresses");
        hl.add(null, "8 7,.                <str>          domain part of ptr");
        hl.add(null, "7 8                rect             limit locations");
        hl.add(null, "8 9                  <num>          begin x coordinate");
        hl.add(null, "9 10                   <num>        begin y coordinate");
        hl.add(null, "10 11                    <num>      end x coordinate");
        hl.add(null, "11 7,.                     <num>    end y coordinate");
        hl.add(null, "7 8                scal             scale locations");
        hl.add(null, "8 9                  <num>          multiply x coordinate");
        hl.add(null, "9 10                   <num>        multiply y coordinate");
        hl.add(null, "10 11                    <num>      substract x coordinate");
        hl.add(null, "11 7,.                     <num>    substract y coordinate");
        hl.add(null, "5 6            lnkinconsistent      inconsistent advertisements of metrics");
        hl.add(null, "6 7,.            <num>              level number");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "5 6            nhinconsistent       inconsistent advertisements of next hops");
        hl.add(null, "6 7,.            <num>              level number");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "5 6            topology             topology about last spf");
        hl.add(null, "6 7,.            <num>              level number");
        hl.add(null, "7 .                [addr]           node to view");
        hl.add(null, "5 6            route                list of routes in area");
        hl.add(null, "6 7,.            <num>              level number");
        hl.add(null, "7 .                [addr]           prefix to view");
        hl.add(null, "5 6,.          originate            list of routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "5 6            other-route          list of other routes in area");
        hl.add(null, "6 7,.            <num>              level number");
        hl.add(null, "7 .                [addr]           prefix to view");
        hl.add(null, "5 6,.          other-originate      list of other routes originated locally");
        hl.add(null, "6 .              [addr]             prefix to view");
        hl.add(null, "3 4        bgp                      border gateway protocol");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 6,.          group                list of groups");
        hl.add(null, "6 7              <num>              group number");
        hl.add(null, "7 .                config           peer configuration");
        hl.add(null, "7 .                status           peer status");
        rtrBgpParam.getAfiList(hl, "7 8,.", "show", false);
        hl.add(null, "8 9,.                [addr]         prefix to view");
        hl.add(null, "9 .                    [rd]         route distinguisher");
        hl.add(null, "5 .            nexthop              list of nexthops");
        hl.add(null, "5 .            bestpath             best path statistics");
        hl.add(null, "5 6            lspf                 linkstate spf statistics");
        hl.add(null, "6 .              spf                information about last spf");
        hl.add(null, "6 .              tree               tree about last spf");
        hl.add(null, "6 7              othertree          tree of other node");
        hl.add(null, "7 .                <addr>           other node to view");
        hl.add(null, "6 7              othertopology      topology of other node");
        hl.add(null, "7 8,.              <addr>           other node to view");
        hl.add(null, "8 .                  [addr]         node to view");
        hl.add(null, "6 7,.            graph              graph about last spf");
        hl.add(null, "7 7,.              cli              cli commands");
        hl.add(null, "7 7,.              svg              svg commands");
        hl.add(null, "7 7,.              nets             connected networks");
        hl.add(null, "7 7,.              ints             connected interfaces");
        hl.add(null, "7 8                locs             resolve locations");
        hl.add(null, "8 7,.                <str>          domain part of txt");
        hl.add(null, "7 8                defl             default location");
        hl.add(null, "8 9                  <str>          default x coordinate");
        hl.add(null, "9 7,.                  <str>        default y coordinate");
        hl.add(null, "7 8                dns              resolve addresses");
        hl.add(null, "8 7,.                <str>          domain part of ptr");
        hl.add(null, "7 8                rect             limit locations");
        hl.add(null, "8 9                  <num>          begin x coordinate");
        hl.add(null, "9 10                   <num>        begin y coordinate");
        hl.add(null, "10 11                    <num>      end x coordinate");
        hl.add(null, "11 7,.                     <num>    end y coordinate");
        hl.add(null, "7 8                scal             scale locations");
        hl.add(null, "8 9                  <num>          multiply x coordinate");
        hl.add(null, "9 10                   <num>        multiply y coordinate");
        hl.add(null, "10 11                    <num>      substract x coordinate");
        hl.add(null, "11 7,.                     <num>    substract y coordinate");
        hl.add(null, "6 7,.            lnkinconsistent    inconsistent advertisements of metrics");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "6 7,.            nhinconsistent     inconsistent advertisements of next hops");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "6 7,.            topology           topology about last spf");
        hl.add(null, "7 .                [addr]           node to view");
        hl.add(null, "6 7,.            route              list of routes in database");
        hl.add(null, "7 .                [addr]           prefix to view");
        hl.add(null, "6 7,.            originate          list of routes originated locally");
        hl.add(null, "7 .                [addr]           prefix to view");
        hl.add(null, "5 .            messages             message statistics");
        hl.add(null, "5 .            attributes           attribute statistics");
        hl.add(null, "5 .            graceful-restart     list of graceful restart");
        hl.add(null, "5 .            longlived-graceful   list of long lived graceful restart");
        hl.add(null, "5 .            multiple-labels      list of multiple labels");
        hl.add(null, "5 .            afi                  list of address families");
        hl.add(null, "5 .            resolve              list of domain names");
        hl.add(null, "5 .            additional-path      list of additional path");
        hl.add(null, "5 .            router-id            list of router id");
        hl.add(null, "5 .            buffer               list of buffer");
        hl.add(null, "5 .            description          list of description");
        hl.add(null, "5 .            hostname             list of hostname");
        hl.add(null, "5 .            software             list of software");
        hl.add(null, "5 .            unknowns             list of unknown attributes");
        hl.add(null, "5 .            compression          list of compression");
        hl.add(null, "5 .            connection           list of connection");
        hl.add(null, "5 .            summary              list of neighbors");
        hl.add(null, "5 .            desummary            list of neighbors with description");
        hl.add(null, "5 .            asummary             list of neighbors with as names");
        hl.add(null, "5 .            pfxsummary           list of prefix movements");
        hl.add(null, "5 6            template             information about template");
        hl.add(null, "6 7              <str>              template name");
        hl.add(null, "7 .                config           peer configuration");
        hl.add(null, "5 6            neighbor             information about neighbor");
        hl.add(null, "6 7              <addr>             neighbor address");
        hl.add(null, "7 8                dampening        route flap dampening");
        hl.add(null, "8 .                  <str>          int matcher");
        hl.add(null, "7 .                attributes       collected attributes statistics");
        hl.add(null, "7 .                messages         collected message statistics");
        hl.add(null, "7 .                config           peer configuration");
        hl.add(null, "7 .                status           peer status");
        rtrBgpParam.getAfiList(hl, "7 8", "show", false);
        hl.add(null, "8 9,.              learned          routes learned from neighbor");
        hl.add(null, "9 10,.               [addr]         prefix to view");
        hl.add(null, "10 .                   [rd]         route distinguisher");
        hl.add(null, "8 9,.              accepted         routes accepted from neighbor");
        hl.add(null, "9 10,.               [addr]         prefix to view");
        hl.add(null, "10 .                   [rd]         route distinguisher");
        hl.add(null, "8 9,.              willing          routes will advertised to neighbor");
        hl.add(null, "9 10,.               [addr]         prefix to view");
        hl.add(null, "10 .                   [rd]         route distinguisher");
        hl.add(null, "8 9,.              advertised       routes advertised to neighbor");
        hl.add(null, "9 10,.               [addr]         prefix to view");
        hl.add(null, "10 .                   [rd]         route distinguisher");
        rtrBgpParam.getAfiList(hl, "5 6", "show", false);
        hl.add(null, "6 .              summary            list of neighbors");
        hl.add(null, "6 7              route-map          list of routes in database");
        hl.add(null, "7 8,.              <name:rm>        name of list");
        hl.add(null, "8 9,.                [addr]         prefix to view");
        hl.add(null, "9 .                    [rd]         route distinguisher");
        hl.add(null, "6 7              route-policy       list of routes in database");
        hl.add(null, "7 8,.              <name:rpl>       name of list");
        hl.add(null, "8 9,.                [addr]         prefix to view");
        hl.add(null, "9 .                    [rd]         route distinguisher");
        hl.add(null, "6 7              prefix-list        list of routes in database");
        hl.add(null, "7 8,.              <name:pl>        name of list");
        hl.add(null, "8 9,.                [addr]         prefix to view");
        hl.add(null, "9 .                    [rd]         route distinguisher");
        hl.add(null, "6 7              measure-map        measure filtering by route map");
        hl.add(null, "7 .                <name:rm>        name of list");
        hl.add(null, "6 7              measure-policy     measure filtering by route policy");
        hl.add(null, "7 .                <name:rpl>       name of list");
        hl.add(null, "6 7              measure-list       measure filtering by prefix list");
        hl.add(null, "7 .                <name:pl>        name of list");
        hl.add(null, "6 7,.            database           list of routes in database");
        hl.add(null, "7 8,.              [addr]           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7,.            compress           list of compressed routes");
        hl.add(null, "7 8,.              [addr]           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7,.            deaggregated       list of deaggregated routes");
        hl.add(null, "7 8,.              [addr]           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 .              prefix-unused      list of unused routes");
        hl.add(null, "6 7,.            hacked             list of hacked routes");
        hl.add(null, "7 8,.              [addr]           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7,.            changes            list of changed routes");
        hl.add(null, "7 8,.              [addr]           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7              wireformat         encode a route reachable from database");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7              wireunformat       encode a route unreachable from database");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7,.            flapstat           prefix flap statistics");
        hl.add(null, "7 .                [num]            minimum flap count");
        hl.add(null, "6 7              flappath           flap statistics for prefix");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7              flaprevpath        reversed aspath flap statistics for prefix");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7              validof            perform rpki lookups for an asn");
        hl.add(null, "7 8                <num>            as number");
        cfgRtr.getRouterList(hl, 6, "");
        hl.add(null, "9 .                <num:rtr>        process number");
        hl.add(null, "6 7              validsum           count rpki lookups for a table");
        cfgRtr.getRouterList(hl, 5, "");
        hl.add(null, "8 .                <num:rtr>        process number");
        hl.add(null, "6 7              validtest          perform rpki lookups for a table");
        cfgRtr.getRouterList(hl, 5, "");
        hl.add(null, "8 .                <num:rtr>        process number");
        hl.add(null, "6 7              validmismark       verify rpki markings for a table");
        cfgRtr.getRouterList(hl, 5, "");
        hl.add(null, "8 .                <num:rtr>        process number");
        hl.add(null, "6 7,.            validated          validation status of the paths");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7,.            asnames            names of the asns in the paths");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7,.            asinfos            infos of the asns in the paths");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7,.            asmixed            numbers and names in the paths");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 .              asgraph            connectivity graph of ases");
        hl.add(null, "6 7,.            astree             connectivity tree of ases");
        hl.add(null, "7 .                <num>            asn to start with");
        hl.add(null, "6 .              asorigin           originating ases");
        hl.add(null, "6 .              asuplink           uplink ases");
        hl.add(null, "6 .              astransit          transiting ases");
        hl.add(null, "6 .              asconn             connectivity of ases");
        hl.add(null, "6 7              pathsof            paths contain asn");
        hl.add(null, "7 .                <num>            asn to look for");
        hl.add(null, "6 7              pathsat            paths around asn");
        hl.add(null, "7 .                <num>            asn to look for");
        hl.add(null, "6 .              pathstat           path length statistics");
        hl.add(null, "6 7,.            asinconsistent     inconsistent advertisements of ases");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "6 7,.            nhinconsistent     inconsistent advertisements of next hops");
        hl.add(null, "7 .                [str]            int matcher");
        hl.add(null, "6 .              nhprefixes         prefix count of next hops");
        hl.add(null, "6 .              nhtransit          transiting ases of next hops");
        hl.add(null, "6 .              nhorigin           originating ases of next hops");
        hl.add(null, "6 .              prefix-lengths     prefix length distribution");
        hl.add(null, "6 7              allroute           list of specific routes");
        hl.add(null, "7 8,.              <addr>           prefix to view");
        hl.add(null, "8 .                  [rd]           route distinguisher");
        hl.add(null, "6 7              differ             compare route from neighbors");
        hl.add(null, "7 8                <addr>           neighbor address");
        hl.add(null, "8 9                  <addr>         neighbor address");
        hl.add(null, "9 10,.                 <addr>       prefix to view");
        hl.add(null, "10 .                     [rd]       route distinguisher");
        hl.add(null, "6 7              compare            compare routes from neighbors");
        hl.add(null, "6 7              dcompare           double compare routes from neighbors");
        hl.add(null, "7 8                <addr>           neighbor address");
        hl.add(null, "8 9,.                <addr>         neighbor address");
        tabRouteAttr.ignoreHelp(hl, 9);
        hl.add(null, "9 10                   time         specify time");
        hl.add(null, "10 9,.                   <num>      milliseconds");
        hl.add(null, "9 10                   exclude      exclude prefixes from compare");
        hl.add(null, "10 9,.                   <name:rm>  name of route map");
        hl.add(null, "9 10                   update       update prefixes before compare");
        hl.add(null, "10 9,.                   <name:rm>  name of route map");
        hl.add(null, "6 7              unusual            prefixes with attributes");
        tabRouteAttr.ignoreHelp(hl, 7);
        hl.add(null, "6 .              labels             remote and local labels");
        hl.add(null, "6 .              segrout            segment routing information");
        hl.add(null, "6 .              bier               bier information");
        hl.add(null, "6 .              ecmp               ecmp info");
        getRouMapMtch(hl, 6);
        hl.add(null, "3 4        logger                   route logger");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 .            prefix-lengths       list of prefix lengths");
        hl.add(null, "5 .            interfaces           list of outgoing interface");
        hl.add(null, "5 6,.          database             list of database");
        hl.add(null, "6 .              [addr]             entry to view");
        hl.add(null, "5 6,.          flapstat             prefix flap statistics");
        hl.add(null, "6 .              [num]              minimum count");
        hl.add(null, "3 4        ghosthunt                ghost route hunter");
        hl.add(null, "4 5          <num:rtr>              process id");
        hl.add(null, "5 .            status               current status");
        hl.add(null, "5 .            found                last found entry");
        hl.add(null, "5 .            attrib               last attribute difference");
        hl.add(null, "5 .            ghost                last recorded ghost");
        hl.add(null, "5 6,.          differ               test difference");
        tabRouteAttr.ignoreHelp(hl, 6);
    }

    private static void getRouMapMtch(userHelping h, int b) {
        h.add(null, b + " .              privateas          list of routes with private asn");
        h.add(null, b + " .              entropy            list of routes with entropy label");
        h.add(null, b + " .              nostdcomm          list of routes without community");
        h.add(null, b + " " + (b + 1) + "              stdcomm            list of routes with community");
        h.add(null, (b + 1) + " " + (b + 2) + ",.              <str>            community");
        h.add(null, (b + 2) + " " + (b + 2) + ",.                [str]          community");
        h.add(null, b + " .              noextcomm          list of routes without community");
        h.add(null, b + " " + (b + 1) + "              extcomm            list of routes with community");
        h.add(null, (b + 1) + " " + (b + 2) + ",.              <str>            community");
        h.add(null, (b + 2) + " " + (b + 2) + ",.                [str]          community");
        h.add(null, b + " .              nolrgcomm          list of routes without community");
        h.add(null, b + " " + (b + 1) + "              lrgcomm            list of routes with community");
        h.add(null, (b + 1) + " " + (b + 2) + ",.              <str>            community");
        h.add(null, (b + 2) + " " + (b + 2) + ",.                [str]          community");
        h.add(null, b + " " + (b + 1) + "              aspath             list of routes with as path");
        h.add(null, (b + 1) + " " + (b + 2) + ",.              <str>            as path regular expression");
        h.add(null, (b + 2) + " " + (b + 2) + ",.                [str]          as path regular expression");
        h.add(null, b + " " + (b + 1) + "              distance           list of routes with distance");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              pathlen            list of routes with path length");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              pathprep           list of routes with path prepends");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              pathloop           list of routes with path loops");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              unknowns           list of routes with unknown attributes");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              asend              list of routes with path ending");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              asbeg              list of routes with path beginning");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              asmid              list of routes with path middle");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              locpref            list of routes with locpref");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              validroa           list of routes with validity");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              validaspa          list of routes with validity");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              aigp               list of routes with aigp");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              bandwidth          list of routes with bandwidth");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              origin             list of routes with origin");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              metric             list of routes with metric");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              tag                list of routes with tag");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              customer           list of routes with customer");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              aggregator         list of routes with aggregator");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              rd                 list of routes with rd");
        h.add(null, (b + 1) + " .                <str>            int matcher");
        h.add(null, b + " " + (b + 1) + "              nexthop            list of routes with nexthop");
        h.add(null, (b + 1) + " .                <str>            address matcher");
        h.add(null, b + " " + (b + 1) + "              network            list of routes from network");
        h.add(null, (b + 1) + " " + (b + 1) + ",.              <str>            prefix matcher");
    }

    /**
     * get pipes help
     *
     * @param hl help to append
     * @param beg beginning
     * @param privi allow privileges
     */
    public static void getHelpPipes(userHelping hl, int beg, boolean privi) {
        hl.possible(-1, beg);
        hl.add(null, beg + " " + (beg + 1) + "  |                       output modifier");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    include               only lines that match");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    hinclude              only lines that match with header");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    exclude               hide lines that match");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    begin                 only lines from match");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    hbegin                only lines from match with header");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    end                   only lines to match");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    sort                  sort lines by");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    revsort               reversed sort lines by");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    repsort               reverseed sort padded lines by");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    padsort               sort padded lines by");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    uniq                  unique lines by");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    hide                  hide columns after");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    section               only sections that match");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    reginc                only lines that match regular expression");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    hreginc               only lines that match regular expression with header");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    regexc                hide lines that match regular expression");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    regbeg                only lines from match regular expression");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    hregbeg               only lines from match regular expression with header");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    regend                only lines to match regular expression");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    regsec                only sections that match regular expression");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    first                 only first some lines");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <num>               number of lines");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    last                  only last some lines");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <num>               number of lines");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " " + (beg + 2) + "    hlast                 only last some lines with header");
        hl.add(null, (beg + 2) + " " + (beg + 3) + ",.    <num>               number of lines");
        hl.add(null, (beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add(null, (beg + 4) + " .            count           count entities");
        hl.add(null, (beg + 4) + " .            summary         summary entities");
        hl.add(null, (beg + 1) + " .      headers               only section headers");
        hl.add(null, (beg + 1) + " .      level                 raw level hierarchy");
        hl.add(null, (beg + 1) + " .      csv                   level hierarchy in csv");
        hl.add(null, (beg + 1) + " .      html                  level hierarchy in html");
        hl.add(null, (beg + 1) + " .      xml                   level hierarchy in xml");
        hl.add(null, (beg + 1) + " .      setdel                level hierarchy in set/delete");
        hl.add(null, (beg + 1) + " .      ccode                 level hierarchy in brackets");
        hl.add(null, (beg + 1) + " .      summary               just the summary of columns");
        hl.add(null, (beg + 1) + " .      linenumbers           prepend lines with numbers");
        hl.add(null, (beg + 1) + " .      hacked                hacker writing");
        hl.add(null, (beg + 1) + " .      raw                   unfiltered");
        hl.add(null, (beg + 1) + " .      count                 count entities");
        hl.add(null, (beg + 1) + " .      viewer                display in viewer");
        hl.add(null, (beg + 1) + " .      pastebin              redirect output to pastebin service");
        if (!privi) {
            return;
        }
        hl.add(null, (beg + 1) + " " + (beg + 9) + "    redirect              redirect output to file");
        hl.add(null, (beg + 9) + " .        <text>              name of file");
    }

    /**
     * get show help
     *
     * @param hl help to append
     * @param privi allow privileges
     */
    public static void getHelpShow(userHelping hl, boolean privi) {
        hl.add(null, "2 3      aaa                     aaa information");
        hl.add(null, "3 .        <name:aaa>            aaa list");
        hl.add(null, "2 3      macsec                  macsec information");
        hl.add(null, "3 .        <name:ifc>            name of interface");
        hl.add(null, "2 3      p2poe                   pppoe information");
        hl.add(null, "3 .        <name:ifc>            name of interface");
        hl.add(null, "2 .      parser                  parser information");
        hl.add(null, "2 .      history                 command history");
        hl.add(null, "2 .      scheduler               scheduler information");
        hl.add(null, "2 .      script                  script information");
        hl.add(null, "2 3      vdc                     virtual device context");
        hl.add(null, "3 .        interface             list of physical interfaces");
        hl.add(null, "3 .        device                list of running devices");
        hl.add(null, "2 .      reload                  scheduled reload");
        hl.add(null, "2 .      banner                  logo of device");
        hl.add(null, "2 .      privilege               list of your aaa details");
        hl.add(null, "2 .      users                   list of interactive users");
        hl.add(null, "2 3,.    logo                    logo of product");
        hl.add(null, "3 3,.      [text]                text to print");
        if (privi) {
            hl.add(null, "2 3,.    flash                   list of flash");
            hl.add(null, "3 3,.      [text]                directory to print");
            hl.add(null, "2 3,.    disk                    flash information");
            hl.add(null, "3 3,.      [text]                directory to print");
            hl.add(null, "2 3      file                    contents of file");
            hl.add(null, "3 3,.      <str>                 file to print");
            hl.add(null, "2 3      url                     download remote content");
            hl.add(null, "3 3,.      <str>                 remote url to download");
        }
        hl.add(null, "2 3      whois                   query whois server");
        hl.add(null, "3 3,.      <text>                directory to print");
        hl.add(null, "2 3      resolve                 query dns server");
        hl.add(null, "3 .        <addr>                name or address");
        hl.add(null, "2 3      transproxy              transparent proxy connections");
        hl.add(null, "3 .        <name:ifc>            name of interface");
        hl.add(null, "2 3,.    version                 version information");
        hl.add(null, "3 .        brief                 just headline");
        hl.add(null, "3 .        number                just numbers");
        hl.add(null, "3 .        date-email            just date");
        hl.add(null, "3 .        date-machine          just date");
        hl.add(null, "3 .        user-agent            just user agent");
        hl.add(null, "3 .        url                   just url");
        hl.add(null, "2 .      alias                   defined alieses");
        hl.add(null, "2 .      platform                platform information");
        hl.add(null, "2 3      me-the                  show something");
        hl.add(null, "3 .        time                  show some time");
        hl.add(null, "3 4        calendar              show some time");
        hl.add(null, "4 5          <num>               year to show");
        hl.add(null, "5 .            <num>             month to show");
        if (privi) {
            hl.add(null, "3 4        ascii                 show some art");
            hl.add(null, "4 .          <str>               filename");
        }
        hl.add(null, "3 4        meme                  show some meme");
        hl.add(null, "4 5          <str>               string to meme");
        hl.add(null, "5 6            <str>             meme domain.tld");
        hl.add(null, "6 7              <str>           target domain to meme");
        hl.add(null, "7 .                <str>         your redirector www");
        getSecretHelp(hl, 3);
        hl.add(null, "3 4        7bit                  show some 7bit");
        hl.add(null, "4 4,.        [str]               string to 7bit");
        hl.add(null, "3 4        rev7                  show some 7bit");
        hl.add(null, "4 4,.        [str]               string to reverse 7bit");
        hl.add(null, "3 4        hack                  show some hack");
        hl.add(null, "4 4,.        [str]               string to hack");
        hl.add(null, "2 3      process                 list processes");
        hl.add(null, "3 4,.      cpu                   internal router threads");
        hl.add(null, "4 .          <num>               thread id");
        hl.add(null, "3 .        external              external processes");
        hl.add(null, "2 3,.    redundancy              redundancy information");
        hl.add(null, "3 .        status                current status");
        hl.add(null, "3 .        description           interface description");
        hl.add(null, "3 .        core                  core hash");
        hl.add(null, "3 .        state                 state hash");
        hl.add(null, "3 .        config                config hash");
        hl.add(null, "3 4        remote                remote command");
        hl.add(null, "4 4,.        [str]               string to execute");
        hl.add(null, "2 .      name-cache              local dns cache");
        hl.add(null, "2 3      as-name                 name of an as");
        hl.add(null, "3 .        <num>                 as name to show");
        hl.add(null, "2 3      as-info                 information of an as");
        hl.add(null, "3 .        <num>                 as name to show");
        hl.add(null, "2 .      asn-cache               local asn cache");
        hl.add(null, "2 3      watchdog                watchdog information");
        hl.add(null, "3 .        gc                    garbage collector information");
        hl.add(null, "3 .        system                system information");
        hl.add(null, "3 .        hardware              hardware watchdog information");
        hl.add(null, "3 .        software              software watchdog information");
        hl.add(null, "3 .        timer                 timer history information");
        hl.add(null, "3 .        memory                memory history information");
        hl.add(null, "2 3,.    interfaces              interface status and configuration");
        hl.add(null, "3 .        full                  full listing");
        hl.add(null, "3 .        description           description listing");
        hl.add(null, "3 .        hwbwmon               hardware bandwidth monitor listing");
        hl.add(null, "3 .        swbwmon               software bandwidth monitor listing");
        hl.add(null, "3 .        summary               summary listing");
        hl.add(null, "3 .        status                status listing");
        hl.add(null, "3 .        lastio                last packet listing");
        hl.add(null, "3 .        hwswratio             hardware by software bytes");
        hl.add(null, "3 .        hwswpratio            hardware by software packets");
        hl.add(null, "3 .        bpratio               byte by packet ratio");
        hl.add(null, "3 .        hwsummary             hardware summary listing");
        hl.add(null, "3 .        swsummary             software summary listing");
        hl.add(null, "3 .        total                 total listing");
        hl.add(null, "3 .        hwtotal               hardware total listing");
        hl.add(null, "3 .        swtotal               software total listing");
        hl.add(null, "3 .        traffic               traffic listing");
        hl.add(null, "3 .        hwtraffic             hardware traffic listing");
        hl.add(null, "3 .        swtraffic             software traffic listing");
        hl.add(null, "3 .        psummary              packet summary listing");
        hl.add(null, "3 .        hwpsummary            hardware packet summary listing");
        hl.add(null, "3 .        swpsummary            software packet summary listing");
        hl.add(null, "3 .        ptotal                packet total listing");
        hl.add(null, "3 .        hwptotal              hardware packet total listing");
        hl.add(null, "3 .        swptotal              software packet total listing");
        hl.add(null, "3 .        ptraffic              packet traffic listing");
        hl.add(null, "3 .        hwptraffic            hardware packet traffic listing");
        hl.add(null, "3 .        swptraffic            software packet traffic listing");
        hl.add(null, "3 .        vrf                   vrf listing");
        hl.add(null, "3 4,.      [name:ifc]            name of (sub)interface");
        hl.add(null, "4 .          full                full listing");
        hl.add(null, "4 .          rates               traffic rates");
        hl.add(null, "4 .          realtime            realtime counters");
        hl.add(null, "4 .          history             historic byte counters");
        hl.add(null, "4 .          rxhistory           historic rx byte counters");
        hl.add(null, "4 .          txhistory           historic tx byte counters");
        hl.add(null, "4 .          drhistory           historic drop byte counters");
        hl.add(null, "4 .          phistory            historic packet counters");
        hl.add(null, "4 .          rxphistory          historic rx packet counters");
        hl.add(null, "4 .          txphistory          historic tx packet counters");
        hl.add(null, "4 .          drphistory          historic drop packet counters");
        hl.add(null, "4 .          numhist             numeric historic byte counters");
        hl.add(null, "4 .          numphist            numeric historic packet counters");
        hl.add(null, "4 .          hwrates             hardware traffic rates");
        hl.add(null, "4 .          hwrealtime          hardware realtime counters");
        hl.add(null, "4 .          hwhistory           hardware historic byte counters");
        hl.add(null, "4 .          hwrxhistory         hardware historic rx byte counters");
        hl.add(null, "4 .          hwtxhistory         hardware historic tx byte counters");
        hl.add(null, "4 .          hwdrhistory         hardware historic drop byte counters");
        hl.add(null, "4 .          hwphistory          hardware historic packet counters");
        hl.add(null, "4 .          hwrxphistory        hardware historic rx packet counters");
        hl.add(null, "4 .          hwtxphistory        hardware historic tx packet counters");
        hl.add(null, "4 .          hwdrphistory        hardware historic drop packet counters");
        hl.add(null, "4 .          hwnumhist           hardware numeric historic byte counters");
        hl.add(null, "4 .          hwnumphist          hardware numeric historic packet counters");
        hl.add(null, "4 .          counters            counters");
        hl.add(null, "4 .          hwcounters          hardware counters");
        hl.add(null, "4 .          ethertypes          ethernet types");
        hl.add(null, "4 .          lossdetect          loss detection");
        hl.add(null, "4 .          packetsizes         packet sizes");
        hl.add(null, "4 .          protocols           protocol numbers");
        hl.add(null, "4 .          timetolives         time to lives");
        hl.add(null, "4 .          trafficclasses      traffic classes");
        hl.add(null, "2 .      terminal                terminal parameters");
        hl.add(null, "2 3,.    tracker                 tracker status");
        hl.add(null, "3 .        [name:trk]            name of tracker");
        hl.add(null, "2 3,.    mtracker                mtracker status");
        hl.add(null, "3 .        [name:mtr]            name of mtracker");
        hl.add(null, "2 3      lldp                    link layer discovery protocol");
        hl.add(null, "3 .        interface             summary list of interfaces");
        hl.add(null, "3 .        neighbor              summary list of neighbors");
        hl.add(null, "3 4        detail                detailed list of neighbors");
        hl.add(null, "4 .          <name:ifc>          name of interface");
        hl.add(null, "2 3      cdp                     cisco discovery protocol");
        hl.add(null, "3 .        interface             summary list of interfaces");
        hl.add(null, "3 .        neighbor              summary list of neighbors");
        hl.add(null, "3 4        detail                detailed list of neighbors");
        hl.add(null, "4 .          <name:ifc>          name of interface");
        hl.add(null, "2 3      radiotap                radiotap neighbors");
        hl.add(null, "3 .        <name:ifc>            name of interface");
        hl.add(null, "2 3      udld                    unidirectional link detection");
        hl.add(null, "3 .        interface             summary list of interfaces");
        hl.add(null, "3 .        neighbor              summary list of neighbors");
        hl.add(null, "3 4        detail                detailed list of neighbors");
        hl.add(null, "4 .          <name:ifc>          name of interface");
        hl.add(null, "2 3      lacp                    link aggregation control protocol");
        hl.add(null, "3 .        interface             summary list of interfaces");
        hl.add(null, "3 .        neighbor              summary list of neighbors");
        hl.add(null, "3 4        detail                detailed list of neighbors");
        hl.add(null, "4 .          <name:ifc>          name of interface");
        hl.add(null, "2 3      policy-map              policy map statistics");
        hl.add(null, "3 4        interface             applied to interface");
        hl.add(null, "4 5          <name:ifc>          name of interface");
        hl.add(null, "5 .            in                ingress policy");
        hl.add(null, "5 .            out               egress policy");
        hl.add(null, "3 4        control-plane         applied to vrf");
        hl.add(null, "4 5          <name:vrf>          name of vrf");
        hl.add(null, "5 .            ipv4in            ipv4 ingress policy");
        hl.add(null, "5 .            ipv4out           ipv4 egress policy");
        hl.add(null, "5 .            ipv6in            ipv6 ingress policy");
        hl.add(null, "5 .            ipv6out           ipv6 egress policy");
        hl.add(null, "3 4        data-plane            applied to vrf");
        hl.add(null, "4 5          <name:vrf>          name of vrf");
        hl.add(null, "5 .            ipv4              ipv4 policy");
        hl.add(null, "5 .            ipv6              ipv6 policy");
        hl.add(null, "3 4        flowspec              applied to vrf");
        hl.add(null, "4 5          <name:vrf>          name of vrf");
        hl.add(null, "5 .            ipv4              ipv4 policy");
        hl.add(null, "5 .            ipv6              ipv6 policy");
        hl.add(null, "2 3      object-group            object group statistics");
        hl.add(null, "3 4        network               network list");
        hl.add(null, "4 .          <name:ogn>          name of list");
        hl.add(null, "3 4        port                  port list");
        hl.add(null, "4 .          <name:ogp>          name of list");
        hl.add(null, "2 3      access-list             access list statistics");
        hl.add(null, "3 .        <name:acl>            name of list");
        hl.add(null, "2 3      acl-merge               access list merge, unroll");
        hl.add(null, "3 4,.      <name:acl>            name of first acl");
        hl.add(null, "4 .          <name:acl>          name of second acl");
        hl.add(null, "2 3      acl-packet              access list action");
        hl.add(null, "3 4        <name:acl>            name of acl");
        hl.add(null, "4 5          <num>               protocol number");
        hl.add(null, "5 6            <addr>            source address");
        hl.add(null, "6 7              <num>           source port");
        hl.add(null, "7 8                <addr>        target address");
        hl.add(null, "8 .                  <num>       target port");
        hl.add(null, "2 3      rm2rpl                  route map converter");
        hl.add(null, "3 .        <name:rm>             name of list");
        hl.add(null, "2 3      rpl2rm                  route policy converter");
        hl.add(null, "3 .        <name:rpl>            name of list");
        hl.add(null, "2 3      route-map               route map statistics");
        hl.add(null, "3 .        <name:rm>             name of list");
        hl.add(null, "2 3      route-policy            route policy statistics");
        hl.add(null, "3 .        <name:rpl>            name of list");
        hl.add(null, "2 3      prefix-list             prefix list statistics");
        hl.add(null, "3 .        <name:pl>             name of list");
        hl.add(null, "2 3      time-map                time map statistics");
        hl.add(null, "3 4,.      <name:tm>             name of list");
        hl.add(null, "4 4,.        <str>               time to test");
        hl.add(null, "2 3      session                 stateful session information");
        hl.add(null, "3 4        <name:ses>            name of session");
        hl.add(null, "4 .          session             list of sessions");
        hl.add(null, "4 .          toptalk             top talker list");
        hl.add(null, "2 3      dial-peer               current dial peer status");
        hl.add(null, "3 .        description           descriptions");
        hl.add(null, "3 .        voice                 for calls");
        hl.add(null, "3 .        message               for messages");
        hl.add(null, "3 4        active                active calls");
        hl.add(null, "4 .          <num>               dial peer");
        hl.add(null, "3 4        history               list of calls");
        hl.add(null, "4 .          <num>               dial peer");
        hl.add(null, "2 3,.    clock                   current date and time");
        hl.add(null, "3 .        big                   a big clock");
        hl.add(null, "3 .        raw                   a raw clock");
        hl.add(null, "2 3,.    logging                 logged messages");
        hl.add(null, "3 4        script                script messages");
        hl.add(null, "4 .          <name:scr>          name of script");
        hl.add(null, "3 4        scheduler             scheduler messages");
        hl.add(null, "4 .          <name:sch>          name of scheduler");
        hl.add(null, "3 4        process               process messages");
        hl.add(null, "4 .          <name:prc>          name of process");
        hl.add(null, "3 4        vdc                   vdcmessages");
        hl.add(null, "4 .          <name:vdc>          name of vdc");
        hl.add(null, "3 .        file                  saved messages");
        hl.add(null, "3 .        bgp-dump              saw bgp dumps");
        hl.add(null, "3 .        old-file              old saved messages");
        hl.add(null, "3 4        last                  last messages");
        hl.add(null, "4 .          <num>               message count");
        hl.add(null, "2 3,.    config-differences      startup->running differences");
        hl.add(null, "3 3,.      [name]                name of section");
        hl.add(null, "3 4,.      this                  current subconfiguration");
        hl.add(null, "4 4,.        all                 do not hide defaults");
        hl.add(null, "4 4,.        hide                hide passwords");
        hl.add(null, "2 3,.    rollback-config         running->startup differences");
        hl.add(null, "3 3,.      [name]                name of section");
        hl.add(null, "3 4,.      this                  current subconfiguration");
        hl.add(null, "4 4,.        all                 do not hide defaults");
        hl.add(null, "4 4,.        hide                hide passwords");
        hl.add(null, "2 3,.    backup-config           backup configuration");
        hl.add(null, "3 3,.      [name]                name of section");
        hl.add(null, "3 4,.      this                  current subconfiguration");
        hl.add(null, "4 4,.        all                 do not hide defaults");
        hl.add(null, "4 4,.        hide                hide passwords");
        hl.add(null, "2 3,.    startup-config          startup configuration");
        hl.add(null, "3 3,.      [name]                name of section");
        hl.add(null, "3 4,.      this                  current subconfiguration");
        hl.add(null, "4 4,.        all                 do not hide defaults");
        hl.add(null, "4 4,.        hide                hide passwords");
        hl.add(null, "2 3,.    running-config          current operating configuration");
        hl.add(null, "3 3,.      [name]                name of section");
        hl.add(null, "3 4,.      this                  current subconfiguration");
        hl.add(null, "4 4,.        all                 do not hide defaults");
        hl.add(null, "4 4,.        hide                hide passwords");
        hl.add(null, "3 4,.      console0              current console configuration");
        hl.add(null, "4 4,.        all                 do not hide defaults");
        hl.add(null, "4 4,.        hide                hide passwords");
        hl.add(null, "3 4        line                  specified line");
        hl.add(null, "4 5,.        <name:lin>          name of line");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        interface             specified interface");
        hl.add(null, "4 5,.        <name:ifc>          name of interface");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        vrf                   specified vrf");
        hl.add(null, "4 5,.        <name:vrf>          name of vrf");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        server                specified server process");
        servGenList.srvHelp(hl, 4, " to show", null);
        hl.add(null, "3 4        router                specified router process");
        cfgRtr.getRouterList(hl, 2, " to show");
        hl.add(null, "5 6,.          <num:rtr>         process id");
        hl.add(null, "6 6,.            all             do not hide defaults");
        hl.add(null, "6 6,.            hide            hide passwords");
        hl.add(null, "3 4        route-map             specified route map");
        hl.add(null, "4 5,.        <name:rm>           name of route map");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        route-policy          specified route policy");
        hl.add(null, "4 5,.        <name:rpl>          name of route policy");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        prefix-list           specified prefix list");
        hl.add(null, "4 5,.        <name:pl>           name of prefix list");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        access-list           specified access list");
        hl.add(null, "4 5,.        <name:acl>          name of access list");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        tracker               specified tracker");
        hl.add(null, "4 5,.        <name:trk>          name of tracker");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        check                 specified check");
        hl.add(null, "4 5,.        <name:chk>          name of check");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        sensor                specified sensor");
        hl.add(null, "4 5,.        <name:sns>          name of sensor");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        vdc                   specified vdc");
        hl.add(null, "4 5,.        <name:vdc>          name of vdc");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        process               specified process");
        hl.add(null, "4 5,.        <name:prc>          name of process");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        scheduler             specified scheduler");
        hl.add(null, "4 5,.        <name:sch>          name of scheduler");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        script                specified script");
        hl.add(null, "4 5,.        <name:scr>          name of script");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4        vpdn                  specified vpdn");
        hl.add(null, "4 5,.        <name:vpd>          name of vpdn");
        hl.add(null, "5 5,.          all               do not hide defaults");
        hl.add(null, "5 5,.          hide              hide passwords");
        hl.add(null, "3 4,.      all                   do not hide defaults");
        hl.add(null, "4 .          hide                hide passwords");
        hl.add(null, "3 4,.      hide                  hide passwords");
        hl.add(null, "4 .          all                 do not hide defaults");
        hl.add(null, "2 3      vrf                     virtual routing/forwarding information");
        hl.add(null, "3 .        routing               routing information");
        hl.add(null, "3 .        icmp                  icmp information");
        hl.add(null, "3 4,.      traffic               traffic information");
        hl.add(null, "4 5          [name:vrf]          name of vrf");
        hl.add(null, "5 .            full              full listing");
        hl.add(null, "5 .            rates             traffic rates");
        hl.add(null, "5 .            realtime          realtime counters");
        hl.add(null, "5 .            history           historic byte counters");
        hl.add(null, "5 .            rxhistory         historic rx byte counters");
        hl.add(null, "5 .            txhistory         historic tx byte counters");
        hl.add(null, "5 .            drhistory         historic drop byte counters");
        hl.add(null, "5 .            phistory          historic packet counters");
        hl.add(null, "5 .            rxphistory        historic rx packet counters");
        hl.add(null, "5 .            txphistory        historic tx packet counters");
        hl.add(null, "5 .            drphistory        historic drop packet counters");
        hl.add(null, "5 .            numhist           numeric historic byte counters");
        hl.add(null, "5 .            numphist          numeric historic packet counters");
        hl.add(null, "2 3,.    dashboard               generate dashboard of this node");
        hl.add(null, "3 4        replace               specify replacement");
        hl.add(null, "4 5          <str>               source");
        hl.add(null, "5 3,.          <str>             target");
        hl.add(null, "3 4        text                  specify static text");
        hl.add(null, "4 3,.        <str>               url to use, %hostname% and %domain% will be replaced");
        hl.add(null, "3 4        iface                 specify intrefaces url");
        hl.add(null, "4 3,.        <str>               url to use, %name% and %desc% will be replaced");
        hl.add(null, "3 4        vrf                   specify vrf url");
        hl.add(null, "4 3,.        <str>               url to use, %name% and %desc% will be replaced");
        hl.add(null, "3 4        router                specify router url");
        hl.add(null, "4 3,.        <str>               url to use, %name% and %id% will be replaced");
        hl.add(null, "2 3      dns                     dns protocol information");
        hl.add(cfgAll.dmnDns.listServers(), "3 4        <name:loc>            server name");
        hl.add(null, "4 .          <str>               zone name");
        hl.add(null, "2 3      amt                     amt protocol information");
        hl.add(cfgAll.dmnAmt.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      etherip                 etherip protocol information");
        hl.add(cfgAll.dmnEtherIp.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      gre                     gre protocol information");
        hl.add(cfgAll.dmnGre.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      gtp                     gtp protocol information");
        hl.add(cfgAll.dmnGtp.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      l2f                     l2f protocol information");
        hl.add(cfgAll.dmnL2f.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      l2tp2                   l2tp2 protocol information");
        hl.add(cfgAll.dmnL2tp2.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      l2tp3                   l2tp3 protocol information");
        hl.add(cfgAll.dmnL2tp3.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      pckoudp                 pckoudp protocol information");
        hl.add(cfgAll.dmnPckOudp.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      vxlan                   vxlan protocol information");
        hl.add(cfgAll.dmnVxlan.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      sdwan                   sdwan protocol information");
        hl.add(cfgAll.dmnSdwan.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      http                    http protocol information");
        hl.add(cfgAll.dmnHttp.listServers(), "3 4,.      <name:loc>            server name");
        hl.add(null, "4 .          stat                statistics");
        hl.add(null, "4 5          zone                host to use");
        hl.add(null, "5 .          <str>               dns name");
        hl.add(null, "2 3      smtp                    smtp protocol information");
        hl.add(cfgAll.dmnSmtp.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      nrpe                    nrpe protocol information");
        hl.add(cfgAll.dmnNrpe.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      rpki                    rpki protocol information");
        hl.add(cfgAll.dmnRpki.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      dhcp4                   dhcp4 protocol information");
        hl.add(cfgAll.dmnDhcp4.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      dhcp6                   dhcp6 protocol information");
        hl.add(cfgAll.dmnDhcp6.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 .      clients                 client information");
        hl.add(null, "2 3,.    check                   check information");
        hl.add(null, "3 .        <name:chk>            check name");
        hl.add(null, "2 3,.    sensor                  sensor information");
        hl.add(null, "3 .        <name:sns>            sensor name");
        hl.add(null, "2 3      netflow                 netflow collector information");
        hl.add(cfgAll.dmnNetflow.listServers(), "3 4        <name:loc>            server name");
        hl.add(null, "4 .          session             list of sessions");
        hl.add(null, "4 .          toptalk             top talker list");
        hl.add(null, "2 3      streamingmdt            streaming telemetry collector information");
        hl.add(cfgAll.dmnStreamingMdt.listServers(), "3 4,.      <name:loc>            server name");
        hl.add(null, "4 5,.        <str>               peer");
        hl.add(null, "5 6            <str>             path");
        hl.add(null, "6 .              <str>           key");
        hl.add(null, "2 3      bmp                     bgp monitoring protocol information");
        hl.add(cfgAll.dmnBmp.listServers(), "3 4,.      <name:loc>            server name");
        hl.add(null, "4 5,.        <addr>              from");
        hl.add(null, "5 .            <addr>            peer");
        hl.add(null, "2 3,.    openflow                openflow protocol information");
        hl.add(cfgAll.dmnOpenflow.listServers(), "3 4,.    <name:loc>            server name");
        hl.add(null, "4 .          general             general status");
        hl.add(null, "2 3,.    p4lang                  p4lang protocol information");
        hl.add(cfgAll.dmnP4lang.listServers(), "3 4,.    <name:loc>            server name");
        hl.add(null, "4 .          general             general status");
        hl.add(null, "4 .          status              current status");
        hl.add(null, "4 .          api-tx              transmitted api message statistics");
        hl.add(null, "4 .          api-rx              received api message statistics");
        hl.add(null, "4 .          port-names          front panel information");
        hl.add(null, "4 .          port-magics         front port magic values");
        hl.add(null, "4 .          done-interface      exported interfaces");
        hl.add(null, "4 .          done-vrf            exported vrfs");
        hl.add(null, "4 .          done-neighbor       exported neighbors");
        hl.add(null, "4 .          done-mpls           exported labels");
        hl.add(null, "4 .          done-nsh            exported chains");
        hl.add(null, "4 5          done-bridge         exported bridge macs");
        hl.add(null, "5 .            <num>             bridge number");
        hl.add(null, "4 5          done-route4         exported ipv4 routes");
        hl.add(null, "5 .            <num>             vrf number");
        hl.add(null, "4 5          done-route6         exported ipv6 routes");
        hl.add(null, "5 .            <num>             vrf number");
        hl.add(null, "4 5          port-counters       dataplane counters");
        hl.add(null, "5 .            <name:ifc>        name of interface");
        hl.add(null, "2 3,.    stack                   stack protocol information");
        hl.add(cfgAll.dmnStack.listServers(), "3 4,.    <name:loc>            server name");
        hl.add(null, "4 .          dataplanes          dataplanes");
        hl.add(null, "4 5          ports               ports");
        hl.add(null, "5 .            <num>             forwarder number");
        hl.add(null, "4 5          spf                 spf");
        hl.add(null, "5 .            <num>             forwarder number");
        hl.add(null, "4 5          topology            topology");
        hl.add(null, "5 .            <num>             forwarder number");
        hl.add(null, "4 5          tree                tree");
        hl.add(null, "5 .            <num>             forwarder number");
        hl.add(null, "4 5          graph               graph");
        hl.add(null, "5 .            <num>             forwarder number");
        hl.add(null, "4 5          route               routes");
        hl.add(null, "5 6,.          <num>             forwarder number");
        hl.add(null, "6 .              [addr]          prefix to view");
        hl.add(null, "2 3      ppp                     ppp protocol information");
        hl.add(null, "3 .        <name:ifc>            name of interface");
        hl.add(null, "2 3      vpdn                    vpdn protocol information");
        hl.add(null, "3 .        <name:vpd>            name of vpdn");
        hl.add(null, "2 3      bridge                  bridging information");
        hl.add(null, "3 .        <num>                 bridge number");
        hl.add(null, "2 3      bundle                  bundling information");
        hl.add(null, "3 .        <num>                 bundle number");
        hl.add(null, "2 3      mpls                    multiprotocol label switching");
        hl.add(null, "3 4,.      forwarding            mpls forwarding table");
        hl.add(null, "4 .          [num]               label to view");
        hl.add(null, "3 .        interfaces            mpls interface table");
        hl.add(null, "3 4        inspect               mpls session table");
        hl.add(null, "4 5          <name:ifc>          interface to see");
        hl.add(null, "5 .            session           list of sessions");
        hl.add(null, "5 .            toptalk           top talker list");
        hl.add(null, "3 4        server-udp            udp protocol information");
        hl.add(cfgAll.dmnMplsUdp.listServers(), "4 .          <name:loc>            server name");
        hl.add(null, "3 4        server-ip             ip protocol information");
        hl.add(cfgAll.dmnMplsIp.listServers(), "4 .          <name:loc>            server name");
        hl.add(null, "2 .      sgt                     security group tag");
        hl.add(null, "2 3      nsh                     network service header");
        hl.add(null, "3 4,.      forwarding            nsh forwarding table");
        hl.add(null, "4 5          [num]               path to view");
        hl.add(null, "5 .            <num>             index to view");
        hl.add(null, "3 .        interfaces            nsh interface table");
        hl.add(null, "2 3      polka                   polynomial key architecture");
        hl.add(null, "3 4        routeid-unicast       polka routeid information");
        hl.add(null, "4 .          <name:ifc>          interface to see");
        hl.add(null, "3 4        routeid-multicast     mpolka routeid information");
        hl.add(null, "4 .          <name:ifc>          interface to see");
        hl.add(null, "3 4,.      interfaces            polka interface table");
        hl.add(null, "4 .          [name:ifc]          interface to see");
        hl.add(null, "2 3      ipx                     ipx information");
        hl.add(null, "3 4        route                 routing table entries");
        hl.add(null, "4 5,.        <name:vrf>          name of routing table");
        hl.add(null, "5 .            [addr]            prefix to view");
        hl.add(null, "2 3      router                  routing protocol information");
        cfgRtr.getRouterList(hl, 1, " to show");
        hl.add(null, "4 5          <num:rtr>           process id");
        hl.add(null, "5 6            redisted          advertised routes");
        hl.add(null, "5 6            computed          computed routes");
        hl.add(null, "6 7,.            unicast         unicast routes");
        hl.add(null, "6 7,.            multicast       multicast routes");
        hl.add(null, "6 7,.            flowspec        flowspec routes");
        hl.add(null, "7 8,.              [addr]        prefix to view");
        hl.add(null, "8 .                  [rd]        route distinguisher");
        hl.add(null, "2 3      ipv4                    ipv4 information");
        hl.add(null, "3 4        arp                   interface arp cache");
        hl.add(null, "4 .          <name:ifc>          name of (sub)interface");
        getHelpShowIpX(hl);
        hl.add(null, "2 3      ipv6                    ipv6 information");
        hl.add(null, "3 4        neighbors             interface neighbor cache");
        hl.add(null, "4 .          <name:ifc>          name of (sub)interface");
        getHelpShowIpX(hl);
        cfgAll.aliasHelps(cfgAlias.aliasType.show, 2, hl);
    }

    private void getHelpFlood(userHelping hl) {
        hl.add(null, ".5 6           tcp                          select tcp");
        hl.add(null, ".5 6           udp                          select udp");
        hl.add(null, ".5 6           ludp                         select ludp");
        hl.add(null, ".5 6           dccp                         select dccp");
        hl.add(null, ".5 6           sctp                         select sctp");
        hl.add(null, ".6 7             <addr>                     source address");
        hl.add(null, ".7 8               <num>                    source port");
        hl.add(null, ".8 9                 <addr>                 target address");
        hl.add(null, ".9 10                  <num>                target port");
        hl.add(null, ".10 11                   <num>              flags");
        hl.add(null, ".11 12                     <num>            size");
        hl.add(null, ".12 .                        <name:pm>      policy map");
    }

    private void getHelpTelnet(userHelping hl) {
        hl.add(null, "2 3,4,.  <host>                  name of host");
        hl.add(null, "3 4,.      [port]                port on host");
        hl.add(null, "4 4,.        tcp                 transmission control protocol");
        hl.add(null, "4 4,.        udp                 user datagram protocol");
        hl.add(null, "4 4,.        ludp                lightweight user datagram protocol");
        hl.add(null, "4 4,.        dccp                user datagram congestion control protocol");
        hl.add(null, "4 4,.        sctp                stream control transmission protocol");
        hl.add(null, "4 4,.        ssh                 specify secure shell");
        hl.add(null, "4 4,.        tls                 specify transport layer security");
        hl.add(null, "4 4,.        dtls                specify datagram transport layer security");
        hl.add(null, "4 4,.        telnet              specify telnet protocol");
        hl.add(null, "4 4,.        ipv4                specify ipv4 to use");
        hl.add(null, "4 4,.        ipv6                specify ipv6 to use");
        if (privileged) {
            hl.add(null, "4 5          record              specify record file");
            hl.add(null, "5 4,.          <str>             name of file");
        }
        hl.add(null, "4 5          vrf                 specify vrf to use");
        hl.add(null, "5 4,.          <name:vrf>        name of vrf");
        hl.add(null, "4 5          pubkey              specify public key to expect");
        hl.add(null, "5 4,.          <str>             public key");
        hl.add(null, "4 5          user                specify username to use");
        hl.add(null, "5 4,.          <str>             username");
        hl.add(null, "4 5          pass                specify password to use");
        hl.add(null, "5 4,.          <str>             password");
        hl.add(null, "4 5          proxy               specify proxy to use");
        hl.add(null, "5 4,.          <name:prx>        name of proxy profile");
        hl.add(null, "4 5          source              specify interface to use");
        hl.add(null, "5 4,.          <name:ifc>        name of interface");
        hl.add(null, "4 5          chat                specify chat script to use");
        hl.add(null, "5 4,.          <name:cht>        name of chat script");
    }

    /**
     * get help text for exec commands
     *
     * @return helping instance
     */
    public userHelping getHelping() {
        userHelping hl = new userHelping();
        hl.expand = needExpand;
        hl.add(null, "1 2    show                           running system information");
        getHelpShow(hl, privileged);
        getHelpPipes(hl, 110, privileged);
        hl.add(null, "1 2    watch                          running system periodic information");
        getHelpShow(hl, privileged);
        hl.add(null, "1 2    view                           running system information");
        getHelpShow(hl, privileged);
        hl.add(null, "1 2    display                        running system periodic information");
        getHelpShow(hl, privileged);
        hl.add(null, "1 2    differs                        running system difference information");
        getHelpShow(hl, privileged);
        hl.add(null, "1 .    logout                         close this exec session");
        hl.add(null, "1 .   " + cmds.finish + "                         close this exec session");
        hl.add(null, "1 2,.  bwmon                          start bandwidth monitor session");
        hl.add(null, "2 2,.    software                     use software counters");
        hl.add(null, "2 2,.    hardware                     use hardware counters");
        hl.add(null, "2 2,.    packets                      use packets counters");
        hl.add(null, "2 2,.    bytes                        use bytes counters");
        hl.add(null, "2 2,.    seconds                      use seconds counters");
        hl.add(null, "2 2,.    minutes                      use minutes counters");
        hl.add(null, "2 2,.    hours                        use hours counters");
        hl.add(null, "1 2,.  netconf                        start netconf session");
        hl.add(null, "2 2,.    format                       format response");
        hl.add(null, "2 2,.    echo                         echo user input");
        hl.add(null, "1 2,.  xml                            start xml session");
        hl.add(null, "2 2,.    format                       format response");
        hl.add(null, "2 2,.    echo                         echo user input");
        hl.add(null, "1 .    ppp                            start framed session");
        hl.add(null, "1 .    modememu                       start modem emulation session");
        hl.add(null, "1 .    gpsemu                         start gps emulation session");
        hl.add(null, "1 .    gpstime                        start gps session");
        hl.add(null, "1 2    menu                           start menu session");
        hl.add(null, "2 3      key                          letter based");
        hl.add(null, "3 .        <name:mnk>                 name of menu");
        hl.add(null, "2 3      tui                          tui based");
        hl.add(null, "3 .        <name:mnt>                 name of menu");
        hl.add(null, "1 2    terminal                       terminal specific parameters");
        hl.add(null, "2 2    " + cmds.negated + "                         negate a parameter");
        hl.add(null, "2 3      width                        set terminal width");
        hl.add(null, "3 .        <num>                      width in columns");
        hl.add(null, "2 3      length                       set terminal length");
        hl.add(null, "3 .        <num>                      height in lines");
        hl.add(null, "2 3      escape                       set escape character");
        hl.add(null, "3 .        <num>                      ascii code");
        hl.add(null, "2 3      deactivate                   set deactivate character");
        hl.add(null, "3 .        <num>                      ascii code");
        hl.add(null, "2 3      riblines                     set routing table size");
        hl.add(null, "3 .        <num>                      number of lines");
        hl.add(null, "2 .      monitor                      log to this terminal");
        hl.add(null, "2 .      detect                       detect size of terminal");
        hl.add(null, "2 .      timestamps                   put time before each executed command");
        hl.add(null, "2 3      background                   select background color");
        hl.add(null, "2 3      foreground                   select foreground color");
        hl.add(null, "2 3      prompt                       select prompt color");
        hl.add(null, "2 3      header                       select header color");
        hl.add(null, "3 .        black                      select color");
        hl.add(null, "3 .        red                        select color");
        hl.add(null, "3 .        green                      select color");
        hl.add(null, "3 .        yellow                     select color");
        hl.add(null, "3 .        blue                       select color");
        hl.add(null, "3 .        magenta                    select color");
        hl.add(null, "3 .        cyan                       select color");
        hl.add(null, "3 .        white                      select color");
        hl.add(null, "3 .        bright-black               select color");
        hl.add(null, "3 .        bright-red                 select color");
        hl.add(null, "3 .        bright-green               select color");
        hl.add(null, "3 .        bright-yellow              select color");
        hl.add(null, "3 .        bright-blue                select color");
        hl.add(null, "3 .        bright-magenta             select color");
        hl.add(null, "3 .        bright-cyan                select color");
        hl.add(null, "3 .        bright-white               select color");
        hl.add(null, "2 3,.    colorize                     sending to ansi terminal");
        hl.add(null, "3 .        normal                     select normal mode");
        hl.add(null, "3 .        header                     select header mode");
        hl.add(null, "3 .        rainbow                    select rainbow mode");
        hl.add(null, "3 .        prompt                     select prompt mode");
        hl.add(null, "2 .      spacetab                     treat space as tabulator");
        hl.add(null, "2 .      stars                        use stars in password prompt");
        hl.add(null, "2 .      title                        resend terminal title");
        hl.add(null, "2 3      clipboard                    send clipboard data");
        hl.add(null, "3 3,.      [str]                      data");
        hl.add(null, "2 .      capslock                     treat lowercase as uppercase");
        hl.add(null, "2 .      bells                        bells the terminal sometimes");
        hl.add(null, "2 3      play                         play ansi music");
        hl.add(null, "3 3,.      [str]                      music");
        hl.add(null, "2 .      beep                         bell ascii terminal");
        hl.add(null, "2 3,.    ansimode                     select ansi coloring mode");
        hl.add(null, "3 .        none                       select black and white mode");
        hl.add(null, "3 .        original                   select 8 colors mode");
        hl.add(null, "3 .        normal                     select 16 colors mode");
        hl.add(null, "3 .        indexed                    select 256 colors mode");
        hl.add(null, "3 .        palette                    select 16m colors mode");
        hl.add(null, "2 3,.    tablemode                    select table formatting mode");
        hl.add(null, "3 .        normal                     select normal mode");
        hl.add(null, "3 .        table                      select table mode");
        hl.add(null, "3 .        fancy                      select fancy mode");
        hl.add(null, "3 .        csv                        select csv mode");
        hl.add(null, "3 .        raw                        select raw mode");
        hl.add(null, "3 .        html                       select html mode");
        hl.add(null, "1 2    hostscan                       scan ports on remote");
        hl.add(null, "2 3      <host>                       starting host");
        hl.add(null, "3 4        <host>                     increment host");
        hl.add(null, "4 5,.        <host>                   number of hosts");
        hl.add(null, "5 6            vrf                    specify vrf to use");
        hl.add(null, "6 5,.            <name:vrf>           name of vrf");
        hl.add(null, "5 6            source                 specify interface to use");
        hl.add(null, "6 5,.            <name:ifc>           name of interface");
        hl.add(null, "5 6            timeout                specify timeout");
        hl.add(null, "6 5,.            <num>                timeout in milliseconds");
        hl.add(null, "5 6            data                   specify data to send");
        hl.add(null, "6 5,.            <num>                payload byte");
        hl.add(null, "5 6            alert                  specify alert to send");
        hl.add(null, "6 5,.            <num>                payload data");
        hl.add(null, "5 6            ttl                    specify ttl value");
        hl.add(null, "6 5,.            <num>                ttl");
        hl.add(null, "5 6            tos                    specify tos value");
        hl.add(null, "6 5,.            <num>                tos");
        hl.add(null, "5 6            sgt                    specify sgt value");
        hl.add(null, "6 5,.            <num>                sgt");
        hl.add(null, "5 6            flow                   specify flow value");
        hl.add(null, "6 5,.            <num>                flow");
        hl.add(null, "5 6            size                   specify payload size");
        hl.add(null, "6 5,.            <num>                byte count");
        hl.add(null, "5 6            port                   specify tcp port");
        hl.add(null, "6 5,.            <num>                port number");
        hl.add(null, "5 5,.          dontfrag               specify dont fragment");
        hl.add(null, "5 5,.          lookup                 perform reverse lookup to");
        hl.add(null, "1 2    portscan                       scan ports on remote");
        hl.add(null, "2 3,.    <host>                       name of host");
        hl.add(null, "3 3,.      ipv4                       specify ipv4 to use");
        hl.add(null, "3 3,.      ipv6                       specify ipv6 to use");
        hl.add(null, "3 4        vrf                        specify vrf to use");
        hl.add(null, "4 3,.        <name:vrf>               name of vrf");
        hl.add(null, "3 4        source                     specify interface to use");
        hl.add(null, "4 3,.        <name:ifc>               name of interface");
        hl.add(null, "3 4        timeout                    specify timeout");
        hl.add(null, "4 3,.        <num>                    timeout in milliseconds");
        hl.add(null, "3 4        min                        specify lower port number");
        hl.add(null, "4 3,.        <num>                    port number");
        hl.add(null, "3 4        max                        specify upper port number");
        hl.add(null, "4 3,.        <num>                    port number");
        hl.add(null, "3 4        ttl                        specify ttl number");
        hl.add(null, "4 3,.        <num>                    ttl number");
        hl.add(null, "3 4        tos                        specify tos number");
        hl.add(null, "4 3,.        <num>                    tos number");
        hl.add(null, "1 2    lookup                         domain name lookup");
        hl.add(null, "2 3      ipv4                         ipv4 address record");
        hl.add(null, "2 3      ipv6                         ipv6 address record");
        hl.add(null, "2 3      mail                         mail exchange record");
        hl.add(null, "2 3      dns                          name server record");
        hl.add(null, "2 3      soa                          authority record");
        hl.add(null, "2 3      srv                          service record");
        hl.add(null, "2 3      txt                          text record");
        hl.add(null, "2 3      reverse                      reverse of address record");
        hl.add(null, "2 3      recur-ipv4                   ipv4 address record");
        hl.add(null, "2 3      recur-ipv6                   ipv6 address record");
        hl.add(null, "2 3      recur-mail                   mail exchange record");
        hl.add(null, "2 3      recur-dns                    name server record");
        hl.add(null, "2 3      recur-soa                    authority record");
        hl.add(null, "2 3      recur-srv                    service record");
        hl.add(null, "2 3      recur-txt                    text record");
        hl.add(null, "2 3      zone                         download whole zone");
        hl.add(null, "3 4,.      <domain>                   domain to look up");
        hl.add(null, "4 .          [server]                 address of ns server");
        hl.add(null, "1 .    disable                        drop privileges");
        hl.add(null, "1 .    enable                         gain privileges");
        hl.add(null, "1 2,.  tclsh                          run tcl shell");
        hl.add(null, "2 .      [file]                       name of script");
        hl.add(null, "1 2    traceroute                     trace route to target");
        hl.add(null, "1 2    mtr                            trace route to target");
        hl.add(null, "2 3,.    <host>                       name of host");
        hl.add(null, "3 3,.      ipv4                       specify ipv4 to use");
        hl.add(null, "3 3,.      ipv6                       specify ipv6 to use");
        hl.add(null, "3 4        vrf                        specify vrf to use");
        hl.add(null, "4 3,.        <name:vrf>               name of vrf");
        hl.add(null, "3 4        source                     specify interface to use");
        hl.add(null, "4 3,.        <name:ifc>               name of interface");
        hl.add(null, "3 4        timeout                    specify timeout");
        hl.add(null, "4 3,.        <num>                    timeout in milliseconds");
        hl.add(null, "3 4        delay                      specify delay between packets");
        hl.add(null, "4 3,.        <num>                    timeout in milliseconds");
        hl.add(null, "3 4        tos                        specify tos value");
        hl.add(null, "4 3,.        <num>                    tos");
        hl.add(null, "3 4        flow                       specify flow value");
        hl.add(null, "4 3,.        <num>                    tos");
        hl.add(null, "3 4        port                       specify port value");
        hl.add(null, "4 3,.        <num>                    port");
        hl.add(null, "3 4        protocol                   specify protocol value");
        hl.add(null, "4 3,.        <num>                    port");
        hl.add(null, "3 4        size                       specify payload size");
        hl.add(null, "4 3,.        <num>                    byte count");
        hl.add(null, "3 4        router                     lookup intermediate hops");
        cfgRtr.getRouterList(hl, 2, " to use");
        hl.add(null, "5 3,.          <num:rtr>              process id");
        hl.add(null, "3 3,.      lookup                     lookup intermediate hops");
        hl.add(null, "1 2,.  send                           send a message to logged in users");
        hl.add(null, "2 2,.    [str]                        string to send");
        hl.add(null, "1 2    ping                           send echo request");
        hl.add(null, "2 3,.    <host>                       name of host");
        hl.add(null, "3 3,.      dontfrag                   specify dont fragment");
        hl.add(null, "3 3,.      multi                      wait for multiple responses");
        hl.add(null, "3 3,.      error                      consider errors in result");
        hl.add(null, "3 3,.      detail                     specify detail mode");
        hl.add(null, "3 4        data                       specify data to send");
        hl.add(null, "4 3,.        <num>                    payload byte");
        hl.add(null, "3 4        alert                      specify alert to send");
        hl.add(null, "4 3,.        <num>                    payload data");
        hl.add(null, "3 3,.      ipv4                       specify ipv4 to use");
        hl.add(null, "3 3,.      ipv6                       specify ipv6 to use");
        hl.add(null, "3 4        vrf                        specify vrf to use");
        hl.add(null, "4 3,.        <name:vrf>               name of vrf");
        hl.add(null, "3 4        source                     specify interface to use");
        hl.add(null, "4 3,.        <name:ifc>               name of interface");
        hl.add(null, "3 4        viahop                     specify nexthop to send");
        hl.add(null, "4 3,.        <addr>                   address to send to");
        hl.add(null, "3 4        timeout                    specify timeout");
        hl.add(null, "4 3,.        <num>                    timeout in milliseconds");
        hl.add(null, "3 4        delay                      specify delay between packets");
        hl.add(null, "4 3,.        <num>                    timeout in milliseconds");
        hl.add(null, "3 3,.      sweep                      specify increasing payload size");
        hl.add(null, "3 4        size                       specify payload size");
        hl.add(null, "4 3,.        <num>                    byte count");
        hl.add(null, "3 4        repeat                     specify repeat count");
        hl.add(null, "4 3,.        <num>                    repeat count");
        hl.add(null, "3 4        ttl                        specify ttl value");
        hl.add(null, "4 3,.        <num>                    ttl");
        hl.add(null, "3 4        tos                        specify tos value");
        hl.add(null, "4 3,.        <num>                    tos");
        hl.add(null, "3 4        sgt                        specify sgt value");
        hl.add(null, "4 3,.        <num>                    sgt");
        hl.add(null, "3 4        flow                       specify flow value");
        hl.add(null, "4 3,.        <num>                    flow");
        hl.add(null, "1 2    sleep                          do nothing for a while");
        hl.add(null, "2 .      <num>                        milliseconds for sleep");
        hl.add(null, "1 2    echo                           print out a line");
        hl.add(null, "2 2,.    [str]                        string to write");
        hl.add(null, "1 2    curl                           download content");
        hl.add(null, "2 3,.    <str>                        url to download");
        hl.add(null, "1 2    whois                          perform whois query");
        hl.add(null, "2 3      <host>                       name of host to query");
        hl.add(null, "3 3,.      <text>                     query string");
        hl.add(null, "1 2    tmux                           multiplex the terminal");
        hl.add(null, "2 .      vertical                     divide vertically");
        hl.add(null, "2 .      horizontal                   divide horizontally");
        hl.add(null, "2 .      both                         divide to four");
        hl.add(null, "2 .      none                         do not divide");
        hl.add(null, "1 2    game                           play games or watch screen savers");
        getSecretHelp(hl, 2);
        if (privileged) {
            hl.add(null, "2 3      ansi                         show some art");
            hl.add(null, "3 .        <str>                      filename");
        }
        hl.add(null, "2 .      chat                         chat with others");
        hl.add(null, "2 3,.    send                         chat with others");
        hl.add(null, "3 3,.      [str]                      string to send");
        hl.add(null, "2 .      color                        take test");
        hl.add(null, "2 .      ascii                        take test");
        hl.add(null, "2 .      vmkeys                       take test");
        hl.add(null, "2 .      clear                        clear screen");
        hl.add(null, "2 .      gomoku                       play game");
        hl.add(null, "2 .      tetris                       play game");
        hl.add(null, "2 .      hanoi                        play game");
        hl.add(null, "2 .      minesweep                    play game");
        hl.add(null, "2 .      clock                        view demo");
        hl.add(null, "2 .      snake                        view demo");
        hl.add(null, "2 .      matrix                       view demo");
        hl.add(null, "2 .      fire                         view demo");
        hl.add(null, "2 .      plasma                       view demo");
        hl.add(null, "2 .      donut                        view demo");
        hl.add(null, "2 .      fractal                      view demo");
        hl.add(null, "2 .      life                         view demo");
        hl.add(null, "2 .      antball                      view demo");
        hl.add(null, "2 3      zenmaster                    play demo");
        hl.add(cfgAll.dmnQuote.listServers(), "3  .      <name:loc>                 name of server");
        hl.add(null, "2 3,.    title                        view demo");
        hl.add(null, "3 3,.      [str]                      text");
        hl.add(null, "2 3,.    text                         view demo");
        hl.add(null, "3 3,.      [str]                      text");
        hl.add(null, "2 3,.    logo                         view demo");
        hl.add(null, "3 3,.      [str]                      text");
        hl.add(null, "1 2    listen                         start listen session");
        hl.add(null, "2 3,.    <port>                       port number");
        hl.add(null, "3 3,.      tcp                        transmission control protocol");
        hl.add(null, "3 3,.      udp                        user datagram protocol");
        hl.add(null, "3 3,.      ludp                       lightweight user datagram protocol");
        hl.add(null, "3 3,.      dccp                       user datagram congestion control protocol");
        hl.add(null, "3 3,.      sctp                       stream control transmission protocol");
        hl.add(null, "3 3,.      ipv4                       specify ipv4 to use");
        hl.add(null, "3 3,.      ipv6                       specify ipv6 to use");
        hl.add(null, "3 4        vrf                        specify vrf to use");
        hl.add(null, "4 3,.        <name:vrf>               name of vrf");
        hl.add(null, "3 4        source                     specify interface to use");
        hl.add(null, "4 3,.        <name:ifc>               name of interface");
        hl.add(null, "1 2    telnet                         start telnet session");
        getHelpTelnet(hl);
        hl.add(null, "1 2    tls                            start tls session");
        getHelpTelnet(hl);
        hl.add(null, "1 2    dtls                           start dtls session");
        getHelpTelnet(hl);
        hl.add(null, "1 2    ssl                            start ssl session");
        getHelpTelnet(hl);
        hl.add(null, "1 2    ssh                            start ssh session");
        getHelpTelnet(hl);
        cfgAll.aliasHelps(cfgAlias.aliasType.exec, 1, hl);
        if (!privileged) {
            return hl;
        }
        hl.add(null, "1 2    clear                          clear running conditions");
        hl.add(null, "2 3,.    hwcounters                   hardware counters on one or more interfaces");
        hl.add(null, "3 .        <name:ifc>                 name of interface");
        hl.add(null, "2 3,.    swcounters                   software counters on one or more interfaces");
        hl.add(null, "3 .        <name:ifc>                 name of interface");
        hl.add(null, "2 3,.    counters                     counters on one or more interfaces");
        hl.add(null, "3 .        <name:ifc>                 name of interface");
        hl.add(null, "2 3      object-group                 object group statistics");
        hl.add(null, "3 4        network                    network list");
        hl.add(null, "4 .          <name:ogn>               name of list");
        hl.add(null, "3 4        port                       port list");
        hl.add(null, "4 .          <name:ogp>               name of list");
        hl.add(null, "2 3      reflected-acl                clear access list entries");
        hl.add(null, "3 .        <name:acl>                 name of list");
        hl.add(null, "2 3      access-list                  access list statistics");
        hl.add(null, "3 .        <name:acl>                 name of list");
        hl.add(null, "2 3      route-map                    route map statistics");
        hl.add(null, "3 .        <name:rm>                  name of list");
        hl.add(null, "2 3      route-policy                 route policy statistics");
        hl.add(null, "3 .        <name:rpl>                 name of list");
        hl.add(null, "2 3      prefix-list                  prefix list statistics");
        hl.add(null, "3 .        <name:pl>                  name of list");
        hl.add(null, "2 3      errors                       error reporter");
        hl.add(null, "3 3,.      <str>                      email address");
        hl.add(null, "2 3      redundancy                   redundancy peer");
        hl.add(null, "3 4        peer                       peer priority");
        hl.add(prtRedun.getIfacesLst(), "4 5          <str:loc>                    interface to use");
        hl.add(null, "5 .            <num>                  priority to use");
        hl.add(null, "3 4        local                      local priority");
        hl.add(null, "4 .          <num>                    priority to use");
        hl.add(null, "3 4        core                       receive routing software");
        hl.add(prtRedun.getIfacesLst(), "4 .          <str:loc>                    interface to use");
        hl.add(null, "3 4        config                     receive startup config");
        hl.add(prtRedun.getIfacesLst(), "4 .          <str:loc>                    interface to use");
        hl.add(null, "3 .        state                      save state");
        hl.add(null, "2 3      bridge                       learnt mac address on a bridge");
        hl.add(null, "3 4,.      <num>                      bridge number");
        hl.add(null, "4 .          <name:ifc>               name of interface");
        hl.add(null, "2 3      sensor                       trigger telemetry export");
        hl.add(null, "3 4        <name:sns>                 name of sensor");
        hl.add(null, "4 5          csv                      save in csv format");
        hl.add(null, "4 5          append-csv               append in csv format");
        hl.add(null, "4 5          xml                      save in xml format");
        hl.add(null, "4 5          prometheus               save in prometheus format");
        hl.add(null, "5 5,.          <str>                  filename");
        hl.add(null, "2 3      telemetry                    trigger telemetry report");
        hl.add(null, "3 4        <name:sns>                 name of sensor");
        hl.add(null, "4 4,.        <name:tlm>               name of telemetry");
        hl.add(null, "2 3      dial-peer                    clear voip call");
        hl.add(null, "3 4        <num>                      number of dial peer");
        hl.add(null, "4 .          <str>                    call id");
        hl.add(null, "2 3      scheduler                    run one scheduler round");
        hl.add(null, "3 4,.      <name:sch>                 name of scheduler");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      script                       run one script round");
        hl.add(null, "3 4,.      <name:scr>                 name of script");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      vpdn                         reconnect vpdn peer");
        hl.add(null, "3 4,.      <name:vpd>                 name of vpdn");
        hl.add(null, "4 .          [num]                    downtime in milliseconds");
        hl.add(null, "2 3      p2poe                        pppoe client");
        hl.add(null, "3 4        <name:ifc>                 name of interface");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      amt                          amt protocol client");
        hl.add(cfgAll.dmnAmt.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      etherip                      etherip protocol client");
        hl.add(cfgAll.dmnEtherIp.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      gre                          gre protocol client");
        hl.add(cfgAll.dmnGre.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      gtp                          gtp protocol client");
        hl.add(cfgAll.dmnGtp.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      l2f                          l2f protocol client");
        hl.add(cfgAll.dmnL2f.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      l2tp2                        l2tp2 protocol client");
        hl.add(cfgAll.dmnL2tp2.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      l2tp3                        l2tp3 protocol client");
        hl.add(cfgAll.dmnL2tp3.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      pckoudp                      pckoudp protocol client");
        hl.add(cfgAll.dmnPckOudp.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      vxlan                        vxlan protocol client");
        hl.add(cfgAll.dmnVxlan.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      sdwan                        sdwan protocol client");
        hl.add(cfgAll.dmnSdwan.listServers(), "3 4        <name:loc>                 server name");
        hl.add(null, "4 .          <addr>                   peer address");
        hl.add(null, "2 3      vdc                          restart vdc process");
        hl.add(null, "3 4,.      <name:vdc>                 name of vdc");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      process                      restart external process");
        hl.add(null, "3 4,.      <name:prc>                 name of process");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      tracker                      run one tracker round");
        hl.add(null, "3 4,.      <name:trk>                 name of tracker");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 .      logging                      logged messages");
        hl.add(null, "2 .      reload                       clear last reload reason");
        hl.add(null, "2 3,.    flash                        clear routing software backup");
        hl.add(null, "3 .        [str]                      path to clean");
        hl.add(null, "2 .      upgrade                      cancel upgrade auto-revert");
        hl.add(null, "2 .      auto-bandwidth               set auto bandwidth values");
        hl.add(null, "2 .      follow-tracker               set interfaces based on trackers");
        hl.add(null, "2 .      tunnel-domain                resolve destination domain names");
        hl.add(null, "2 .      name-cache                   dns local cache");
        hl.add(null, "2 .      asn-cache                    asn local cache");
        hl.add(null, "2 3,.    watchdog                     watchdog");
        hl.add(null, "3 3,.      [name]                     parameter");
        hl.add(null, "2 3      line                         disconnect line");
        hl.add(null, "3 .        <name:lin>                 name of line");
        hl.add(null, "2 3      interface                    disconnect interface");
        hl.add(null, "3 4,.      <name:ifc>                 name of interface");
        hl.add(null, "4 .          [num]                    downtime in milliseconds");
        hl.add(null, "2 3      savemrt                      dump routing table");
        hl.add(null, "3 4        <name:vrf>                 vrf name");
        hl.add(null, "4 .          <name>                   name of file");
        hl.add(null, "2 3      ipv4                         ipv4 parameters");
        hl.add(null, "3 4        arp                        arp table");
        hl.add(null, "4 5          <name:ifc>               interface name");
        hl.add(null, "5 .            <addr>                 peer address");
        getHelpClearIpX(hl);
        hl.add(null, "2 3      ipv6                         ipv6 parameters");
        hl.add(null, "3 4        neighbor                   neighbor table");
        hl.add(null, "4 5          <name:ifc>               interface name");
        hl.add(null, "5 .            <addr>                 peer address");
        getHelpClearIpX(hl);
        hl.add(null, "2 3      bmp                          clear one bmp server");
        hl.add(cfgAll.dmnBmp.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      openflow                     clear one openflow server");
        hl.add(cfgAll.dmnOpenflow.listServers(), "3 .        <name:loc>            server name");
        hl.add(null, "2 3      p4lang                       clear one p4lang server");
        hl.add(cfgAll.dmnP4lang.listServers(), "3 .      <name:loc>            server name");
        hl.add(null, "2 3      socket                       clear one socket");
        hl.add(null, "3 4        <name:vrf>                 vrf name");
        hl.add(null, "4 5          tcp                      select tcp");
        hl.add(null, "4 5          udp                      select udp");
        hl.add(null, "4 5          ludp                     select ludp");
        hl.add(null, "4 5          dccp                     select dccp");
        hl.add(null, "4 5          sctp                     select sctp");
        hl.add(null, "5 6            <name:ifc>             interface name");
        hl.add(null, "6 7              <num>                local port");
        hl.add(null, "7 8                <num>              remote port");
        hl.add(null, "8 .                  <addr>           remote address");
        cfgAll.aliasHelps(cfgAlias.aliasType.clear, 2, hl);
        hl.add(null, "1 2    debug                          start debugging one protocol");
        debugger.getHelping(hl, 2);
        hl.add(null, "1 2,.  undebug                        stop debugging one protocol");
        hl.add(null, "2 .      all                          disable all debugging");
        debugger.getHelping(hl, 2);
        hl.add(null, "1 2    set                            insert configuration command");
        hl.add(null, "2 2,.    [str]                        config line to set");
        hl.add(null, "1 2    delete                         remove configuration command");
        hl.add(null, "2 2,.    [str]                        config line to unset");
        hl.add(null, "1 2,.  configure                      enter configuration mode");
        hl.add(null, "2 3      file                         append to running configuration");
        hl.add(null, "3 .        <file>                     source file");
        hl.add(null, "2 3      replace                      overwrite the running configuration");
        hl.add(null, "3 .        <file>                     source file");
        hl.add(null, "2 .      banner                       edit the banner");
        hl.add(null, "2 .      startup                      edit the startup configuration");
        hl.add(null, "2 3,.    editor                       configure from editor");
        hl.add(null, "3 3,.      [name]                     section name");
        hl.add(null, "2 3,.    viewer                       view current configuration");
        hl.add(null, "3 3,.      [name]                     section name");
        hl.add(null, "2 .      terminal                     configure from this terminal");
        hl.add(null, "2 3      reload                       overwrite the startup configuration");
        hl.add(null, "3 3,.      <url>                      source url");
        hl.add(null, "2 3      network                      append to running configuration");
        hl.add(null, "3 3,.      <url>                      source url");
        hl.add(null, "2 3      overwrite-network            overwrite the running configuration");
        hl.add(null, "3 3,.      <url>                      source url");
        hl.add(null, "2 .      rollback                     configure within auto-revert session");
        hl.add(null, "2 .      revert                       revert to startup configuration");
        hl.add(null, ".2 .     reapply                      try to reapply current configuration");
        userHelping hlp = new userHelping();
        hlp.add(null, "1 2    flash                          file system utility");
        hlp.add(null, "2 3      count                        count directory usage");
        hlp.add(null, "3 .        <file>                     pathname");
        hlp.add(null, "2 3      list                         list directory");
        hlp.add(null, "3 .        <file>                     pathname");
        hlp.add(null, "2 3      type                         type one ascii file");
        hlp.add(null, "3 .        <file>                     filename");
        hlp.add(null, "2 3      info                         information about file");
        hlp.add(null, "3 .        <file>                     filename");
        hlp.add(null, "2 3      hash                         hash of file");
        hlp.add(null, "3 .        <file>                     filename");
        hlp.add(null, "2 3      disk                         information about disk");
        hlp.add(null, "3 .        <file>                     filename");
        hlp.add(null, "2 3      hextype                      type one binary file");
        hlp.add(null, "3 .        <file>                     filename");
        hlp.add(null, "2 3      bintype                      type one binary file");
        hlp.add(null, "3 .        <file>                     filename");
        hlp.add(null, "2 3      7bittype                     type one binary file");
        hlp.add(null, "3 .        <file>                     filename");
        hlp.add(null, "2 3      hacktype                     type one binary file");
        hlp.add(null, "3 .        <file>                     filename");
        getHelpPipes(hlp, 120, privileged);
        hl.addOther(hlp);
        hl.add(null, "2 3      permission                   set file permissions");
        hl.add(null, "3 4        <file>                     target file");
        hl.add(null, "4 .          <str>                    permissions");
        hl.add(null, "2 3      receive                      receive file from network");
        hl.add(null, "3 4        <file>                     target file");
        hl.add(null, "4 4,.        <url>                    source url");
        hl.add(null, "2 3      curl                         display one file from network");
        hl.add(null, "3 3,.      <url>                      source url");
        hl.add(null, "2 3      transmit                     send file to network");
        hl.add(null, "3 4        <file>                     source file");
        hl.add(null, "4 4,.        <url>                    target url");
        hl.add(null, "2 3      compress                     compress file");
        hl.add(null, "3 4        <src>                      source file");
        hl.add(null, "4 .          <trg>                    target file");
        hl.add(null, "2 3      decompress                   decompress file");
        hl.add(null, "3 4        <src>                      source file");
        hl.add(null, "4 .          <trg>                    target file");
        hl.add(null, "2 3      archive                      create/append archive");
        hl.add(null, "3 4        <file>                     archive file");
        hl.add(null, "4 .          <src>                    source directory");
        hl.add(null, "2 3      extract                      exract archive");
        hl.add(null, "3 4        <file>                     archive file");
        hl.add(null, "4 .          <trg>                    target directory");
        hl.add(null, "2 3      rename                       rename disk file");
        hl.add(null, "3 4        <src>                      source file");
        hl.add(null, "4 .          <trg>                    target file");
        hl.add(null, "2 3      copy                         copy disk file");
        hl.add(null, "3 4        <src>                      source file");
        hl.add(null, "4 .          <trg>                    target file");
        hl.add(null, "2 3      delete                       delete directory entry");
        hl.add(null, "3 3,.      <file>                     filename");
        hl.add(null, "2 3      mkdir                        make directory");
        hl.add(null, "3 .        <file>                     filename");
        hl.add(null, "2 .      verify                       verify routing software");
        hl.add(null, "2 .      peer                         upgrade redundancy peers");
        hl.add(null, "2 .      revert                       revert routing software");
        hl.add(null, "2 .      backup                       backup routing software");
        hl.add(null, "2 3,.    cleanup                      clear routing software backup");
        hl.add(null, "3 .        [str]                      path to clean");
        hl.add(null, "2 .      cancel                       cancel auto-revert process");
        hl.add(null, "2 .      toggle-boot                  toggle upgrade reboot mode");
        hl.add(null, "2 3,.    upgrade                      upgrade routing software");
        hl.add(null, "3 4,.      [url]                      parameter of process");
        hl.add(null, "4 4,.        [num]                    parameter of process");
        hl.add(null, "2 3,.    simulate                     simulate upgrade process");
        hl.add(null, "3 3,.      [url]                      parameter of process");
        hl.add(null, "2 3      edit                         compose text file");
        hl.add(null, "3 .        <file>                     name of file");
        hl.add(null, "2 3      view                         read text file");
        hl.add(null, "3 .        <file>                     name of file");
        hl.add(null, "2 3      hexview                      view one binary file");
        hl.add(null, "3 .        <file>                     filename");
        hl.add(null, "2 3      binview                      view one binary file");
        hl.add(null, "3 .        <file>                     filename");
        hl.add(null, "2 3      7bitview                     view one binary file");
        hl.add(null, "3 .        <file>                     filename");
        hl.add(null, "2 3      hackview                     view one binary file");
        hl.add(null, "3 .        <file>                     filename");
        hl.add(null, "2 .      commander                    file manager");
        hl.add(null, "2 3,.    browser                      web browser");
        hl.add(null, "3 .        <url>                      target url");
        hl.add(null, "2 3      mailer                       email reader");
        hl.add(null, "3 .        <dir>                      mail directory");
        cfgAll.aliasHelps(cfgAlias.aliasType.flsh, 2, hl);
        hl.add(null, "1 2,.  write                          save configuration");
        hl.add(null, "2 3      file                         to disk file");
        hl.add(null, "3 .        <file>                     target file");
        hl.add(null, "2 3,.    network                      to network storage");
        hl.add(null, "3 .        <url>                      target url");
        hl.add(null, "2 .      terminal                     to this terminal");
        hl.add(null, "2 .      memory                       to persistent storage");
        hl.add(null, "2 .      erase                        clear persistent storage");
        hl.add(null, "1 2    screenrec                      record terminal session");
        hl.add(null, "2 .      <file>                       name of file");
        hl.add(null, "1 2    attach                         connect to system resources");
        hl.add(null, "2 .      chat                         discuss with other admins");
        hl.add(null, "2 3      vdc                          manage virtual device context");
        hl.add(null, "3 .        <name:vdc>                 name of vdc");
        hl.add(null, "2 3      process                      manage external process");
        hl.add(null, "3 .        <name:prc>                 name of process");
        hl.add(null, "2 3      scheduler                    manage scheduler");
        hl.add(null, "3 .        <name:sch>                 name of process");
        hl.add(null, "2 3      script                       manage script");
        hl.add(null, "3 .        <name:scr>                 name of process");
        hl.add(null, ".2 3     shell1                       run interactive shell process");
        hl.add(null, ".3 4,.     <cmd>                      name of process");
        hl.add(null, ".4 4,.       [param]                  parameter of process");
        hl.add(null, ".2 3     shell2                       run outputting shell process");
        hl.add(null, ".3 4,.     <cmd>                      name of process");
        hl.add(null, ".4 4,.       [param]                  parameter of process");
        hl.add(null, ".2 3     shell3                       run interactive shell with crlf");
        hl.add(null, ".3 4,.     <cmd>                      name of process");
        hl.add(null, ".4 4,.       [param]                  parameter of process");
        hl.add(null, "2 3      line                         access physical line");
        hl.add(null, "3 .        <name:lin>                 name of line");
        hl.add(null, "1 2    packet                         packet related things");
        hl.add(null, "2 3      portscan                     scan port on remote");
        hl.add(null, "3 4        <host>                     name of host");
        hl.add(null, "4 5,.        <port>                   port on host");
        hl.add(null, "5 5,.          ipv4                   specify ipv4 to use");
        hl.add(null, "5 5,.          ipv6                   specify ipv6 to use");
        hl.add(null, "5 6            vrf                    specify vrf to use");
        hl.add(null, "6 5,.            <name:vrf>           name of vrf");
        hl.add(null, "5 6            source                 specify interface to use");
        hl.add(null, "6 5,.            <name:ifc>           name of interface");
        hl.add(null, "5 6            timeout                specify timeout");
        hl.add(null, "6 5,.            <num>                timeout in milliseconds");
        hl.add(null, "5 6            min                    specify lower port number");
        hl.add(null, "6 5,.            <num>                port number");
        hl.add(null, "5 6            max                    specify upper port number");
        hl.add(null, "6 5,.            <num>                port number");
        hl.add(null, "5 6            ttl                    specify ttl number");
        hl.add(null, "6 5,.            <num>                ttl number");
        hl.add(null, "5 6            tos                    specify tos number");
        hl.add(null, "6 5,.            <num>                tos number");
        hl.add(null, "2 3      pmtud                        discover available mtu");
        hl.add(null, "3 4,.      <host>                     name of host");
        hl.add(null, "4 4,.        ipv4                     specify ipv4 to use");
        hl.add(null, "4 4,.        ipv6                     specify ipv6 to use");
        hl.add(null, "4 5          vrf                      specify vrf to use");
        hl.add(null, "5 4,.          <name:vrf>             name of vrf");
        hl.add(null, "4 5          source                   specify interface to use");
        hl.add(null, "5 4,.          <name:ifc>             name of interface");
        hl.add(null, "4 5          viahop                   specify nexthop to send");
        hl.add(null, "5 4,.          <addr>                 address to send to");
        hl.add(null, "4 5          min                      specify minimum size");
        hl.add(null, "5 4,.          <num>                  byte count");
        hl.add(null, "4 5          max                      specify maximum size");
        hl.add(null, "5 4,.          <num>                  byte count");
        hl.add(null, "4 5          data                     specify data to send");
        hl.add(null, "5 4,.          <num>                  payload byte");
        hl.add(null, "4 5          timeout                  specify timeout");
        hl.add(null, "5 4,.          <num>                  time in milliseconds");
        hl.add(null, "4 5          timediv                  specify time divider");
        hl.add(null, "5 4,.          <num>                  time in milliseconds");
        hl.add(null, "4 5          timemax                  specify maximum time");
        hl.add(null, "5 4,.          <num>                  time in milliseconds");
        hl.add(null, "4 5          delay                    specify delay between packets");
        hl.add(null, "5 4,.          <num>                  timeout in milliseconds");
        hl.add(null, "4 5          ttl                      specify ttl value");
        hl.add(null, "5 4,.          <num>                  ttl");
        hl.add(null, "4 5          tos                      specify tos value");
        hl.add(null, "5 4,.          <num>                  tos");
        hl.add(null, "4 5          sgt                      specify sgt value");
        hl.add(null, "5 4,.          <num>                  sgt");
        hl.add(null, "4 5          flow                     specify flow value");
        hl.add(null, "5 4,.          <num>                  flow");
        hl.add(null, ".2 3     arping                       send arp request");
        hl.add(null, ".3 4       <host>                     name of host");
        hl.add(null, ".4 5,.       <name:ifc>               name of interface");
        hl.add(null, ".5 5,.         ipv4                   specify ipv4 to use");
        hl.add(null, ".5 5,.         ipv6                   specify ipv6 to use");
        hl.add(null, ".5 6           delay                  specify delay between packets");
        hl.add(null, ".6 5,.           <num>                timeout in milliseconds");
        hl.add(null, ".5 6           repeat                 specify repeat count");
        hl.add(null, ".6 5,.           <num>                repeat count");
        hl.add(null, "2 3      capture                      capture interface traffic");
        hl.add(null, "3 4,.      <name:ifc>                 name of interface");
        hl.add(null, "4 .          [file]                   name of file");
        hl.add(null, "2 3      buffer                       save buffered traffic");
        hl.add(null, "3 4,.      <name:ifc>                 name of interface");
        hl.add(null, "4 .          [file]                   name of file");
        hl.add(null, "2 3      monitor                      monitor interface traffic");
        hl.add(null, "3 4,.      <name:ifc>                 name of interface");
        hl.add(null, "4 .          [name:ifc]               name of target interface");
        hl.add(null, ".2 3      openflow                    send packet over the api");
        hl.add(cfgAll.dmnOpenflow.listServers(), ".3 4        <name:loc>            server name");
        hl.add(null, "4 5          <num>                    counter to use");
        hl.add(null, "5 6            <num>                  interface number to send to");
        hl.add(null, "6 7              <addr>               target mac address");
        hl.add(null, "7 8                <addr>             source mac address");
        hl.add(null, "8 8,.                  [num]          packet contents");
        hl.add(null, ".2 3      p4lang                      send packet over the api");
        hl.add(cfgAll.dmnP4lang.listServers(), ".3 4        <name:loc>            server name");
        hl.add(null, "4 5          <num>                    counter to use");
        hl.add(null, "5 6            <num>                  interface number to send to");
        hl.add(null, "6 7              <addr>               target mac address");
        hl.add(null, "7 8                <addr>             source mac address");
        hl.add(null, "8 8,.                [num]            packet contents");
        hl.add(null, ".2 3     udpflood                     flood packets");
        hl.add(null, ".3 4       <name:vrf>                 name of vrf");
        hl.add(null, ".4 5         <addr>                   source address");
        hl.add(null, ".5 6           <num>                  source port");
        hl.add(null, ".6 7             <addr>               target prefix");
        hl.add(null, ".7 8               <num>              target range");
        hl.add(null, ".8 9                 <num>            size range");
        hl.add(null, ".9 .                   <name:pm>      policy map");
        hl.add(null, ".2 3     flood                        flood packets");
        hl.add(null, ".3 4       vrf                        through a vrf");
        hl.add(null, ".4 5         <name:vrf>               name of vrf");
        getHelpFlood(hl);
        hl.add(null, ".3 4       iface                      through an interface");
        hl.add(null, ".4 5         <name:ifc>               name of interface");
        getHelpFlood(hl);
        hl.add(null, ".2 3     replay                       replay capture on interface");
        hl.add(null, ".3 4       <name:ifc>                 name of interface");
        hl.add(null, ".4 5,.       <file>                   name of file");
        hl.add(null, ".5 .           [num]                  interpacket gap in millisecs");
        hl.add(null, ".2 3     inject                       inject packet to interface");
        hl.add(null, ".3 4       <name:ifc>                 name of interface");
        hl.add(null, ".4 4,.       [byte]                   byte in hex");
        hl.add(null, "2 3      wakeup                       wake up one host");
        hl.add(null, "3 4        <name:ifc>                 name of interface");
        hl.add(null, "4 .          <addr>                   address of host");
        hl.add(null, "2 3      txt2mrt                      text log to mrt");
        hl.add(null, "3 4        <file>                     source text");
        hl.add(null, "4 .          <file>                   target mrt");
        hl.add(null, "2 3      txt2full                     text log to console");
        hl.add(null, "3 .        <file>                     name of file");
        hl.add(null, "2 3      txt2sum                      text log to console");
        hl.add(null, "3 .        <file>                     name of file");
        hl.add(null, "2 3      ris2flt                      ris to filter");
        hl.add(null, "3 4        <url>                      url of ris live server");
        hl.add(null, "4 .          <addr>                   address to look up");
        hl.add(null, "2 3      ris2con                      ris to console");
        hl.add(null, "3 .        <url>                      url of ris live server");
        hl.add(null, "2 3      ris2bmp                      ris to bmp");
        hl.add(null, "3 4        <url>                      url of ris live server");
        hl.add(null, "4 5          <name:prx>               proxy profile");
        hl.add(null, "5 6            <str>                  hostname");
        hl.add(null, "6 .              <num>                port number");
        hl.add(null, "2 3      mrt2full                     mrt to console");
        hl.add(null, "3 .        <file>                     name of file");
        hl.add(null, "2 3      mrt2sum                      mrt to console");
        hl.add(null, "3 .        <file>                     name of file");
        hl.add(null, "2 3      mrt2stat                     mrt statistics");
        hl.add(null, "3 .        <file>                     name of file");
        hl.add(null, "2 3      mrt2flt                      mrt to filter");
        hl.add(null, "3 4        <file>                     name of file");
        hl.add(null, "4 .          <addr>                   address to look up");
        hl.add(null, ".2 3     mrtfilter                    filter mrt file using filters of a peer");
        cfgRtr.getRouterList(hl, 1, " to filter with");
        hl.add(null, ".4 5         <num:rtr>                process number");
        hl.add(null, ".5 6           <addr>                 peer address");
        hl.add(null, ".6 7             <file>               source mrt file");
        hl.add(null, ".7 8               <file>             target mrt file");
        hl.add(null, ".8 9                 <addr>           source peer");
        hl.add(null, ".9 .                   <addr>         target peer");
        hl.add(null, ".2 3     mrt2self                     replay mrt as if got from a peer");
        cfgRtr.getRouterList(hl, 1, " to replay to");
        hl.add(null, ".4 5         <num:rtr>                process number");
        hl.add(null, ".5 6           <addr>                 peer address");
        hl.add(null, ".6 7             <file>               mrt file");
        hl.add(null, ".7 8               <addr>             source peer");
        hl.add(null, ".8 .                 <addr>           target peer");
        hl.add(null, "2 3      mrt2pcap                     convert mrt to pcap");
        hl.add(null, "3 4        <file>                     name of mrt file");
        hl.add(null, "4 .          <file>                   name of pcap file");
        hl.add(null, ".2 3     mrt2bgp                      run mrt prefix sender");
        hl.add(null, ".3 4       <name:vrf>                 vrf name");
        hl.add(null, ".4 5         <name:ifc>               source interface name");
        hl.add(null, ".5 6           <addr>                 target address");
        hl.add(null, ".6 7             <num>                local as");
        hl.add(null, ".7 8               <file>             mrt file");
        hl.add(null, ".8 9                 <addr>           source peer");
        hl.add(null, ".9 10,.                <addr>         target peer");
        hl.add(null, ".10 .                    [num]        safi number");
        hl.add(null, "2 3     mrt2bmp                       run mrt prefix sender");
        hl.add(null, "3 4       <name:vrf>                  vrf name");
        hl.add(null, "4 5         <name:ifc>                source interface name");
        hl.add(null, "5 6           <addr>                  target address");
        hl.add(null, "6 7             <port>                target port");
        hl.add(null, "7 8               <file>              mrt file");
        hl.add(null, "8 9                 <addr>            source peer");
        hl.add(null, "9 .                   <addr>          target peer");
        hl.add(null, ".2 3     random                       run random packet generator");
        hl.add(null, ".3 4       <name:ifc>                 name of interface");
        hl.add(null, ".4 4,.       [byte]                   byte in hex");
        hl.add(null, ".2 3     bgpattr                      run attribute injector");
        hl.add(null, ".3 4       <name:vrf>                 vrf name");
        hl.add(null, ".4 5         <name:ifc>               source interface name");
        hl.add(null, ".5 6           <addr>                 target address");
        hl.add(null, ".6 7             <num>                local as");
        hl.add(null, ".7 8                <addr>            prefix to originate");
        hl.add(null, ".8 9                 <name:rm>        route map to apply");
        hl.add(null, ".9 9,.                 <num>          attribute byte");
        hl.add(null, ".2 3     bgpgen                       run random prefix generator");
        hl.add(null, ".3 4       <name:vrf>                 vrf name");
        hl.add(null, ".4 5         <name:ifc>               source interface name");
        hl.add(null, ".5 6           <addr>                 target address");
        hl.add(null, ".6 7             <num>                local as");
        hl.add(null, ".7 8               <addr>             prefix to originate");
        hl.add(null, ".8 9                 <name:rm>        route map to apply");
        hl.add(null, ".9 .                   <num>          number of prefixes");
        hl.add(null, "2 3      modem                        open modem session");
        hl.add(null, "3 4,.      <addr>                     address to call");
        hl.add(null, "4 .          <addr>                   address who calling");
        hl.add(null, "2 3      voice                        open voice session");
        hl.add(null, "3 4,.      <addr>                     address to call");
        hl.add(null, "4 5,.        <addr>                   address who calling");
        hl.add(null, "5 .            <file>                 script to run");
        hl.add(null, "2 3      message                      send voip message");
        hl.add(null, "3 4        <addr>                     address to call");
        hl.add(null, "4 5          <addr>                   address who calling");
        hl.add(null, "5 5,.          <text>                 message text");
        hl.add(null, "2 3      conference                   start voice conference");
        hl.add(null, "3 4,.      <addr>                     address who calling");
        hl.add(null, "4 4,.        <addr>                   address to call");
        hl.add(null, "2 3      speed                        test speed clients");
        hl.add(null, "3 .        <str>                      name of server");
        hl.add(null, "2 3      websock                      test websocket client");
        hl.add(null, "3 4        <str>                      url of server");
        hl.add(null, "4 4,.        <str>                    protocols");
        hl.add(null, "2 3      xotpad                       test xotpad client");
        hl.add(null, "3 4        <addr>                     address of server");
        hl.add(null, "4 5,.        <str>                    called number");
        hl.add(null, "5 .            <str>                  calling number");
        hl.add(null, "2 3      netconf                      do netconf commands");
        hl.add(null, "3 4        get                        do a get request");
        hl.add(null, "3 4        read                       do a get-config request");
        hl.add(null, "3 4        edit                       do a edit-config request");
        hl.add(null, "3 4        copy                       do a copy-config request");
        hl.add(null, "3 4        delete                     do a delete-config request");
        hl.add(null, "4 5          <addr>                   server to query");
        hl.add(null, "5 6            <text>                 username to use");
        hl.add(null, "6 7              <text>               password to use");
        hl.add(null, "7 8                <text>             xml path");
        hl.add(null, "8 .                  <text>           namespace of root");
        hl.add(null, "2 3      snmp                         do snmp commands");
        hl.add(null, "3 4        get                        do a get request");
        hl.add(null, "3 4        next                       do a getnext request");
        hl.add(null, "4 5          <addr>                   server to query");
        hl.add(null, "5 6            <text>                 community to use");
        hl.add(null, "6 .              <oid>                oid to query");
        hl.add(null, "2 3      smtp                         send email message");
        hl.add(null, "3 4,.      <str>                      email address");
        hl.add(null, "4 4,.        <str>                    email text");
        hl.add(null, "2 3      ntp                          check remote time");
        hl.add(null, "3 .        <str>                      server address");
        hl.add(null, "2 3      nrpe                         check remote status");
        hl.add(null, "3 4        <str>                      server address");
        hl.add(null, "4 4,.        <str>                    check name");
        hl.add(null, "2 3      aaa                          test aaa config");
        hl.add(null, "3 .        <str:aaa>                  aaa list");
        hl.add(null, "2 3      pcep                         get a path from pcep");
        hl.add(null, "3 4        <str>                      server address");
        hl.add(null, "4 5          <name:vrf>               vrf to use");
        hl.add(null, "5 6            <str>                  interface to use");
        hl.add(null, "6 7              te                   traffic engineering");
        hl.add(null, "6 7              sr                   segment routing");
        hl.add(null, "7 8                <str>              source address");
        hl.add(null, "8 .                  <str>            target address");
        cfgAll.aliasHelps(cfgAlias.aliasType.pckt, 2, hl);
        hl.add(null, "1 2    test                           test various things");
        hl.add(null, ".2 3      p4lang                      p4lang protocol api testing");
        hl.add(cfgAll.dmnP4lang.listServers(), "3 4      <name:loc>            server name");
        hl.add(null, "4 4,.        <str>                    string to send");
        hl.add(null, "2 3      whois                        asn name cache");
        hl.add(null, "3 .        <num>                      asn number");
        hl.add(null, "2 3      yangsensor                   create yang from sensor");
        hl.add(null, "3 4        <file>                     source");
        hl.add(null, "4 .          <file>                   target");
        hl.add(null, "2 3      yangconfig                   create yang from config");
        hl.add(null, "3 4        <file>                     source");
        hl.add(null, "4 .          <file>                   target");
        hl.add(null, ".2 3     swapkeys                     swap hidden keys");
        hl.add(null, "3 .        <str>                      new master secret");
        hl.add(null, "2 3      dns                          dns zone creator");
        hl.add(null, "3 .        <str>                      name of zone");
        hl.add(null, "2 3      translation                  translation rule");
        hl.add(null, "3 4        <name:trn>                 name of rule");
        hl.add(null, "4 4,.        <str>                    text");
        hl.add(null, "2 3      logging                      log one line");
        hl.add(null, "3 4,.      debug                      debug message");
        hl.add(null, "3 4,.      error                      error message");
        hl.add(null, "3 4,.      warning                    warning message");
        hl.add(null, "3 4,.      informational              informational message");
        hl.add(null, "4 4,.        <str>                    text to log");
        hl.add(null, ".3 4,.     traceback                  traceback message");
        hl.add(null, ".4 5         <str>                    message to test");
        hl.add(null, ".5 5,.         <str>                  text to log");
        hl.add(null, "2 3,.    password                     decode encoded password");
        hl.add(null, "3 3,.      <str>                      encoded string");
        hl.add(null, "2 3,.    otppass                      generate password");
        hl.add(null, "3 3,.      <str>                      encoded string");
        hl.add(null, "2 3,.    asn1                         decode asn1 encoded bytes");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    base64                       decode base64 encoded bytes");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    macaddr                      convert mac address");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    url                          decode url");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    xml                          decode xml");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    json                         decode json");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    protobuf                     decode protobuf");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    thrift                       decode thrift");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    addr                         decode address");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    prefix                       decode prefix");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    routing                      test routing lookup performance");
        hl.add(null, "3 3,.      [str]                      parameters");
        hl.add(null, "2 .      pipeline                     test pipeline throughput");
        hl.add(null, "2 .      ssh                          test ssh throughput");
        hl.add(null, "2 3,.    tls                          test tls throughput");
        hl.add(null, "3 4        <num>                      min");
        hl.add(null, "4 .          <num>                    max");
        hl.add(null, "2 3,.    dtls                         test dtls throughput");
        hl.add(null, "3 4        <num>                      min");
        hl.add(null, "4 .          <num>                    max");
        hl.add(null, ".2 .     gc                           run garbage collector");
        hl.add(null, "2 .      crypto                       test encryption and hash");
        hl.add(null, "2 3,.    digsig                       test digital signatures");
        hl.add(null, "3 3,.      [str]                      parameters");
        hl.add(null, "2 3,.    primes                       test digital primes");
        hl.add(null, "3 3,.      [str]                      parameters");
        hl.add(null, "2 3,.    window                       test window handler");
        hl.add(null, "3 4        <num>                      x size");
        hl.add(null, "4 .          <num>                    y size");
        hl.add(null, "2 3      vercore                      test vercore updater");
        hl.add(null, "3 4        <key>                      key file to use");
        hl.add(null, "4 .          <key>                    key file to include");
        hl.add(null, "2 3      vermkey                      test version maker");
        hl.add(null, "3 .        <key>                      key size in bits");
        hl.add(null, "2 3      verfile                      test version updater");
        hl.add(null, "3 4,.      <key>                      key file to use");
        hl.add(null, "4 4,.        [str]                    file to include in release");
        hl.add(null, "2 3,.    hwext                        perform forwarding externalization");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    hwpop                        perform forwarding port population");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    hwdet                        perform hw detection");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    hwred                        perform hw redetection");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, ".2 3,.   hwcfg                        perform hw configuration");
        hl.add(null, ".3 3,.     <str>                      parameter");
        hl.add(null, "2 3,.    image                        perform image creation");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    tester                       perform image tests");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    tstsum                       perform test summary conversion");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    tstmov                       perform test mover conversion");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    tstcpy                       perform test copier conversion");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    changelog                    perform changelog conversion");
        hl.add(null, "3 3,.      <str>                      parameter");
        hl.add(null, "2 3,.    template                     perform image templates");
        hl.add(null, "3 3,.      <str>                      parameter");
        cfgAll.aliasHelps(cfgAlias.aliasType.test, 2, hl);
        hl.add(null, "1 2,.  reload                         restart the system");
        hl.add(null, "2 3      in                           reload after a time interval");
        hl.add(null, "3 .        <num>                      minutes");
        hl.add(null, "2 3      at                           reload at a specified time");
        hl.add(null, "3 3,.      <str>                      datetime");
        hl.add(null, "2 .      cancel                       cancel pending reload");
        hl.add(null, "2 3,.    cold                         reboot the whole computer");
        hl.add(null, "3 4        in                         reload after a time interval");
        hl.add(null, "4 .          <num>                    minutes");
        hl.add(null, "3 4        at                         reload at a specified time");
        hl.add(null, "4 4,.        <str>                    datetime");
        hl.add(null, "2 3,.    warm                         reboot the router process");
        hl.add(null, "3 4        in                         reload after a time interval");
        hl.add(null, "4 .          <num>                    minutes");
        hl.add(null, "3 4        at                         reload at a specified time");
        hl.add(null, "4 4,.        <str>                    datetime");
        hl.add(null, "2 .      force                        reboot the router process without saving");
        hl.add(null, "2 .      peer                         reboot redundant router processes");
        hl.add(null, "2 3      vdc                          reboot virtual device context");
        hl.add(null, "3 4,.      <name:vdc>                 name of vdc");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      process                      reboot external process");
        hl.add(null, "3 4,.      <name:prc>                 name of process");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      tracker                      run one tracker round");
        hl.add(null, "3 4,.      <name:trk>                 name of tracker");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      scheduler                    run one scheduler round");
        hl.add(null, "3 4,.      <name:sch>                 name of scheduler");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        hl.add(null, "2 3      script                       run one script round");
        hl.add(null, "3 4,.      <name:scr>                 name of script");
        hl.add(null, "4 .          stop                     stop");
        hl.add(null, "4 .          start                    start");
        return hl;
    }

    /**
     * execute one command
     *
     * @return status of operation, see at one command
     */
    public cmdRes doCommand() {
        rollback = false;
        reader.setContext(getHelping(), cfgAll.hostName + (privileged ? "#" : ">"));
        String s = reader.readLine(null);
        if (s == null) {
            return cmdRes.logout;
        }
        if (pipe.settingsGet(pipeSetting.times, false)) {
            pipe.linePut(logger.getTimestamp());
        }
        return executeCommand(s);
    }

    /**
     * repair one command line
     *
     * @param s string to repair
     * @return repaired string
     */
    public String repairCommand(String s) {
        s = getHelping().repairLine(s);
        return s;
    }

    /**
     * execute one command
     *
     * @param a the command to execute
     * @return status of operation
     */
    public cmdRes executeCommand(String a) {
        reader.setFilter(null);
        if (a == null) {
            a = "";
        }
        cmd = new cmds("exec", a);
        cmd.pipe = pipe;
        if (authorization != null) {
            authResult ntry = authorization.authUserCommand(username, a);
            if (ntry.result != authResult.authSuccessful) {
                cmd.error("not authorized to execute that");
                return cmdRes.command;
            }
        }
        if (debugger.userExecEvnt) {
            logger.debug(cmd.getOriginal());
        }
        a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.exec, false);
        if (alias != null) {
            alias.doCommands(this, cmd);
            return cmdRes.command;
        }
        if (a.equals("exit")) {
            return cmdRes.logout;
        }
        if (a.equals("logout")) {
            return cmdRes.logout;
        }
        if (a.equals("netconf")) {
            doXml(false);
            return cmdRes.command;
        }
        if (a.equals("xml")) {
            doXml(true);
            return cmdRes.command;
        }
        if (a.equals("ppp")) {
            doPpp();
            return cmdRes.command;
        }
        if (a.equals("modememu")) {
            new userModem(pipe).doWork();
            return cmdRes.command;
        }
        if (a.equals("gpsemu")) {
            new userGps(pipe).doWorkTx();
            return cmdRes.command;
        }
        if (a.equals("gpstime")) {
            new userGps(pipe).doWorkRx();
            return cmdRes.command;
        }
        if (a.equals("lookup")) {
            doLookup();
            return cmdRes.command;
        }
        if (a.equals("listen")) {
            doListen();
            return cmdRes.command;
        }
        if (a.equals("telnet")) {
            doTelnet(0);
            userScreen.sendTit(pipe, cfgAll.hostName);
            return cmdRes.command;
        }
        if (a.equals("echo")) {
            cmd = reader.setFilter(cmd);
            reader.putStrArr(bits.str2lst(cmd.getRemaining()));
            return cmdRes.command;
        }
        if (a.equals("sleep")) {
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                i = 1;
            }
            bits.sleep(i);
            return cmdRes.command;
        }
        if (a.equals("curl")) {
            a = cmd.getRemaining();
            List<String> res = clntCurl.doGetUrl(pipe, a);
            cmd.error(cmds.doneFail(res == null));
            reader.putStrArr(res);
            return cmdRes.command;
        }
        if (a.equals("whois")) {
            clntWhois w = new clntWhois(pipe, cfgAll.getClntPrx(cfgAll.whoisProxy), cmd.word(), null);
            a = cmd.getRemaining();
            reader.putStrArr(w.doQuery(a));
            return cmdRes.command;
        }
        if (a.equals("ssh")) {
            doTelnet(servGeneric.protoSsh);
            return cmdRes.command;
        }
        if (a.equals("ssl")) {
            doTelnet(servGeneric.protoTls);
            return cmdRes.command;
        }
        if (a.equals("tls")) {
            doTelnet(servGeneric.protoTls);
            return cmdRes.command;
        }
        if (a.equals("dtls")) {
            doTelnet(servGeneric.protoDtls);
            return cmdRes.command;
        }
        if (a.equals("traceroute")) {
            doTraceroute();
            return cmdRes.command;
        }
        if (a.equals("mtr")) {
            doMtr();
            return cmdRes.command;
        }
        if (a.equals("send")) {
            userGame t = new userGame(new userScreen(pipe), reader);
            a = t.doSend(cmd);
            cmd.error(a);
            return cmdRes.command;
        }
        if (a.equals("ping")) {
            doPing();
            return cmdRes.command;
        }
        if (a.equals("menu")) {
            a = cmd.word();
            if (a.equals("key")) {
                doMenuK();
                return cmdRes.command;
            }
            if (a.equals("tui")) {
                doMenuT();
                return cmdRes.command;
            }
            cmd.badCmd();
            return cmdRes.command;
        }
        if (a.equals("portscan")) {
            doPortscan();
            return cmdRes.command;
        }
        if (a.equals("hostscan")) {
            doHostscan();
            return cmdRes.command;
        }
        if (a.equals("terminal")) {
            doTerminal();
            return cmdRes.command;
        }
        if (a.equals("enable")) {
            if (cfgAll.enaPass == null) {
                cmd.error("no enable configured");
                return cmdRes.command;
            }
            pipe.strPut("password:");
            int i;
            if (pipe.settingsGet(pipeSetting.passStar, false)) {
                i = 0x33;
            } else {
                i = 0x31;
            }
            a = pipe.lineGet(i);
            if (authLocal.secretTest(cfgAll.enaPass, a)) {
                cmd.error("invalid password");
                return cmdRes.command;
            }
            privileged = true;
            return cmdRes.command;
        }
        if (a.equals("disable")) {
            privileged = false;
            return cmdRes.command;
        }
        if (a.equals("tmux")) {
            a = cmd.word();
            int i = 0;
            if (a.equals("horizontal")) {
                i = 1;
            }
            if (a.equals("vertical")) {
                i = 2;
            }
            if (a.equals("both")) {
                i = 3;
            }
            reader.keyFlush();
            userTmux t = new userTmux(new userScreen(pipe), this);
            if (t.doInit(i)) {
                cmd.error("error initializing");
                return cmdRes.command;
            }
            t.doWork();
            reader.keyFlush();
            return cmdRes.command;
        }
        if (a.equals("game")) {
            reader.keyFlush();
            userGame t = new userGame(new userScreen(pipe), reader);
            t.doStart();
            t.doCommand(cmd);
            t.doFinish();
            reader.keyFlush();
            return cmdRes.command;
        }
        if (a.equals("bwmon")) {
            doBwmon();
            return cmdRes.command;
        }
        if (a.equals("tclsh")) {
            doTclsh();
            return cmdRes.command;
        }
        if (a.equals("watch")) {
            doWatch();
            return cmdRes.command;
        }
        if (a.equals("differs")) {
            doDiffers();
            return cmdRes.command;
        }
        if (a.equals("display")) {
            doDisplay();
            return cmdRes.command;
        }
        if (a.equals("view")) {
            doView();
            return cmdRes.command;
        }
        if (a.equals("show")) {
            userShow s = new userShow();
            cmd = reader.setFilter(cmd);
            s.cmd = cmd;
            s.rdr = reader;
            s.hlp = getHelping();
            alias = s.doer();
            if (alias == null) {
                return cmdRes.command;
            }
            alias.doCommands(this, cmd);
            return cmdRes.command;
        }
        if (!privileged) {
            cmd.error("not enough privilege");
            return cmdRes.command;
        }
        if (a.equals("screenrec")) {
            a = cmd.word();
            reader.keyFlush();
            userRecord t = new userRecord(a, this);
            t.doWork();
            reader.keyFlush();
            return cmdRes.command;
        }
        if (a.equals("attach")) {
            doAttach();
            return cmdRes.command;
        }
        if (a.equals("clear")) {
            userClear c = new userClear();
            cmd = reader.setFilter(cmd);
            c.cmd = cmd;
            c.rdr = reader;
            alias = c.doer();
            if (alias == null) {
                return cmdRes.command;
            }
            alias.doCommands(this, cmd);
            return cmdRes.command;
        }
        if (a.equals("debug")) {
            if (debugger.setByName(cmd, true)) {
                cmd.badCmd();
            }
            return cmdRes.command;
        }
        if (a.equals("undebug")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                debugger.setAll(false);
                return cmdRes.command;
            }
            if (a.equals("all")) {
                debugger.setAll(false);
                return cmdRes.command;
            }
            if (debugger.setByName(cmd, false)) {
                cmd.badCmd();
            }
            return cmdRes.command;
        }
        if (a.equals("set")) {
            doSetUnset(pipe, reader, cmd, false);
            return cmdRes.command;
        }
        if (a.equals("delete")) {
            doSetUnset(pipe, reader, cmd, true);
            return cmdRes.command;
        }
        if (a.equals("configure")) {
            if (cfgAll.configExclusive > 1) {
                cmd.error("operation forbidden by exclusive configuration mode");
                return cmdRes.command;
            }
            cmd = reader.setFilter(cmd);
            a = cmd.word();
            if (a.length() < 1) {
                a = "terminal";
            }
            if (a.equals("terminal")) {
                rollback = false;
                return cmdRes.config;
            }
            if (a.equals("rollback")) {
                rollback = true;
                return cmdRes.config;
            }
            if (a.equals("reapply")) {
                List<String> cfg = cfgAll.getShRun(1);
                int res = cfgInit.executeSWcommands(cfg, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                return cmdRes.command;
            }
            if (a.equals("file")) {
                List<String> cfg = bits.txt2buf(cmd.word());
                if (cfg == null) {
                    cmd.error("error reading file");
                    return cmdRes.command;
                }
                int res = cfgInit.executeSWcommands(cfg, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                return cmdRes.command;
            }
            if (a.equals("replace")) {
                List<String> c2 = bits.txt2buf(cmd.word());
                if (c2 == null) {
                    cmd.error("error reading file");
                    return cmdRes.command;
                }
                List<String> c1 = cfgAll.getShRun(1);
                List<String> c3 = userFilter.getDiffs(c1, c2);
                reader.putStrArr(bits.lst2lin(c3, false));
                int res = cfgInit.executeSWcommands(c3, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                c3 = userFilter.getDiffs(c2, c1);
                reader.putStrArr(c3);
                return cmdRes.command;
            }
            if (a.equals("reload")) {
                a = cfgInit.getRWpath() + "cfg" + bits.randomD() + ".tmp";
                boolean dl = userFlash.doReceive(pipe, encUrl.parseOne(cmd.getRemaining()), new File(a));
                List<String> cfg = bits.txt2buf(a);
                userFlash.delete(a);
                if (dl) {
                    cmd.error("error fetching file");
                    return cmdRes.command;
                }
                if (cfg == null) {
                    cmd.error("error reading file");
                    return cmdRes.command;
                }
                doCfgBackup();
                boolean b = bits.buf2txt(true, cfg, cfgInit.cfgFileSw);
                cmd.error(cmds.doneFail(b));
                prtRedun.doConfig();
                prtRedun.doReload();
                return cmdRes.command;
            }
            if (a.equals("overwrite-network")) {
                a = cfgInit.getRWpath() + "cfg" + bits.randomD() + ".tmp";
                boolean dl = userFlash.doReceive(pipe, encUrl.parseOne(cmd.getRemaining()), new File(a));
                List<String> c2 = bits.txt2buf(a);
                userFlash.delete(a);
                if (dl) {
                    cmd.error("error fetching file");
                    return cmdRes.command;
                }
                if (c2 == null) {
                    cmd.error("error reading file");
                    return cmdRes.command;
                }
                List<String> c1 = cfgAll.getShRun(1);
                List<String> c3 = userFilter.getDiffs(c1, c2);
                reader.putStrArr(bits.lst2lin(c3, false));
                int res = cfgInit.executeSWcommands(c3, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                c3 = userFilter.getDiffs(c2, c1);
                reader.putStrArr(c3);
                return cmdRes.command;
            }
            if (a.equals("network")) {
                a = cfgInit.getRWpath() + "cfg" + bits.randomD() + ".tmp";
                boolean dl = userFlash.doReceive(pipe, encUrl.parseOne(cmd.getRemaining()), new File(a));
                List<String> cfg = bits.txt2buf(a);
                userFlash.delete(a);
                if (dl) {
                    cmd.error("error fetching file");
                    return cmdRes.command;
                }
                if (cfg == null) {
                    cmd.error("error reading file");
                    return cmdRes.command;
                }
                int res = cfgInit.executeSWcommands(cfg, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                return cmdRes.command;
            }
            if (a.equals("banner")) {
                List<String> txt = new ArrayList<String>();
                a = "";
                for (int i = 0; i < cfgAll.banner.length; i++) {
                    byte[] buf = new byte[1];
                    buf[0] = cfgAll.banner[i];
                    if (buf[0] == 13) {
                        txt.add(a);
                        a = "";
                    }
                    if (buf[0] < 32) {
                        continue;
                    }
                    a = a + new String(buf);
                }
                txt.add(a);
                userEditor e = new userEditor(new userScreen(pipe), txt, "banner", false);
                if (e.doEdit()) {
                    return null;
                }
                String s = "";
                for (int i = 0; i < txt.size(); i++) {
                    byte[] buf = pipeSide.getEnding(pipeSide.modTyp.modeCRLF);
                    s += txt.get(i) + new String(buf);
                }
                byte[] buf = s.getBytes();
                cfgAll.banner = buf;
                s = encBase64.encodeBytes(buf, 0, buf.length);
                cmd.error("banner set as " + s);
                return null;
            }
            if (a.equals("startup")) {
                List<String> c1 = bits.txt2buf(cfgInit.cfgFileSw);
                List<String> c2 = new ArrayList<String>();
                if (c1 == null) {
                    cmd.error("error reading file");
                    return cmdRes.command;
                }
                c2.addAll(c1);
                userEditor e = new userEditor(new userScreen(pipe), c2, "startup config", false);
                if (e.doEdit()) {
                    return cmdRes.command;
                }
                reader.putStrArr(userFilter.getDiffs(c1, c2));
                reader.putStrArr(userFilter.getDiffs(c2, c1));
                doCfgBackup();
                boolean b = bits.buf2txt(true, c2, cfgInit.cfgFileSw);
                cmd.error(cmds.doneFail(b));
                prtRedun.doConfig();
                prtRedun.doReload();
                return null;
            }
            if (a.equals("editor")) {
                List<String> c1 = cfgAll.getShRun(1);
                if (cmd.size() > 0) {
                    c1 = userFilter.getSection(c1, userReader.filter2reg(cmd.getRemaining()));
                }
                List<String> c2 = new ArrayList<String>();
                c2.addAll(c1);
                userEditor e = new userEditor(new userScreen(pipe), c2, "running config", false);
                if (e.doEdit()) {
                    return cmdRes.command;
                }
                List<String> c3 = userFilter.getDiffs(c1, c2);
                reader.putStrArr(bits.lst2lin(c3, false));
                int res = cfgInit.executeSWcommands(c3, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                c3 = userFilter.getDiffs(c2, c1);
                reader.putStrArr(c3);
                return cmdRes.command;
            }
            if (a.equals("viewer")) {
                List<String> c1 = cfgAll.getShRun(1);
                if (cmd.size() > 0) {
                    c1 = userFilter.getSection(c1, userReader.filter2reg(cmd.getRemaining()));
                }
                userEditor v = new userEditor(new userScreen(pipe), c1, "running config", false);
                v.doView();
                return cmdRes.command;
            }
            if (a.equals("revert")) {
                List<String> c1 = cfgAll.getShRun(1);
                List<String> c2 = bits.txt2buf(cfgInit.cfgFileSw);
                if (c2 == null) {
                    cmd.error("error reading file");
                    return cmdRes.command;
                }
                List<String> c3 = userFilter.getDiffs(c1, c2);
                reader.putStrArr(bits.lst2lin(c3, false));
                int res = cfgInit.executeSWcommands(c3, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                c3 = userFilter.getDiffs(c2, c1);
                reader.putStrArr(c3);
                return cmdRes.command;
            }
            cmd.badCmd();
            return cmdRes.command;
        }
        if (a.equals("flash")) {
            cmd = reader.setFilter(cmd);
            userFlash t = new userFlash();
            t.cmd = cmd;
            t.pip = pipe;
            t.rdr = reader;
            alias = t.doer();
            if (alias == null) {
                return cmdRes.command;
            }
            alias.doCommands(this, cmd);
            return cmdRes.command;
        }
        if (a.equals("write")) {
            cmd = reader.setFilter(cmd);
            a = cmd.word();
            if (a.length() < 1) {
                a = "memory";
            }
            if (a.equals("erase")) {
                doCfgBackup();
                cmd.error("erasing configuration");
                boolean b = bits.buf2txt(true, new ArrayList<String>(), cfgInit.cfgFileSw);
                cmd.error(cmds.doneFail(b));
                prtRedun.doConfig();
                prtRedun.doReload();
                return cmdRes.command;
            }
            if (a.equals("terminal")) {
                reader.putStrArr(cfgAll.getShRun(1));
                return cmdRes.command;
            }
            if (a.equals("file")) {
                cmd.error("exporting configuration");
                boolean b = bits.buf2txt(true, cfgAll.getShRun(1), cmd.getRemaining());
                cmd.error(cmds.doneFail(b));
                return cmdRes.command;
            }
            if (a.equals("memory")) {
                doCfgBackup();
                cmd.error("saving configuration");
                boolean b = bits.buf2txt(true, cfgAll.getShRun(1), cfgInit.cfgFileSw);
                cmd.error(cmds.doneFail(b));
                prtRedun.doConfig();
                prtRedun.doReload();
                if (!cfgAll.configAbackup) {
                    return cmdRes.command;
                }
                a = "network";
            }
            if (a.equals("network")) {
                cmd.error("archiving configuration");
                encUrl url = encUrl.parseOne(cmd.word());
                if (url.server.length() < 1) {
                    url.fromString(cfgAll.configServer);
                    if (cfgAll.configUser != null) {
                        url.username = "" + cfgAll.configUser;
                        url.password = "" + cfgAll.configPass;
                    }
                }
                if (url.filName.length() < 1) {
                    url.filName = "" + cfgAll.hostName;
                    url.filExt = ".txt";
                }
                a = cfgInit.getRWpath() + "wrt" + bits.randomD() + ".tmp";
                if (bits.buf2txt(true, cfgAll.getShRun(1), a)) {
                    cmd.error("failed to write temporary file!");
                    return cmdRes.command;
                }
                userFlash.doSend(pipe, url, new File(a));
                userFlash.delete(a);
                return cmdRes.command;
            }
            return cmdRes.command;
        }
        if (a.equals("test")) {
            userTest t = new userTest();
            t.cmd = cmd;
            t.pip = pipe;
            t.rdr = reader;
            alias = t.doer();
            if (alias == null) {
                return cmdRes.command;
            }
            alias.doCommands(this, cmd);
            return cmdRes.command;
        }
        if (a.equals("packet")) {
            userPacket t = new userPacket();
            t.cmd = cmd;
            t.pip = pipe;
            t.rdr = reader;
            alias = t.doer();
            if (alias == null) {
                return cmdRes.command;
            }
            alias.doCommands(this, cmd);
            return cmdRes.command;
        }
        if (a.equals("reload")) {
            a = cmd.word();
            if (a.equals("scheduler")) {
                cfgSched sch = cfgAll.schedFind(cmd.word(), false);
                if (sch == null) {
                    cmd.error("no such scheduler");
                    return null;
                }
                a = cmd.word();
                if (a.equals("start")) {
                    sch.startNow();
                }
                if (a.equals("stop")) {
                    sch.stopNow();
                }
                if (a.equals("")) {
                    sch.doRound();
                }
                return null;
            }
            if (a.equals("script")) {
                cfgScrpt sch = cfgAll.scrptFind(cmd.word(), false);
                if (sch == null) {
                    cmd.error("no such script");
                    return null;
                }
                a = cmd.word();
                if (a.equals("start")) {
                    sch.startNow();
                }
                if (a.equals("stop")) {
                    sch.stopNow();
                }
                if (a.equals("")) {
                    sch.doRound(null);
                }
                return null;
            }
            if (a.equals("vdc")) {
                cfgVdc ntry = cfgInit.vdcLst.find(new cfgVdc(cmd.word()));
                if (ntry == null) {
                    cmd.error("no such vdc");
                    return cmdRes.command;
                }
                a = cmd.word();
                if (a.equals("start")) {
                    ntry.setRespawn(true);
                }
                if (a.equals("stop")) {
                    ntry.setRespawn(false);
                }
                ntry.restartNow();
                return cmdRes.command;
            }
            if (a.equals("process")) {
                cfgPrcss ntry = cfgAll.prcFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such process");
                    return cmdRes.command;
                }
                a = cmd.word();
                if (a.equals("start")) {
                    ntry.startNow();
                }
                if (a.equals("stop")) {
                    ntry.stopNow();
                }
                ntry.restartNow();
                return cmdRes.command;
            }
            if (a.equals("tracker")) {
                cfgTrack ntry = cfgAll.trackFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such tracker");
                    return cmdRes.command;
                }
                a = cmd.word();
                if (a.equals("start")) {
                    ntry.worker.startNow();
                }
                if (a.equals("stop")) {
                    ntry.worker.stopNow();
                }
                if (a.equals("")) {
                    ntry.worker.doRound();
                }
                return cmdRes.command;
            }
            if (a.equals("peer")) {
                prtRedun.doReload();
                return cmdRes.command;
            }
            if (a.equals("cancel")) {
                doReload(false, -1);
                return cmdRes.command;
            }
            if (a.equals("force")) {
                cfgInit.stopRouter(true, 11, "user requested");
                return cmdRes.command;
            }
            if (userFilter.doDiffer(cfgAll.getShRun(1), bits.txt2buf(cfgInit.cfgFileSw)) > 1) {
                String b = pipe.strChr("configuration not saved, proceed?", "ynYN").toLowerCase();
                if (!b.equals("y")) {
                    return cmdRes.command;
                }
            }
            if (a.equals("warm")) {
                a = cmd.word();
                if (a.equals("in")) {
                    doReload(false, bits.getTime() + (bits.str2num(cmd.word()) * 60000));
                    return cmdRes.command;
                }
                if (a.equals("at")) {
                    doReload(false, bits.str2time(cfgAll.timeZoneName, cmd.getRemaining()));
                    return cmdRes.command;
                }
                if (!a.equals("")) {
                    cmd.badCmd();
                    return cmdRes.command;
                }
                cfgInit.stopRouter(true, 3, "user requested");
                return cmdRes.command;
            }
            if (a.equals("cold")) {
                a = cmd.word();
                if (a.equals("in")) {
                    doReload(true, bits.getTime() + (bits.str2num(cmd.word()) * 60000));
                    return cmdRes.command;
                }
                if (a.equals("at")) {
                    doReload(true, bits.str2time(cfgAll.timeZoneName, cmd.getRemaining()));
                    return cmdRes.command;
                }
                if (!a.equals("")) {
                    cmd.badCmd();
                    return cmdRes.command;
                }
                cfgInit.stopRouter(true, 4, "user requested");
                return cmdRes.command;
            }
            if (a.equals("in")) {
                doReload(false, bits.getTime() + (bits.str2num(cmd.word()) * 60000));
                return cmdRes.command;
            }
            if (a.equals("at")) {
                doReload(false, bits.str2time(cfgAll.timeZoneName, cmd.getRemaining()));
                return cmdRes.command;
            }
            if (!a.equals("")) {
                cmd.badCmd();
                return cmdRes.command;
            }
            cfgInit.stopRouter(true, 14, "user requested");
            return cmdRes.command;
        }
        cmd.badCmd();
        return cmdRes.command;
    }

    private boolean need2stop() {
        if (pipe.isClosed() != 0) {
            return true;
        }
        boolean brk = false;
        for (;;) {
            byte[] buf = new byte[1];
            if (pipe.nonBlockGet(buf, 0, buf.length) != buf.length) {
                break;
            }
            switch (buf[0]) {
                case 0x03: // ctrl+c;
                case 0x11: // ctrl+q;
                case 0x18: // ctrl+x;
                    brk = true;
                    break;
            }
        }
        return brk;
    }

    /**
     * do set unset command
     *
     * @param pipe pipe to use
     * @param reader reader to use
     * @param cmd command to execute
     * @param negated true if unset
     */
    public static void doSetUnset(pipeSide pipe, userReader reader, cmds cmd, boolean negated) {
        userConfig cfg = new userConfig(pipe, reader);
        reader.setFilter(null);
        userHelping hlp;
        String s = "";
        String a = "";
        boolean last;
        for (;;) {
            a = cmd.word();
            s = (s + " " + a).trim();
            hlp = cfg.getHelping(true, true, true);
            reader.setContext(hlp, "");
            last = cmd.size() < 1;
            a = hlp.repairLine(s);
            if (!hlp.endOfCmd(a)) {
                break;
            }
            if (!last) {
                continue;
            }
            reader.putStrArr(hlp.getHelp(s, true));
            return;
        }
        if (last && negated) {
            a = cmds.negated + cmds.tabulator + a;
        }
        cfg.executeCommand(a);
        if (last) {
            return;
        }
        s = cmd.getRemaining();
        hlp = cfg.getHelping(true, true, true);
        reader.setContext(hlp, "");
        a = hlp.repairLine(s);
        if (hlp.endOfCmd(a)) {
            reader.putStrArr(hlp.getHelp(s, true));
            return;
        }
        if (negated) {
            a = cmds.negated + cmds.tabulator + a;
        }
        cfg.executeCommand(a);
    }

    private void doMenuK() {
        String a = cmd.word();
        cfgMenuK ntry = cfgAll.menuKfind(a, false);
        if (ntry == null) {
            cmd.error("no such menu");
            return;
        }
        String s = ntry.doMenu(pipe);
        if (s == null) {
            return;
        }
        userExec exe = new userExec(pipe, reader);
        exe.privileged = privileged;
        s = exe.repairCommand(s);
        pipe.linePut(a + " - " + s);
        if (pipe.settingsGet(pipeSetting.logging, false)) {
            logger.info("command menu:" + s + " from " + pipe.settingsGet(pipeSetting.origin, "?"));
        }
        exe.executeCommand(s);
    }

    private void doMenuT() {
        String a = cmd.word();
        cfgMenuT ntry = cfgAll.menuTfind(a, false);
        if (ntry == null) {
            cmd.error("no such menu");
            return;
        }
        ntry.doMenu(pipe, reader, privileged);
    }

    private void doPortscan() {
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int timeout = 1000;
        int min = 1;
        int max = 1024;
        int ttl = -1;
        int tos = -1;
        int proto = 0;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("source")) {
                ifc = cfgAll.ifcFind(cmd.word(), 0);
                continue;
            }
            if (a.equals("ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("ipv6")) {
                proto = 6;
                continue;
            }
            if (a.equals("timeout")) {
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("min")) {
                min = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("max")) {
                max = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("ttl")) {
                ttl = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        userTerminal trm = new userTerminal(pipe);
        addrIP trg = trm.resolveAddr(rem, proto);
        if (trg == null) {
            return;
        }
        if (timeout < 1) {
            timeout = 1;
        }
        addrIP src = null;
        if (ifc != null) {
            src = ifc.getLocAddr(trg);
        }
        clntPorts trc = new clntPorts();
        trc.vrf = vrf;
        trc.ifc = ifc;
        trc.trg = trg;
        trc.ttl = ttl;
        trc.tos = tos;
        trc.tim = timeout;
        pipe.linePut("scanning " + trg + ", src=" + src + ", vrf=" + vrf.name + ", ttl=" + ttl + ", tos=" + tos + ", ran=" + min + ".." + max + ", tim=" + timeout);
        for (int i = min; i < max; i++) {
            if (need2stop()) {
                break;
            }
            pipe.strPut("" + i);
            pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
            if (trc.testOne(0, i)) {
                continue;
            }
            pipe.linePut("port " + i + " open");
        }
    }

    private void doBwmon() {
        int interval = 0;
        int counter = 0;
        int mode = 0;
        if (cfgAll.moreInterfaces(7) > 0) {
            mode = 1;
        }
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("software")) {
                mode = 0;
                continue;
            }
            if (a.equals("hardware")) {
                mode = 1;
                continue;
            }
            if (a.equals("bytes")) {
                counter = 0;
                continue;
            }
            if (a.equals("packets")) {
                counter = 1;
                continue;
            }
            if (a.equals("seconds")) {
                interval = 0;
                continue;
            }
            if (a.equals("minutes")) {
                interval = 1;
                continue;
            }
            if (a.equals("hours")) {
                interval = 2;
                continue;
            }
        }
        mode = (mode * 6) + (counter * 3) + interval + 21;
        reader.keyFlush();
        List<String> lst = new ArrayList<String>();
        userEditor edtr = new userEditor(new userScreen(pipe), lst, cfgAll.hostName + "#bwmon", pipe.settingsGet(pipeSetting.times, false));
        for (;;) {
            lst.clear();
            lst.addAll(cfgAll.getShIntTxt(mode));
            if (edtr.doTimed(1000, false)) {
                break;
            }
        }
        edtr.doClear();
        reader.keyFlush();
    }

    private void doMtr() {
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int timeout = 1000;
        int tos = 0;
        int flow = 0;
        int len = 64;
        int ipver = 0;
        int port = 33440;
        int proto = 0;
        boolean resolv = false;
        ipRtr rtr = null;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("source")) {
                ifc = cfgAll.ifcFind(cmd.word(), 0);
                continue;
            }
            if (a.equals("timeout")) {
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("port")) {
                port = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("protocol")) {
                proto = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("ipv4")) {
                ipver = 4;
                continue;
            }
            if (a.equals("ipv6")) {
                ipver = 6;
                continue;
            }
            if (a.equals("tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("flow")) {
                flow = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("size")) {
                len = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("router")) {
                tabRouteAttr.routeType typ = cfgRtr.name2num(cmd.word());
                if (typ == null) {
                    continue;
                }
                cfgRtr cfg = cfgAll.rtrFind(typ, bits.str2num(cmd.word()), false);
                if (cfg == null) {
                    continue;
                }
                rtr = cfg.getRouter();
                continue;
            }
            if (a.equals("lookup")) {
                resolv = true;
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        userTerminal trm = new userTerminal(pipe);
        addrIP trg = trm.resolveAddr(rem, ipver);
        if (trg == null) {
            return;
        }
        if (timeout < 1) {
            timeout = 1;
        }
        prtTraceroute trc = new prtTraceroute();
        trc.vrf = vrf;
        trc.ifc = ifc;
        trc.trg = trg;
        trc.port = port;
        trc.proto = proto;
        if (resolv) {
            trc.domainCln = new clntDns();
            trc.domainLst = cfgAll.nameServerAddr;
        }
        trc.routerPrc = rtr;
        if (trc.register2ip()) {
            cmd.error("bind error");
            return;
        }
        addrIP src = null;
        if (ifc != null) {
            src = ifc.getLocAddr(trg);
        }
        pipe.linePut("tracing " + trg + ", src=" + src + ", vrf=" + vrf.name + ", prt=" + proto + "/" + port + ", tim=" + timeout + ", tos=" + tos + ", flow=" + flow + ", len=" + len);
        len -= prtIcmptun.adjustSize(trg);
        int none = 0;
        int ttl = 0;
        reader.keyFlush();
        List<String> lst = new ArrayList<String>();
        userEditor edtr = new userEditor(new userScreen(pipe), lst, cfgAll.hostName + "#trace " + trg, pipe.settingsGet(pipeSetting.times, false));
        int request[] = new int[256];
        int reply[] = new int[256];
        int timeCur[] = new int[256];
        int timeMin[] = new int[256];
        int timeMax[] = new int[256];
        int timeSum[] = new int[256];
        int label[] = new int[256];
        addrIP reportA[] = new addrIP[256];
        String reportN[] = new String[256];
        String path1[] = new String[256];
        String path2[] = new String[256];
        String path3[] = new String[256];
        for (int i = 0; i < request.length; i++) {
            timeMin[i] = Integer.MAX_VALUE;
            timeMax[i] = Integer.MIN_VALUE;
        }
        for (;;) {
            userFormat res = new userFormat("|", "hop|req|rep|los|addr|name|tim|min|avg|max|mpls|path|name|info");
            for (int i = 1; i < request.length; i++) {
                int o = reply[i];
                if (o < 1) {
                    o = 1;
                }
                res.add(i + "|" + request[i] + "|" + reply[i] + "|" + (request[i] - reply[i]) + "|" + reportA[i] + "|" + reportN[i] + "|" + timeCur[i] + "|" + timeMin[i] + "|" + (timeSum[i] / o) + "|" + timeMax[i] + "|" + label[i] + "|" + path1[i] + "|" + path2[i] + "|" + path3[i]);
            }
            lst.clear();
            lst.addAll(res.formatAll(pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal)));
            ttl++;
            if (ttl >= 255) {
                none = 0;
                ttl = 1;
            }
            if (none > 16) {
                none = 0;
                ttl = 1;
            }
            trc.doRound1(ttl, tos, flow, 0, len);
            if (edtr.doTimed(timeout, false)) {
                break;
            }
            request[ttl]++;
            trc.doRound2();
            if (trc.errRtr == null) {
                none++;
                continue;
            }
            none = 0;
            reply[ttl]++;
            timeCur[ttl] = trc.errTim;
            timeSum[ttl] += trc.errTim;
            if (timeMin[ttl] > trc.errTim) {
                timeMin[ttl] = trc.errTim;
            }
            if (timeMax[ttl] < trc.errTim) {
                timeMax[ttl] = trc.errTim;
            }
            label[ttl] = trc.errLab;
            reportA[ttl] = trc.errRtr.copyBytes();
            if (trc.routerNtry != null) {
                path1[ttl] = trc.routerNtry.asPathStr();
                path2[ttl] = trc.routerNtry.asNameStr();
                path3[ttl] = trc.routerNtry.asInfoStr();
            }
            if (resolv) {
                reportN[ttl] = trc.domainNam;
            }
            if (trg.compareTo(trc.errRtr) == 0) {
                ttl = 0;
            }
        }
        trc.unregister2ip();
        edtr.doClear();
        reader.keyFlush();
    }

    private void doTraceroute() {
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int timeout = 1000;
        int tos = 0;
        int flow = 0;
        int len = 64;
        int ipver = 0;
        int delay = 0;
        int port = 33440;
        int proto = 0;
        boolean resolv = false;
        ipRtr rtr = null;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("source")) {
                ifc = cfgAll.ifcFind(cmd.word(), 0);
                continue;
            }
            if (a.equals("timeout")) {
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("port")) {
                port = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("protocol")) {
                proto = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("delay")) {
                delay = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("ipv4")) {
                ipver = 4;
                continue;
            }
            if (a.equals("ipv6")) {
                ipver = 6;
                continue;
            }
            if (a.equals("tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("flow")) {
                flow = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("size")) {
                len = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("router")) {
                tabRouteAttr.routeType typ = cfgRtr.name2num(cmd.word());
                if (typ == null) {
                    continue;
                }
                cfgRtr cfg = cfgAll.rtrFind(typ, bits.str2num(cmd.word()), false);
                if (cfg == null) {
                    continue;
                }
                rtr = cfg.getRouter();
                continue;
            }
            if (a.equals("lookup")) {
                resolv = true;
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        userTerminal trm = new userTerminal(pipe);
        addrIP trg = trm.resolveAddr(rem, ipver);
        if (trg == null) {
            return;
        }
        if (timeout < 1) {
            timeout = 1;
        }
        ipFwd fwd = vrf.getFwd(trg);
        prtTraceroute trc = new prtTraceroute();
        trc.vrf = vrf;
        trc.ifc = ifc;
        trc.trg = trg;
        trc.port = port;
        trc.proto = proto;
        if (resolv) {
            trc.domainCln = new clntDns();
            trc.domainLst = cfgAll.nameServerAddr;
        }
        trc.routerPrc = rtr;
        if (trc.register2ip()) {
            cmd.error("bind error");
            return;
        }
        addrIP src = null;
        if (ifc != null) {
            src = ifc.getLocAddr(trg);
        }
        pipe.linePut("tracing " + trg + ", src=" + src + ", vrf=" + vrf.name + ", prt=" + proto + "/" + port + ", tim=" + timeout + ", tos=" + tos + ", flow=" + flow + ", len=" + len);
        pipe.linePut(trc.getHeadLine());
        len -= prtIcmptun.adjustSize(trg);
        int none = 0;
        for (int ttl = 1; ttl < 255; ttl++) {
            if (need2stop()) {
                break;
            }
            if (none > 16) {
                break;
            }
            if (delay > 0) {
                bits.sleep(delay);
            }
            trc.doRound1(ttl, tos, flow, timeout, len);
            trc.doRound2();
            pipe.linePut(trc.getCurrLine());
            if (trc.errRtr == null) {
                none++;
                continue;
            }
            none = 0;
            if (trg.compareTo(trc.errRtr) == 0) {
                break;
            }
        }
        trc.unregister2ip();
    }

    private void doHostscan() {
        addrIP strt = new addrIP();
        addrIP incr = new addrIP();
        addrIP numb = new addrIP();
        strt.fromString(cmd.word());
        incr.fromString(cmd.word());
        numb.fromString(cmd.word());
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int tim = 100;
        int ttl = 255;
        int sgt = 0;
        int tos = 0;
        int flow = 0;
        int data = 0;
        int len = 64;
        int prt = -1;
        int alrt = -1;
        boolean dntfrg = false;
        boolean lok = false;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("source")) {
                ifc = cfgAll.ifcFind(cmd.word(), 0);
                continue;
            }
            if (a.equals("timeout")) {
                tim = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("data")) {
                data = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("dontfrag")) {
                dntfrg = true;
                continue;
            }
            if (a.equals("alert")) {
                alrt = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("ttl")) {
                ttl = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("sgt")) {
                sgt = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("flow")) {
                flow = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("size")) {
                len = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("port")) {
                prt = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("lookup")) {
                lok = true;
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        if (tim < 1) {
            tim = 1;
        }
        addrIP src = null;
        if (ifc != null) {
            src = ifc.getLocAddr(strt);
        }
        pipe.linePut("scanning " + strt + ", src=" + src + ", vrf=" + vrf.name + ", inc=" + incr + ", num=" + numb + ", tim=" + tim + ", len=" + len + ", df=" + dntfrg + ", alrt=" + alrt);
        len -= prtIcmptun.adjustSize(strt);
        for (;;) {
            if (need2stop()) {
                break;
            }
            pipe.strPut(strt + "    ");
            byte[] buf = new byte[1];
            buf[0] = 13;
            pipe.blockingPut(buf, 0, buf.length);
            if (numb.isFilled(0)) {
                break;
            }
            strt.setAdd(strt, incr);
            addrIP adr = new addrIP();
            adr.fromString("0::1");
            numb.setSub(numb, adr);
            String a = "";
            if (lok) {
                clntDns clnt = new clntDns();
                clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(strt), false, packDnsRec.typePTR);
                a = " (" + clnt.getPTR() + ")";
            }
            if (prt > 0) {
                clntPorts trc = new clntPorts();
                trc.vrf = vrf;
                trc.ifc = ifc;
                trc.trg = strt;
                trc.tim = tim;
                if (!trc.testOne(0, prt)) {
                    pipe.linePut(strt + a + " is open.");
                }
                continue;
            }
            ipFwd fwd = vrf.getFwd(strt);
            ipFwdEcho ping = fwd.echoSendReq(src, strt, null, len, dntfrg, alrt, ttl, sgt, tos, flow, data, false);
            if (ping == null) {
                continue;
            }
            ping.notif.sleep(tim);
            if (ping.notif.totalNotifies() < 1) {
                continue;
            }
            if (ping.res.size() < 1) {
                continue;
            }
            ipFwdEchod res = ping.res.get(0);
            if (res.err == null) {
                pipe.linePut(strt + a + " is alive.");
            } else {
                pipe.linePut(strt + a + " is " + res.err + " at " + res.rtr);
            }
        }
    }

    private void doPing() {
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        addrIP via = null;
        int size = 64;
        int data = 0;
        int alrt = -1;
        int timeout = 1000;
        int repeat = 5;
        int sgt = 0;
        int tos = 0;
        int flow = 0;
        int ttl = 255;
        int proto = 0;
        int delay = 0;
        boolean detail = false;
        boolean sweep = false;
        boolean multi = false;
        boolean error = false;
        boolean dntfrg = false;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("dontfrag")) {
                dntfrg = true;
                continue;
            }
            if (a.equals("multi")) {
                multi = true;
                continue;
            }
            if (a.equals("error")) {
                error = true;
                continue;
            }
            if (a.equals("sweep")) {
                sweep = true;
                continue;
            }
            if (a.equals("data")) {
                data = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("alert")) {
                alrt = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("detail")) {
                detail = true;
                continue;
            }
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("source")) {
                ifc = cfgAll.ifcFind(cmd.word(), 0);
                continue;
            }
            if (a.equals("viahop")) {
                via = new addrIP();
                via.fromString(cmd.word());
                continue;
            }
            if (a.equals("timeout")) {
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("delay")) {
                delay = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("size")) {
                size = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("repeat")) {
                repeat = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("ttl")) {
                ttl = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("sgt")) {
                sgt = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("flow")) {
                flow = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("ipv6")) {
                proto = 6;
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        userTerminal trm = new userTerminal(pipe);
        addrIP trg = trm.resolveAddr(rem, proto);
        if (trg == null) {
            return;
        }
        addrIP src = null;
        if (ifc != null) {
            src = ifc.getLocAddr(trg);
        }
        ipFwd fwd = vrf.getFwd(trg);
        if (repeat < 1) {
            repeat = 1;
        }
        int sent = 0;
        int recv = 0;
        int lost = 0;
        int errs = 0;
        userExecStats timS = new userExecStats(0, timeout * 10);
        userExecStats ttlS = new userExecStats(0, 256);
        userExecStats tosS = new userExecStats(0, 256);
        long timBeg = bits.getTime();
        pipe.linePut("pinging " + trg + ", src=" + src + ", vrf=" + vrf.name + ", via=" + via + ", cnt=" + repeat + ", len=" + size + ", df=" + dntfrg + ", tim=" + timeout + ", gap=" + delay + ", ttl=" + ttl + ", tos=" + tos + ", sgt=" + sgt + ", flow=" + flow + ", fill=" + data + ", alrt=" + alrt + ", sweep=" + sweep + ", multi=" + multi);
        size -= prtIcmptun.adjustSize(trg);
        for (int i = 0; i < repeat; i++) {
            if (sweep) {
                size++;
            }
            if (delay > 0) {
                bits.sleep(delay);
            }
            if (need2stop()) {
                if (detail) {
                    pipe.strPut("aborted ");
                    break;
                }
                pipe.strPut("*");
                break;
            }
            sent++;
            ipFwdEcho ping = fwd.echoSendReq(src, trg, via, size, dntfrg, alrt, ttl, sgt, tos, flow, data, multi);
            if (ping == null) {
                lost++;
                if (detail) {
                    pipe.strPut("noroute ");
                    continue;
                }
                pipe.strPut("N");
                continue;
            }
            if (timeout < 1) {
                lost++;
                if (detail) {
                    pipe.strPut("miss ");
                    continue;
                }
                pipe.strPut(".");
                continue;
            }
            if (multi) {
                bits.sleep(timeout);
            } else {
                if (ping.notif.totalNotifies() < 1) {
                    ping.notif.sleep(timeout);
                }
            }
            if (ping.notif.totalNotifies() < 1) {
                lost++;
                if (detail) {
                    pipe.strPut("missed ");
                    continue;
                }
                pipe.strPut(".");
                continue;
            }
            if (ping.res.size() < 1) {
                lost++;
                if (detail) {
                    pipe.strPut("timeout ");
                    continue;
                }
                pipe.strPut(".");
                continue;
            }
            for (int o = 0; o < ping.res.size(); o++) {
                ipFwdEchod res = ping.res.get(o);
                if (res.err != null) {
                    errs++;
                    if (error) {
                        recv--;
                    }
                    if (detail) {
                        pipe.strPut(res.err + "@" + res.rtr + " ");
                        continue;
                    }
                    String a;
                    switch (res.err) {
                        case noRoute:
                            a = "R";
                            break;
                        case denied:
                            a = "D";
                            break;
                        case notInTab:
                            a = "H";
                            break;
                        case fragment:
                            a = "F";
                            break;
                        case reassembly:
                            a = "A";
                            break;
                        case ttlExceed:
                            a = "T";
                            break;
                        case badProto:
                            a = "P";
                            break;
                        case badPort:
                            a = "p";
                            break;
                        default:
                            a = "?";
                            break;
                    }
                    pipe.strPut(a);
                    continue;
                }
                recv++;
                timS.val(res.tim);
                ttlS.val(res.ttl);
                tosS.val(res.tos);
                if (detail) {
                    pipe.strPut(res.tim + "ms," + res.ttl + "ttl," + res.tos + "tos@" + res.rtr + " ");
                    continue;
                }
                pipe.strPut("!");
            }
            if (detail && multi) {
                pipe.linePut("");
            }
        }
        pipe.linePut("");
        pipe.linePut("result=" + bits.percent(recv, sent) + ", recv/sent/lost/err=" + recv + "/" + sent + "/" + lost + "/" + errs + ", took " + (bits.getTime() - timBeg) + ", min/avg/max/dev rtt=" + timS.res() + ", ttl " + ttlS.res() + ", tos " + tosS.res());
    }

    private void doListen() {
        int port = bits.str2num(cmd.word());
        int trns = servGeneric.protoTcp;
        int proto = clntDns.getPriPref();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        addrIP rem = null;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("source")) {
                ifc = cfgAll.ifcFind(cmd.word(), 0);
                continue;
            }
            if (a.equals("tcp")) {
                trns = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("udp")) {
                trns = servGeneric.protoUdp;
                continue;
            }
            if (a.equals("ludp")) {
                trns = servGeneric.protoLudp;
                continue;
            }
            if (a.equals("dccp")) {
                trns = servGeneric.protoDccp;
                continue;
            }
            if (a.equals("sctp")) {
                trns = servGeneric.protoSctp;
                continue;
            }
            if (a.equals("ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("ipv6")) {
                proto = 6;
                continue;
            }
            if (a.equals("remote")) {
                rem = new addrIP();
                rem.fromString(cmd.word());
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        prtGen prt = null;
        ipFwdIface ipi = null;
        if (proto == 4) {
            if (ifc != null) {
                ipi = ifc.fwdIf4;
            }
            switch (trns) {
                case servGeneric.protoTcp:
                    prt = vrf.tcp4;
                    break;
                case servGeneric.protoUdp:
                    prt = vrf.udp4;
                    break;
                case servGeneric.protoLudp:
                    prt = vrf.ludp4;
                    break;
                case servGeneric.protoDccp:
                    prt = vrf.dccp4;
                    break;
                case servGeneric.protoSctp:
                    prt = vrf.sctp4;
                    break;
                default:
                    break;
            }
        } else {
            if (ifc != null) {
                ipi = ifc.fwdIf6;
            }
            switch (trns) {
                case servGeneric.protoTcp:
                    prt = vrf.tcp6;
                    break;
                case servGeneric.protoUdp:
                    prt = vrf.udp6;
                    break;
                case servGeneric.protoLudp:
                    prt = vrf.ludp6;
                    break;
                case servGeneric.protoDccp:
                    prt = vrf.dccp6;
                    break;
                case servGeneric.protoSctp:
                    prt = vrf.sctp6;
                    break;
                default:
                    break;
            }
        }
        if (prt == null) {
            cmd.error("no such transport");
            return;
        }
        pipe.linePut("listening on " + port);
        prtAccept acc = new prtAccept(prt, new pipeLine(65535, false), ipi, port, rem, 0, "listen", -1, null, -1, -1);
        acc.wait4conn(60000);
        pipeSide conn = acc.getConn(true);
        if (conn == null) {
            cmd.error("timed out");
            return;
        }
        pipeTerm trm = new pipeTerm(pipe, conn, null);
        trm.doTerm();
    }

    private void doTelnet(int secur) {
        String rem = cmd.word();
        cmd = cmd.copyBytes(false);
        String a = cmd.word();
        int prt = bits.str2num(a);
        if (prt < 1) {
            cmd = cmd.copyBytes(true);
        }
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        clntProxy prx = null;
        cfgChat cht = null;
        String user = null;
        String pass = null;
        byte[] pubkey = null;
        int proto = 0;
        int dgrm = servGeneric.protoTcp;
        String recn = null;
        for (;;) {
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("record")) {
                recn = cmd.word();
                continue;
            }
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("chat")) {
                cht = cfgAll.chatFind(cmd.word(), false);
                continue;
            }
            if (a.equals("user")) {
                user = cmd.word();
                continue;
            }
            if (a.equals("pass")) {
                pass = cmd.word();
                continue;
            }
            if (a.equals("source")) {
                ifc = cfgAll.ifcFind(cmd.word(), 0);
                continue;
            }
            if (a.equals("pubkey")) {
                pubkey = encBase64.decodeBytes(cmd.word());
                continue;
            }
            if (a.equals("proxy")) {
                cfgProxy prox = cfgAll.proxyFind(cmd.word(), false);
                if (prox == null) {
                    continue;
                }
                prx = prox.proxy;
                continue;
            }
            if (a.equals("tcp")) {
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("udp")) {
                dgrm = servGeneric.protoUdp;
                continue;
            }
            if (a.equals("ludp")) {
                dgrm = servGeneric.protoLudp;
                continue;
            }
            if (a.equals("dccp")) {
                dgrm = servGeneric.protoDccp;
                continue;
            }
            if (a.equals("sctp")) {
                dgrm = servGeneric.protoSctp;
                continue;
            }
            if (a.equals("ssh")) {
                secur = servGeneric.protoSsh;
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("tls")) {
                secur = servGeneric.protoTls;
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("dtls")) {
                secur = servGeneric.protoDtls;
                dgrm = servGeneric.protoUdp;
                continue;
            }
            if (a.equals("telnet")) {
                secur = servGeneric.protoTelnet;
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("ipv6")) {
                proto = 6;
                continue;
            }
        }
        if (prt < 1) {
            prt = 23;
            if (secur == servGeneric.protoSsh) {
                prt = 22;
            }
        }
        if (!privileged) {
            recn = null;
        }
        RandomAccessFile recf = null;
        if (recn != null) {
            try {
                recf = new RandomAccessFile(new File(recn), "rw");
                recf.setLength(0);
            } catch (Exception e) {
                pipe.linePut("file open error");
            }
        }
        userTerminal term = new userTerminal(pipe);
        addrIP adr = term.resolveAddr(rem, proto);
        if (prx == null) {
            prx = clntProxy.makeTemp(vrf, ifc);
        }
        pipe.strPut("connecting to " + adr + " " + prt);
        pipeSide strm = prx.doConnect(dgrm, adr, prt, "telnet");
        if (strm == null) {
            pipe.linePut(" failed!");
            return;
        }
        pipe.linePut(" ok!");
        strm = term.startSecurity(strm, secur, pubkey, user, pass);
        if (strm == null) {
            return;
        }
        if (cht != null) {
            cht.script.doScript(strm);
        }
        pipeTerm trm = new pipeTerm(pipe, strm, recf);
        trm.doTerm();
        if (recf == null) {
            return;
        }
        try {
            recf.close();
        } catch (Exception e) {
        }
    }

    private void doAttach() {
        if (cfgAll.evalVdcPrivs()) {
            cmd.error("not in a vdc");
            return;
        }
        String a = cmd.word();
        if (a.equals("chat")) {
            userChat c = new userChat(pipe, reader);
            c.doChat();
            return;
        }
        if (a.equals("script")) {
            cfgScrpt ntry = cfgAll.scrptFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such script");
                return;
            }
            if (ntry.con != null) {
                ntry.con.setClose();
            }
            pipeLine pl = new pipeLine(65536, false);
            ntry.con = pl.getSide();
            pipeTerm trm = new pipeTerm(pipe, pl.getSide(), null);
            trm.doTerm();
            return;
        }
        if (a.equals("scheduler")) {
            cfgSched ntry = cfgAll.schedFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such scheduler");
                return;
            }
            if (ntry.con != null) {
                ntry.con.setClose();
            }
            pipeLine pl = new pipeLine(65536, false);
            ntry.con = pl.getSide();
            pipeTerm trm = new pipeTerm(pipe, pl.getSide(), null);
            trm.doTerm();
            return;
        }
        if (a.equals("vdc")) {
            cfgVdc ntry = new cfgVdc(cmd.word());
            ntry = cfgInit.vdcLst.find(ntry);
            if (ntry == null) {
                cmd.error("no such vdc");
                return;
            }
            if (ntry.con != null) {
                ntry.con.setClose();
            }
            pipeLine pl = new pipeLine(65536, false);
            ntry.con = pl.getSide();
            pipeTerm trm = new pipeTerm(pipe, pl.getSide(), null);
            trm.doTerm();
            return;
        }
        if (a.equals("process")) {
            cfgPrcss ntry = cfgAll.prcFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such process");
                return;
            }
            if (ntry.con != null) {
                ntry.con.setClose();
            }
            pipeLine pl = new pipeLine(65536, false);
            ntry.con = pl.getSide();
            pipeTerm trm = new pipeTerm(pipe, pl.getSide(), null);
            trm.doTerm();
            return;
        }
        if (a.equals("shell1")) {
            pipeShell sh = pipeShell.exec(pipe, cmd.getRemaining(), null, false, true, false, true);
            if (sh == null) {
                return;
            }
            sh.waitFor();
            int i = sh.resultNum();
            if (i != 0) {
                cmd.error("returned " + i);
            }
            return;
        }
        if (a.equals("shell2")) {
            reader.putStrArr(pipeShell.exec(cmd.getRemaining(), null, true, false, true));
            return;
        }
        if (a.equals("shell3")) {
            pipeShell sh = pipeShell.exec(pipe, cmd.getRemaining(), null, false, true, true, true);
            if (sh == null) {
                return;
            }
            sh.waitFor();
            int i = sh.resultNum();
            if (i != 0) {
                cmd.error("returned " + i);
            }
            return;
        }
        if (a.equals("line")) {
            cfgLin lin = cfgAll.linFind(cmd.word());
            if (lin == null) {
                cmd.error("no such line");
                return;
            }
            pipeTerm trm = new pipeTerm(pipe, lin.runner.doAttach(), null);
            trm.doTerm();
            return;
        }
        cmd.badCmd();
    }

    private void doLookup() {
        String a = cmd.word();
        int i = -1;
        boolean recur = false;
        if (a.startsWith("recur-")) {
            a = a.substring(6, a.length());
            recur = true;
        }
        if (a.equals("ipv4")) {
            i = packDnsRec.typeA;
        }
        if (a.equals("ipv6")) {
            i = packDnsRec.typeAAAA;
        }
        if (a.equals("mail")) {
            i = packDnsRec.typeMX;
        }
        if (a.equals("dns")) {
            i = packDnsRec.typeNS;
        }
        if (a.equals("soa")) {
            i = packDnsRec.typeSOA;
        }
        if (a.equals("srv")) {
            i = packDnsRec.typeSRV;
        }
        if (a.equals("txt")) {
            i = packDnsRec.typeTXT;
        }
        if (a.equals("zone")) {
            i = packDnsRec.typeAXFR;
        }
        if (a.equals("reverse")) {
            i = packDnsRec.typePTR;
        }
        if (i < 0) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        addrIP srv = new addrIP();
        List<addrIP> srvs = new ArrayList<addrIP>();
        srvs.add(srv);
        if (srv.fromString(cmd.word())) {
            srvs = cfgAll.nameServerAddr;
        }
        addrIP adr = new addrIP();
        if (!adr.fromString(a)) {
            a = packDnsRec.generateReverse(adr);
        }
        pipe.linePut("resolving " + packDnsRec.type2str(i) + " " + a);
        clntDns clnt = new clntDns();
        if (i == packDnsRec.typeAXFR) {
            packDnsZone res = clnt.doZoneXfer(srv, new packDnsZone(a), true);
            if (res == null) {
                cmd.error("no reply");
                return;
            }
            reader.putStrTab(res.toUserStr(false));
            return;
        }
        if (recur) {
            packDnsZone res = clnt.doRecursive(srvs, a, i);
            if (res == null) {
                cmd.error("no reply");
                return;
            }
            reader.putStrTab(res.toUserStr(false));
            return;
        }
        clnt.doResolvList(srvs, a, true, i);
        packDnsRec res = clnt.findAnswer(i);
        if (res == null) {
            cmd.error("no reply");
            return;
        }
        reader.putStrArr(res.toUserStr(" ", "", false));
    }

    private void doTclsh() {
        if (cmd.size() > 0) {
            List<String> l = bits.txt2buf(cmd.getRemaining());
            if (l == null) {
                cmd.error("no such file");
                return;
            }
            userScript s = new userScript(pipe, "");
            s.allowExec = true;
            s.allowConfig = privileged;
            s.addLines(l);
            s.cmdAll();
            return;
        }
        userScript s = new userScript(pipe, "");
        s.allowExec = true;
        s.allowConfig = privileged;
        for (;;) {
            if (pipe.isClosed() != 0) {
                return;
            }
            pipe.strPut("tcl>");
            String a = pipe.lineGet(0x32);
            if (a.equals("exit")) {
                return;
            }
            s.addLine(a);
            if (!s.ready2run()) {
                continue;
            }
            a = s.cmdAll();
            pipe.linePut("tcl:" + a);
        }
    }

    private void setBackground(int col) {
        int[] idx = {pipeSetting.colNormal, pipeSetting.colHeader, pipeSetting.colPrompt};
        int[] def = {userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrGreen};
        for (int i = 0; i < idx.length; i++) {
            int p = idx[i];
            int o = pipe.settingsGet(p, def[i]);
            o = userScreen.setBackground(o, col);
            pipe.settingsPut(p, o);
        }
    }

    private void setForeground(int idx, int def, int col) {
        int i = pipe.settingsGet(idx, def);
        i = userScreen.setForeground(i, col);
        pipe.settingsPut(idx, i);
    }

    private void setForeground(int idx, int def) {
        int i = pipe.settingsGet(idx, def);
        i = userScreen.setForeground(i, def);
        pipe.settingsPut(idx, i);
    }

    private void doTerminal() {
        String a = cmd.word();
        if (a.equals("detect")) {
            cmd.error(cmds.doneFail(userScreen.updtSiz(pipe)));
            return;
        }
        if (a.equals("beep")) {
            userScreen.sendBeep(pipe);
            return;
        }
        if (a.equals("title")) {
            userScreen.sendTit(pipe, cfgAll.hostName);
            return;
        }
        if (a.equals("clipboard")) {
            userScreen.sendClp(pipe, cmd.getRemaining());
            return;
        }
        if (a.equals("monitor")) {
            logger.pipeStart(pipe);
            return;
        }
        if (a.equals("width")) {
            userReader.setTermWdt(pipe, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("timestamps")) {
            pipe.settingsPut(pipeSetting.times, true);
            return;
        }
        if (a.equals("stars")) {
            pipe.settingsPut(pipeSetting.passStar, true);
            return;
        }
        if (a.equals("foreground")) {
            int i = userScreen.string2color(cmd.word());
            if (i < 0) {
                return;
            }
            setForeground(pipeSetting.colNormal, userScreen.colWhite, i);
            return;
        }
        if (a.equals("header")) {
            int i = userScreen.string2color(cmd.word());
            if (i < 0) {
                return;
            }
            setForeground(pipeSetting.colHeader, userScreen.colBrYellow, i);
            return;
        }
        if (a.equals("prompt")) {
            int i = userScreen.string2color(cmd.word());
            if (i < 0) {
                return;
            }
            setForeground(pipeSetting.colPrompt, userScreen.colBrGreen, i);
            return;
        }
        if (a.equals("background")) {
            int i = userScreen.string2color(cmd.word());
            if (i < 0) {
                return;
            }
            setBackground(i);
            return;
        }
        if (a.equals("colorize")) {
            pipe.settingsPut(pipeSetting.colors, userFormat.str2colmod(cmd.word()));
            return;
        }
        if (a.equals("spacetab")) {
            pipe.settingsPut(pipeSetting.spacTab, true);
            return;
        }
        if (a.equals("capslock")) {
            pipe.settingsPut(pipeSetting.capsLock, true);
            return;
        }
        if (a.equals("play")) {
            userScreen.sendMusicAnsi(pipe, cmd.getRemaining());
            return;
        }
        if (a.equals("bells")) {
            pipe.settingsPut(pipeSetting.termBells, true);
            return;
        }
        if (a.equals("ansimode")) {
            a = cmd.word();
            userScreen.ansiMode am = userScreen.string2mode(a);
            pipe.settingsPut(pipeSetting.ansiMode, am);
            return;
        }
        if (a.equals("histroy")) {
            reader.setHistory(bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("length")) {
            userReader.setTermLen(pipe, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("escape")) {
            pipe.settingsPut(pipeSetting.escape, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("riblines")) {
            userReader.setRibLin(pipe, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("deactivate")) {
            pipe.settingsPut(pipeSetting.deactive, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("tablemode")) {
            a = cmd.word();
            if (a.length() < 1) {
                pipe.settingsPut(pipeSetting.tabMod, userFormat.tableMode.fancy);
                return;
            }
            pipe.settingsPut(pipeSetting.tabMod, userFormat.str2tabmod(a));
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("monitor")) {
            logger.pipeStop(pipe);
            return;
        }
        if (a.equals("width")) {
            userReader.setTermWdt(pipe, 80);
            return;
        }
        if (a.equals("timestamps")) {
            pipe.settingsPut(pipeSetting.times, false);
            return;
        }
        if (a.equals("stars")) {
            pipe.settingsPut(pipeSetting.passStar, false);
            return;
        }
        if (a.equals("foreground")) {
            setForeground(pipeSetting.colNormal, userScreen.colWhite);
            return;
        }
        if (a.equals("header")) {
            setForeground(pipeSetting.colHeader, userScreen.colBrYellow);
            return;
        }
        if (a.equals("prompt")) {
            setForeground(pipeSetting.colPrompt, userScreen.colBrGreen);
            return;
        }
        if (a.equals("background")) {
            setBackground(userScreen.colBlack);
            return;
        }
        if (a.equals("colorize")) {
            pipe.settingsPut(pipeSetting.colors, userFormat.colorMode.normal);
            return;
        }
        if (a.equals("spacetab")) {
            pipe.settingsPut(pipeSetting.spacTab, false);
            return;
        }
        if (a.equals("capslock")) {
            pipe.settingsPut(pipeSetting.capsLock, false);
            return;
        }
        if (a.equals("bells")) {
            pipe.settingsPut(pipeSetting.termBells, false);
            return;
        }
        if (a.equals("length")) {
            userReader.setTermLen(pipe, 25);
            return;
        }
        if (a.equals("riblines")) {
            userReader.setRibLin(pipe, 8192);
            return;
        }
        if (a.equals("ansimode")) {
            pipe.settingsPut(pipeSetting.ansiMode, userScreen.ansiMode.normal);
            return;
        }
        if (a.equals("tablemode")) {
            pipe.settingsPut(pipeSetting.tabMod, userFormat.tableMode.normal);
            return;
        }
        cmd.badCmd();
    }

    private void doXml(boolean xml) {
        boolean frm = false;
        boolean ech = false;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("format")) {
                frm = true;
                continue;
            }
            if (a.equals("echo")) {
                ech = true;
                continue;
            }
        }
        if (xml) {
            new userXml(pipe, privileged, frm, ech).doWork();
        } else {
            new userNetconf(pipe, privileged, frm, ech).doServer();
        }
    }

    private void doPpp() {
        if (framedIface == null) {
            cmd.error("not allowed on this line");
            return;
        }
        if (physicalLin) {
            lineHdlc h = new lineHdlc(pipe);
            framedIface.setLowerHandler(h);
            h.wait4working();
            framedIface.setLowerHandler(new ifcNull());
        } else {
            lineHdlc h = new lineHdlc(pipe);
            cfgIfc i = framedIface.cloneStart(h);
            h.wait4working();
            i.cloneStop();
        }
    }

    private void doReload(boolean mod, long at) {
        if (at >= 0) {
            if (at <= bits.getTime()) {
                cmd.error("bad time");
                return;
            }
        }
        if (cfgAll.reload != null) {
            cfgAll.reload.stopWorking();
            cfgAll.reload = null;
        }
        if (at <= 0) {
            return;
        }
        cfgAll.reload = new userReload(mod, at);
    }

    /**
     * get a show from a pipeline
     *
     * @param col use colors
     * @return converted pipe
     */
    public final pipeSide getShPipe(boolean col) {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, null);
        userReader.setTermWdt(pip, pipe.settingsGet(pipeSetting.width, 80));
        userReader.setTermLen(pip, 0);
        pip.settingsPut(pipeSetting.tabMod, pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal));
        pip.settingsPut(pipeSetting.times, pipe.settingsGet(pipeSetting.times, false));
        if (col) {
            pip.settingsPut(pipeSetting.colors, pipe.settingsGet(pipeSetting.colors, userFormat.colorMode.normal));
        } else {
            pip.settingsPut(pipeSetting.colors, userFormat.colorMode.normal);
        }
        userExec exe = new userExec(pip, rdr);
        exe.privileged = privileged;
        pip.setTime(60000);
        String a = "show " + cmd.getRemaining();
        a = exe.repairCommand(a);
        exe.executeCommand(a);
        pip = pl.getSide();
        pl.setClose();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRtryLF;
        return pip;
    }

    private void doView() {
        List<String> lst = new ArrayList<String>();
        packText pt = new packText(getShPipe(false));
        pt.recvAll(lst);
        userEditor edtr = new userEditor(new userScreen(pipe), lst, cfgAll.hostName + "#show " + cmd.getRemaining(), false);
        edtr.doView();
    }

    private void doWatch() {
        reader.keyFlush();
        boolean color = pipe.settingsGet(pipeSetting.colors, userFormat.colorMode.normal) != userFormat.colorMode.normal;
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            String a = getShPipe(true).strGet(1024 * 1024);
            userScreen.sendCur(pipe, 0, 0);
            userScreen.sendCls(pipe);
            if (color) {
                userScreen.sendAnsCol(pipe, pipe.settingsGet(pipeSetting.colPrompt, userScreen.colBrGreen));
            }
            pipe.linePut(cfgAll.hostName + "#show " + cmd.getRemaining());
            if (color) {
                userScreen.sendAnsCol(pipe, pipe.settingsGet(pipeSetting.colNormal, userScreen.colWhite));
            }
            if (pipe.settingsGet(pipeSetting.times, false)) {
                pipe.linePut(logger.getTimestamp());
            }
            pipe.strPut(a);
            bits.sleep(1000);
            if (pipe.ready2rx() > 0) {
                break;
            }
        }
        reader.keyFlush();
    }

    private void doDisplay() {
        reader.keyFlush();
        List<String> lst = new ArrayList<String>();
        userEditor edtr = new userEditor(new userScreen(pipe), lst, cfgAll.hostName + "#watch " + cmd.getRemaining(), pipe.settingsGet(pipeSetting.times, false));
        for (;;) {
            lst.clear();
            packText pt = new packText(getShPipe(false));
            pt.recvAll(lst);
            if (edtr.doTimed(1000, false)) {
                break;
            }
        }
        edtr.doClear();
        reader.keyFlush();
    }

    private void doDiffers() {
        List<String> r1 = new packText(getShPipe(false)).recvAll();
        reader.keyFlush();
        List<String> lst = new ArrayList<String>();
        userEditor edtr = new userEditor(new userScreen(pipe), lst, cfgAll.hostName + "#watch " + cmd.getRemaining(), pipe.settingsGet(pipeSetting.times, false));
        for (;;) {
            List<String> r2 = new packText(getShPipe(false)).recvAll();
            differ df = new differ();
            df.calc(r1, r2);
            lst.clear();
            lst.addAll(df.getText(pipe.settingsGet(pipeSetting.width, 80), edtr.getOfs()));
            if (edtr.doTimed(1000, true)) {
                break;
            }
        }
        edtr.doClear();
        reader.keyFlush();
    }

    private void doCfgBackup() {
        String a = cfgInit.getBackupCfgName();
        if (a == null) {
            return;
        }
        cmd.error("backing up configuration");
        List<String> old = bits.txt2buf(cfgInit.cfgFileSw);
        if (old == null) {
            cmd.error("error reading file");
            return;
        }
        boolean b = bits.buf2txt(true, old, a);
        cmd.error(cmds.doneFail(b));
    }

}

class userExecStats {

    private long min;

    private long max;

    private long seq;

    private float ak;

    private float qk;

    public userExecStats(int l, int h) {
        min = h;
        max = l;
        seq = 0;
        ak = 0;
        qk = 0;
    }

    public void val(int v) {
        seq++;
        if (v < min) {
            min = v;
        }
        if (v > max) {
            max = v;
        }
        float x = v;
        float ak1 = ak;
        float qk1 = qk;
        ak = ak1 + ((x - ak1) / seq);
        qk = qk1 + ((x - ak1) * (x - ak));
    }

    public String res() {
        return min + "/" + bits.toPrecise(ak) + "/" + max + "/" + bits.toPrecise(qk / seq);
    }

}
