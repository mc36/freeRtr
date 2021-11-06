package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authLocal;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAlias;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgChat;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgLin;
import net.freertr.cfg.cfgMenu;
import net.freertr.cfg.cfgPrcss;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgSched;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgVdc;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntDns;
import net.freertr.clnt.clntPorts;
import net.freertr.clnt.clntProxy;
import net.freertr.clnt.clntTrace;
import net.freertr.clnt.clntWhois;
import net.freertr.cry.cryBase64;
import net.freertr.ifc.ifcNull;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdEcho;
import net.freertr.ip.ipFwdEchod;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipIcmp4;
import net.freertr.ip.ipIcmp6;
import net.freertr.ip.ipRtr;
import net.freertr.line.lineHdlc;
import net.freertr.pack.packDnsRec;
import net.freertr.pack.packDnsZone;
import net.freertr.pack.packText;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeShell;
import net.freertr.pipe.pipeSide;
import net.freertr.pipe.pipeTerm;
import net.freertr.prt.prtAccept;
import net.freertr.prt.prtGen;
import net.freertr.prt.prtRedun;
import net.freertr.rtr.rtrBgpParam;
import net.freertr.serv.servGeneric;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.differ;
import net.freertr.util.logger;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

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
     * reader of user
     */
    protected final userReader reader;

    private cmds cmd; // currently processed string

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

    private void getHelpClearIpX(userHelping hl) {
        hl.add("3 4        route                 routing table");
        hl.add("4 .          <name>              vrf name");
        hl.add("3 4        nat                   address translation table");
        hl.add("4 .          <name>              vrf name");
        hl.add("3 4        babel                 babel routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        bfd                   bidirectional forwarding detection");
        hl.add("4 5          <name>              vrf name");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        bgp                   border gateway protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            peer              select address");
        hl.add("6 7              <addr>          neighbor address regexp");
        hl.add("7 .                hard          flap session");
        hl.add("7 8                in            send route refresh");
        hl.add("7 8                out           resend prefixes");
        rtrBgpParam.getAfiList(hl, "8 .", "clear", false);
        hl.add("5 6            asn               select asn");
        hl.add("6 7              <num>           neighbor asn regexp");
        hl.add("7 .                hard          flap session");
        hl.add("7 8                in            send route refresh");
        hl.add("7 8                out           resend prefixes");
        rtrBgpParam.getAfiList(hl, "8 .", "clear", false);
        hl.add("5 6            ibgp              select ibgp peers");
        hl.add("6 .                hard          flap session");
        hl.add("6 7                in            send route refresh");
        hl.add("6 7                out           resend prefixes");
        rtrBgpParam.getAfiList(hl, "7 .", "clear", false);
        hl.add("5 6            ebgp              select ebgp peers");
        hl.add("6 .                hard          flap session");
        hl.add("6 7                in            send route refresh");
        hl.add("6 7                out           resend prefixes");
        rtrBgpParam.getAfiList(hl, "7 .", "clear", false);
        hl.add("5 6            all               select every peer");
        hl.add("6 .                hard          flap session");
        hl.add("6 7                in            send route refresh");
        hl.add("6 7                out           resend prefixes");
        rtrBgpParam.getAfiList(hl, "7 .", "clear", false);
        hl.add("5 .            recompute         trigger full compute round");
        hl.add("5 .            flaps             collected flaps");
        hl.add("3 4        eigrp                 enhanced interior gateway routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        isis                  intermediate system to intermediate system");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            <addr>            neighbor address");
        hl.add("6 .              <num>           level");
        hl.add("3 4        ldp                   label distribution protocol");
        hl.add("4 5          <name>              vrf name");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        lsrp                  link state routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        msdp                  multicast source discovery protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        olsr                  optimized link state routing");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        ospf                  open shortest path first");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            <num>             area id");
        hl.add("6 .              <addr>          neighbor address");
        hl.add("3 4        pvrp                  path vector routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        rip                   routing information protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            <addr>            neighbor address");
        hl.add("3 4        logger                route flap collector");
        hl.add("4 .          <num>               process id");
    }

    private static void getHelpShowIpX(userHelping hl) {
        hl.add("3 4,.      interface             interface information");
        hl.add("4 .          [name]              name of interface");
        hl.add("3 4,.      vrf                   vrf information");
        hl.add("4 5          [name]              name of vrf");
        hl.add("5 .            full              full listing");
        hl.add("5 .            interface         interface list");
        hl.add("5 .            rates             traffic rates");
        hl.add("5 .            realtime          realtime counters");
        hl.add("5 .            history           historic byte counters");
        hl.add("5 .            rxhistory         historic rx byte counters");
        hl.add("5 .            txhistory         historic tx byte counters");
        hl.add("5 .            drhistory         historic drop byte counters");
        hl.add("5 .            phistory          historic packet counters");
        hl.add("5 .            rxphistory        historic rx packet counters");
        hl.add("5 .            txphistory        historic tx packet counters");
        hl.add("5 .            drphistory        historic drop packet counters");
        hl.add("5 .            numhist           numeric historic byte counters");
        hl.add("5 .            numphist          numeric historic packet counters");
        hl.add("3 4        counter               unicast routing table traffic");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        ecmp                  unicast routing table ecmp");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        labels                unicast routing table labels");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        distribution          routing interfaces");
        hl.add("4 .          <name>              vrf name");
        hl.add("3 4        route                 unicast routing table entries");
        hl.add("4 5,.        <vrf>               name of routing table");
        hl.add("5 .            [addr]            prefix to view");
        hl.add("3 4        rpf                   multicast routing table entries");
        hl.add("4 5,.        <vrf>               name of routing table");
        hl.add("5 .            [addr]            prefix to view");
        hl.add("3 4        flwspc                flowspec routing table entries");
        hl.add("4 5,.        <vrf>               name of routing table");
        hl.add("5 6,.          [addr]            prefix to view");
        hl.add("6 .              [rd]            route distinguisher");
        hl.add("3 4        protocol              routing protocol summary");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        mroute                multicast forwarding table entries");
        hl.add("4 5,.        <vrf>               name of routing table");
        hl.add("5 6            [addr]            source");
        hl.add("6 .              <addr>          group");
        hl.add("3 4        segrout               segment routing forwarding table entries");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        srindex               segment routing index table entries");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        bier                  bier forwarding table entries");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        inspect               upper layer inspection");
        hl.add("4 .          <name>              name of interface");
        hl.add("3 4        toptalk               top talker list");
        hl.add("4 .          <name>              name of interface");
        hl.add("3 4        flow                  netflow table entries");
        hl.add("4 5          <vrf>               name of routing table");
        hl.add("5 .            session           list of sessions");
        hl.add("5 .            toptalk           top talker list");
        hl.add("3 4        pbr                   pbr table entries");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        nat                   nat table entries");
        hl.add("4 5          <vrf>               name of routing table");
        hl.add("5 .            statistics        list of configuration entries");
        hl.add("5 .            translations      list of translation entries");
        hl.add("3 4        sockets               socket table entries");
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        bfd                   bidirectional forwarding detection protocol");
        hl.add("4 5          <vrf>               specify routing table");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("3 4        hsrp                  hot standby router protocol");
        hl.add("4 5          <vrf>               specify routing table");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("3 4        vrrp                  virtual router redundancy protocol");
        hl.add("4 5          <vrf>               specify routing table");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("3 4        pim                   protocol independent multicast");
        hl.add("4 5          <vrf>               specify routing table");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("5 .            interface         list of interfaces");
        hl.add("3 4        msdp                  multicast source discovert protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("5 .            database          list of sources");
        hl.add("3 4        rsvp                  resource reservation protocol");
        hl.add("4 5          <vrf>               specify routing table");
        hl.add("5 .            summary           list of tunnels in database");
        hl.add("5 6            detail            display a specific tunnel");
        hl.add("6 7              <addr>          source address");
        hl.add("7 8                <num>         source id");
        hl.add("8 9                  <addr>      subgroup address");
        hl.add("9 .                    <num>     subgroup id");
        hl.add("3 4        ldp                   label distribution protocol");
        hl.add("4 5          <vrf>               specify routing table");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 8,.              nulled        null labels learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("7 8,.              learned       labels learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("7 8,.              advertised    labels advertised to neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("7 .                l2learned     pseudowires learned from neighbor");
        hl.add("7 .                l2advertised  pseudowires advertised to neighbor");
        hl.add("7 .                l2needed      pseudowires needed to neighbor");
        hl.add("7 .                mplearned     multipoints learned from neighbor");
        hl.add("7 .                mpadvertised  multipoints advertised to neighbor");
        hl.add("7 .                status        status of neighbor");
        hl.add("5 .            summary           list of neighbors");
        hl.add("5 .            nulled-summary    list of neighbors null label counts");
        hl.add("5 6,.          database          list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 .            mpdatabase        list of multipoint sessions");
        hl.add("3 4        babel                 babel routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 8,.              learned       routes learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("5 .            summary           list of neighbors");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6,.          database          list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        olsr                  optimized link state routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 8,.              learned       routes learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("5 .            summary           list of neighbors");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6,.          database          list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        rip                   routing information protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 8,.              learned       routes learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("5 .            summary           list of neighbors");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6,.          database          list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        pvrp                  path vector routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 8,.              learned       routes learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("7 8,.              adverted      routes advertised to neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("5 .            summary           list of neighbors");
        hl.add("5 .            metric            list of metrics");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6,.          route             list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        lsrp                  link state routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("5 .            metric            list of metrics");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 .            software          list of software");
        hl.add("5 .            middleware        list of middleware");
        hl.add("5 .            kernel            list of kernel");
        hl.add("5 6            zone-rev          list of reverse zone");
        hl.add("6 7              <name>          name of zone");
        hl.add("7 8,.              <str>         separator");
        hl.add("8 9                  <str>       replace from");
        hl.add("9 8,.                  <str>     replace to");
        hl.add("5 6            zone-fwd          list of forward zone");
        hl.add("6 7              <name>          name of zone");
        hl.add("7 8,.              <str>         separator");
        hl.add("8 9                  <str>       replace from");
        hl.add("9 8,.                  <str>     replace to");
        hl.add("5 .            uptime            list of uptime");
        hl.add("5 6,.          database          list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 .            spf               information about last spf");
        hl.add("5 .            hostnames         hostnames from database");
        hl.add("5 .            tree              tree about last spf");
        hl.add("5 6            othertree         tree of other node");
        hl.add("6 .              <addr>          other node to view");
        hl.add("5 6            othertopology     topology of other node");
        hl.add("6 7,.            <addr>          other node to view");
        hl.add("7 .                [addr]        node to view");
        hl.add("5 6,.          graph             graph about last spf");
        hl.add("6 6,.            nocli           skip cli commands");
        hl.add("6 6,.            nonets          skip connected networks");
        hl.add("6 6,.            noints          skip connected interfaces");
        hl.add("5 6,.          nhinconsistent    inconsistent advertisements of next hops");
        hl.add("6 .              [str]           int matcher");
        hl.add("5 6,.          topology          topology about last spf");
        hl.add("6 .              [addr]          node to view");
        hl.add("5 6,.          route             list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        eigrp                 enhanced interior gateway routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 8,.              learned       routes learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("7 8,.              adverted      routes advertised to neighbor");
        hl.add("8 .                  [addr]      prefix to view");
        hl.add("5 .            summary           list of neighbors");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6,.          route             list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        ospf                  open shortest path first protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6            database          list of lsas in area");
        hl.add("6 7,.            <num>           area number");
        hl.add("7 8                [addr]        entry to view");
        hl.add("8 .                  [addr]      entry to view");
        hl.add("5 6            spf               information about last spf");
        hl.add("6 .              <num>           area number");
        hl.add("5 6            hostnames         hostnames from database");
        hl.add("6 .              <num>           area number");
        hl.add("5 6            tree              tree about last spf");
        hl.add("6 .              <num>           area number");
        hl.add("5 6            othertree         tree of other node");
        hl.add("6 7              <num>           area number");
        hl.add("7 .                <addr>        other node to view");
        hl.add("5 6            othertopology     topology of other node");
        hl.add("6 7              <num>           area number");
        hl.add("7 8,.              <addr>        other node to view");
        hl.add("8 .                  [addr]      node to view");
        hl.add("5 6            graph             graph about last spf");
        hl.add("6 7,.            <num>           area number");
        hl.add("7 7,.              nocli         skip cli commands");
        hl.add("7 7,.              nonets        skip connected networks");
        hl.add("7 7,.              noints        skip connected interfaces");
        hl.add("5 6            nhinconsistent    inconsistent advertisements of next hops");
        hl.add("6 7,.            <num>           area number");
        hl.add("7 .                [str]         int matcher");
        hl.add("5 6            topology          topology about last spf");
        hl.add("6 7,.            <num>           area number");
        hl.add("7 .                [addr]        node to view");
        hl.add("5 6            route             list of routes in area");
        hl.add("6 7,.            <num>           area number");
        hl.add("7 .                [addr]        prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        isis                  intermediate system intermediate system protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            neighbor          list of neighbors");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6            database          list of lsas in area");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 .                [addr]        entry to view");
        hl.add("5 6            spf               information about last spf");
        hl.add("6 .              <num>           level number");
        hl.add("5 6            hostnames         hostnames from database");
        hl.add("6 .              <num>           level number");
        hl.add("5 6            tree              tree about last spf");
        hl.add("6 .              <num>           level number");
        hl.add("5 6            othertree         tree of other node");
        hl.add("6 7              <num>           level number");
        hl.add("7 .                <addr>        other node to view");
        hl.add("5 6            othertopology     topology of other node");
        hl.add("6 7              <num>           level number");
        hl.add("7 8,.              <addr>        other node to view");
        hl.add("8 .                  [addr]      node to view");
        hl.add("5 6            graph             graph about last spf");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 7,.              nocli         skip cli commands");
        hl.add("7 7,.              nonets        skip connected networks");
        hl.add("7 7,.              noints        skip connected interfaces");
        hl.add("5 6            nhinconsistent    inconsistent advertisements of next hops");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 .                [str]         int matcher");
        hl.add("5 6            topology          topology about last spf");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 .                [addr]        node to view");
        hl.add("5 6            route             list of routes in area");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 .                [addr]        prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6            other-route       list of other routes in area");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 .                [addr]        prefix to view");
        hl.add("5 6,.          other-originate   list of other routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        bgp                   border gateway protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6,.          group             list of groups");
        hl.add("6 7              <num>           group number");
        hl.add("7 .                config        peer configuration");
        hl.add("7 .                status        peer status");
        rtrBgpParam.getAfiList(hl, "7 8,.", "show", false);
        hl.add("8 9,.                [addr]      prefix to view");
        hl.add("9 .                    [rd]      route distinguisher");
        hl.add("5 .            nexthop           list of nexthops");
        hl.add("5 .            bestpath          best path statistics");
        hl.add("5 .            graceful-restart  list of graceful restart");
        hl.add("5 .            multiple-labels   list of multiple labels");
        hl.add("5 .            afi               list of address families");
        hl.add("5 .            resolve           list of domain names");
        hl.add("5 .            additional-path   list of additional path");
        hl.add("5 .            router-id         list of router id");
        hl.add("5 .            buffer            list of buffer");
        hl.add("5 .            description       list of description");
        hl.add("5 .            hostname          list of hostname");
        hl.add("5 .            compression       list of compression");
        hl.add("5 .            connection        list of connection");
        hl.add("5 .            rpkisum           list of servers");
        hl.add("5 .            rpkitab           list of prefixes");
        hl.add("5 .            summary           list of neighbors");
        hl.add("5 6            template          information about template");
        hl.add("6 7              <nam>           template name");
        hl.add("7 .                config        peer configuration");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 .                config        peer configuration");
        hl.add("7 .                status        peer status");
        rtrBgpParam.getAfiList(hl, "7 8", "show", false);
        hl.add("8 9,.              learned       routes learned from neighbor");
        hl.add("9 10,.               [addr]      prefix to view");
        hl.add("10 .                   [rd]      route distinguisher");
        hl.add("8 9,.              accepted      routes accepted from neighbor");
        hl.add("9 10,.               [addr]      prefix to view");
        hl.add("10 .                   [rd]      route distinguisher");
        hl.add("8 9,.              willing       routes will advertised to neighbor");
        hl.add("9 10,.               [addr]      prefix to view");
        hl.add("10 .                   [rd]      route distinguisher");
        hl.add("8 9,.              advertised    routes advertised to neighbor");
        hl.add("9 10,.               [addr]      prefix to view");
        hl.add("10 .                   [rd]      route distinguisher");
        rtrBgpParam.getAfiList(hl, "5 6", "show", false);
        hl.add("6 .              summary         list of neighbors");
        hl.add("6 7              route-map       list of routes in database");
        hl.add("7 .                <name>        name of list");
        hl.add("6 7              route-policy    list of routes in database");
        hl.add("7 .                <name>        name of list");
        hl.add("6 7              prefix-list     list of routes in database");
        hl.add("7 .                <name>        name of list");
        hl.add("6 7              measure-map     measure filtering by route map");
        hl.add("7 .                <name>        name of list");
        hl.add("6 7              measure-policy  measure filtering by route policy");
        hl.add("7 .                <name>        name of list");
        hl.add("6 7              measure-list    measure filtering by prefix list");
        hl.add("7 .                <name>        name of list");
        hl.add("6 7,.            database        list of routes in database");
        hl.add("7 8,.              [addr]        prefix to view");
        hl.add("8 .                  [rd]        route distinguisher");
        hl.add("6 .              privateas       list of routes with private asn");
        hl.add("6 .              nostdcomm       list of routes without community");
        hl.add("6 7              stdcomm         list of routes with community");
        hl.add("7 8,.              <str>         community");
        hl.add("8 8,.                [str]       community");
        hl.add("6 .              noextcomm       list of routes without community");
        hl.add("6 7              extcomm         list of routes with community");
        hl.add("7 8,.              <str>         community");
        hl.add("8 8,.                [str]       community");
        hl.add("6 .              nolrgcomm       list of routes without community");
        hl.add("6 7              lrgcomm         list of routes with community");
        hl.add("7 8,.              <str>         community");
        hl.add("8 8,.                [str]       community");
        hl.add("6 7              regexp          list of routes with as path");
        hl.add("7 8,.              <str>         as path regular expression");
        hl.add("8 8,.                [str]       as path regular expression");
        hl.add("6 7              distance        list of routes with distance");
        hl.add("7 .                <str>         distance matcher");
        hl.add("6 7              pathlen         list of routes with path length");
        hl.add("7 .                <str>         distance matcher");
        hl.add("6 7              locpref         list of routes with locpref");
        hl.add("7 .                <str>         locpref matcher");
        hl.add("6 7              validity        list of routes with validity");
        hl.add("7 .                <str>         validity matcher");
        hl.add("6 7              aigp            list of routes with aigp");
        hl.add("7 .                <str>         aigp matcher");
        hl.add("6 7              bandwidth       list of routes with bandwidth");
        hl.add("7 .                <str>         bandwidth matcher");
        hl.add("6 7              origin          list of routes with origin");
        hl.add("7 .                <str>         origin matcher");
        hl.add("6 7              metric          list of routes with metric");
        hl.add("7 .                <str>         metric matcher");
        hl.add("6 7              tag             list of routes with tag");
        hl.add("7 .                <str>         tag matcher");
        hl.add("6 7              rd              list of routes with rd");
        hl.add("7 .                <str>         tag matcher");
        hl.add("6 7              nexthop         list of routes with nexthop");
        hl.add("7 .                <str>         address matcher");
        hl.add("6 7              network         list of routes from network");
        hl.add("7 7,.              <str>         prefix matcher");
        hl.add("6 7,.            flapstat        prefix flap statistics");
        hl.add("7 .                [num]         minimum flap count");
        hl.add("6 7              flappath        flap statistics for prefix");
        hl.add("7 8,.              <addr>        prefix to view");
        hl.add("8 .                  [rd]        route distinguisher");
        hl.add("6 7              flaprevpath     reversed aspath flap statistics for prefix");
        hl.add("7 8,.              <addr>        prefix to view");
        hl.add("8 .                  [rd]        route distinguisher");
        hl.add("6 .              labels          remote and local labels");
        hl.add("6 .              ecmp            ecmp info");
        hl.add("6 .              asgraph         connectivity graph of ases");
        hl.add("6 .              asorigin        originating ases");
        hl.add("6 .              astransit       transiting ases");
        hl.add("6 .              asconn          connectivity of ases");
        hl.add("6 7,.            asinconsistent  inconsistent advertisements of ases");
        hl.add("7 .                [str]         int matcher");
        hl.add("6 7,.            nhinconsistent  inconsistent advertisements of next hops");
        hl.add("7 .                [str]         int matcher");
        hl.add("6 .              prefix-lengths  prefix length distribution");
        hl.add("6 7              allroute        list of specific routes");
        hl.add("7 8,.              <addr>        prefix to view");
        hl.add("8 .                  [rd]        route distinguisher");
        hl.add("6 7              differ          compare route from neighbors");
        hl.add("7 8                <addr>        neighbor address");
        hl.add("8 9                  <addr>      neighbor address");
        hl.add("9 10,.                 <addr>    prefix to view");
        hl.add("10 .                     [rd]    route distinguisher");
        hl.add("6 7              compare         compare routes from neighbors");
        hl.add("6 7              dcompare        double compare routes from neighbors");
        hl.add("7 8                <addr>        neighbor address");
        hl.add("8 9,.                <addr>      neighbor address");
        hl.add("9 9,.                  bier      ignore bier");
        hl.add("9 9,.                  attrset   ignore attribute set");
        hl.add("9 9,.                  cluster   ignore cluster list");
        hl.add("9 9,.                  nexthop   ignore nexthop");
        hl.add("9 9,.                  origin    ignore origin");
        hl.add("9 9,.                  metric    ignore metric");
        hl.add("9 9,.                  locpref   ignore local preference");
        hl.add("9 9,.                  distance  ignore distance");
        hl.add("9 9,.                  tag       ignore tag");
        hl.add("9 9,.                  validity  ignore validity");
        hl.add("9 9,.                  aspath    ignore as path");
        hl.add("9 9,.                  asconf    ignore confed path");
        hl.add("9 9,.                  stdcomm   ignore standard community");
        hl.add("9 9,.                  extcomm   ignore extended community");
        hl.add("9 9,.                  lrgcomm   ignore large community");
        hl.add("9 9,.                  sortcomm  sort communities");
        hl.add("9 9,.                  lnksta    ignore link state");
        hl.add("9 9,.                  aigp      ignore accumulated igp");
        hl.add("9 9,.                  bandwidth ignore bandwidth");
        hl.add("9 9,.                  label     ignore labels");
        hl.add("9 9,.                  aggregate ignore aggregator");
        hl.add("9 9,.                  orignted  ignore originator");
        hl.add("9 9,.                  pmsi      ignore pmsi");
        hl.add("9 9,.                  segrout   ignore segment routing");
        hl.add("9 9,.                  tunnel    ignore tunnel");
        hl.add("9 10                   time      specify time");
        hl.add("10 9,.                   <num>   milliseconds");
        hl.add("9 10                   exclude   exclude prefixes from compare");
        hl.add("10 9,.                   <name>  name of route map");
        hl.add("9 10                   update    update prefixes before compare");
        hl.add("10 9,.                   <name>  name of route map");
        hl.add("3 4        logger                route logger");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            prefix-lengths    list of prefix lengths");
        hl.add("5 .            interfaces        list of outgoing interface");
        hl.add("5 6,.          unicast           list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 6,.          multicast         list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 6,.          flowspec          list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 6,.          flapstat          prefix flap statistics");
        hl.add("6 .              [num]           minimum count");
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
        hl.add(beg + " " + (beg + 1) + "  |                       output modifier");
        hl.add((beg + 1) + " " + (beg + 2) + "    include               only lines that match");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    exclude               hide lines that match");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    begin                 only lines from match");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    end                   only lines to match");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    sort                  sort lines by");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    padsort               sort padded lines by");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    uniq                  unique lines by");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              column name");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    section               only sections that match");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    reginc                only lines that match regular expression");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    regexc                hide lines that match regular expression");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    regbeg                only lines from match regular expression");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    regend                only lines to match regular expression");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    regsec                only sections that match regular expression");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <text>              filter text");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    first                 only first some lines");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <num>               number of lines");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " " + (beg + 2) + "    last                  only last some lines");
        hl.add((beg + 2) + " " + (beg + 3) + ",.    <num>               number of lines");
        hl.add((beg + 3) + " " + (beg + 4) + "        |                 output modifier");
        hl.add((beg + 4) + " .            count           count entities");
        hl.add((beg + 4) + " .            summary         summary entities");
        hl.add((beg + 1) + " .      headers               only section headers");
        hl.add((beg + 1) + " .      level                 raw level hierarchy");
        hl.add((beg + 1) + " .      csv                   level hierarchy in csv");
        hl.add((beg + 1) + " .      html                  level hierarchy in html");
        hl.add((beg + 1) + " .      setdel                level hierarchy in set/delete");
        hl.add((beg + 1) + " .      summary               just the summary of columns");
        hl.add((beg + 1) + " .      linenumbers           prepend lines with numbers");
        hl.add((beg + 1) + " .      raw                   unfiltered");
        hl.add((beg + 1) + " .      count                 count entities");
        hl.add((beg + 1) + " .      viewer                display in viewer");
        if (!privi) {
            return;
        }
        hl.add((beg + 1) + " " + (beg + 9) + "    redirect              redirect output to file");
        hl.add((beg + 9) + " .        <text>              name of file");
    }

    /**
     * get show help
     *
     * @param hl help to append
     * @param privi allow privileges
     */
    public static void getHelpShow(userHelping hl, boolean privi) {
        hl.add("2 3      aaa                     aaa information");
        hl.add("3 .        <str>                 aaa list");
        hl.add("2 3      macsec                  macsec information");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 3      pppoe                   pppoe information");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 .      parser                  parser information");
        hl.add("2 .      history                 command history");
        hl.add("2 .      scheduler               scheduler information");
        hl.add("2 .      script                  script information");
        hl.add("2 3      vdc                     virtual device context");
        hl.add("3 .        interface             list of physical interfaces");
        hl.add("3 .        device                list of running devices");
        hl.add("2 .      reload                  scheduled reload");
        hl.add("2 .      banner                  logo of device");
        hl.add("2 .      privilege               list of your aaa details");
        hl.add("2 .      users                   list of interactive users");
        hl.add("2 3,.    logo                    logo of product");
        hl.add("3 3,.      [text]                text to print");
        if (privi) {
            hl.add("2 3,.    flash                   list of flash");
            hl.add("3 3,.      [text]                directory to print");
        }
        hl.add("2 3      whois                   query whois server");
        hl.add("3 3,.      [text]                directory to print");
        hl.add("2 3      transproxy              transparent proxy connections");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 3,.    version                 version information");
        hl.add("3 .        brief                 just headline");
        hl.add("3 .        number                just numbers");
        hl.add("3 .        date-email            just date");
        hl.add("3 .        date-machine          just date");
        hl.add("3 .        user-agent            just user agent");
        hl.add("3 .        url                   just url");
        hl.add("2 .      alias                   defined alieses");
        hl.add("2 .      platform                platform information");
        hl.add("2 3      me-the                  show something");
        hl.add("3 .        love                  show some love");
        hl.add("3 .        key                   show some key");
        hl.add("3 .        bug                   show some bug");
        hl.add("2 3      process                 list processes");
        hl.add("3 .        cpu                   internal router threads");
        hl.add("3 .        external              external processes");
        hl.add("2 .      redundancy              redundancy information");
        hl.add("2 .      name-cache              local dns cache");
        hl.add("2 3      watchdog                watchdog information");
        hl.add("3 .        gc                    garbage collector information");
        hl.add("3 .        sys                   system information");
        hl.add("3 .        hardware              hardware watchdog information");
        hl.add("3 .        software              software watchdog information");
        hl.add("3 .        timer                 timer history information");
        hl.add("3 .        memory                memory history information");
        hl.add("2 3,.    interfaces              interface status and configuration");
        hl.add("3 .        full                  full listing");
        hl.add("3 .        description           description listing");
        hl.add("3 .        summary               summary listing");
        hl.add("3 .        hwsummary             hardware summary listing");
        hl.add("3 .        swsummary             software summary listing");
        hl.add("3 .        total                 total listing");
        hl.add("3 .        hwtotal               hardware total listing");
        hl.add("3 .        swtotal               software total listing");
        hl.add("3 .        traffic               traffic listing");
        hl.add("3 .        hwtraffic             hardware traffic listing");
        hl.add("3 .        swtraffic             software traffic listing");
        hl.add("3 .        psummary              packet summary listing");
        hl.add("3 .        hwpsummary            hardware packet summary listing");
        hl.add("3 .        swpsummary            software packet summary listing");
        hl.add("3 .        ptotal                packet total listing");
        hl.add("3 .        hwptotal              hardware packet total listing");
        hl.add("3 .        swptotal              software packet total listing");
        hl.add("3 .        ptraffic              packet traffic listing");
        hl.add("3 .        hwptraffic            hardware packet traffic listing");
        hl.add("3 .        swptraffic            software packet traffic listing");
        hl.add("3 .        vrf                   vrf listing");
        hl.add("3 4,.      [name]                name of (sub)interface");
        hl.add("4 .          full                full listing");
        hl.add("4 .          rates               traffic rates");
        hl.add("4 .          realtime            realtime counters");
        hl.add("4 .          history             historic byte counters");
        hl.add("4 .          rxhistory           historic rx byte counters");
        hl.add("4 .          txhistory           historic tx byte counters");
        hl.add("4 .          drhistory           historic drop byte counters");
        hl.add("4 .          phistory            historic packet counters");
        hl.add("4 .          rxphistory          historic rx packet counters");
        hl.add("4 .          txphistory          historic tx packet counters");
        hl.add("4 .          drphistory          historic drop packet counters");
        hl.add("4 .          numhist             numeric historic byte counters");
        hl.add("4 .          numphist            numeric historic packet counters");
        hl.add("4 .          hwrates             hardware traffic rates");
        hl.add("4 .          hwrealtime          hardware realtime counters");
        hl.add("4 .          hwhistory           hardware historic byte counters");
        hl.add("4 .          hwrxhistory         hardware historic rx byte counters");
        hl.add("4 .          hwtxhistory         hardware historic tx byte counters");
        hl.add("4 .          hwdrhistory         hardware historic drop byte counters");
        hl.add("4 .          hwphistory          hardware historic packet counters");
        hl.add("4 .          hwrxphistory        hardware historic rx packet counters");
        hl.add("4 .          hwtxphistory        hardware historic tx packet counters");
        hl.add("4 .          hwdrphistory        hardware historic drop packet counters");
        hl.add("4 .          hwnumhist           hardware numeric historic byte counters");
        hl.add("4 .          hwnumphist          hardware numeric historic packet counters");
        hl.add("4 .          counters            counters");
        hl.add("4 .          hwcounters          hardware counters");
        hl.add("4 .          ethertypes          ethernet types");
        hl.add("4 .          lossdetect          loss detection");
        hl.add("4 .          packetsizes         packet sizes");
        hl.add("4 .          protocols           protocol numbers");
        hl.add("4 .          trafficclasses      traffic classes");
        hl.add("2 .      terminal                terminal parameters");
        hl.add("2 3,.    tracker                 tracker status");
        hl.add("3 .        [name]                name of tracker");
        hl.add("2 3,.    mtracker                mtracker status");
        hl.add("3 .        [name]                name of mtracker");
        hl.add("2 3      lldp                    link layer discovery protocol");
        hl.add("3 .        neighbor              summary list of neighbors");
        hl.add("3 4        detail                detailed list of neighbors");
        hl.add("4 .          <name>              name of interface");
        hl.add("2 3      cdp                     cisco discovery protocol");
        hl.add("3 .        neighbor              summary list of neighbors");
        hl.add("3 4        detail                detailed list of neighbors");
        hl.add("4 .          <name>              name of interface");
        hl.add("2 3      udld                    unidirectional link detection");
        hl.add("3 .        neighbor              summary list of neighbors");
        hl.add("3 4        detail                detailed list of neighbors");
        hl.add("4 .          <name>              name of interface");
        hl.add("2 3      lacp                    link aggregation control protocol");
        hl.add("3 .        neighbor              summary list of neighbors");
        hl.add("3 4        detail                detailed list of neighbors");
        hl.add("4 .          <name>              name of interface");
        hl.add("2 3      policy-map              policy map statistics");
        hl.add("3 4        interface             applied to interface");
        hl.add("4 5          <name>              name of interface");
        hl.add("5 .            in                ingress policy");
        hl.add("5 .            out               egress policy");
        hl.add("3 4        control-plane         applied to vrf");
        hl.add("4 5          <name>              name of vrf");
        hl.add("5 .            ipv4in            ipv4 ingress policy");
        hl.add("5 .            ipv4out           ipv4 egress policy");
        hl.add("5 .            ipv6in            ipv6 ingress policy");
        hl.add("5 .            ipv6out           ipv6 egress policy");
        hl.add("3 4        data-plane            applied to vrf");
        hl.add("4 5          <name>              name of vrf");
        hl.add("5 .            ipv4              ipv4 policy");
        hl.add("5 .            ipv6              ipv6 policy");
        hl.add("3 4        flowspec              applied to vrf");
        hl.add("4 5          <name>              name of vrf");
        hl.add("5 .            ipv4              ipv4 policy");
        hl.add("5 .            ipv6              ipv6 policy");
        hl.add("2 3      object-group            object group statistics");
        hl.add("3 4        network               network list");
        hl.add("4 .          <name>              name of list");
        hl.add("3 4        port                  port list");
        hl.add("4 .          <name>              name of list");
        hl.add("2 3      access-list             access list statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      route-map               route map statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      route-policy            route policy statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      prefix-list             prefix list statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      session                 stateful session information");
        hl.add("3 4        <name>                name of session");
        hl.add("4 .          session             list of sessions");
        hl.add("4 .          toptalk             top talker list");
        hl.add("2 3      dial-peer               current dial peer status");
        hl.add("3 .        description           descriptions");
        hl.add("3 .        voice                 for calls");
        hl.add("3 .        message               for messages");
        hl.add("3 4        active                active calls");
        hl.add("4 .          <num>               dial peer");
        hl.add("3 4        history               list of calls");
        hl.add("4 .          <num>               dial peer");
        hl.add("2 3,.    clock                   current date and time");
        hl.add("3 .        big                   a big clock");
        hl.add("3 .        raw                   a raw clock");
        hl.add("2 3,.    logging                 logged messages");
        hl.add("3 .        file                  saved messages");
        hl.add("3 .        old-file              old saved messages");
        hl.add("3 4        last                  last messages");
        hl.add("4 .          <num>               message count");
        hl.add("2 .      config-differences      startup->running differences");
        hl.add("2 .      rollback-config         running->startup differences");
        hl.add("2 3,.    startup-config          startup configuration");
        hl.add("3 3,.      [name]                name of section");
        hl.add("2 3,.    running-config          current operating configuration");
        hl.add("3 3,.      [name]                name of section");
        hl.add("3 4,.      this                  current subconfiguration");
        hl.add("4 4,.        all                 do not hide defaults");
        hl.add("4 4,.        hide                hide passwords");
        hl.add("3 4        interface             specified interface");
        hl.add("4 5,.        <name>              name of interface");
        hl.add("5 5,.          all               do not hide defaults");
        hl.add("5 5,.          hide              hide passwords");
        hl.add("3 4        router                specified router process");
        cfgRtr.getRouterList(hl, 2, "");
        hl.add("5 6,.          <num>             process id");
        hl.add("6 6,.            all             do not hide defaults");
        hl.add("6 6,.            hide            hide passwords");
        hl.add("3 4,.      all                   do not hide defaults");
        hl.add("4 .          hide                hide passwords");
        hl.add("3 4,.      hide                  hide passwords");
        hl.add("4 .          all                 do not hide defaults");
        hl.add("2 3      vrf                     virtual routing/forwarding information");
        hl.add("3 .        routing               routing information");
        hl.add("3 .        icmp                  icmp information");
        hl.add("3 4,.      traffic               traffic information");
        hl.add("4 5          [name]              name of vrf");
        hl.add("5 .            full              full listing");
        hl.add("5 .            rates             traffic rates");
        hl.add("5 .            realtime          realtime counters");
        hl.add("5 .            history           historic byte counters");
        hl.add("5 .            rxhistory         historic rx byte counters");
        hl.add("5 .            txhistory         historic tx byte counters");
        hl.add("5 .            drhistory         historic drop byte counters");
        hl.add("5 .            phistory          historic packet counters");
        hl.add("5 .            rxphistory        historic rx packet counters");
        hl.add("5 .            txphistory        historic tx packet counters");
        hl.add("5 .            drphistory        historic drop packet counters");
        hl.add("5 .            numhist           numeric historic byte counters");
        hl.add("5 .            numphist          numeric historic packet counters");
        hl.add("2 3,.    dashboard               generate dashboard of this node");
        hl.add("3 4        replace               specify replacement");
        hl.add("4 5          <str>               source");
        hl.add("5 3,.          <str>             target");
        hl.add("3 4        text                  specify static text");
        hl.add("4 3,.        <str>               url to use, %hostname% and %domain% will be replaced");
        hl.add("3 4        iface                 specify intrefaces url");
        hl.add("4 3,.        <str>               url to use, %name% and %desc% will be replaced");
        hl.add("3 4        vrf                   specify vrf url");
        hl.add("4 3,.        <str>               url to use, %name% and %desc% will be replaced");
        hl.add("3 4        router                specify router url");
        hl.add("4 3,.        <str>               url to use, %name% and %id% will be replaced");
        hl.add("2 3      dns                     dns protocol information");
        hl.add("3 4        <name>                server name");
        hl.add("4 .          <name>              zone name");
        hl.add("2 3      http                    http protocol information");
        hl.add("3 .        <name>                server name");
        hl.add("2 3,.    check                   check information");
        hl.add("3 .          <name>              check name");
        hl.add("2 3,.    sensor                  sensor information");
        hl.add("3 .        <name>                exporter name");
        hl.add("2 3      netflow                 netflow collector information");
        hl.add("3 4        <name>                server name");
        hl.add("4 .          session             list of sessions");
        hl.add("4 .          toptalk             top talker list");
        hl.add("2 3      streamingmdt            streaming telemetry collector information");
        hl.add("3 4,.      <name>                server name");
        hl.add("4 5,.        <name>              peer");
        hl.add("5 6            <name>            path");
        hl.add("6 .              <name>          key");
        hl.add("2 3      bmp                     bgp monitoring protocol information");
        hl.add("3 4,.      <name>                server name");
        hl.add("4 5,.        <addr>              from");
        hl.add("5 .            <addr>            peer");
        hl.add("2 3      bridge                  bridging information");
        hl.add("3 .        <num>                 bridge number");
        hl.add("2 3      bundle                  bundling information");
        hl.add("3 .        <num>                 bundle number");
        hl.add("2 3      mpls                    multiprotocol label switching");
        hl.add("3 4,.      forwarding            mpls forwarding table");
        hl.add("4 .          [num]               label to view");
        hl.add("3 .        interfaces            mpls interface table");
        hl.add("3 4        inspect               mpls inspection table");
        hl.add("4 5          <name>              interface to see");
        hl.add("5 .            session           list of sessions");
        hl.add("5 .            toptalk           top talker list");
        hl.add("2 3      nsh                     network service header");
        hl.add("3 4,.      forwarding            nsh forwarding table");
        hl.add("4 5          [num]               path to view");
        hl.add("5 .            <num>             index to view");
        hl.add("3 .        interfaces            nsh interface table");
        hl.add("2 3      polka                   polynomial key architecture");
        hl.add("3 4        routeid               polka routeid information");
        hl.add("4 .          <name>              interface to see");
        hl.add("3 4,.      interfaces            polka interface table");
        hl.add("4 .          [name]              interface to see");
        hl.add("2 3      ipx                     ipx information");
        hl.add("3 4        route                 routing table entries");
        hl.add("4 5,.        <vrf>               name of routing table");
        hl.add("5 .            [addr]            prefix to view");
        hl.add("2 3      router                  routing protocol information");
        cfgRtr.getRouterList(hl, 1, "");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            redisted          advertised routes");
        hl.add("5 6            computed          computed routes");
        hl.add("6 7,.            unicast         unicast routes");
        hl.add("6 7,.            multicast       multicast routes");
        hl.add("6 7,.            flowspec        flowspec routes");
        hl.add("7 8,.              [addr]        prefix to view");
        hl.add("8 .                  [rd]        route distinguisher");
        hl.add("2 3      ipv4                    ipv4 information");
        hl.add("3 4        arp                   interface arp cache");
        hl.add("4 .          <name>              name of (sub)interface");
        getHelpShowIpX(hl);
        hl.add("2 3      ipv6                    ipv6 information");
        hl.add("3 4        neighbors             interface neighbor cache");
        hl.add("4 .          <name>              name of (sub)interface");
        getHelpShowIpX(hl);
        cfgAll.aliasHelps(cfgAlias.aliasType.show, 2, hl);
    }

    private void getHelpTelnet(userHelping hl) {
        hl.add("2 3,4,.  <host>                  name of host");
        hl.add("3 4,.      [port]                port on host");
        hl.add("4 4,.        /tcp                transmission control protocol");
        hl.add("4 4,.        /udp                user datagram protocol");
        hl.add("4 4,.        /ludp               lightweight user datagram protocol");
        hl.add("4 4,.        /dccp               user datagram congestion control protocol");
        hl.add("4 4,.        /sctp               stream control transmission protocol");
        hl.add("4 4,.        /ssh                specify secure shell");
        hl.add("4 4,.        /tls                specify transport layer security");
        hl.add("4 4,.        /dtls               specify datagram transport layer security");
        hl.add("4 4,.        /telnet             specify telnet protocol");
        hl.add("4 4,.        /ipv4               specify ipv4 to use");
        hl.add("4 4,.        /ipv6               specify ipv6 to use");
        hl.add("4 5          /vrf                specify vrf to use");
        hl.add("5 4,.          <vrf>             name of vrf");
        hl.add("4 5          /user               specify username to use");
        hl.add("5 4,.          <str>             username");
        hl.add("4 5          /pass               specify password to use");
        hl.add("5 4,.          <str>             password");
        hl.add("4 5          /proxy              specify proxy to use");
        hl.add("5 4,.          <name>            name of proxy profile");
        hl.add("4 5          /interface          specify interface to use");
        hl.add("5 4,.          <name>            name of interface");
        hl.add("4 5          /chat               specify chat script to use");
        hl.add("5 4,.          <name>            name of chat script");
    }

    /**
     * get help text for exec commands
     *
     * @return helping instance
     */
    public userHelping getHelping() {
        userHelping hl = new userHelping();
        hl.add("1 2    show                      running system information");
        getHelpShow(hl, privileged);
        getHelpPipes(hl, 110, privileged);
        hl.add("1 2    watch                     running system periodic information");
        getHelpShow(hl, privileged);
        hl.add("1 2    view                      running system information");
        getHelpShow(hl, privileged);
        hl.add("1 2    display                   running system periodic information");
        getHelpShow(hl, privileged);
        hl.add("1 2    differs                   running system difference information");
        getHelpShow(hl, privileged);
        hl.add("1 .    logout                    close this exec session");
        hl.add("1 .    exit                      close this exec session");
        hl.add("1 2,.  netconf                   start netconf session");
        hl.add("2 2,.    format                  format response");
        hl.add("2 2,.    echo                    echo user input");
        hl.add("1 2,.  xml                       start xml session");
        hl.add("2 2,.    format                  format response");
        hl.add("2 2,.    echo                    echo user input");
        hl.add("1 .    ppp                       start framed session");
        hl.add("1 .    modememu                  start modem emulation session");
        hl.add("1 .    gpsemu                    start gps emulation session");
        hl.add("1 .    gpstime                   start gps session");
        hl.add("1 2    menu                      start menu session");
        hl.add("2 .      <name>                  name of menu");
        hl.add("1 2    terminal                  terminal specific parameters");
        hl.add("2 2      no                      negate a parameter");
        hl.add("2 3      width                   set terminal width");
        hl.add("3 .        <num>                 width in columns");
        hl.add("2 3      length                  set terminal length");
        hl.add("3 .        <num>                 height in lines");
        hl.add("2 3      escape                  set escape character");
        hl.add("3 .        <num>                 ascii code");
        hl.add("2 3      deactivate              set deactivate character");
        hl.add("3 .        <num>                 ascii code");
        hl.add("2 .      monitor                 log to this terminal");
        hl.add("2 .      timestamps              put time before each executed command");
        hl.add("2 .      colorized               sending to ansi terminal");
        hl.add("2 .      spacetab                treat space as tabulator");
        hl.add("2 3      tablemode               select table formatting mode");
        hl.add("3 .        normal                select normal mode");
        hl.add("3 .        table                 select table mode");
        hl.add("3 .        fancy                 select fancy mode");
        hl.add("3 .        csv                   select csv mode");
        hl.add("3 .        raw                   select raw mode");
        hl.add("3 .        html                  select html mode");
        hl.add("1 2    hostscan                  scan ports on remote");
        hl.add("2 3      <host>                  starting host");
        hl.add("3 4        <host>                increment host");
        hl.add("4 5,.        <host>              number of hosts");
        hl.add("5 6            /vrf              specify vrf to use");
        hl.add("6 5,.            <vrf>           name of vrf");
        hl.add("5 6            /interface        specify interface to use");
        hl.add("6 5,.            <name>          name of interface");
        hl.add("5 6            /timeout          specify timeout");
        hl.add("6 5,.            <num>           timeout in milliseconds");
        hl.add("5 6            /ttl              specify ttl value");
        hl.add("6 5,.            <num>           ttl");
        hl.add("5 6            /tos              specify tos value");
        hl.add("6 5,.            <num>           tos");
        hl.add("5 6            /size             specify payload size");
        hl.add("6 5,.            <num>           byte count");
        hl.add("5 6            /port             specify tcp port");
        hl.add("6 5,.            <num>           port number");
        hl.add("5 .            /lookup           perform reverse lookup to");
        hl.add("1 2    portscan                  scan ports on remote");
        hl.add("2 3,.    <host>                  name of host");
        hl.add("3 3,.      /ipv4                 specify ipv4 to use");
        hl.add("3 3,.      /ipv6                 specify ipv6 to use");
        hl.add("3 4        /vrf                  specify vrf to use");
        hl.add("4 3,.        <vrf>               name of vrf");
        hl.add("3 4        /interface            specify interface to use");
        hl.add("4 3,.        <name>              name of interface");
        hl.add("3 4        /timeout              specify timeout");
        hl.add("4 3,.        <num>               timeout in milliseconds");
        hl.add("3 4        /min                  specify lower port number");
        hl.add("4 3,.        <num>               port number");
        hl.add("3 4        /max                  specify upper port number");
        hl.add("4 3,.        <num>               port number");
        hl.add("1 2    lookup                    domain name lookup");
        hl.add("2 3      ipv4                    ipv4 address record");
        hl.add("2 3      ipv6                    ipv6 address record");
        hl.add("2 3      mail                    mail exchange record");
        hl.add("2 3      dns                     name server record");
        hl.add("2 3      soa                     authority record");
        hl.add("2 3      srv                     service record");
        hl.add("2 3      txt                     text record");
        hl.add("2 3      reverse                 reverse of address record");
        hl.add("2 3      recur-ipv4              ipv4 address record");
        hl.add("2 3      recur-ipv6              ipv6 address record");
        hl.add("2 3      recur-mail              mail exchange record");
        hl.add("2 3      recur-dns               name server record");
        hl.add("2 3      recur-soa               authority record");
        hl.add("2 3      recur-srv               service record");
        hl.add("2 3      recur-txt               text record");
        hl.add("2 3      zone                    download whole zone");
        hl.add("3 4,.      <domain>              domain to look up");
        hl.add("4 .          [server]            address of ns server");
        hl.add("1 .    disable                   drop privileges");
        hl.add("1 .    enable                    gain privileges");
        hl.add("1 2,.  tclsh                     run tcl shell");
        hl.add("2 .      [file]                  name of script");
        hl.add("1 2    traceroute                trace route to target");
        hl.add("2 3,.    <host>                  name of host");
        hl.add("3 3,.      /ipv4                 specify ipv4 to use");
        hl.add("3 3,.      /ipv6                 specify ipv6 to use");
        hl.add("3 4        /vrf                  specify vrf to use");
        hl.add("4 3,.        <vrf>               name of vrf");
        hl.add("3 4        /interface            specify interface to use");
        hl.add("4 3,.        <name>              name of interface");
        hl.add("3 4        /timeout              specify timeout");
        hl.add("4 3,.        <num>               timeout in milliseconds");
        hl.add("3 4        /delay                specify delay between packets");
        hl.add("4 3,.        <num>               timeout in milliseconds");
        hl.add("3 4        /tos                  specify tos value");
        hl.add("4 3,.        <num>               tos");
        hl.add("3 4        /flow                 specify flow value");
        hl.add("4 3,.        <num>               tos");
        hl.add("3 4        /port                 specify port value");
        hl.add("4 3,.        <num>               port");
        hl.add("3 4        /protocol             specify protocol value");
        hl.add("4 3,.        <num>               port");
        hl.add("3 4        /size                 specify payload size");
        hl.add("4 3,.        <num>               byte count");
        hl.add("3 4        /router               lookup intermediate hops");
        cfgRtr.getRouterList(hl, 2, "");
        hl.add("5 3,.          <num>             process id");
        hl.add("3 3,.      /lookup               lookup intermediate hops");
        hl.add("1 2    ping                      send echo request");
        hl.add("2 3,.    <host>                  name of host");
        hl.add("3 3,.      /multi                wait for multiple responses");
        hl.add("3 3,.      /detail               specify detail mode");
        hl.add("3 4        /data                 specify data to send");
        hl.add("4 3,.        <num>               payload byte");
        hl.add("3 3,.      /ipv4                 specify ipv4 to use");
        hl.add("3 3,.      /ipv6                 specify ipv6 to use");
        hl.add("3 4        /vrf                  specify vrf to use");
        hl.add("4 3,.        <vrf>               name of vrf");
        hl.add("3 4        /interface            specify interface to use");
        hl.add("4 3,.        <name>              name of interface");
        hl.add("3 4        /timeout              specify timeout");
        hl.add("4 3,.        <num>               timeout in milliseconds");
        hl.add("3 4        /delay                specify delay between packets");
        hl.add("4 3,.        <num>               timeout in milliseconds");
        hl.add("3 3,.      /sweep                specify increasing payload size");
        hl.add("3 4        /size                 specify payload size");
        hl.add("4 3,.        <num>               byte count");
        hl.add("3 4        /repeat               specify repeat count");
        hl.add("4 3,.        <num>               repeat count");
        hl.add("3 4        /ttl                  specify ttl value");
        hl.add("4 3,.        <num>               ttl");
        hl.add("3 4        /tos                  specify tos value");
        hl.add("4 3,.        <num>               tos");
        hl.add("3 4        /flow                 specify flow value");
        hl.add("4 3,.        <num>               tos");
        hl.add("1 2    sleep                     do nothing for a while");
        hl.add("2 .      <num>                   milliseconds for sleep");
        hl.add("1 2    whois                     perform whois query");
        hl.add("2 3      <host>                  name of host to query");
        hl.add("3 3,.      <text>                query string");
        hl.add("1 2    game                      play games or watch screen savers");
        hl.add("2 .      color                   take test");
        hl.add("2 .      ascii                   take test");
        hl.add("2 .      keys                    take test");
        hl.add("2 .      clear                   clear screen");
        hl.add("2 .      gomoku                  play game");
        hl.add("2 .      tetris                  play game");
        hl.add("2 .      minesweep               play game");
        hl.add("2 .      clock                   view demo");
        hl.add("2 .      snake                   view demo");
        hl.add("2 .      matrix                  view demo");
        hl.add("2 .      fire                    view demo");
        hl.add("2 .      life                    view demo");
        hl.add("2 .      antball                 view demo");
        hl.add("2 3,.    text                    view demo");
        hl.add("3 3,.      [str]                 text");
        hl.add("2 3,.    logo                    view demo");
        hl.add("3 3,.      [str]                 text");
        if (privileged) {
            hl.add("2 3      image                   view image");
            hl.add("3 .        <file>                filename");
        }
        hl.add("1 2    listen                    start listen session");
        hl.add("2 3,.    <port>                  port number");
        hl.add("3 3,.        /tcp                transmission control protocol");
        hl.add("3 3,.        /udp                user datagram protocol");
        hl.add("3 3,.        /ludp               lightweight user datagram protocol");
        hl.add("3 3,.        /dccp               user datagram congestion control protocol");
        hl.add("3 3,.        /sctp               stream control transmission protocol");
        hl.add("3 3,.        /ipv4               specify ipv4 to use");
        hl.add("3 3,.        /ipv6               specify ipv6 to use");
        hl.add("3 4          /vrf                specify vrf to use");
        hl.add("4 3,.          <vrf>             name of vrf");
        hl.add("3 4          /interface          specify interface to use");
        hl.add("4 3,.          <name>            name of interface");
        hl.add("1 2    telnet                    start telnet session");
        getHelpTelnet(hl);
        hl.add("1 2    tls                       start tls session");
        getHelpTelnet(hl);
        hl.add("1 2    dtls                      start dtls session");
        getHelpTelnet(hl);
        hl.add("1 2    ssl                       start ssl session");
        getHelpTelnet(hl);
        hl.add("1 2    ssh                       start ssh session");
        getHelpTelnet(hl);
        cfgAll.aliasHelps(cfgAlias.aliasType.exec, 1, hl);
        if (!privileged) {
            return hl;
        }
        hl.add("1 2    clear                     clear running conditions");
        hl.add("2 3,.    hwcounters              hardware counters on one or more interfaces");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 3,.    swcounters              software counters on one or more interfaces");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 3,.    counters                counters on one or more interfaces");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 3      object-group            object group statistics");
        hl.add("3 4        network               network list");
        hl.add("4 .          <name>              name of list");
        hl.add("3 4        port                  port list");
        hl.add("4 .          <name>              name of list");
        hl.add("2 3      reflected-acl           clear access list entries");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      access-list             access list statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      route-map               route map statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      route-policy            route policy statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      prefix-list             prefix list statistics");
        hl.add("3 .        <name>                name of list");
        hl.add("2 3      errors                  error reporter");
        hl.add("3 3,.      <str>                 email address");
        hl.add("2 3      bridge                  learnt mac address on a bridge");
        hl.add("3 4,.      <num>                 bridge number");
        hl.add("4 .          <name>              name of interface");
        hl.add("2 3      telemetry               trigger telemetry report");
        hl.add("3 4        <name>                name of sensor");
        hl.add("4 4,.        <name>              name of telemetry");
        hl.add("2 3      dial-peer               clear voip call");
        hl.add("3 4        <num>                 number of dial peer");
        hl.add("4 .          <str>               call id");
        hl.add("2 3      scheduler               run one scheduler round");
        hl.add("3 .        <name>                name of scheduler");
        hl.add("2 3      script                  run one script round");
        hl.add("3 .        <name>                name of script");
        hl.add("2 3      vpdn                    reconnect vpdn peer");
        hl.add("3 4,.      <name>                name of vpdn");
        hl.add("4 .          [num]               downtime in milliseconds");
        hl.add("2 3      vdc                     restart vdc process");
        hl.add("3 4,.      <name>                name of vdc");
        hl.add("4 .          stop                stop");
        hl.add("4 .          start               start");
        hl.add("2 3      process                 restart external process");
        hl.add("3 .        <name>                name of process");
        hl.add("2 .      logging                 logged messages");
        hl.add("2 .      auto-bandwidth          set auto bandwidth values");
        hl.add("2 .      follow-tracker          set interfaces based on trackers");
        hl.add("2 .      tunnel-domain           resolve destination domain names");
        hl.add("2 .      name-cache              dns local cache");
        hl.add("2 3,.    watchdog                watchdog");
        hl.add("3 3,.      [name]                parameter");
        hl.add("2 3      line                    disconnect line");
        hl.add("3 .        <name>                name of line");
        hl.add("2 3      interface               disconnect interface");
        hl.add("3 4,.      <name>                name of interface");
        hl.add("4 .          [num]               downtime in milliseconds");
        hl.add("2 3      ipv4                    ipv4 parameters");
        hl.add("3 4        arp                   arp table");
        hl.add("4 5          <name>              interface name");
        hl.add("5 .            <addr>            peer address");
        getHelpClearIpX(hl);
        hl.add("2 3      ipv6                    ipv6 parameters");
        hl.add("3 4        neighbor              neighbor table");
        hl.add("4 5          <name>              interface name");
        hl.add("5 .            <addr>            peer address");
        getHelpClearIpX(hl);
        hl.add("2 3      bmp                     clear one bmp server");
        hl.add("3 .        <name>                server name");
        hl.add("2 3      socket                  clear one socket");
        hl.add("3 4        <name>                vrf name");
        hl.add("4 5          tcp                 select tcp");
        hl.add("4 5          udp                 select udp");
        hl.add("4 5          ludp                select ludp");
        hl.add("4 5          dccp                select dccp");
        hl.add("4 5          sctp                select sctp");
        hl.add("5 6            <name>            interface name");
        hl.add("6 7              <num>           local port");
        hl.add("7 8                <num>         remote port");
        hl.add("8 .                  <addr>      remote address");
        cfgAll.aliasHelps(cfgAlias.aliasType.clear, 2, hl);
        hl.add("1 2    debug                     start debugging one protocol");
        debugger.getHelping(hl);
        hl.add("1 2,.  undebug                   stop debugging one protocol");
        hl.add("2 .      all                     disable all debugging");
        debugger.getHelping(hl);
        hl.add("1 2    set                       insert configuration command");
        hl.add("2 2,.    [str]                   config line to set");
        hl.add("1 2    delete                    remove configuration command");
        hl.add("2 2,.    [str]                   config line to unset");
        hl.add("1 2,.  configure                 enter configuration mode");
        hl.add("2 3      file                    append to running configuration");
        hl.add("3 .        <file>                source file");
        hl.add("2 3      replace                 overwrite the running configuration");
        hl.add("3 .        <file>                source file");
        hl.add("2 .      banner                  edit the banner");
        hl.add("2 .      startup                 edit the startup configuration");
        hl.add("2 3,.    editor                  configure from editor");
        hl.add("3 3,.      [name]                section name");
        hl.add("2 3,.    viewer                  view current configuration");
        hl.add("3 3,.      [name]                section name");
        hl.add("2 .      terminal                configure from this terminal");
        hl.add("2 3      reload                  overwrite the startup configuration");
        hl.add("3 3,.      <url>                 source url");
        hl.add("2 3      network                 append to running configuration");
        hl.add("3 3,.      <url>                 source url");
        hl.add("2 3      overwrite-network       overwrite the running configuration");
        hl.add("3 3,.      <url>                 source url");
        hl.add("2 .      rollback                configure within auto-revert session");
        hl.add("2 .      revert                  revert to startup configuration");
        hl.add(".2 .     reapply                 try to reapply current configuration");
        userHelping hlp = new userHelping();
        hlp.add("1 2    flash                     file system utility");
        hlp.add("2 3      list                    list directory");
        hlp.add("3 .        <file>                pathname");
        hlp.add("2 3      type                    type one ascii file");
        hlp.add("3 .        <file>                filename");
        hlp.add("2 3      info                    information about file");
        hlp.add("3 .        <file>                filename");
        hlp.add("2 3      hash                    hash of file");
        hlp.add("3 .        <file>                filename");
        hlp.add("2 3      disk                    information about disk");
        hlp.add("3 .        <file>                filename");
        hlp.add("2 3      bintype                 type one binary file");
        hlp.add("3 .        <file>                filename");
        userExec.getHelpPipes(hlp, 120, privileged);
        hl.addOther(hlp);
        hl.add("2 3      receive                 receive file from network");
        hl.add("3 4        <file>                target file");
        hl.add("4 4,.        <url>               source url");
        hl.add("2 3      transmit                send file to network");
        hl.add("3 4        <file>                source file");
        hl.add("4 4,.        <url>               target url");
        hl.add("2 3      rename                  rename disk file");
        hl.add("3 4        <src>                 source file");
        hl.add("4 .          <trg>               target file");
        hl.add("2 3      copy                    copy disk file");
        hl.add("3 4        <src>                 source file");
        hl.add("4 .          <trg>               target file");
        hl.add("2 3      delete                  delete directory entry");
        hl.add("3 3,.      <file>                filename");
        hl.add("2 3      mkdir                   make directory");
        hl.add("3 .        <file>                filename");
        hl.add("2 .      verify                  verify routing software");
        hl.add("2 .      revert                  revert routing software");
        hl.add("2 .      backup                  backup routing software");
        hl.add("2 3,.    upgrade                 upgrade routing software");
        hl.add("3 3,.      [url]                 parameter of process");
        hl.add("2 3,.    simulate                simulate upgrade process");
        hl.add("3 3,.      [url]                 parameter of process");
        hl.add("2 3      editor                  compose text file");
        hl.add("3 .        <name>                name of file");
        hl.add("2 3      viewer                  read text file");
        hl.add("3 .        <name>                name of file");
        hl.add("2 3      binviewer               view one binary file");
        hl.add("3 .        <file>                filename");
        hl.add("2 .      commander               file manager");
        hl.add("2 3,.    browser                 web browser");
        hl.add("3 .        <url>                 target url");
        cfgAll.aliasHelps(cfgAlias.aliasType.flsh, 2, hl);
        hl.add("1 2,.  write                     save configuration");
        hl.add("2 3      file                    to disk file");
        hl.add("3 .        <file>                target file");
        hl.add("2 3,.    network                 to network storage");
        hl.add("3 .        <url>                 target url");
        hl.add("2 .      terminal                to this terminal");
        hl.add("2 .      memory                  to persistent storage");
        hl.add("2 .      erase                   clear persistent storage");
        hl.add("1 2    attach                    connect to system resources");
        hl.add("2 .      chat                    discuss with other admins");
        hl.add("2 3      vdc                     manage virtual device context");
        hl.add("3 .        <name>                name of vdc");
        hl.add("2 3      process                 manage external process");
        hl.add("3 .        <name>                name of process");
        hl.add("2 3      scheduler               manage scheduler");
        hl.add("3 .        <name>                name of process");
        hl.add("2 3      script                  manage script");
        hl.add("3 .        <name>                name of process");
        hl.add(".2 3     shell1                  run interactive shell process");
        hl.add(".3 4,.     <cmd>                 name of process");
        hl.add(".4 4,.       [param]             parameter of process");
        hl.add(".2 3     shell2                  run outputting shell process");
        hl.add(".3 4,.     <cmd>                 name of process");
        hl.add(".4 4,.       [param]             parameter of process");
        hl.add("2 3      line                    access physical line");
        hl.add("3 .        <name>                name of line");
        hl.add("1 2    packet                    packet related things");
        hl.add("2 3      capture                 capture interface traffic");
        hl.add("3 4,.      <name>                name of interface");
        hl.add("4 .          [file]              name of file");
        hl.add("2 3      buffer                  save buffered traffic");
        hl.add("3 4,.      <name>                name of interface");
        hl.add("4 .          [file]              name of file");
        hl.add("2 3      monitor                 monitor interface traffic");
        hl.add("3 4,.      <name>                name of interface");
        hl.add("4 .          <name>              name of target interface");
        hl.add(".2 3     flood                   flood packets");
        hl.add(".3 4       <name>                name of vrf");
        hl.add(".4 5         tcp                 select tcp");
        hl.add(".4 5         udp                 select udp");
        hl.add(".4 5         ludp                select ludp");
        hl.add(".4 5         dccp                select dccp");
        hl.add(".4 5         sctp                select sctp");
        hl.add(".5 6           <addr>            source address");
        hl.add(".6 7             <num>           source port");
        hl.add(".7 8               <addr>        target address");
        hl.add(".8 9                 <num>       target port");
        hl.add(".9 10                  <num>     flags");
        hl.add(".10 .                    <num>   size");
        hl.add(".2 3     replay                  replay capture on interface");
        hl.add(".3 4       <name>                name of interface");
        hl.add(".4 5,.       <file>              name of file");
        hl.add(".5 .           [num]             interpacket gap in millisecs");
        hl.add(".2 3     inject                  inject packet to interface");
        hl.add(".3 4       <name>                name of interface");
        hl.add(".4 4,.       [byte]              byte in hex");
        hl.add("2 3      wakeup                  wake up one host");
        hl.add("3 4        <name>                name of interface");
        hl.add("4 .          <addr>              address of host");
        hl.add(".2 3     mrtfilter               filter mrt file using filters of a peer");
        cfgRtr.getRouterList(hl, 1, "");
        hl.add(".4 5         <num>               process number");
        hl.add(".5 6           <addr>            peer address");
        hl.add(".6 7             <file>          source mrt file");
        hl.add(".7 8               <file>        target mrt file");
        hl.add(".8 9                 <addr>      source peer");
        hl.add(".9 .                   <addr>    target peer");
        hl.add(".2 3     mrt2self                replay mrt as if got from a peer");
        cfgRtr.getRouterList(hl, 1, "");
        hl.add(".4 5         <num>               process number");
        hl.add(".5 6           <addr>            peer address");
        hl.add(".6 7             <file>          mrt file");
        hl.add(".7 8               <addr>        source peer");
        hl.add(".8 .                 <addr>      target peer");
        hl.add("2 3      mrt2pcap                convert mrt to pcap");
        hl.add("3 4        <file>                name of mrt file");
        hl.add("4 .          <file>              name of pcap file");
        hl.add(".2 3     mrtplay                 run mrt prefix sender");
        hl.add(".3 4       <name>                vrf name");
        hl.add(".4 5         <name>              source interface name");
        hl.add(".5 6           <addr>            target address");
        hl.add(".6 7             <num>           local as");
        hl.add(".7 8               <file>        mrt file");
        hl.add(".8 9                 <addr>      source peer");
        hl.add(".9 10,.                <addr>    target peer");
        hl.add(".10 .                    [num]   safi number");
        hl.add(".2 3     random                  run random packet generator");
        hl.add(".3 4       <name>                name of interface");
        hl.add(".4 4,.       [byte]              byte in hex");
        hl.add(".2 3     bgpattr                 run attribute injector");
        hl.add(".3 4       <name>                vrf name");
        hl.add(".4 5         <name>              source interface name");
        hl.add(".5 6           <addr>            target address");
        hl.add(".6 7             <num>           local as");
        hl.add(".7 8                <addr>        prefix to originate");
        hl.add(".8 9                 <name>      route map to apply");
        hl.add(".9 9,.                 <num>     attribute byte");
        hl.add(".2 3     bgpgen                  run random prefix generator");
        hl.add(".3 4       <name>                vrf name");
        hl.add(".4 5         <name>              source interface name");
        hl.add(".5 6           <addr>            target address");
        hl.add(".6 7             <num>           local as");
        hl.add(".7 8               <addr>        prefix to originate");
        hl.add(".8 9                 <name>      route map to apply");
        hl.add(".9 .                   <num>     number of prefixes");
        hl.add("2 3      modem                   open modem session");
        hl.add("3 4,.      <addr>                address to call");
        hl.add("4 .          <addr>              address who calling");
        hl.add("2 3      voice                   open voice session");
        hl.add("3 4,.      <addr>                address to call");
        hl.add("4 5,.        <addr>              address who calling");
        hl.add("5 .            <name>            script to run");
        hl.add("2 3      message                 send voip message");
        hl.add("3 4        <addr>                address to call");
        hl.add("4 5          <addr>              address who calling");
        hl.add("5 5,.          <text>            message text");
        hl.add("2 3      conference              start voice conference");
        hl.add("3 4,.      <addr>                address who calling");
        hl.add("4 4,.        <addr>              address to call");
        hl.add("2 3      speed                   test speed clients");
        hl.add("3 3,.      <str>                 name of server");
        hl.add("2 3      websock                 test websocket client");
        hl.add("3 4        <str>                 url of server");
        hl.add("4 4,.        <str>               protocols");
        hl.add("2 3      netconf                 do netconf commands");
        hl.add("3 4        get                   do a get request");
        hl.add("3 4        read                  do a get-config request");
        hl.add("3 4        edit                  do a edit-config request");
        hl.add("3 4        copy                  do a copy-config request");
        hl.add("3 4        delete                do a delete-config request");
        hl.add("4 5          <addr>              server to query");
        hl.add("5 6            <text>            username to use");
        hl.add("6 7              <text>          password to use");
        hl.add("7 8                <text>        xml path");
        hl.add("8 .                  <text>      namespace of root");
        hl.add("2 3      snmp                    do snmp commands");
        hl.add("3 4        get                   do a get request");
        hl.add("3 4        next                  do a getnext request");
        hl.add("4 5          <addr>              server to query");
        hl.add("5 6            <text>            community to use");
        hl.add("6 .              <oid>           oid to query");
        hl.add("2 3      smtp                    send email message");
        hl.add("3 4,.      <str>                 email address");
        hl.add("4 4,.        <str>               email text");
        hl.add("2 3      ntp                     check remote time");
        hl.add("3 .        <str>                 server address");
        hl.add("2 3      nrpe                    check remote status");
        hl.add("3 4        <str>                 server address");
        hl.add("4 4,.        <str>               check name");
        hl.add("2 3      aaa                     test aaa config");
        hl.add("3 .        <str>                 aaa list");
        hl.add("2 3      pcep                    get a path from pcep");
        hl.add("3 4        <str>                 server address");
        hl.add("4 5          <str>               vrf to use");
        hl.add("5 6            <str>             interface to use");
        hl.add("6 7              te              traffic engineering");
        hl.add("6 7              sr              segment routing");
        hl.add("7 8                <str>         source address");
        hl.add("8 .                  <str>       target address");
        cfgAll.aliasHelps(cfgAlias.aliasType.pckt, 2, hl);
        hl.add("1 2    test                      test various things");
        hl.add("2 3      acl                     access list merge, unroll");
        hl.add("3 4,.      <name>                name of first acl");
        hl.add("4 .          <name>              name of second acl");
        hl.add("2 3      yangsensor              create yang from sensor");
        hl.add("3 4        <name>                source");
        hl.add("4 .          <name>              target");
        hl.add("2 3      yangconfig              create yang from config");
        hl.add("3 4        <name>                source");
        hl.add("4 .          <name>              target");
        hl.add("2 3      dns                     dns zone creator");
        hl.add("3 .        <name>                name of zone");
        hl.add("2 3      translation             translation rule");
        hl.add("3 4        <name>                name of rule");
        hl.add("4 4,.        <str>               text");
        hl.add("2 3      logging                 log one line");
        hl.add("3 4,.      debug                 debug message");
        hl.add("3 4,.      error                 error message");
        hl.add("3 4,.      warning               warning message");
        hl.add("3 4,.      informational         informational message");
        hl.add("4 4,.        <str>               text to log");
        hl.add(".3 4,.     traceback             traceback message");
        hl.add(".4 4,.       <str>               text to log");
        hl.add("2 3,.    password                decode encoded password");
        hl.add("3 3,.      <str>                 encoded string");
        hl.add("2 3,.    otppass                 generate password");
        hl.add("3 3,.      <str>                 encoded string");
        hl.add("2 3,.    asn1parser              decode asn1 encoded bytes");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    base64                  decode base64 encoded bytes");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    url                     decode url");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    xml                     decode xml");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    json                    decode json");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    protobuf                decode protobuf");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    addr                    decode address");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    prefix                  decode prefix");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3      vm                      run virtual machine");
        hl.add("3 4,.      <name>                file name");
        hl.add("4 4,.        [str]               parameter to give");
        hl.add("2 3,.    routing                 test routing lookup performance");
        hl.add("3 3,.      [str]                 parameters");
        hl.add("2 .      pipeline                test pipeline throughput");
        hl.add("2 .      ssh                     test ssh throughput");
        hl.add("2 3,.    tls                     test tls throughput");
        hl.add("3 4        <num>                 min");
        hl.add("4 .          <num>               max");
        hl.add("2 3,.    dtls                    test dtls throughput");
        hl.add("3 4        <num>                 min");
        hl.add("4 .          <num>               max");
        hl.add(".2 .     gc                      run garbage collector");
        hl.add("2 .      crypto                  test encryption and hash");
        hl.add("2 3,.    digsig                  test digital signatures");
        hl.add("3 3,.      [str]                 parameters");
        hl.add("2 3,.    window                  test window handler");
        hl.add("3 4        <num>                 x size");
        hl.add("4 .          <num>               y size");
        hl.add("2 3      vercore                 test vercore updater");
        hl.add("3 4        <key>                 key file to use");
        hl.add("4 .          <key>               key file to include");
        hl.add("2 3      verfile                 test version updater");
        hl.add("3 4,.      <key>                 key file to use");
        hl.add("4 4,.        [str]               file to include in release");
        hl.add("2 3,.    hwext                   perform forwarding externalization");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    hwpop                   perform forwarding port population");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    hwdet                   perform hw detection");
        hl.add("3 3,.      <str>                 parameter");
        hl.add(".2 3,.   hwcfg                   perform hw configuration");
        hl.add(".3 3,.     <str>                 parameter");
        hl.add("2 3,.    image                   perform image creation");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    tester                  perform image tests");
        hl.add("3 3,.      <str>                 parameter");
        hl.add("2 3,.    template                perform image templates");
        hl.add("3 3,.      <str>                 parameter");
        cfgAll.aliasHelps(cfgAlias.aliasType.test, 2, hl);
        hl.add("1 2    reload                    restart the system");
        hl.add("2 3      in                      reload after a time interval");
        hl.add("3 .        <min>                 minutes");
        hl.add("2 3      at                      reload at a specified time");
        hl.add("3 3,.      <str>                 datetime");
        hl.add("2 .      cancel                  cancel pending reload");
        hl.add("2 .      cold                    reboot the whole computer");
        hl.add("2 .      warm                    reboot the router process");
        hl.add("2 .      force                   reboot the router process without saving");
        hl.add("2 .      peer                    reboot redundant router processes");
        hl.add("2 3      vdc                     reboot virtual device context");
        hl.add("3 4,.      <name>                name of vdc");
        hl.add("4 .          stop                stop");
        hl.add("4 .          start               start");
        hl.add("2 3      process                 reboot external process");
        hl.add("3 .        <name>                name of process");
        return hl;
    }

    /**
     * execute some commands
     *
     * @return status of operation, see at one command
     */
    public cmdRes doCommands() {
        rollback = false;
        for (;;) {
            reader.setContext(getHelping(), cfgAll.hostName + (privileged ? "#" : ">"));
            String s = reader.readLine(null);
            if (s == null) {
                return cmdRes.logout;
            }
            if (pipe.settingsGet(pipeSetting.times, false)) {
                pipe.linePut(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3));
            }
            if (authorization != null) {
                authResult ntry = authorization.authUserCommand(username, s);
                if (ntry.result != authResult.authSuccessful) {
                    pipe.linePut("% not authorized to execute that");
                    continue;
                }
            }
            cmdRes i = executeCommand(s);
            if (i != cmdRes.command) {
                return i;
            }
        }
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
        if (a.equals("whois")) {
            clntWhois w = new clntWhois(cmd.word());
            w.quest = cmd.getRemaining();
            reader.putStrArr(w.doQuery(cmd));
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
        if (a.equals("ping")) {
            doPing();
            return cmdRes.command;
        }
        if (a.equals("menu")) {
            doMenu();
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
            cmd.pipe.strPut("password:");
            int i;
            if (cfgAll.passwdStars) {
                i = 0x33;
            } else {
                i = 0x31;
            }
            a = cmd.pipe.lineGet(i);
            if (!authLocal.secretTest(cfgAll.enaPass, a)) {
                privileged = true;
            }
            return cmdRes.command;
        }
        if (a.equals("disable")) {
            privileged = false;
            return cmdRes.command;
        }
        if (a.equals("game")) {
            reader.keyFlush();
            userGame t = new userGame(new userScreen(pipe));
            t.doStart();
            t.doCommand(cmd);
            t.doFinish();
            reader.keyFlush();
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
                a = version.myWorkDir() + "cfg" + bits.randomD() + ".tmp";
                boolean dl = userFlash.doReceive(pipe, uniResLoc.parseOne(cmd.getRemaining()), new File(a));
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
                cmd.error(doneFail(b));
                prtRedun.doConfig();
                prtRedun.doReload();
                return cmdRes.command;
            }
            if (a.equals("overwrite-network")) {
                a = version.myWorkDir() + "cfg" + bits.randomD() + ".tmp";
                boolean dl = userFlash.doReceive(pipe, uniResLoc.parseOne(cmd.getRemaining()), new File(a));
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
                a = version.myWorkDir() + "cfg" + bits.randomD() + ".tmp";
                boolean dl = userFlash.doReceive(pipe, uniResLoc.parseOne(cmd.getRemaining()), new File(a));
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
                s = cryBase64.encodeBytes(buf, 0, buf.length);
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
                cmd.error(doneFail(b));
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
                cmd.error(doneFail(b));
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
                cmd.error(doneFail(b));
                return cmdRes.command;
            }
            if (a.equals("memory")) {
                doCfgBackup();
                cmd.error("saving configuration");
                boolean b = bits.buf2txt(true, cfgAll.getShRun(1), cfgInit.cfgFileSw);
                cmd.error(doneFail(b));
                prtRedun.doConfig();
                prtRedun.doReload();
                if (!cfgAll.configAbackup) {
                    return cmdRes.command;
                }
                a = "network";
            }
            if (a.equals("network")) {
                cmd.error("archiving configuration");
                uniResLoc url = uniResLoc.parseOne(cmd.word());
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
                a = version.myWorkDir() + "wrt" + bits.randomD() + ".tmp";
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
                ntry.restartNow();
                return cmdRes.command;
            }
            if (a.equals("peer")) {
                prtRedun.doReload();
                return cmdRes.command;
            }
            if (a.equals("cancel")) {
                doReload(-1);
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
                cfgInit.stopRouter(true, 3, "user requested");
                return cmdRes.command;
            }
            if (a.equals("cold")) {
                cfgInit.stopRouter(true, 4, "user requested");
                return cmdRes.command;
            }
            if (a.equals("in")) {
                doReload(bits.getTime() + (bits.str2num(cmd.word()) * 60000));
                return cmdRes.command;
            }
            if (a.equals("at")) {
                doReload(bits.str2time(cfgAll.timeZoneName, cmd.getRemaining()));
                return cmdRes.command;
            }
            cmd.badCmd();
            return cmdRes.command;
        }
        cmd.badCmd();
        return cmdRes.command;
    }

    /**
     * get result text
     *
     * @param b status
     * @return text
     */
    public static String doneFail(boolean b) {
        if (b) {
            return "failed";
        } else {
            return "success";
        }
    }

    private boolean need2stop() {
        if (pipe.isClosed() != 0) {
            return true;
        }
        boolean brk = false;
        for (; pipe.ready2rx() > 0;) {
            byte[] buf = new byte[1];
            pipe.nonBlockGet(buf, 0, buf.length);
            brk |= buf[0] == 3;
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
            hlp = cfg.getHelping(true, true);
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
            a = "no " + a;
        }
        cfg.executeCommand(a);
        if (last) {
            return;
        }
        s = cmd.getRemaining();
        hlp = cfg.getHelping(true, true);
        reader.setContext(hlp, "");
        a = hlp.repairLine(s);
        if (hlp.endOfCmd(a)) {
            reader.putStrArr(hlp.getHelp(s, true));
            return;
        }
        if (negated) {
            a = "no " + a;
        }
        cfg.executeCommand(a);
    }

    private void doMenu() {
        String a = cmd.word();
        cfgMenu ntry = cfgAll.menuFind(a, false);
        if (ntry == null) {
            cmd.error("no such menu");
            return;
        }
        ntry.putMenu(cmd.pipe);
        a = cmd.pipe.strChr("choose:", ntry.getKeys());
        String s = ntry.findKey(a);
        if (s == null) {
            return;
        }
        if (s.length() < 1) {
            return;
        }
        userExec exe = new userExec(cmd.pipe, reader);
        exe.privileged = privileged;
        s = exe.repairCommand(s);
        cmd.pipe.linePut(a + " - " + s);
        if (pipe.settingsGet(pipeSetting.logging, false)) {
            logger.info("command menu:" + s + " from " + pipe.settingsGet(pipeSetting.origin, "?"));
        }
        exe.executeCommand(s);
    }

    private void doPortscan() {
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int timeout = 1000;
        int min = 1;
        int max = 1024;
        int proto = 0;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("/vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("/interface")) {
                ifc = cfgAll.ifcFind(cmd.word(), false);
                continue;
            }
            if (a.equals("/ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("/ipv6")) {
                proto = 6;
                continue;
            }
            if (a.equals("/timeout")) {
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/min")) {
                min = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/max")) {
                max = bits.str2num(cmd.word());
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        addrIP trg = userTerminal.justResolv(rem, proto);
        if (trg == null) {
            cmd.error("bad host");
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
        trc.tim = timeout;
        pipe.linePut("scanning " + trg + ", src=" + src + ", vrf=" + vrf.name + ", ran=" + min + ".." + max + ", tim=" + timeout);
        for (int i = min; i < max; i++) {
            if (need2stop()) {
                break;
            }
            pipe.strPut("" + i);
            pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
            if (trc.testOne(i)) {
                continue;
            }
            pipe.linePut("port " + i + " open");
        }
    }

    private static int adjustSize(addrIP adr) {
        if (adr.isIPv4()) {
            return ipCor4.size + ipIcmp4.size;
        } else {
            return ipCor6.size + ipIcmp6.size;
        }
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
            if (a.equals("/vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("/interface")) {
                ifc = cfgAll.ifcFind(cmd.word(), false);
                continue;
            }
            if (a.equals("/timeout")) {
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/port")) {
                port = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/protocol")) {
                proto = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/delay")) {
                delay = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/ipv4")) {
                ipver = 4;
                continue;
            }
            if (a.equals("/ipv6")) {
                ipver = 6;
                continue;
            }
            if (a.equals("/tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/flow")) {
                flow = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/size")) {
                len = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/router")) {
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
            if (a.equals("/lookup")) {
                resolv = true;
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        addrIP trg = userTerminal.justResolv(rem, ipver);
        if (trg == null) {
            cmd.error("bad host");
            return;
        }
        if (timeout < 1) {
            timeout = 1;
        }
        clntTrace trc = new clntTrace();
        trc.vrf = vrf;
        trc.ifc = ifc;
        trc.trg = trg;
        trc.port = port;
        trc.proto = proto;
        if (trc.register2ip()) {
            cmd.error("bind error");
            return;
        }
        addrIP src = null;
        if (ifc != null) {
            src = ifc.getLocAddr(trg);
        }
        pipe.linePut("tracing " + trg + ", src=" + src + ", vrf=" + vrf.name + ", prt=" + proto + "/" + port + ", tim=" + timeout + ", tos=" + tos + ", flow=" + flow + ", len=" + len);
        len -= adjustSize(trg);
        int none = 0;
        for (int ttl = 1; ttl < 255; ttl++) {
            if (need2stop()) {
                break;
            }
            if (delay > 0) {
                bits.sleep(delay);
            }
            trc.doRound(ttl, tos, flow, timeout, len);
            String a = "";
            if (trc.errLab > 0) {
                a += ", mpls=" + trc.errLab;
            }
            if (resolv && (trc.errRtr != null)) {
                clntDns clnt = new clntDns();
                clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(trc.errRtr), false, packDnsRec.typePTR);
                String nam = clnt.getPTR();
                a += ", name=" + nam;
            }
            if ((rtr != null) && (trc.errRtr != null)) {
                tabRouteEntry<addrIP> ntry = rtr.routerComputedU.route(trc.errRtr);
                if (ntry != null) {
                    a += ", path=" + ntry.best.asPathStr();
                }
            }
            pipe.linePut(ttl + " " + trc.errRtr + " time=" + trc.errTim + a);
            if (none >= 8) {
                break;
            }
            if (trc.errRtr == null) {
                none++;
                continue;
            }
            none = 0;
            if (trg.compare(trg, trc.errRtr) == 0) {
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
        int tos = 0;
        int len = 64;
        int prt = -1;
        boolean lok = false;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("/vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("/interface")) {
                ifc = cfgAll.ifcFind(cmd.word(), false);
                continue;
            }
            if (a.equals("/timeout")) {
                tim = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/ttl")) {
                ttl = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/size")) {
                len = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/port")) {
                prt = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/lookup")) {
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
        pipe.linePut("scanning " + strt + ", src=" + src + ", vrf=" + vrf.name + ", inc=" + incr + ", num=" + numb + ", tim=" + tim + ", len=" + len);
        len -= adjustSize(strt);
        for (;;) {
            if (need2stop()) {
                break;
            }
            pipe.strPut("" + strt);
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
                if (!trc.testOne(prt)) {
                    pipe.linePut(strt + a + " is open.");
                }
                continue;
            }
            ipFwd fwd = vrf.getFwd(strt);
            ipFwdEcho ping = fwd.echoSendReq(src, strt, len, ttl, tos, 0, 0, false);
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
        long beg = bits.getTime();
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int size = 64;
        int data = 0;
        int timeout = 1000;
        int repeat = 5;
        int tos = 0;
        int flow = 0;
        int ttl = 255;
        int proto = 0;
        int delay = 0;
        boolean detail = false;
        boolean sweep = false;
        boolean multi = false;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("/multi")) {
                multi = true;
                continue;
            }
            if (a.equals("/sweep")) {
                sweep = true;
                continue;
            }
            if (a.equals("/data")) {
                data = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/detail")) {
                detail = true;
                continue;
            }
            if (a.equals("/vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("/interface")) {
                ifc = cfgAll.ifcFind(cmd.word(), false);
                continue;
            }
            if (a.equals("/timeout")) {
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/delay")) {
                delay = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/size")) {
                size = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/repeat")) {
                repeat = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/ttl")) {
                ttl = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/flow")) {
                flow = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("/ipv6")) {
                proto = 6;
                continue;
            }
        }
        if (vrf == null) {
            cmd.error("vrf not specified");
            return;
        }
        addrIP trg = userTerminal.justResolv(rem, proto);
        if (trg == null) {
            cmd.error("bad host");
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
        int tiMin = timeout * 10;
        int tiMax = 0;
        int tiSum = 0;
        pipe.linePut("pinging " + trg + ", src=" + src + ", vrf=" + vrf.name + ", cnt=" + repeat + ", len=" + size + ", tim=" + timeout + ", gap=" + delay + ", ttl=" + ttl + ", tos=" + tos + ", flow=" + flow + ", fill=" + data + ", sweep=" + sweep + ", multi=" + multi + ", detail=" + detail);
        size -= adjustSize(trg);
        for (int i = 0; i < repeat; i++) {
            if (sweep) {
                size++;
            }
            if (delay > 0) {
                bits.sleep(delay);
            }
            if (need2stop()) {
                pipe.strPut("*");
                break;
            }
            sent++;
            ipFwdEcho ping = fwd.echoSendReq(src, trg, size, ttl, tos, flow, data, multi);
            if (ping == null) {
                pipe.strPut("N");
                continue;
            }
            if (timeout < 1) {
                lost++;
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
                pipe.strPut(".");
                continue;
            }
            if (ping.res.size() < 1) {
                lost++;
                pipe.strPut(".");
                continue;
            }
            for (int o = 0; o < ping.res.size(); o++) {
                ipFwdEchod res = ping.res.get(o);
                if (res.err != null) {
                    errs++;
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
                tiSum += res.tim;
                if (res.tim < tiMin) {
                    tiMin = res.tim;
                }
                if (res.tim > tiMax) {
                    tiMax = res.tim;
                }
                if (detail) {
                    pipe.strPut(res.tim + "ms" + "@" + res.rtr + " ");
                    continue;
                }
                pipe.strPut("!");
            }
            if (detail && multi) {
                pipe.linePut("");
            }
        }
        pipe.linePut("");
        int perc = 0;
        if (recv > 0) {
            tiSum /= recv;
        }
        if (sent > 0) {
            perc = (recv * 100) / sent;
        }
        pipe.linePut("result=" + perc + "%, recv/sent/lost/err=" + recv + "/"
                + sent + "/" + lost + "/" + errs + ", rtt min/avg/max/total="
                + tiMin + "/" + tiSum + "/" + tiMax + "/" + (bits.getTime() - beg));
    }

    private void doListen() {
        int port = bits.str2num(cmd.word());
        int trns = servGeneric.protoTcp;
        int proto;
        if (cfgAll.preferIpv6) {
            proto = 6;
        } else {
            proto = 4;
        }
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        addrIP rem = null;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("/vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("/interface")) {
                ifc = cfgAll.ifcFind(cmd.word(), false);
                continue;
            }
            if (a.equals("/tcp")) {
                trns = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("/udp")) {
                trns = servGeneric.protoUdp;
                continue;
            }
            if (a.equals("/ludp")) {
                trns = servGeneric.protoLudp;
                continue;
            }
            if (a.equals("/dccp")) {
                trns = servGeneric.protoDccp;
                continue;
            }
            if (a.equals("/sctp")) {
                trns = servGeneric.protoSctp;
                continue;
            }
            if (a.equals("/ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("/ipv6")) {
                proto = 6;
                continue;
            }
            if (a.equals("/remote")) {
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
        prtAccept acc = new prtAccept(prt, new pipeLine(65535, false), ipi, port, rem, 0, "listen", null, 0);
        acc.wait4conn(60000);
        pipeSide conn = acc.getConn(true);
        if (conn == null) {
            cmd.error("timed out");
            return;
        }
        pipeTerm trm = new pipeTerm(pipe, conn);
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
        int proto = 0;
        int dgrm = servGeneric.protoTcp;
        for (;;) {
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("/vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                ifc = null;
                continue;
            }
            if (a.equals("/chat")) {
                cht = cfgAll.chatFind(cmd.word(), false);
                continue;
            }
            if (a.equals("/user")) {
                user = cmd.word();
                continue;
            }
            if (a.equals("/pass")) {
                pass = cmd.word();
                continue;
            }
            if (a.equals("/interface")) {
                ifc = cfgAll.ifcFind(cmd.word(), false);
                continue;
            }
            if (a.equals("/proxy")) {
                cfgProxy prox = cfgAll.proxyFind(cmd.word(), false);
                if (prox == null) {
                    continue;
                }
                prx = prox.proxy;
                continue;
            }
            if (a.equals("/tcp")) {
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("/udp")) {
                dgrm = servGeneric.protoUdp;
                continue;
            }
            if (a.equals("/ludp")) {
                dgrm = servGeneric.protoLudp;
                continue;
            }
            if (a.equals("/dccp")) {
                dgrm = servGeneric.protoDccp;
                continue;
            }
            if (a.equals("/sctp")) {
                dgrm = servGeneric.protoSctp;
                continue;
            }
            if (a.equals("/ssh")) {
                secur = servGeneric.protoSsh;
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("/tls")) {
                secur = servGeneric.protoTls;
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("/dtls")) {
                secur = servGeneric.protoDtls;
                dgrm = servGeneric.protoUdp;
                continue;
            }
            if (a.equals("/telnet")) {
                secur = servGeneric.protoTelnet;
                dgrm = servGeneric.protoTcp;
                continue;
            }
            if (a.equals("/ipv4")) {
                proto = 4;
                continue;
            }
            if (a.equals("/ipv6")) {
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
        userTerminal term = new userTerminal(new pipeProgress(pipe));
        addrIP adr = term.resolveAddr(rem, proto);
        if (prx == null) {
            prx = clntProxy.makeTemp(vrf, ifc);
        }
        pipeSide strm = term.startConn(prx, dgrm, adr, prt, "telnet");
        if (strm == null) {
            return;
        }
        strm = term.startSecurity(secur, user, pass);
        if (strm == null) {
            return;
        }
        if (cht != null) {
            cht.script.doScript(strm);
        }
        pipeTerm trm = new pipeTerm(pipe, strm);
        trm.doTerm();
    }

    private void doAttach() {
        String a = cmd.word();
        if (a.equals("chat")) {
            userChat c = new userChat(pipe, reader);
            c.doChat();
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
            pipeTerm trm = new pipeTerm(pipe, pl.getSide());
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
            pipeTerm trm = new pipeTerm(pipe, pl.getSide());
            trm.doTerm();
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
            pipeTerm trm = new pipeTerm(pipe, pl.getSide());
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
            pipeTerm trm = new pipeTerm(pipe, pl.getSide());
            trm.doTerm();
            return;
        }
        if (a.equals("shell1")) {
            pipeShell sh = pipeShell.exec(pipe, cmd.getRemaining(), null, false, true);
            if (sh == null) {
                return;
            }
            sh.waitFor();
            return;
        }
        if (a.equals("shell2")) {
            reader.putStrArr(pipeShell.exec(cmd.getRemaining(), null, true));
            return;
        }
        if (a.equals("line")) {
            cfgLin lin = cfgAll.linFind(cmd.word());
            if (lin == null) {
                cmd.error("no such line");
                return;
            }
            pipeTerm trm = new pipeTerm(pipe, lin.runner.doAttach());
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

    private void doTerminal() {
        String a = cmd.word();
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
        if (a.equals("colorized")) {
            pipe.settingsPut(pipeSetting.colors, true);
            return;
        }
        if (a.equals("spacetab")) {
            pipe.settingsPut(pipeSetting.spacTab, true);
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
        if (a.equals("deactivate")) {
            pipe.settingsPut(pipeSetting.deactive, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("tablemode")) {
            pipe.settingsPut(pipeSetting.tabMod, userFormat.str2tabmod(cmd.word()));
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("monitor")) {
            logger.pipeStop(pipe);
            return;
        }
        if (a.equals("timestamps")) {
            pipe.settingsPut(pipeSetting.times, false);
            return;
        }
        if (a.equals("colorized")) {
            pipe.settingsPut(pipeSetting.colors, false);
            return;
        }
        if (a.equals("spacetab")) {
            pipe.settingsPut(pipeSetting.spacTab, false);
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

    private void doReload(long at) {
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
        cfgAll.reload = new userReload(at);
    }

    private pipeSide getShPipe(boolean col) {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, null);
        pip.settingsPut(pipeSetting.width, pipe.settingsGet(pipeSetting.width, 80));
        pip.settingsPut(pipeSetting.height, 0);
        pip.settingsPut(pipeSetting.tabMod, pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal));
        pip.settingsPut(pipeSetting.times, pipe.settingsGet(pipeSetting.times, false));
        pip.settingsPut(pipeSetting.colors, pipe.settingsGet(pipeSetting.colors, false) & col);
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
        final boolean color = pipe.settingsGet(pipeSetting.colors, false);
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            String a = getShPipe(true).strGet(1024 * 1024);
            userScreen.sendCur(pipe, 0, 0);
            userScreen.sendCls(pipe);
            if (color) {
                userScreen.sendCol(pipe, userScreen.colBrGreen);
            }
            pipe.linePut(cfgAll.hostName + "#show " + cmd.getRemaining());
            if (color) {
                userScreen.sendCol(pipe, userScreen.colWhite);
            }
            if (pipe.settingsGet(pipeSetting.times, false)) {
                pipe.linePut(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3));
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
        if (cfgAll.configBackup == null) {
            return;
        }
        cmd.error("backing up configuration");
        String a = cfgAll.configBackup;
        if (a.length() < 1) {
            a = cfgInit.cfgFileSw;
            int i = a.lastIndexOf(".");
            if (i > 0) {
                a = a.substring(0, i);
            }
            a = a + ".bak";
        }
        List<String> old = bits.txt2buf(cfgInit.cfgFileSw);
        if (old == null) {
            cmd.error("error reading file");
            return;
        }
        boolean b = bits.buf2txt(true, old, a);
        cmd.error(doneFail(b));
    }

}
