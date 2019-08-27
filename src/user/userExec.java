package user;

import addr.addrIP;
import auth.authGeneric;
import auth.authLocal;
import auth.authResult;
import cfg.cfgAlias;
import cfg.cfgAll;
import cfg.cfgChat;
import cfg.cfgIfc;
import cfg.cfgInit;
import cfg.cfgLin;
import cfg.cfgProxy;
import cfg.cfgVdc;
import cfg.cfgPrcss;
import cfg.cfgVrf;
import cfg.cfgMenu;
import cfg.cfgRtr;
import cfg.cfgSched;
import cfg.cfgScrpt;
import clnt.clntDns;
import clnt.clntPorts;
import clnt.clntProxy;
import clnt.clntTrace;
import clnt.clntWhois;
import ifc.ifcNull;
import ip.ipFwd;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import cry.cryBase64;
import cry.cryHashMd5;
import cry.cryHashSha1;
import cry.cryHashSha2256;
import cry.cryHashSha2512;
import ip.ipRtr;
import line.lineHdlc;
import pack.packDnsRec;
import pack.packDnsZone;
import pack.packText;
import pipe.pipeLine;
import pipe.pipeProgress;
import pipe.pipeShell;
import pipe.pipeSide;
import pipe.pipeTerm;
import prt.prtRedun;
import rtr.rtrBgpParam;
import serv.servGeneric;
import tab.tabRouteEntry;
import util.bits;
import util.cmds;
import util.debugger;
import util.differ;
import util.logger;
import util.notifier;
import util.uniResLoc;

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
        hl.add("3 4        bgp                   border gateway protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            <addr>            neighbor address");
        hl.add("6 .              hard            flap session");
        hl.add("6 7              in              send route refresh");
        hl.add("6 7              out             resend prefixes");
        rtrBgpParam.getAfiList(hl, "7 .", "clear", false);
        hl.add("5 .            recompute         trigger full compute round");
    }

    private void getHelpShowIpX(userHelping hl) {
        hl.add("3 .        interface             interface information");
        hl.add("3 4,.      vrf                   vrf information");
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
        hl.add("3 4        counter               unicast routing table traffic");
        hl.add("4 5,.        <vrf>               name of routing table");
        hl.add("5 .            [addr]            prefix to view");
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
        hl.add("4 .          <vrf>               name of routing table");
        hl.add("3 4        segrout               segment routing forwarding table entries");
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
        hl.add("5 6,.          database          list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 .            mpdatabase        list of multipoint sessions");
        hl.add("3 4        babel                 babel routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 6            neighbor          information about neighbor");
        hl.add("6 7              <addr>          neighbor address");
        hl.add("7 8,.              learned       routes learned from neighbor");
        hl.add("8 .                  [addr]      prefix to view");
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
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 6,.          route             list of routes in database");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
        hl.add("6 .              [addr]          prefix to view");
        hl.add("3 4        lsrp                  link state routing protocol");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            neighbor          list of neighbor");
        hl.add("5 .            interface         list of interfaces");
        hl.add("5 .            segrout           list of segment routing");
        hl.add("5 .            bier              list of bier");
        hl.add("5 .            software          list of software");
        hl.add("5 6            zonefile          list of reverse zone");
        hl.add("6 .              <name>          name of zone");
        hl.add("5 .            uptime            list of uptime");
        hl.add("5 6,.          database          list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 .            spf               information about last spf");
        hl.add("5 .            tree              tree about last spf");
        hl.add("5 .            graph             graph about last spf");
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
        hl.add("5 6            tree              tree about last spf");
        hl.add("6 .              <num>           area number");
        hl.add("5 6            graph             graph about last spf");
        hl.add("6 .              <num>           area number");
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
        hl.add("5 6            tree              tree about last spf");
        hl.add("6 .              <num>           level number");
        hl.add("5 6            graph             graph about last spf");
        hl.add("6 .              <num>           level number");
        hl.add("5 6            topology          topology about last spf");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 .                [addr]        node to view");
        hl.add("5 6            route             list of routes in area");
        hl.add("6 7,.            <num>           level number");
        hl.add("7 .                [addr]        prefix to view");
        hl.add("5 6,.          originate         list of routes originated locally");
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
        hl.add("6 7,.            database        list of routes in database");
        hl.add("7 8,.              [addr]        prefix to view");
        hl.add("8 .                  [rd]        route distinguisher");
        hl.add("6 7              stdcomm         list of routes with community");
        hl.add("7 8,.              <str>         community");
        hl.add("8 8,.                [str]       community");
        hl.add("6 7              extcomm         list of routes with community");
        hl.add("7 8,.              <str>         community");
        hl.add("8 8,.                [str]       community");
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
        hl.add("6 .              labels          remote and local labels");
        hl.add("6 .              asgraph         connectivity graph of ases");
        hl.add("6 .              asconn          connectivity of ases");
        hl.add("6 .              asinconsistent  inconsistent advertisements of ases");
        hl.add("6 .              prefix-lengths  prefix length distribution");
        hl.add("6 7              allroute        list of specific routes");
        hl.add("7 8,.              <addr>        prefix to view");
        hl.add("8 .                  [rd]        route distinguisher");
        hl.add("6 7              compare         compare routes from neighbors");
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
        hl.add("9 9,.                  aigp      ignore accumulated igp");
        hl.add("9 9,.                  bandwidth ignore bandwidth");
        hl.add("9 9,.                  label     ignore labels");
        hl.add("9 9,.                  aggregate ignore aggregator");
        hl.add("9 9,.                  orignted  ignore originator");
        hl.add("9 9,.                  pmsi      ignore pmsi");
        hl.add("9 9,.                  segrout   ignore segment routing");
        hl.add("9 9,.                  tunnel    ignore tunnel");
        hl.add("3 4        logger                route logger");
        hl.add("4 5          <num>               process id");
        hl.add("5 .            prefix-lengths    list of prefix lengths");
        hl.add("5 6,.          unicast           list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 6,.          multicast         list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 6,.          flowspec          list of database");
        hl.add("6 .              [addr]          entry to view");
        hl.add("5 6,.          flapstat          prefix flap statistics");
        hl.add("6 .              [num]           minimum count");
    }

    private void getHelpShow(userHelping hl) {
        hl.add("2 .      scheduler               scheduler information");
        hl.add("2 .      script                  script information");
        hl.add("2 3      vdc                     virtual device context");
        hl.add("3 .        interface             list of physical interfaces");
        hl.add("3 .        device                list of running devices");
        hl.add("2 .      reload                  scheduled reload");
        hl.add("2 .      banner                  logo of device");
        hl.add("2 3,.    logo                    logo of product");
        hl.add(".3 3,.     [text]                text to print");
        hl.add("2 3,.    flash                   list of flash");
        hl.add("3 3,.      [text]                directory to print");
        hl.add("2 3      whois                   query whois server");
        hl.add("3 3,.      [text]                directory to print");
        hl.add("2 3      transproxy              transparent proxy connections");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 3,.    version                 version information");
        hl.add("3 .        number                just numbers");
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
        hl.add("3 .        hardware              hardware watchdog information");
        hl.add("3 .        software              software watchdog information");
        hl.add("3 .        timer                 timer history information");
        hl.add("3 .        memory                memory history information");
        hl.add("2 3,.    interfaces              interface status and configuration");
        hl.add("3 .        full                  full listing");
        hl.add("3 .        description           description listing");
        hl.add("3 .        summary               summary listing");
        hl.add("3 .        total                 total listing");
        hl.add("3 .        traffic               traffic listing");
        hl.add("3 .        psummary              packet summary listing");
        hl.add("3 .        ptotal                packet total listing");
        hl.add("3 .        ptraffic              packet traffic listing");
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
        hl.add("4 .          counters            counters");
        hl.add("4 .          ethertypes          ethernet types");
        hl.add("4 .          lossdetect          loss detection");
        hl.add("4 .          packetsizes         packet sizes");
        hl.add("4 .          protocols           protocol numbers");
        hl.add("4 .          trafficclasses      traffic classes");
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
        hl.add("3 4        <name>                name of interface");
        hl.add("4 .          in                  ingress policy");
        hl.add("4 .          out                 egress policy");
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
        hl.add("3 4        last                  last messages");
        hl.add("4 .          <num>               message count");
        hl.add("2 .      config-differences      startup->running differences");
        hl.add("2 .      rollback-config         running->startup differences");
        hl.add("2 3,.    startup-config          startup configuration");
        hl.add("3 3,.      [name]                name of section");
        hl.add("2 3,.    running-config          current operating configuration");
        hl.add("3 3,.      [name]                name of section");
        hl.add("3 4        interface             specified interface");
        hl.add("4 .          <name>              name of interface");
        hl.add("3 .        all                   do not hide defaults");
        hl.add("2 3,.    vrf                     virtual routing/forwarding information");
        hl.add("3 4        [name]                name of vrf");
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
        hl.add("2 3      bmp                     bgp monitoring protocol information");
        hl.add("3 4,.      <name>                bmp name");
        hl.add("4 5          <addr>              from");
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
        getHelpShow(hl);
        hl.possible(-1, 110);
        hl.add("110 111  |                       output modifier");
        hl.add("111 112    include               only lines that match");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    exclude               hide lines that match");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    begin                 only lines from match");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    end                   only lines to match");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    sort                  sort lines by");
        hl.add("112 112,.    <text>              column name");
        hl.add("111 112    uniq                  unique lines by");
        hl.add("112 112,.    <text>              column name");
        hl.add("111 112    section               only sections that match");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    reginc                only lines that match regular expression");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    regexc                hide lines that match regular expression");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    regbeg                only lines from match regular expression");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    regend                only lines to match regular expression");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    regsec                only sections that match regular expression");
        hl.add("112 112,.    <text>              filter text");
        hl.add("111 112    redirect              redirect output to file");
        hl.add("112 .        <text>              name of file");
        hl.add("111 .      headers               only section headers");
        hl.add("111 .      count                 count entities");
        hl.add("111 .      viewer                display in viewer");
        hl.add("111 .      level                 raw level hierarchy");
        hl.add("111 .      csv                   level hierarchy in csv");
        hl.add("111 .      html                  level hierarchy in html");
        hl.add("111 .      linenumbers           prepend lines with numbers");
        hl.add("111 112    first                 only first some lines");
        hl.add("112 .        <num>               number of lines");
        hl.add("111 112    last                  only last some lines");
        hl.add("112 .        <num>               number of lines");
        hl.add("111 .      raw                   unfiltered");
        hl.add("1 2    watch                     running system periodic information");
        getHelpShow(hl);
        hl.add("1 2    view                      running system information");
        getHelpShow(hl);
        hl.add("1 2    display                   running system periodic information");
        getHelpShow(hl);
        hl.add("1 2    differs                   running system difference information");
        getHelpShow(hl);
        hl.add("1 .    logout                    close this exec session");
        hl.add("1 .    exit                      close this exec session");
        hl.add("1 .    ppp                       start framed session");
        hl.add("1 .    modememu                  start modem emulation session");
        hl.add("1 .    gpsemu                    start gps emulation session");
        hl.add("1 .    gpstime                  start gps session");
        hl.add("1 .    nullemu                   start null session");
        hl.add("1 2    menu                      start menu session");
        hl.add("2 .      <name>                  name of menu");
        hl.add("1 2    terminal                  terminal specific parameters");
        hl.add("2 2      no                      negate a parameter");
        hl.add("2 3      width                   set terminal width");
        hl.add("3 .        <num>                 width in columns");
        hl.add("2 3      length                  set terminal length");
        hl.add("3 .        <num>                 height in lines");
        hl.add("2 .      monitor                 log to this terminal");
        hl.add("2 .      timestamps              put time before each executed command");
        hl.add("2 .      colorized               sending to ansi terminal");
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
        hl.add("3 4        /port                 specify port value");
        hl.add("4 3,.        <num>               port");
        hl.add("3 4        /size                 specify payload size");
        hl.add("4 3,.        <num>               byte count");
        hl.add("3 4        /router               lookup intermediate hops");
        cfgRtr.getRouterList(hl, 2, "");
        hl.add("5 3,.          <num>             process id");
        hl.add("3 3,.      /lookup               lookup intermediate hops");
        hl.add("1 2    ping                      send echo request");
        hl.add("2 3,.    <host>                  name of host");
        hl.add("3 3,.      /flood                specify flood mode");
        hl.add("3 3,.      /detail               specify detail mode");
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
        hl.add("1 2    sleep                     do nothing for a while");
        hl.add("2 .      <num>                   seconds for sleep");
        hl.add("1 2    whois                     perform whois query");
        hl.add("2 3      <host>                  name of host to query");
        hl.add("3 3,.      <text>                query string");
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
        hl.add("2 3,.    counters                counters on one or more interfaces");
        hl.add("3 .        <name>                name of interface");
        hl.add("2 3      dial-peer               clear voip call");
        hl.add("3 4        <num>                 number of dial peer");
        hl.add("4 .          <str>               call id");
        hl.add("2 3      scheduler               run one scheduler round");
        hl.add("3 .        <name>                name of scheduler");
        hl.add("2 3      script                  run one script round");
        hl.add("3 .        <name>                name of script");
        hl.add("2 3      vpdn                    reconnect vpdn peer");
        hl.add("3 4,.      <name>                name of vpdn");
        hl.add("4 .          [num]               downtime in seconds");
        hl.add("2 3      vdc                     restart vdc process");
        hl.add("3 4,.      <name>                name of vdc");
        hl.add("4 .          stop                stop");
        hl.add("4 .          start               start");
        hl.add("2 3      process                 restart external process");
        hl.add("3 .        <name>                name of process");
        hl.add("2 .      logging                 logged messages");
        hl.add("2 .      auto-bandwidth          set auto bandwidth values");
        hl.add("2 .      tunnel-domain           resolve destination domain names");
        hl.add("2 .      name-cache              dns local cache");
        hl.add("2 3,.    watchdog                watchdog");
        hl.add("3 3,.      [name]                parameter");
        hl.add("2 3      line                    disconnect line");
        hl.add("3 .        <name>                name of line");
        hl.add("2 3      interface               disconnect interface");
        hl.add("3 4,.      <name>                name of interface");
        hl.add("4 .          [num]               downtime in seconds");
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
        hl.add("2 3      file                    from disk file");
        hl.add("3 .        <file>                source file");
        hl.add("2 .      banner                  edit the banner");
        hl.add("2 .      startup                 edit the startup configuration");
        hl.add("2 3,.    editor                  configure from editor");
        hl.add("3 3,.      [name]                section name");
        hl.add("2 3,.    viewer                  view current configuration");
        hl.add("3 3,.      [name]                section name");
        hl.add("2 .      terminal                configure from this terminal");
        hl.add("2 3      reload                  configure the startup configuration");
        hl.add("3 3,.      <url>                 source url");
        hl.add("2 3      network                 configure the running configuration");
        hl.add("3 3,.      <url>                 source url");
        hl.add("2 .      rollback                configure within auto-revert session");
        hl.add("2 .      revert                  revert to startup configuration");
        hl.add(".2 .     reapply                 try to reapply current configuration");
        hl.add("1 2    flash                     file system utility");
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
        hl.add("2 3      list                    list directory");
        hl.add("3 .        <file>                pathname");
        hl.add("2 3      type                    type one ascii file");
        hl.add("3 .        <file>                filename");
        hl.add("2 3      info                    information about file");
        hl.add("3 .        <file>                filename");
        hl.add("2 3      hash                    hash of file");
        hl.add("3 .        <file>                filename");
        hl.add("2 3      disk                    information about disk");
        hl.add("3 .        <file>                filename");
        hl.add("2 3      bintype                 type one binary file");
        hl.add("3 .        <file>                filename");
        hl.add("2 3,.    upgrade                 upgrade routing software");
        hl.add("3 3,.      [url]                 parameter of process");
        hl.add("2 3,.    simulate                simulate upgrade process");
        hl.add("3 3,.      [url]                 parameter of process");
        hl.add("2 .      verify                  verify routing software");
        hl.add("2 3      editor                  compose text file");
        hl.add("3 .        <name>                name of file");
        hl.add("2 3      viewer                  read text file");
        hl.add("3 .        <name>                name of file");
        hl.add("2 3      binviewer               view one binary file");
        hl.add("3 .        <file>                filename");
        hl.add("2 .      commander               file manager");
        hl.add("2 3,.    browser                 web browser");
        hl.add("3 .        <url>                 target url");
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
        hl.add("3 4,.      <cmd>                 name of process");
        hl.add("4 4,.        [param]             parameter of process");
        hl.add(".2 3     shell2                  run outputting shell process");
        hl.add("3 4,.      <cmd>                 name of process");
        hl.add("4 4,.        [param]             parameter of process");
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
        hl.add("3 4        <name>                name of vrf");
        hl.add("4 5          tcp                 select tcp");
        hl.add("4 5          udp                 select udp");
        hl.add("4 5          ludp                select ludp");
        hl.add("4 5          dccp                select dccp");
        hl.add("4 5          sctp                select sctp");
        hl.add("5 6            <addr>            source address");
        hl.add("6 7              <num>           source port");
        hl.add("7 8                <addr>        target address");
        hl.add("8 9                  <num>       target port");
        hl.add("9 10                   <num>     flags");
        hl.add("10 .                     <num>   size");
        hl.add(".2 3      replay                 replay capture on interface");
        hl.add("3 4        <name>                name of interface");
        hl.add("4 5,.        <file>              name of file");
        hl.add("5 .            [num]             interpacket gap in millisecs");
        hl.add(".2 3      inject                 inject packet to interface");
        hl.add("3 4        <name>                name of interface");
        hl.add("4 4,.        [byte]              byte in hex");
        hl.add("2 3      wakeup                  wake up one host");
        hl.add("3 4        <name>                name of interface");
        hl.add("4 .          <addr>              address of host");
        hl.add(".2 3     mrt2self                replay mrt as if got from a peer");
        hl.add("3 4        <name>                process name");
        hl.add("4 5          <num>               process number");
        hl.add("5 6            <addr>            peer address");
        hl.add("6 7              <file>          mrt file");
        hl.add("7 8                <addr>        source peer");
        hl.add("8 .                  <addr>      target peer");
        hl.add("2 3      mrt2pcap                convert mrt to pcap");
        hl.add("3 4        <file>                name of mrt file");
        hl.add("4 .          <file>              name of pcap file");
        hl.add(".2 3     mrtplay                 run mrt prefix sender");
        hl.add("3 4        <name>                vrf name");
        hl.add("4 5          <name>              source interface name");
        hl.add("5 6            <addr>            target address");
        hl.add("6 7              <num>           local as");
        hl.add("7 8                <file>        mrt file");
        hl.add("8 9                  <addr>      source peer");
        hl.add("9 10,.                 <addr>    target peer");
        hl.add("10 .                     [num]   safi number");
        hl.add(".2 3     random                  run random packet generator");
        hl.add("3 4        <name>                name of interface");
        hl.add("4 4,.        [byte]              byte in hex");
        hl.add(".2 3     bgpattr                 run attribute injector");
        hl.add("3 4        <name>                vrf name");
        hl.add("4 5          <name>              source interface name");
        hl.add("5 6            <addr>            target address");
        hl.add("6 7              <num>           local as");
        hl.add("7 8                <addr>        prefix to originate");
        hl.add("8 9                  <name>      route map to apply");
        hl.add("9 9,.                  <num>     attribute byte");
        hl.add(".2 3     bgpgen                  run random prefix generator");
        hl.add("3 4        <name>                vrf name");
        hl.add("4 5          <name>              source interface name");
        hl.add("5 6            <addr>            target address");
        hl.add("6 7              <num>           local as");
        hl.add("7 8                <addr>        prefix to originate");
        hl.add("8 9                  <name>      route map to apply");
        hl.add("9 .                    <num>     number of prefixes");
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
        hl.add("5 5,.          <txt>             message text");
        hl.add("2 3      conference              start voice conference");
        hl.add("3 4,.      <addr>                address who calling");
        hl.add("4 4,.        <addr>              address to call");
        hl.add("2 3      speed                   test speed clients");
        hl.add("3 3,.      <str>                 name of server");
        hl.add("2 3      websock                 test websocket client");
        hl.add("3 4        <str>                 url of server");
        hl.add("4 4,.        <str>               protocols");
        hl.add("2 3      snmp                    do snmp commands");
        hl.add("3 4        get                   do a get request");
        hl.add("3 4        next                  do a getnext request");
        hl.add("4 5          <addr>              server to query");
        hl.add("5 6            <txt>             community to use");
        hl.add("6 .              <oid>           oid to query");
        hl.add("2 3      smtp                    send email message");
        hl.add("3 4,.      <str>                 email address");
        hl.add("4 4,.        <str>               email text");
        hl.add("2 3      nrpe                    check remote status");
        hl.add("3 4        <str>                 server address");
        hl.add("4 4,.        <str>               check name");
        hl.add("1 2    test                      test various things");
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
        hl.add("2 .      tls                     test tls throughput");
        hl.add("2 .      dtls                    test dtls throughput");
        hl.add(".2 .     gc                      run garbage collector");
        hl.add("2 .      crypto                  test encryption and hash");
        hl.add("2 3,.    digsig                  test digital signatures");
        hl.add("3 3,.      [str]                 parameters");
        hl.add("2 .      window                  test window handler");
        hl.add("2 3      vercore                 test vercore updater");
        hl.add("3 4        <key>                 key file to use");
        hl.add("4 .          <key>               key file to include");
        hl.add("2 3      verfile                 test version updater");
        hl.add("3 4,.      <key>                 key file to use");
        hl.add("4 4,.        [str]               file to include in release");
        hl.add("2 3,.    screen                  test screen handler");
        hl.add("3 .        gomoku                play game");
        hl.add("3 .        tetris                play game");
        hl.add("3 .        minesweep             play game");
        hl.add("3 .        clear                 clear screen");
        hl.add("3 .        color                 view demo");
        hl.add("3 .        ascii                 view demo");
        hl.add("3 .        clock                 view demo");
        hl.add("3 .        snake                 view demo");
        hl.add("3 .        fire                  view demo");
        hl.add("3 .        life                  view demo");
        hl.add("3 .        antball               view demo");
        hl.add("3 4,.      text                  view demo");
        hl.add("4 4,.        [str]               text");
        hl.add("3 4,.      logo                  view demo");
        hl.add("4 4,.        [str]               text");
        hl.add("3 4        image                 view image");
        hl.add("4 4,.        [str]               file");
        hl.add("2 3,.    hwdet                   perform hw detection");
        hl.add("3 3,.      <str>                 parameter");
        hl.add(".2 3,.   hwcfg                   perform hw configuration");
        hl.add("3 3,.      <str>                 parameter");
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
            String s = reader.readLine(reader.deactive);
            if (s == null) {
                return cmdRes.logout;
            }
            if (reader.timeStamp) {
                pipe.linePut(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3));
            }
            if (authorization != null) {
                authResult ntry = authorization.authUserCommand(username, s);
                if (ntry.result != authResult.authSuccessful) {
                    pipe.linePut("% not authorized to do that");
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
            a = alias.getCommand(cmd);
            a = repairCommand(a);
            executeCommand(a);
            return cmdRes.command;
        }
        if (a.equals("exit")) {
            return cmdRes.logout;
        }
        if (a.equals("logout")) {
            return cmdRes.logout;
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
        if (a.equals("nullemu")) {
            new userGps(pipe).doWorkRx();
            return cmdRes.command;
        }
        if (a.equals("lookup")) {
            doLookup();
            return cmdRes.command;
        }
        if (a.equals("telnet")) {
            doTelnet(0);
            return cmdRes.command;
        }
        if (a.equals("sleep")) {
            int o = bits.str2num(cmd.word());
            for (int i = 0; i < o; i++) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                bits.sleep(1000);
            }
            return cmdRes.command;
        }
        if (a.equals("whois")) {
            clntWhois w = new clntWhois(cmd.word());
            w.quest = cmd.getRemaining();
            w.doQuery(cmd);
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
                cmd.error("no way!");
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
            a = s.doer();
            if (a == null) {
                return cmdRes.command;
            }
            a = repairCommand(a);
            executeCommand(a);
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
            userClear s = new userClear();
            cmd = reader.setFilter(cmd);
            s.cmd = cmd;
            s.rdr = reader;
            a = s.doer();
            if (a == null) {
                return cmdRes.command;
            }
            a = repairCommand(a);
            executeCommand(a);
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
            doSetUnset(false);
            return cmdRes.command;
        }
        if (a.equals("delete")) {
            doSetUnset(true);
            return cmdRes.command;
        }
        if (a.equals("configure")) {
            if (cfgAll.configExclusive > 1) {
                cmd.error("operation forbidden by exclusive configuration mode");
                return cmdRes.command;
            }
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
                List<String> cfg = cfgAll.getShRun(true);
                int res = cfgInit.executeSWcommands(cfg, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                return cmdRes.command;
            }
            if (a.equals("file")) {
                cmd = reader.setFilter(cmd);
                List<String> cfg = bits.txt2buf(cmd.word());
                int res = cfgInit.executeSWcommands(cfg, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                return cmdRes.command;
            }
            if (a.equals("reload")) {
                userFlash.doReceive(pipe,
                        uniResLoc.parseOne(cmd.getRemaining()), new File(
                        cfgInit.cfgFileSw));
                return cmdRes.command;
            }
            if (a.equals("network")) {
                a = cfgInit.cfgFileSw + ".tmp";
                userFlash.doReceive(pipe,
                        uniResLoc.parseOne(cmd.getRemaining()), new File(a));
                List<String> cfg = bits.txt2buf(a);
                userFlash.delete(a);
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
                userEditor e = new userEditor(new userScreen(cmd.pipe, reader.width, reader.height), txt, "banner", false);
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
                c2.addAll(c1);
                userEditor e = new userEditor(new userScreen(pipe, reader.width, reader.height), c2, "section '" + a + "'", false);
                if (e.doEdit()) {
                    return cmdRes.command;
                }
                List<String> c3 = userFilter.getDiffs(c1, c2);
                reader.putStrArr(bits.lst2lin(c3, false));
                int res = cfgInit.executeSWcommands(c3, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                c3 = userFilter.getDiffs(c2, c1);
                reader.putStrArr(c3);
                return null;
            }
            if (a.equals("editor")) {
                List<String> c1 = cfgAll.getShRun(true);
                a = cmd.word();
                if (a.length() > 0) {
                    c1 = userFilter.getSection(c1, userReader.filter2reg(a + " " + cmd.getRemaining()));
                }
                List<String> c2 = new ArrayList<String>();
                c2.addAll(c1);
                userEditor e = new userEditor(new userScreen(pipe, reader.width, reader.height), c2, "section '" + a + "'", false);
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
                List<String> c1 = cfgAll.getShRun(true);
                a = cmd.word();
                if (a.length() > 0) {
                    c1 = userFilter.getSection(
                            c1,
                            userReader.filter2reg(a + " "
                                    + cmd.getRemaining()));
                }
                userEditor v = new userEditor(new userScreen(pipe, reader.width, reader.height), c1, "section '" + a + "'", false);
                v.doView();
                return cmdRes.command;
            }
            if (a.equals("revert")) {
                List<String> cfg = userFilter.getDiffs(cfgAll.getShRun(true),
                        bits.txt2buf(cfgInit.cfgFileSw));
                reader.putStrArr(bits.lst2lin(cfg, false));
                int res = cfgInit.executeSWcommands(cfg, false);
                reader.putStrArr(bits.str2lst("errors=" + res));
                return cmdRes.command;
            }
            cmd.badCmd();
            return cmdRes.command;
        }
        if (a.equals("flash")) {
            doFlash();
            return cmdRes.command;
        }
        if (a.equals("write")) {
            a = cmd.word();
            if (a.length() < 1) {
                a = "memory";
            }
            if (a.equals("erase")) {
                boolean b = bits.buf2txt(true, new ArrayList<String>(),
                        cfgInit.cfgFileSw);
                prtRedun.doNotify();
                cmd.error(doneFail(b));
                return cmdRes.command;
            }
            if (a.equals("terminal")) {
                cmd = reader.setFilter(cmd);
                reader.putStrArr(cfgAll.getShRun(true));
                return cmdRes.command;
            }
            if (a.equals("file")) {
                boolean b = bits.buf2txt(true, cfgAll.getShRun(true),
                        cmd.getRemaining());
                cmd.error(doneFail(b));
                return cmdRes.command;
            }
            if (a.equals("memory")) {
                List<String> old = null;
                if (cfgAll.configBackup != null) {
                    old = bits.txt2buf(cfgInit.cfgFileSw);
                }
                boolean b = bits.buf2txt(true, cfgAll.getShRun(true),
                        cfgInit.cfgFileSw);
                if ((cfgAll.configBackup != null) && (old != null)) {
                    bits.buf2txt(true, old, cfgAll.configBackup);
                }
                prtRedun.doNotify();
                cmd.error(doneFail(b));
                if (!cfgAll.configAbackup) {
                    return cmdRes.command;
                }
                a = "network";
            }
            if (a.equals("network")) {
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
                a = cfgInit.cfgFileSw + ".tmp";
                if (bits.buf2txt(true, cfgAll.getShRun(true), a)) {
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
            a = t.doer();
            if (a == null) {
                return cmdRes.command;
            }
            a = repairCommand(a);
            executeCommand(a);
            return cmdRes.command;
        }
        if (a.equals("packet")) {
            userPacket t = new userPacket();
            t.cmd = cmd;
            t.pip = pipe;
            t.rdr = reader;
            t.doer();
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
                prtRedun.doNotify();
                return cmdRes.command;
            }
            if (a.equals("cancel")) {
                doReload(-1);
                return cmdRes.command;
            }
            if (a.equals("force")) {
                cfgInit.stopRouter(true, 3, "user requested");
                return cmdRes.command;
            }
            if (userFilter.doDiffer(cfgAll.getShRun(true),
                    bits.txt2buf(cfgInit.cfgFileSw)) > 1) {
                String b = pipe.strChr("configuration not saved, proceed?",
                        "ynYN").toLowerCase();
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

    private String doneFail(boolean b) {
        if (b) {
            return "failed";
        } else {
            return "success";
        }
    }

    private boolean need2stop() {
        boolean brk = false;
        for (; pipe.ready2rx() > 0;) {
            byte[] buf = new byte[1];
            pipe.nonBlockGet(buf, 0, buf.length);
            brk |= buf[0] == 3;
        }
        return brk;
    }

    private void doSetUnset(boolean negated) {
        userConfig cfg = new userConfig(pipe, reader);
        userHelping hlp;
        String s = "";
        String a = "";
        boolean last;
        for (;;) {
            a = cmd.word();
            s = (s + " " + a).trim();
            hlp = cfg.getHelping();
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
            a = cmds.negated + a;
        }
        cfg.executeCommand(a);
        if (last) {
            return;
        }
        s = cmd.getRemaining();
        hlp = cfg.getHelping();
        reader.setContext(hlp, "");
        a = hlp.repairLine(s);
        if (hlp.endOfCmd(a)) {
            reader.putStrArr(hlp.getHelp(s, true));
            return;
        }
        if (negated) {
            a = cmds.negated + a;
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
        cmd.pipe.linePut("menu " + a + ":");
        for (int i = 0; i < ntry.letter.size(); i++) {
            cmd.pipe.linePut(ntry.letter.get(i) + "");
        }
        cmd.pipe.strPut("choose:");
        for (;;) {
            if (cmd.pipe.ready2rx() < 0) {
                return;
            }
            a = cmd.pipe.strGet(1);
            if (a == null) {
                return;
            }
            String s = ntry.findKey(a);
            if (s == null) {
                continue;
            }
            if (s.length() < 1) {
                continue;
            }
            userExec exe = new userExec(cmd.pipe, reader);
            exe.privileged = privileged;
            s = exe.repairCommand(s);
            cmd.pipe.strPut(a + " - " + s);
            if (reader.logging) {
                logger.info("command menu:" + s + " from " + reader.from);
            }
            exe.executeCommand(s);
            return;
        }
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
        clntPorts trc = new clntPorts();
        trc.vrf = vrf;
        trc.ifc = ifc;
        trc.trg = trg;
        trc.tim = timeout;
        pipe.linePut("scanning " + trg + ", ran=" + min + ".." + max + ", tim="
                + timeout);
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

    private void doTraceroute() {
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int timeout = 1000;
        int tos = 0;
        int len = 64;
        int proto = 0;
        int delay = 0;
        int port = 33440;
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
            if (a.equals("/delay")) {
                delay = bits.str2num(cmd.word());
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
            if (a.equals("/tos")) {
                tos = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/size")) {
                len = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("/router")) {
                tabRouteEntry.routeType typ = cfgRtr.name2num(cmd.word());
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
        addrIP trg = userTerminal.justResolv(rem, proto);
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
        trc.prt = port;
        if (trc.register2ip()) {
            cmd.error("bind error");
            return;
        }
        addrIP src = null;
        if (ifc != null) {
            src = ifc.getLocAddr(trg);
        }
        pipe.linePut("tracing " + trg + ", src=" + src + ", prt=" + port + ", tim=" + timeout + ", tos=" + tos + ", len=" + len);
        int none = 0;
        for (int ttl = 1; ttl < 255; ttl++) {
            if (need2stop()) {
                break;
            }
            if (delay > 0) {
                bits.sleep(delay);
            }
            addrIP adr = trc.doRound(ttl, tos, timeout, len);
            String a = "";
            if (resolv && (adr != null)) {
                clntDns clnt = new clntDns();
                clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(adr), packDnsRec.typePTR);
                a += " (" + clnt.getPTR() + ")";
            }
            if ((rtr != null) && (adr != null)) {
                tabRouteEntry<addrIP> ntry = rtr.routerComputedU.route(adr);
                if (ntry != null) {
                    a += " [" + ntry.asPathStr() + "]";
                }
            }
            pipe.linePut(ttl + " " + adr + a);
            if (none >= 8) {
                break;
            }
            if (adr == null) {
                none++;
                continue;
            }
            none = 0;
            if (trg.compare(trg, adr) == 0) {
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
        pipe.linePut("scanning " + strt + ", inc=" + incr + ", num=" + numb + ", tim=" + tim + ", len=" + len);
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
                clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(strt), packDnsRec.typePTR);
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
            addrIP src = null;
            if (ifc != null) {
                src = ifc.getLocAddr(strt);
            }
            notifier notif = fwd.echoSendReq(src, strt, len, ttl, tos);
            if (notif == null) {
                continue;
            }
            notif.sleep(tim);
            if (notif.totalNotifies() < 1) {
                continue;
            }
            pipe.linePut(strt + a + " is alive.");
        }
    }

    private void doPing() {
        long beg = bits.getTime();
        String rem = cmd.word();
        cfgVrf vrf = cfgAll.getClntVrf();
        cfgIfc ifc = cfgAll.getClntIfc();
        int size = 64;
        int timeout = 1000;
        int repeat = 5;
        int tos = 0;
        int ttl = 255;
        int proto = 0;
        int delay = 0;
        boolean flood = false;
        boolean detail = false;
        boolean sweep = false;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("/sweep")) {
                sweep = true;
                continue;
            }
            if (a.equals("/flood")) {
                flood = true;
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
        if (timeout < 1) {
            timeout = 1;
        }
        int sent = 0;
        int recv = 0;
        long tiMin = timeout * 10;
        long tiMax = 0;
        long tiSum = 0;
        pipe.linePut("pinging " + trg + ", src=" + src + ", cnt=" + repeat + ", len=" + size + ", tim=" + timeout + ", ttl=" + ttl + ", tos=" + tos + ", sweep=" + sweep);
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
            long tiBeg = bits.getTime();
            if (flood) {
                pipe.strPut(".");
                continue;
            }
            sent++;
            notifier notif = fwd.echoSendReq(src, trg, size, ttl, tos);
            if (notif == null) {
                pipe.strPut("R");
                continue;
            }
            if (notif.totalNotifies() < 1) {
                notif.sleep(timeout);
            }
            tiBeg = bits.getTime() - tiBeg;
            if (notif.totalNotifies() < 1) {
                pipe.strPut(".");
                continue;
            }
            recv++;
            tiSum += tiBeg;
            if (tiBeg < tiMin) {
                tiMin = tiBeg;
            }
            if (tiBeg > tiMax) {
                tiMax = tiBeg;
            }
            if (detail) {
                pipe.strPut(tiBeg + "ms  ");
                continue;
            }
            if (!flood) {
                pipe.strPut("!");
                continue;
            }
            byte[] buf = new byte[1];
            buf[0] = 8;
            pipe.blockingPut(buf, 0, 1);
        }
        pipe.linePut("");
        int perc = 0;
        if (recv > 0) {
            tiSum /= recv;
        }
        if (sent > 0) {
            perc = (recv * 100) / sent;
        }
        pipe.linePut("result=" + perc + "%, recv/sent/lost=" + recv + "/"
                + sent + "/" + (sent - recv) + ", rtt min/avg/max/total=" + tiMin
                + "/" + tiSum + "/" + tiMax + "/" + (bits.getTime() - beg));
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
            reader.putStrTab(res.toUserStr());
            return;
        }
        if (recur) {
            packDnsZone res = clnt.doRecursive(srvs, a, i);
            if (res == null) {
                cmd.error("no reply");
                return;
            }
            reader.putStrTab(res.toUserStr());
            return;
        }
        clnt.doResolvList(srvs, a, i);
        packDnsRec res = clnt.findAnswer(i);
        if (res == null) {
            cmd.error("no reply");
            return;
        }
        reader.putStrArr(bits.str2lst(res.toUserStr(" ")));
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
            reader.width = bits.str2num(cmd.word());
            final int min = 20;
            if (reader.width < min) {
                reader.width = min;
            }
            return;
        }
        if (a.equals("timestamps")) {
            reader.timeStamp = true;
            return;
        }
        if (a.equals("colorized")) {
            reader.colorize = true;
            return;
        }
        if (a.equals("length")) {
            reader.height = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("tablemode")) {
            reader.tabMod = userFormat.tableMode.valueOf(cmd.word());
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
            reader.timeStamp = false;
            return;
        }
        if (a.equals("colorized")) {
            reader.colorize = false;
            return;
        }
        cmd.badCmd();
    }

    private void doFlash() {
        String a = cmd.word();
        if (a.equals("editor")) {
            a = cmd.getRemaining();
            List<String> b = bits.txt2buf(a);
            if (b == null) {
                b = new ArrayList<String>();
            }
            userEditor e = new userEditor(new userScreen(pipe, reader.width, reader.height), b, a, false);
            if (e.doEdit()) {
                return;
            }
            bits.buf2txt(true, b, a);
            return;
        }
        if (a.equals("viewer")) {
            a = cmd.getRemaining();
            List<String> b = bits.txt2buf(a);
            userEditor v = new userEditor(new userScreen(pipe, reader.width, reader.height), b, a, false);
            v.doView();
            return;
        }
        if (a.equals("commander")) {
            userFilman f = new userFilman(new userScreen(pipe, reader.width, reader.height));
            f.doWork();
            return;
        }
        if (a.equals("browser")) {
            userBrowser f = new userBrowser(new userScreen(pipe, reader.width, reader.height), cmd.getRemaining());
            f.doWork();
            return;
        }
        if (a.equals("binviewer")) {
            a = cmd.getRemaining();
            List<String> b = userFlash.binRead(a);
            userEditor v = new userEditor(new userScreen(pipe, reader.width, reader.height), b, a, false);
            v.doView();
            return;
        }
        if (a.equals("receive")) {
            a = cmd.word();
            userFlash.doReceive(pipe, uniResLoc.parseOne(cmd.getRemaining()), new File(a));
            return;
        }
        if (a.equals("transmit")) {
            a = cmd.word();
            userFlash.doSend(pipe, uniResLoc.parseOne(cmd.getRemaining()), new File(a));
            return;
        }
        if (a.equals("hash")) {
            a = cmd.getRemaining();
            cmd.error("file=" + a);
            cmd.error("md5=" + userUpgrade.calcFileHash(new cryHashMd5(), a));
            cmd.error("sha1=" + userUpgrade.calcFileHash(new cryHashSha1(), a));
            cmd.error("sha256=" + userUpgrade.calcFileHash(new cryHashSha2256(), a));
            cmd.error("sha512=" + userUpgrade.calcFileHash(new cryHashSha2512(), a));
            return;
        }
        if (a.equals("disk")) {
            a = cmd.getRemaining();
            File f = new File(a);
            try {
                cmd.error("path=" + f.getCanonicalPath());
                cmd.error("free=" + f.getFreeSpace());
                cmd.error("total=" + f.getTotalSpace());
                cmd.error("usable=" + f.getUsableSpace());
            } catch (Exception e) {
            }
            return;
        }
        if (a.equals("info")) {
            a = cmd.getRemaining();
            File f = new File(a);
            try {
                cmd.error("file=" + f.getCanonicalPath());
                cmd.error("size=" + f.length());
                cmd.error("modify=" + bits.time2str(cfgAll.timeZoneName, f.lastModified(), 3));
            } catch (Exception e) {
            }
            return;
        }
        if (a.equals("upgrade")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doUpgrade();
            return;
        }
        if (a.equals("simulate")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doSimulate();
            return;
        }
        if (a.equals("verify")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doVerify();
            return;
        }
        if (a.equals("type")) {
            cmd = reader.setFilter(cmd);
            reader.putStrArr(bits.txt2buf(cmd.getRemaining()));
            return;
        }
        if (a.equals("bintype")) {
            cmd = reader.setFilter(cmd);
            reader.putStrArr(userFlash.binRead(cmd.getRemaining()));
            return;
        }
        userFlash fl = new userFlash(pipe);
        if (a.equals("copy")) {
            String s = cmd.word();
            fl.copy(s, cmd.word());
            return;
        }
        if (a.equals("rename")) {
            String s = cmd.word();
            userFlash.rename(s, cmd.word(), true, false);
            return;
        }
        if (a.equals("delete")) {
            userFlash.delete(cmd.getRemaining());
            return;
        }
        if (a.equals("mkdir")) {
            userFlash.mkdir(cmd.word());
            return;
        }
        if (a.equals("list")) {
            cmd = reader.setFilter(cmd);
            reader.putStrTab(userFlash.dir2txt(userFlash.dirList(cmd.getRemaining())));
            return;
        }
        cmd.badCmd();
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

    private pipeSide getShPipe() {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, 8191);
        rdr.height = 0;
        userExec exe = new userExec(pip, rdr);
        exe.privileged = privileged;
        exe.reader.tabMod = reader.tabMod;
        exe.reader.timeStamp = reader.timeStamp;
        exe.reader.colorize = reader.colorize;
        pip.timeout = 60000;
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
        pipeSide pip = getShPipe();
        List<String> lst = new ArrayList<String>();
        packText pt = new packText(getShPipe());
        pt.recvAll(lst);
        userEditor edtr = new userEditor(new userScreen(pipe, reader.width, reader.height), lst, cfgAll.hostName + "#show " + cmd.getRemaining(), false);
        edtr.doView();
    }

    private void doWatch() {
        reader.keyFlush();
        for (;;) {
            String a = getShPipe().strGet(65536);
            userScreen.sendCur(pipe, 0, 0);
            userScreen.sendCls(pipe);
            pipe.linePut(cfgAll.hostName + "#show " + cmd.getRemaining());
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
        userEditor edtr = new userEditor(new userScreen(pipe, reader.width, reader.height), lst, cfgAll.hostName + "#watch " + cmd.getRemaining(), reader.timeStamp);
        for (;;) {
            lst.clear();
            packText pt = new packText(getShPipe());
            pt.recvAll(lst);
            if (edtr.doTimed(1000, false)) {
                break;
            }
        }
        edtr.doClear();
        reader.keyFlush();
    }

    private void doDiffers() {
        List<String> r1 = new packText(getShPipe()).recvAll();
        reader.keyFlush();
        List<String> lst = new ArrayList<String>();
        userEditor edtr = new userEditor(new userScreen(pipe, reader.width, reader.height), lst, cfgAll.hostName + "#watch " + cmd.getRemaining(), reader.timeStamp);
        for (;;) {
            List<String> r2 = new packText(getShPipe()).recvAll();
            differ df = new differ();
            df.calc(r1, r2);
            lst.clear();
            lst.addAll(df.getText(reader.width, edtr.getOfs()));
            if (edtr.doTimed(1000, true)) {
                break;
            }
        }
        edtr.doClear();
        reader.keyFlush();
    }

}
