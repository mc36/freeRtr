package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwdMcast;
import org.freertr.prt.prtGenServ;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabConnect;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabNatTraN;
import org.freertr.tab.tabPbrN;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;

/**
 * one p4lang vrf
 *
 * @author matecsaba
 */
public class servP4langVrf implements Comparable<servP4langVrf> {

    /**
     * vrf id
     */
    protected int id;

    /**
     * vrf handler
     */
    protected cfgVrf vrf;

    /**
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> copp4c = null;

    /**
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> copp6c = null;

    /**
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> copp4p = null;

    /**
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> copp6p = null;

    /**
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> copp4f = new tabListing<tabAceslstN<addrIP>, addrIP>();

    /**
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> copp6f = new tabListing<tabAceslstN<addrIP>, addrIP>();

    /**
     * export prefix list
     */
    protected tabListing<tabPrfxlstN, addrIP> prflst4;

    /**
     * export prefix list
     */
    protected tabListing<tabPrfxlstN, addrIP> prflst6;

    /**
     * export route map
     */
    protected tabListing<tabRtrmapN, addrIP> roumap4;

    /**
     * export route map
     */
    protected tabListing<tabRtrmapN, addrIP> roumap6;

    /**
     * export route policy
     */
    protected tabListing<tabRtrplcN, addrIP> roupol4;

    /**
     * export route policy
     */
    protected tabListing<tabRtrplcN, addrIP> roupol6;

    /**
     * export compressed fib
     */
    protected boolean compress4;

    /**
     * export compressed fib
     */
    protected boolean compress6;

    /**
     * sent multicast
     */
    protected boolean sentMcast;

    /**
     * sent samples
     */
    protected int sentSample4;

    /**
     * sent samples
     */
    protected int sentSample6;

    /**
     * sent routes
     */
    protected tabGen<servP4langStrI<tabRouteEntry<addrIP>>> routed4 = new tabGen<servP4langStrI<tabRouteEntry<addrIP>>>();

    /**
     * sent routes
     */
    protected tabGen<servP4langStrI<tabRouteEntry<addrIP>>> routed6 = new tabGen<servP4langStrI<tabRouteEntry<addrIP>>>();

    /**
     * sent routes
     */
    protected tabRoute<addrIP> routes4 = new tabRoute<addrIP>("sent");

    /**
     * sent routes
     */
    protected tabRoute<addrIP> routes6 = new tabRoute<addrIP>("sent");

    /**
     * sent sockets
     */
    protected tabConnect<addrIP, prtGenServ> udp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent sockets
     */
    protected tabConnect<addrIP, prtGenServ> udp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent sockets
     */
    protected tabConnect<addrIP, prtGenServ> tcp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent sockets
     */
    protected tabConnect<addrIP, prtGenServ> tcp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent mroutes
     */
    protected tabGen<ipFwdMcast> mroutes4 = new tabGen<ipFwdMcast>();

    /**
     * sent mroutes
     */
    protected tabGen<ipFwdMcast> mroutes6 = new tabGen<ipFwdMcast>();

    /**
     * sent mroutes
     */
    protected tabGen<servP4langStrL<ipFwdMcast, addrIP>> mrouted4 = new tabGen<servP4langStrL<ipFwdMcast, addrIP>>();

    /**
     * sent mroutes
     */
    protected tabGen<servP4langStrL<ipFwdMcast, addrIP>> mrouted6 = new tabGen<servP4langStrL<ipFwdMcast, addrIP>>();

    /**
     * sent nat config
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> natCfg4;

    /**
     * sent nat config
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> natCfg6;

    /**
     * sent nat config
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> natCfg4f;

    /**
     * sent nat config
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> natCfg6f;

    /**
     * sent pbr config
     */
    protected tabListing<tabPbrN, addrIP> pbrCfg4;

    /**
     * sent pbr config
     */
    protected tabListing<tabPbrN, addrIP> pbrCfg6;

    /**
     * sent flowspec config
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> flwSpc4;

    /**
     * sent flowspec config
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> flwSpc6;

    /**
     * sent nat translations
     */
    protected tabGen<tabNatTraN> natTrns4 = new tabGen<tabNatTraN>();

    /**
     * sent nat translations
     */
    protected tabGen<tabNatTraN> natTrns6 = new tabGen<tabNatTraN>();

    /**
     * sent polka indexes
     */
    protected tabGen<servP4langStrI<tabIndex<addrIP>>> indexUs4 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();

    /**
     * sent polka indexes
     */
    protected tabGen<servP4langStrI<tabIndex<addrIP>>> indexUs6 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();

    /**
     * sent polka indexes
     */
    protected tabGen<tabIndex<addrIP>> indexUd4 = new tabGen<tabIndex<addrIP>>();

    /**
     * sent polka indexes
     */
    protected tabGen<tabIndex<addrIP>> indexUd6 = new tabGen<tabIndex<addrIP>>();

    /**
     * sent mpolka indexes
     */
    protected tabGen<servP4langStrI<tabIndex<addrIP>>> indexCs4 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();

    /**
     * sent mpolka indexes
     */
    protected tabGen<servP4langStrI<tabIndex<addrIP>>> indexCs6 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();

    /**
     * sent mpolka indexes
     */
    protected tabGen<tabIndex<addrIP>> indexCd4 = new tabGen<tabIndex<addrIP>>();

    /**
     * sent mpolka indexes
     */
    protected tabGen<tabIndex<addrIP>> indexCd6 = new tabGen<tabIndex<addrIP>>();

    /**
     * create instance
     *
     * @param i id
     */
    protected servP4langVrf(int i) {
        id = i;
    }

    public int compareTo(servP4langVrf o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        return 0;
    }

    /**
     * clear tables
     */
    protected void doClear() {
        sentMcast = false;
        sentSample4 = 0;
        sentSample6 = 0;
        copp4p = null;
        copp6p = null;
        copp4f = new tabListing<tabAceslstN<addrIP>, addrIP>();
        copp6f = new tabListing<tabAceslstN<addrIP>, addrIP>();
        routed4 = new tabGen<servP4langStrI<tabRouteEntry<addrIP>>>();
        routed6 = new tabGen<servP4langStrI<tabRouteEntry<addrIP>>>();
        routes4 = new tabRoute<addrIP>("sent");
        routes6 = new tabRoute<addrIP>("sent");
        mroutes4 = new tabGen<ipFwdMcast>();
        mroutes6 = new tabGen<ipFwdMcast>();
        mrouted4 = new tabGen<servP4langStrL<ipFwdMcast, addrIP>>();
        mrouted6 = new tabGen<servP4langStrL<ipFwdMcast, addrIP>>();
        natCfg4 = new tabListing<tabAceslstN<addrIP>, addrIP>();
        natCfg4f = new tabListing<tabAceslstN<addrIP>, addrIP>();
        natCfg6 = new tabListing<tabAceslstN<addrIP>, addrIP>();
        natCfg6f = new tabListing<tabAceslstN<addrIP>, addrIP>();
        natTrns4 = new tabGen<tabNatTraN>();
        natTrns6 = new tabGen<tabNatTraN>();
        pbrCfg4 = new tabListing<tabPbrN, addrIP>();
        pbrCfg6 = new tabListing<tabPbrN, addrIP>();
        flwSpc4 = new tabListing<tabAceslstN<addrIP>, addrIP>();
        flwSpc6 = new tabListing<tabAceslstN<addrIP>, addrIP>();
        udp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        udp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        tcp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        tcp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        indexUs4 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();
        indexUs6 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();
        indexUd4 = new tabGen<tabIndex<addrIP>>();
        indexUd6 = new tabGen<tabIndex<addrIP>>();
        indexCs4 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();
        indexCs6 = new tabGen<servP4langStrI<tabIndex<addrIP>>>();
        indexCd4 = new tabGen<tabIndex<addrIP>>();
        indexCd6 = new tabGen<tabIndex<addrIP>>();
    }

}
