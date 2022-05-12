package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwdMcast;
import net.freertr.prt.prtGenServ;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabConnect;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabNatTraN;
import net.freertr.tab.tabPbrN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;

/**
 * one p4lang vrf
 *
 * @author matecsaba
 */
public class servP4langVrf implements Comparator<servP4langVrf> {

    /**
     * vrf id
     */
    public final int id;

    /**
     * vrf handler
     */
    public cfgVrf vrf;

    /**
     * sent multicast
     */
    public boolean sentMcast;

    /**
     * sent routes
     */
    public tabGen<servP4langStr<tabRouteEntry<addrIP>>> routed4 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();

    /**
     * sent routes
     */
    public tabGen<servP4langStr<tabRouteEntry<addrIP>>> routed6 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();

    /**
     * sent routes
     */
    public tabRoute<addrIP> routes4 = new tabRoute<addrIP>("sent");

    /**
     * sent routes
     */
    public tabRoute<addrIP> routes6 = new tabRoute<addrIP>("sent");

    /**
     * sent sockets
     */
    public tabConnect<addrIP, prtGenServ> udp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent sockets
     */
    public tabConnect<addrIP, prtGenServ> udp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent sockets
     */
    public tabConnect<addrIP, prtGenServ> tcp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent sockets
     */
    public tabConnect<addrIP, prtGenServ> tcp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    /**
     * sent mroutes
     */
    public tabGen<ipFwdMcast> mroutes4 = new tabGen<ipFwdMcast>();

    /**
     * sent mroutes
     */
    public tabGen<ipFwdMcast> mroutes6 = new tabGen<ipFwdMcast>();

    /**
     * sent nat config
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg4;

    /**
     * sent nat config
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg6;

    /**
     * sent nat config
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg4f;

    /**
     * sent nat config
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg6f;

    /**
     * sent pbr config
     */
    public tabListing<tabPbrN, addrIP> pbrCfg4;

    /**
     * sent pbr config
     */
    public tabListing<tabPbrN, addrIP> pbrCfg6;

    /**
     * sent flowspec config
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> flwSpc4;

    /**
     * sent flowspec config
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> flwSpc6;

    /**
     * sent nat translations
     */
    public tabGen<tabNatTraN> natTrns4 = new tabGen<tabNatTraN>();

    /**
     * sent nat translations
     */
    public tabGen<tabNatTraN> natTrns6 = new tabGen<tabNatTraN>();

    /**
     * sent polka indexes
     */
    public tabGen<servP4langStr<tabIndex<addrIP>>> indexUs4 = new tabGen<servP4langStr<tabIndex<addrIP>>>();

    /**
     * sent polka indexes
     */
    public tabGen<servP4langStr<tabIndex<addrIP>>> indexUs6 = new tabGen<servP4langStr<tabIndex<addrIP>>>();

    /**
     * sent polka indexes
     */
    public tabGen<tabIndex<addrIP>> indexUd4 = new tabGen<tabIndex<addrIP>>();

    /**
     * sent polka indexes
     */
    public tabGen<tabIndex<addrIP>> indexUd6 = new tabGen<tabIndex<addrIP>>();

    /**
     * sent mpolka indexes
     */
    public tabGen<servP4langStr<tabIndex<addrIP>>> indexCs4 = new tabGen<servP4langStr<tabIndex<addrIP>>>();

    /**
     * sent mpolka indexes
     */
    public tabGen<servP4langStr<tabIndex<addrIP>>> indexCs6 = new tabGen<servP4langStr<tabIndex<addrIP>>>();

    /**
     * sent mpolka indexes
     */
    public tabGen<tabIndex<addrIP>> indexCd4 = new tabGen<tabIndex<addrIP>>();

    /**
     * sent mpolka indexes
     */
    public tabGen<tabIndex<addrIP>> indexCd6 = new tabGen<tabIndex<addrIP>>();

    /**
     * create instance
     *
     * @param i id
     */
    public servP4langVrf(int i) {
        id = i;
    }

    public int compare(servP4langVrf o1, servP4langVrf o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    /**
     * clear tables
     */
    public void doClear() {
        routed4 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();
        routed6 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();
        routes4 = new tabRoute<addrIP>("sent");
        routes6 = new tabRoute<addrIP>("sent");
        mroutes4 = new tabGen<ipFwdMcast>();
        mroutes6 = new tabGen<ipFwdMcast>();
        sentMcast = false;
        natCfg4 = null;
        natCfg4f = new tabListing<tabAceslstN<addrIP>, addrIP>();
        natCfg6 = null;
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
        indexUs4 = new tabGen<servP4langStr<tabIndex<addrIP>>>();
        indexUs6 = new tabGen<servP4langStr<tabIndex<addrIP>>>();
        indexUd4 = new tabGen<tabIndex<addrIP>>();
        indexUd6 = new tabGen<tabIndex<addrIP>>();
        indexCs4 = new tabGen<servP4langStr<tabIndex<addrIP>>>();
        indexCs6 = new tabGen<servP4langStr<tabIndex<addrIP>>>();
        indexCd4 = new tabGen<tabIndex<addrIP>>();
        indexCd6 = new tabGen<tabIndex<addrIP>>();
    }

}
