package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeReader;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.tab.tabGen;
import net.freertr.user.userFormat;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.syncInt;

/**
 * whois (rfc3912) client
 *
 * @author matecsaba
 */
public class clntWhois {

    /**
     * startup counter
     */
    public final static syncInt cntrStart = new syncInt(0);

    /**
     * error counter
     */
    public final static syncInt cntrError = new syncInt(0);

    /**
     * stop counter
     */
    public final static syncInt cntrStop = new syncInt(0);

    /**
     * port number
     */
    public final static int port = 43;

    /**
     * asn name cache
     */
    private final static tabGen<clntWhoisAsn> asnameCache = new tabGen<clntWhoisAsn>();

    /**
     * show local cache
     *
     * @return text
     */
    public static userFormat showLocalCache() {
        userFormat res = new userFormat("|", "asn|name|ago|created|hit|info");
        for (int i = 0; i < asnameCache.size(); i++) {
            clntWhoisAsn ntry = asnameCache.get(i);
            if (ntry == null) {
                continue;
            }
            res.add("" + ntry + "|" + clntWhois.asn2infos(ntry.asn));
        }
        return res;
    }

    /**
     * clear local cache
     */
    public static void purgeLocalCache() {
        asnameCache.clear();
    }

    private final clntProxy proxy;

    private final String server;

    private final pipeSide console;

    /**
     * create new client
     *
     * @param con console to log
     * @param srv server to use
     * @param prx proxy to use
     */
    public clntWhois(pipeSide con, clntProxy prx, String srv) {
        console = pipeDiscard.needAny(con);
        server = srv;
        proxy = prx;
    }

    /**
     * do one query
     *
     * @param quest question to ask
     * @return response, null if error
     */
    public List<String> doQuery(String quest) {
        if (debugger.clntIpInfo) {
            logger.debug("working on " + quest + " at " + server);
        }
        cntrStart.add(1);
        console.linePut("querying " + quest + " at " + server);
        if (proxy == null) {
            cntrError.add(1);
            return null;
        }
        if (server == null) {
            cntrError.add(1);
            return null;
        }
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            cntrError.add(1);
            return null;
        }
        pipeSide pipe = proxy.doConnect(servGeneric.protoTcp, trg, port, "whois");
        if (pipe == null) {
            cntrError.add(1);
            return null;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.linePut(quest);
        pipeReader rd = new pipeReader();
        rd.setLineMode(pipeSide.modTyp.modeCRorLF);
        pipeConnect.connect(pipe, rd.getPipe(), true);
        rd.waitFor();
        List<String> res = rd.getResult();
        String asNum = null;
        String asNam = null;
        for (int i = 0; i < res.size(); i++) {
            String s = res.get(i);
            s = s.replaceAll(" ", "").trim();
            int o = s.indexOf(":");
            if (o < 1) {
                continue;
            }
            String a = s.substring(0, o).toLowerCase();
            s = s.substring(o + 1, s.length()).trim();
            if (a.equals("aut-num")) {
                asNum = s;
                continue;
            }
            if (a.equals("as-name")) {
                asNam = s;
                continue;
            }
        }
        cntrStop.add(1);
        if (asNam == null) {
            return res;
        }
        if (asNum == null) {
            return res;
        }
        asNum = asNum.trim().toLowerCase();
        if (!asNum.startsWith("as")) {
            return res;
        }
        clntWhoisAsn ntry = new clntWhoisAsn(bits.str2num(asNum.substring(2, asNum.length())));
        ntry.name = asNam;
        asnameCache.put(ntry);
        return res;
    }

    /**
     * get info for an asn
     *
     * @param i asn to query
     * @return info got, null if nothing
     */
    public static String asn2info(int i) {
        if (cfgAll.whoisOnline == null) {
            return null;
        }
        int o = cfgAll.whoisOnline.size();
        if (o < 1) {
            return null;
        }
        return cfgAll.whoisOnline.get(bits.random(0, o)) + bits.num2str(i);
    }

    /**
     * get all infos for an asn
     *
     * @param i asn to query
     * @return info got, null if nothing
     */
    public static List<String> asn2infos(int i) {
        List<String> res = new ArrayList<String>();
        if (cfgAll.whoisOnline == null) {
            return res;
        }
        int p = cfgAll.whoisOnline.size();
        for (int o = 0; o < p; o++) {
            res.add(cfgAll.whoisOnline.get(o) + bits.num2str(i));
        }
        return res;
    }

    /**
     * find name of asn
     *
     * @param i asn number
     * @param b true to do an external lookup on local cache miss
     * @return name, null if not found
     */
    public static String asn2name(int i, boolean b) {
        clntWhoisAsn ntry = new clntWhoisAsn(i);
        ntry = asnameCache.find(ntry);
        if (ntry != null) {
            ntry.hits++;
            return ntry.name;
        }
        if (!b) {
            return null;
        }
        clntWhois w = new clntWhois(null, cfgAll.getClntPrx(cfgAll.whoisProxy), cfgAll.whoisServer);
        if (w.doQuery("as" + i) == null) {
            asnameCache.put(new clntWhoisAsn(i));
            return null;
        }
        ntry = new clntWhoisAsn(i);
        ntry = asnameCache.find(ntry);
        if (ntry != null) {
            ntry.hits++;
            return ntry.name;
        }
        asnameCache.put(new clntWhoisAsn(i));
        return null;
    }

    /**
     * convert asn list to string
     *
     * @param lst list to convert
     * @param beg beginning
     * @param end ending
     * @return list
     */
    public static String asnList2str(List<Integer> lst, String beg, String end) {
        if (lst == null) {
            return "";
        }
        if (lst.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < lst.size(); i++) {
            int o = lst.get(i);
            String a = clntWhois.asn2name(o, true);
            if (a == null) {
                a = "as#" + bits.num2str(o);
            }
            s += " " + a;
        }
        return beg + s.substring(1, s.length()) + end;
    }

    /**
     * convert asn list to string
     *
     * @param lst list to convert
     * @param beg beginning
     * @param end ending
     * @return list
     */
    public static String asnList2info(List<Integer> lst, String beg, String end) {
        if (lst == null) {
            return "";
        }
        if (lst.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < lst.size(); i++) {
            int o = lst.get(i);
            String a = clntWhois.asn2info(o);
            if (a == null) {
                a = "as#" + bits.num2str(o);
            }
            s += " " + a;
        }
        return beg + s.substring(1, s.length()) + end;
    }

}
