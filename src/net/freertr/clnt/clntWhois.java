package net.freertr.clnt;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeReader;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.tab.tabGen;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;

/**
 * whois (rfc3912) client
 *
 * @author matecsaba
 */
public class clntWhois {

    /**
     * port number
     */
    public static final int port = 43;

    /**
     * asn name cache
     */
    public final static tabGen<clntWhoisAsName> asnameCache = new tabGen<clntWhoisAsName>();

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
        console.linePut("querying " + quest + " at " + server);
        if (proxy == null) {
            return null;
        }
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            return null;
        }
        pipeSide pipe = proxy.doConnect(servGeneric.protoTcp, trg, port, "whois");
        if (pipe == null) {
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
            s = s.replaceAll(" ", "").toLowerCase();
            int o = s.indexOf(":");
            String a = s.substring(0, o);
            s = s.substring(o, s.length());
            if (a.equals("aut-num")) {
                asNum = s;
                continue;
            }
            if (a.equals("as-name")) {
                asNam = s;
                continue;
            }
        }
        if (!asNam.startsWith("AS")) {
            return res;
        }
        clntWhoisAsName ntry = new clntWhoisAsName(bits.str2num(asNam.substring(2, asNam.length())));
        ntry.name = asNam;
        asnameCache.put(ntry);
        return res;
    }

}

class clntWhoisAsName implements Comparator<clntWhoisAsName> {

    public final int asn;

    public String name;

    public clntWhoisAsName(int i) {
        asn = i;
    }

    public int compare(clntWhoisAsName o1, clntWhoisAsName o2) {
        if (o1.asn < o2.asn) {
            return -1;
        }
        if (o1.asn > o2.asn) {
            return +1;
        }
        return 0;
    }

}
