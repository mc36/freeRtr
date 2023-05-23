package net.freertr.clnt;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.enc.encUrl;
import net.freertr.pipe.pipeSide;
import net.freertr.user.userExec;
import net.freertr.user.userFlash;
import net.freertr.util.bits;
import net.freertr.util.version;

/**
 * see url client
 *
 * @author matecsaba
 */
public class clntCurl {

    /**
     * default constructor
     */
    private clntCurl() {
    }

    /**
     * download one url
     *
     * @param p pipeline to log to
     * @param u url to download
     * @return returned text, null if nothing
     */
    public final static List<String> doGetUrl(pipeSide p, String u) {
        List<String> res = new ArrayList<String>();
        String a = version.getRWpath() + "curl" + bits.randomD() + ".tmp";
        userFlash.doReceive(p, encUrl.parseOne(u), new File(a), true);
        List<String> got = bits.txt2buf(a);
        userFlash.delete(a);
        a = "result=" + userExec.doneFail(got == null);
        p.linePut(a);
        return got;
    }

}
