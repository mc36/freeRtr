package org.freertr.clnt;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.enc.encUrl;
import org.freertr.pipe.pipeSide;
import org.freertr.user.userFlash;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.version;

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
        userFlash.doReceive(p, encUrl.parseOne(u), new File(a));
        List<String> got = bits.txt2buf(a);
        userFlash.delete(a);
        a = "result=" + cmds.doneFail(got == null);
        p.linePut(a);
        return got;
    }

}
