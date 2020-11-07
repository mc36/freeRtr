package user;

import java.util.ArrayList;
import java.util.List;
import pipe.pipeSide;
import util.extMrkLng;

/**
 * netconf (rfc6241) handler
 *
 * @author matecsaba
 */
public class userNetconf {

    /**
     * header separator
     */
    public final static String headerEnd = "]]>]]>";

    private final pipeSide conn;

    private final boolean privi;

    private final boolean form;

    private final boolean echo;

    private boolean oldVer;

    /**
     * create handler
     *
     * @param pipe pipe to use
     * @param prv privileged
     * @param frm format response
     * @param ech echo input
     */
    public userNetconf(pipeSide pipe, boolean prv, boolean frm, boolean ech) {
        conn = pipe;
        privi = prv;
        form = frm;
        echo = ech;
    }

    private List<String> doRead() {
        List<String> l = new ArrayList<String>();
        return l;
    }

    private void doSend(extMrkLng  x) {
        List<String>l=x.toXMLlst();
        for(int i=0;i<l.size();i++){
            
        }
        if (oldVer)conn.linePut(headerEnd);
    }

    /**
     * do work
     */
    public void doWork() {
        oldVer = true;
        doRead();
        ////////////////
    }

}
