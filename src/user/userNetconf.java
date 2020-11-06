package user;

import pipe.pipeSide;

/**
 * netconf (rfc6241) handler
 *
 * @author matecsaba
 */
public class userNetconf {

    private final pipeSide conn;

    private final boolean privi;
    
    private final boolean form;

    private final boolean echo;

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

    /**
     * do work
     */
    public void doWork() {
        ////////////////
    }

}
