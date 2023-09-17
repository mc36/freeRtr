package net.freertr.serv;

/**
 * one host specific worker
 *
 * @author matecsaba
 */
public class servHttpWork {

    private final servHttpConn cn;

    private final servHttpHost hs;

    /**
     * create instance
     *
     * @param conn connection
     * @param host host config
     */
    public servHttpWork(servHttpConn conn, servHttpHost host) {
        cn = conn;
        hs = host;
    }

}
