package line;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import prt.prtLocTcp;
import util.logger;

/**
 * external interface handler over tcp
 *
 * @author matecsaba
 */
public class lineTcpLine extends lineThread {

    private final String rnam;

    private final String lnam;

    private final int rprt;

    private int lprt;

    private Socket sck;

    private InputStream istr;

    private OutputStream ostr;

    private InetAddress radr;

    private InetAddress ladr;

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "line on " + ladr + " " + lprt + " " + radr + " " + rprt;
    }

    /**
     * creates new interface
     *
     * @param lname name of local
     * @param lport local port to bind to
     * @param rname name of remote
     * @param rport remote port to connect to
     */
    public lineTcpLine(String lname, int lport, String rname, int rport) {
        lprt = lport;
        lnam = lname;
        rprt = rport;
        rnam = rname;
    }

    /**
     * open interface
     */
    protected synchronized void rxtxOpen() {
        try {
            radr = InetAddress.getByName(rnam);
            ladr = InetAddress.getByName(lnam);
            sck = new Socket(radr, rprt);
            istr = sck.getInputStream();
            ostr = sck.getOutputStream();
            lprt = sck.getLocalPort();
            sck.setTcpNoDelay(true);
            prtLocTcp.doSession(pipS, sck);
        } catch (Exception e) {
            logger.warn("reopening " + this);
            rxtxClose();
        }
    }

    /**
     * close interface
     */
    protected synchronized void rxtxClose() {
        try {
            ostr.flush();
        } catch (Exception e) {
        }
        try {
            istr.close();
        } catch (Exception e) {
        }
        try {
            ostr.close();
        } catch (Exception e) {
        }
        try {
            sck.close();
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
        sck = null;
        istr = null;
        ostr = null;
    }

    /**
     * send control
     *
     * @param ctrl control
     */
    protected void txCtrlBit(int ctrl) {
    }

    /**
     * set flow control
     *
     * @param ctrl control
     */
    protected void txFlowCtrl(int ctrl) {
    }

}
