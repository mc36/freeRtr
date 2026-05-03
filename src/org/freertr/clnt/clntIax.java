package org.freertr.clnt;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgDial;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.enc.encCallHnd;
import org.freertr.enc.encCallOne;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packIax;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.tab.tabGen;
import org.freertr.util.logger;

/**
 * inter asterisk exchange (rfc5456) client
 *
 * @author matecsaba
 */
public class clntIax implements Runnable, encCallHnd {

    /**
     * create instance
     */
    public clntIax() {
    }

    /**
     * upper
     */
    public cfgDial upper;

    /**
     * client or server
     */
    public boolean client;

    /**
     * local port
     */
    public int portLoc = 0;

    /**
     * remote port
     */
    public int portRem = packIax.port;

    /**
     * codec, true=alaw, false=ulaw
     */
    public boolean aLaw = true;

    /**
     * keepalive interval
     */
    public int keepalive = 0;

    /**
     * register interval
     */
    public int register = 0;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * target
     */
    public String trgDom;

    /**
     * username
     */
    public String usr;

    /**
     * password
     */
    public String pwd;

    /**
     * udp handler
     */
    protected prtGen udp;

    /**
     * forwarder interface
     */
    protected ipFwdIface srcFwd;

    /**
     * connection to peer
     */
    protected pipeSide conn;

    private final tabGen<clntSipOut> outs = new tabGen<clntSipOut>();

    private final tabGen<clntSipIn> ins = new tabGen<clntSipIn>();

    private final tabGen<clntSipMsg> msgs = new tabGen<clntSipMsg>();

    private ipFwd fwd;

    private addrIP trgAdr;

    /**
     * keepalive
     */
    protected clntSipKeep timKeep;

    /**
     * need to run
     */
    protected boolean need2run;

    private int seqRx;

    private int seqTx;

    public void run() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public int numCalls(boolean dir) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public int numMsgs(boolean dir) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public List<String> listCalls(boolean dir) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean isReady() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean sendMsg(String calling, String called, List<String> msg) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public String makeCall(String calling, String called) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public void stopCall(String cid) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public encCallOne getCall(String cid) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }


    /**
     * start work
     */
    public void startWork() {
        need2run = true;
        seqRx = 0;
        seqTx = 0;
        logger.startThread(this);
        if ((keepalive + register) < 1) {
            return;
        }
        ///timKeep = new clntSipKeep(this);
        timKeep.start();
    }
    
    public void stopWork() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}
