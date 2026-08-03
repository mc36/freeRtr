package org.freertr.clnt;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.enc.encUrl;
import org.freertr.pack.packText;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeProgress;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servPop3;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * post office protocol 3 (rfc1939) client
 *
 * @author matecsaba
 */
public class clntPop3 {

    private pipeProgress cons;

    private pipeSide pipe;

    private RandomAccessFile fr;

    /**
     * create new client
     *
     * @param console console to use
     */
    public clntPop3(pipeSide console) {
        cons = new pipeProgress(pipeDiscard.needAny(console));
    }

    private void sendLine(String s) {
        if (debugger.clntPop3traf) {
            logger.debug("tx:" + s);
        }
        cons.debugTx(s);
        pipe.linePut(s);
    }

    private String getLine() {
        packText pck = new packText(pipe);
        String s = pck.codedRecv();
        if (s == null) {
            s = "";
        }
        if (debugger.clntFtpTraf) {
            logger.debug("rx:" + s);
        }
        cons.debugRx(s);
        return s;
    }

    private boolean downOne(int num, File trg, boolean del) {
        sendLine("RETR " + num);
        if (!getLine().toLowerCase().startsWith("+ok")) {
            return true;
        }
        packText pt = new packText(pipe);
        List<String> txt = pt.dottedRecvAll();
        cons.debugStat("got " + txt.size() + " lines");
        if (bits.buf2txt(true, txt, trg.getAbsolutePath() + num + ".msg")) {
            return true;
        }
        if (!del) {
            return false;
        }
        sendLine("DELE " + num);
        getLine();
        return false;
    }

    /**
     * download one file
     *
     * @param src source
     * @param trg target
     * @param del remove from server
     * @return result code
     */
    public boolean download(encUrl src, File trg, boolean del) {
        clntProxy prx = cfgAll.getClntPrx(cfgAll.pop3proxy);
        if (prx == null) {
            return true;
        }
        addrIP srvr = clntDns.justResolv(src.server, 0);
        if (srvr == null) {
            return true;
        }
        servPop3 srv = new servPop3();
        pipe = prx.doConnect(servGeneric.protoTcp, srvr, srv.srvPort(), "pop3");
        if (pipe == null) {
            return true;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        getLine();
        sendLine("USER " + src.username);
        getLine();
        sendLine("PASS " + src.password);
        getLine();
        sendLine("STAT");
        String a = getLine();
        int i = a.indexOf(" ");
        if (i < 0) {
            return true;
        }
        a = a.substring(i + 1, a.length());
        i = a.indexOf(" ");
        if (i < 0) {
            return true;
        }
        a = a.substring(0, i);
        int o = bits.str2num(a);
        for (i = 0; i < o; i++) {
            cons.debugStat("message " + (i + 1) + " of " + o);
            if (downOne(i + 1, trg, del)) {
                return true;
            }
        }
        sendLine("QUIT");
        getLine();
        pipe.setClose();
        return false;
    }

    /**
     * clean up state
     */
    public void cleanUp() {
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
    }

}
