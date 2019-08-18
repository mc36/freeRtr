package clnt;

import addr.addrIP;
import addr.addrIPv4;
import cfg.cfgAll;
import ip.ipFwdIface;
import ip.ipFwdTab;
import java.io.File;
import java.io.RandomAccessFile;
import pack.packText;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeProgress;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import serv.servFtp;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * file transfer protocol (rfc959) client
 *
 * @author matecsaba
 */
public class clntFtp implements prtServS {

    private final int locprt = 12121;

    private pipeSide pipe;

    private clntProxy prx;

    private ipFwdIface ifc;

    private pipeSide data;

    private pipeProgress cons;

    private RandomAccessFile fr;

    /**
     * create new client
     *
     * @param console console to use
     */
    public clntFtp(pipeSide console) {
        cons = new pipeProgress(pipeDiscard.needAny(console));
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
            data.setClose();
        } catch (Exception e) {
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
    }

    private void sendLine(String s) {
        if (debugger.clntFtpTraf) {
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

    private void setAnonymFtp(uniResLoc src) {
        if (src.username.length() > 0) {
            return;
        }
        src.username = "anonymous";
        src.password = "ftp@" + cfgAll.hostName;
    }

    private boolean begDatCon(String trg) {
        if (!cfgAll.ftpPassive) {
            addrIP adri = new userTerminal(cons).resolveAddr(trg, 0);
            if (adri == null) {
                return true;
            }
            prx = cfgAll.getClntPrx();
            ifc = null;
            if (prx.srcIfc != null) {
                ifc = prx.srcIfc.getFwdIfc(adri);
            }
            if (ifc == null) {
                ifc = ipFwdTab.findSendingIface(prx.vrf.fwd4, adri);
            }
            if (ifc == null) {
                return true;
            }
            prx.vrf.tcp4.streamListen(this, new pipeLine(65536, false), ifc, locprt, new addrIP(), 0, 0, "ftpc", null, -1);
            byte buf[] = new byte[6];
            addrIPv4 adr4 = ifc.addr.toIPv4();
            adr4.toBuffer(buf, 0);
            bits.msbPutW(buf, 4, locprt);
            String a = "";
            for (int i = 0; i < buf.length; i++) {
                a += "," + (buf[i] & 0xff);
            }
            sendLine("PORT " + a.substring(1, a.length()));
            getLine();
            return false;
        }
        sendLine("PASV");
        cmds cmd = new cmds("ftp", getLine());
        cmd.word("(");
        cmd = new cmds("ftp", cmd.word(")"));
        byte buf[] = new byte[6];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) (bits.str2num(cmd.word(",")) & 0xff);
        }
        addrIPv4 adr = new addrIPv4();
        adr.fromBuf(buf, 0);
        int prt = bits.msbGetW(buf, 4);
        data = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, "" + adr, prt, "ftp");
        return data == null;
    }

    private boolean endDatCon() {
        if (cfgAll.ftpPassive) {
            return false;
        }
        prx.vrf.tcp4.listenStop(ifc, locprt, new addrIP(), 0, 0);
        return data == null;
    }

    /**
     * download one file
     *
     * @param src source
     * @param trg target
     * @return result code
     */
    public boolean download(uniResLoc src, File trg) {
        setAnonymFtp(src);
        servFtp srv = new servFtp();
        pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, src.server, srv.srvPort(), "ftp");
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
        sendLine("TYPE I");
        getLine();
        sendLine("MODE S");
        getLine();
        sendLine("STRU F");
        getLine();
        sendLine("SIZE " + src.toPathName());
        cmds cmd = new cmds("ftp", getLine());
        cmd.word();
        cons.setMax(bits.str2num(cmd.word()));
        if (begDatCon(src.server)) {
            pipe.setClose();
            return true;
        }
        sendLine("RETR " + src.toPathName());
        getLine();
        if (endDatCon()) {
            pipe.setClose();
            return true;
        }
        try {
            trg.createNewFile();
        } catch (Exception e) {
            return true;
        }
        try {
            fr = new RandomAccessFile(trg, "rw");
        } catch (Exception e) {
            return true;
        }
        long pos = 0;
        try {
            fr.setLength(pos);
        } catch (Exception e) {
            return true;
        }
        cons.debugStat("receiving " + cons.getMax() + " bytes");
        for (;;) {
            final int max = 8192;
            byte buf[] = new byte[max];
            int siz = data.moreGet(buf, 0, max);
            if (siz < 1) {
                break;
            }
            pos += siz;
            cons.setCurr(pos);
            try {
                fr.write(buf, 0, siz);
            } catch (Exception ex) {
                return true;
            }
        }
        data.setClose();
        getLine();
        sendLine("QUIT");
        getLine();
        pipe.setClose();
        cons.debugRes(pos + " bytes done");
        return false;
    }

    /**
     * upload one file
     *
     * @param trg source
     * @param src target
     * @return result code
     */
    public boolean upload(uniResLoc trg, File src) {
        setAnonymFtp(trg);
        servFtp srv = new servFtp();
        pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, trg.server, srv.srvPort(), "ftp");
        if (pipe == null) {
            return true;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        getLine();
        sendLine("USER " + trg.username);
        getLine();
        sendLine("PASS " + trg.password);
        getLine();
        sendLine("TYPE I");
        getLine();
        sendLine("MODE S");
        getLine();
        sendLine("STRU F");
        getLine();
        if (begDatCon(trg.server)) {
            pipe.setClose();
            return true;
        }
        sendLine("STOR " + trg.toPathName());
        getLine();
        if (endDatCon()) {
            pipe.setClose();
            return true;
        }
        long pos = 0;
        long siz = 0;
        try {
            fr = new RandomAccessFile(src, "r");
            siz = fr.length();
        } catch (Exception e) {
            return true;
        }
        cons.setMax(siz);
        cons.debugStat("sending " + cons.getMax() + " bytes");
        for (; pos < siz;) {
            final int max = 8192;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            pos += rndl;
            int rndi = (int) rndl;
            byte buf[] = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            if (data.morePut(buf, 0, rndi) < rndi) {
                return true;
            }
            cons.setCurr(pos);
        }
        data.setClose();
        getLine();
        sendLine("QUIT");
        getLine();
        pipe.setClose();
        cons.debugRes(pos + " bytes done");
        return false;
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false on success, true on error
     */
    public boolean streamAccept(pipeSide pipe, prtGenConn id) {
        if (data != null) {
            data.setClose();
        }
        data = pipe;
        return false;
    }

    /**
     * get blocking mode
     *
     * @return mode
     */
    public boolean streamForceBlock() {
        return true;
    }

}
