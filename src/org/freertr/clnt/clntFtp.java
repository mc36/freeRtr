package org.freertr.clnt;

import java.io.File;
import java.io.RandomAccessFile;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgAll;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.pack.packText;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeProgress;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.serv.servFtp;
import org.freertr.serv.servGeneric;
import org.freertr.enc.encUrl;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * file transfer protocol (rfc959) client
 *
 * @author matecsaba
 */
public class clntFtp implements prtServS {

    /**
     * startup counter
     */
    public final static syncInt cntrStart = new syncInt(0);

    /**
     * error counter
     */
    public final static syncInt cntrError = new syncInt(0);

    /**
     * stop counter
     */
    public final static syncInt cntrStop = new syncInt(0);

    private final int locprt = 12121;

    private pipeSide pipe;

    private clntProxy prx;

    private ipFwdIface ifc;

    private addrIP srvr;

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

    private void setAnonymFtp(encUrl src) {
        if (src.username.length() > 0) {
            return;
        }
        src.username = "anonymous";
        src.password = "ftp@" + cfgAll.hostName;
    }

    private boolean begDatCon() {
        if (!cfgAll.ftpPassive) {
            ifc = null;
            if (prx.srcIfc != null) {
                ifc = prx.srcIfc.getFwdIfc(srvr);
            }
            if (ifc == null) {
                ifc = ipFwdTab.findSendingIface(prx.vrf.fwd4, srvr);
            }
            if (ifc == null) {
                return true;
            }
            if (srvr.isIPv4()) {
                prx.vrf.tcp4.streamListen(this, new pipeLine(65536, false), ifc, locprt, null, 0, "ftpc", -1, null, -1, -1);
                byte[] buf = new byte[6];
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
            } else {
                prx.vrf.tcp6.streamListen(this, new pipeLine(65536, false), ifc, locprt, null, 0, "ftpc", -1, null, -1, -1);
                sendLine("EPRT |2|" + ifc.addr.toIPv6() + "|" + locprt);
                getLine();
                return false;
            }
        }
        if (srvr.isIPv4()) {
            sendLine("PASV");
            cmds cmd = new cmds("ftp", getLine());
            cmd.word("(");
            cmd = new cmds("ftp", cmd.word(")"));
            byte[] buf = new byte[6];
            for (int i = 0; i < buf.length; i++) {
                buf[i] = (byte) (bits.str2num(cmd.word(",")) & 0xff);
            }
            addrIPv4 adr4 = new addrIPv4();
            adr4.fromBuf(buf, 0);
            int prt = bits.msbGetW(buf, 4);
            addrIP addr = new addrIP();
            addr.fromIPv4addr(adr4);
            data = prx.doConnect(servGeneric.protoTcp, addr, prt, "ftp");
            return data == null;
        } else {
            sendLine("EPSV");
            cmds cmd = new cmds("ftp", getLine());
            cmd.word("(");
            cmd = new cmds("ftp", cmd.word(")"));
            addrIPv6 adr6 = new addrIPv6();
            cmd.word("|");
            cmd.word("|");
            if (adr6.fromString(cmd.word("|"))) {
                adr6 = srvr.toIPv6();
            }
            int prt = bits.str2num(cmd.word("|"));
            addrIP addr = new addrIP();
            addr.fromIPv6addr(adr6);
            data = prx.doConnect(servGeneric.protoTcp, srvr, prt, "ftp");
            return data == null;
        }
    }

    private boolean endDatCon() {
        if (cfgAll.ftpPassive) {
            return false;
        }
        if (cfgAll.preferIpv6) {
            prx.vrf.tcp6.listenStop(ifc, locprt, null, 0);
        } else {
            prx.vrf.tcp4.listenStop(ifc, locprt, null, 0);
        }
        return data == null;
    }

    private boolean doRslvCnn(encUrl src) {
        prx = cfgAll.getClntPrx(cfgAll.ftpProxy);
        if (prx == null) {
            return true;
        }
        srvr = clntDns.justResolv(src.server, 0);
        if (srvr == null) {
            return true;
        }
        servFtp srv = new servFtp();
        pipe = prx.doConnect(servGeneric.protoTcp, srvr, srv.srvPort(), "ftp");
        if (pipe == null) {
            return true;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        return false;
    }

    /**
     * download one file
     *
     * @param src source
     * @param trg target
     * @return result code
     */
    public boolean download(encUrl src, File trg) {
        cntrStart.add(1);
        setAnonymFtp(src);
        if (doRslvCnn(src)) {
            cntrError.add(1);
            return true;
        }
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
        long len = bits.str2long(cmd.word());
        cons.setMax(len);
        if (begDatCon()) {
            pipe.setClose();
            cntrError.add(1);
            return true;
        }
        sendLine("RETR " + src.toPathName());
        getLine();
        if (endDatCon()) {
            pipe.setClose();
            cntrError.add(1);
            return true;
        }
        try {
            trg.createNewFile();
        } catch (Exception e) {
            cntrError.add(1);
            return true;
        }
        try {
            fr = new RandomAccessFile(trg, "rw");
        } catch (Exception e) {
            cntrError.add(1);
            return true;
        }
        long pos = 0;
        try {
            fr.setLength(pos);
        } catch (Exception e) {
            cntrError.add(1);
            return true;
        }
        cons.debugStat("receiving " + cons.getMax() + " bytes");
        for (;;) {
            final int max = 8192;
            byte[] buf = new byte[max];
            int siz = data.moreGet(buf, 0, max);
            if (siz < 1) {
                break;
            }
            pos += siz;
            cons.setCurr(pos);
            try {
                fr.write(buf, 0, siz);
            } catch (Exception ex) {
                cntrError.add(1);
                return true;
            }
        }
        data.setClose();
        getLine();
        sendLine("QUIT");
        getLine();
        pipe.setClose();
        cons.debugRes(pos + " bytes done");
        if (pos < len) {
            cntrError.add(1);
            return true;
        }
        cntrStop.add(1);
        return false;
    }

    /**
     * upload one file
     *
     * @param trg source
     * @param src target
     * @return result code
     */
    public boolean upload(encUrl trg, File src) {
        cntrStart.add(1);
        setAnonymFtp(trg);
        if (doRslvCnn(trg)) {
            cntrError.add(1);
            return true;
        }
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
        if (begDatCon()) {
            pipe.setClose();
            cntrError.add(1);
            return true;
        }
        sendLine("STOR " + trg.toPathName());
        getLine();
        if (endDatCon()) {
            pipe.setClose();
            cntrError.add(1);
            return true;
        }
        long pos = 0;
        long siz = 0;
        try {
            fr = new RandomAccessFile(src, "r");
            siz = fr.length();
        } catch (Exception e) {
            cntrError.add(1);
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
            byte[] buf = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                cntrError.add(1);
                return true;
            }
            if (data.morePut(buf, 0, rndi) < rndi) {
                cntrError.add(1);
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
        cntrStop.add(1);
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
