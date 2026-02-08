package org.freertr.serv;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.List;
import org.freertr.pack.packHolder;
import org.freertr.pack.packTftp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.enc.encUrl;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFlash;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * trivial file transfer protocol (rfc1350) server
 *
 * @author matecsaba
 */
public class servTftp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servTftp() {
    }

    /**
     * root folder
     */
    public String rootFolder = "/data/";

    /**
     * read only server
     */
    public boolean readOnly = true;

    /**
     * maximum block size
     */
    public int blockSize = packTftp.size;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server tftp .*", cmds.tabulator + "port " + packTftp.port, null),
        new userFilter("server tftp .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server tftp .*", cmds.tabulator + "blocksize " + packTftp.size, null),
        new userFilter("server tftp .*", cmds.tabulator + "readonly", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servTftpConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !readOnly, beg, "readonly", "");
        l.add(beg + "blocksize " + blockSize);
        l.add(beg + "path " + rootFolder);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("path")) {
            rootFolder = "/" + encUrl.normalizePath(cmd.word() + "/");
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = true;
            return false;
        }
        if (s.equals("blocksize")) {
            blockSize = bits.str2num(cmd.word());
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("path")) {
            rootFolder = "/data/";
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = false;
            return false;
        }
        if (s.equals("blocksize")) {
            blockSize = packTftp.size;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "path", "set root folder");
        l.add(null, false, 2, new int[]{-1}, "<path>", "name of root folder");
        l.add(null, false, 1, new int[]{-1}, "readonly", "set write protection");
        l.add(null, false, 1, new int[]{2}, "blocksize", "set maximum block size");
        l.add(null, false, 2, new int[]{-1}, "<num>", "maximum payload size");
    }

    public String srvName() {
        return "tftp";
    }

    public int srvPort() {
        return packTftp.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);

    }

}

class servTftpConn implements Runnable {

    public pipeSide pipe;

    public servTftp lower;

    private RandomAccessFile fil;

    private long siz;

    private long blk;

    private int bs;

    public servTftpConn(pipeSide conn, servTftp parent) {
        pipe = conn;
        lower = parent;
        logger.startThread(this);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            fil.close();
        } catch (Exception e) {
        }
        pipe.setClose();
    }

    private void doer() {
        packHolder pckBin = pipe.readPacket(true);
        if (pckBin == null) {
            logger.info("got no packet");
            return;
        }
        packTftp pckTft = new packTftp();
        if (pckTft.parsePacket(pckBin)) {
            return;
        }
        if (debugger.servTftpTraf) {
            logger.debug("rx " + pckTft.dump());
        }
        String a = lower.rootFolder + encUrl.normalizePath("" + pckTft.nam);
        File fh = new File(a);
        boolean red;
        switch (pckTft.typ) {
            case packTftp.msgRead:
                red = true;
                if (!fh.exists()) {
                    sendError(1, "file not exists");
                    return;
                }
                if (!fh.isFile()) {
                    sendError(2, "not a file");
                    return;
                }
                siz = fh.length();
                try {
                    fil = new RandomAccessFile(fh, "r");
                } catch (Exception e) {
                    sendError(2, "error opening file");
                    return;
                }
                break;
            case packTftp.msgWrite:
                if (lower.readOnly) {
                    sendError(3, "not allowed");
                    return;
                }
                red = false;
                userFlash.mkfile(a);
                if (!fh.exists()) {
                    sendError(1, "file not exists");
                    return;
                }
                if (!fh.isFile()) {
                    sendError(2, "not a file");
                    return;
                }
                siz = 0;
                try {
                    fil = new RandomAccessFile(fh, "rw");
                    fil.setLength(0);
                } catch (Exception e) {
                    sendError(2, "error opening file");
                    return;
                }
                break;
            default:
                return;
        }
        bs = packTftp.size;
        if (pckTft.bSiz > 0) {
            bs = pckTft.bSiz;
            if (bs > lower.blockSize) {
                bs = lower.blockSize;
            }
        }
        blk = 0;
        for (;;) {
            if (red) {
                replyRead(pckTft);
            } else {
                replyWrite(pckTft);
            }
            pckBin = pipe.readPacket(true);
            if (pckBin == null) {
                return;
            }
            pckTft = new packTftp();
            if (pckTft.parsePacket(pckBin)) {
                return;
            }
            if (debugger.servTftpTraf) {
                logger.debug("rx " + pckTft.dump());
            }
        }
    }

    private void sendError(int cod, String str) {
        packTftp pckTft = new packTftp();
        pckTft.blk = cod;
        pckTft.nam = str;
        pckTft.typ = packTftp.msgError;
        if (debugger.servTftpTraf) {
            logger.debug("tx " + pckTft.dump());
        }
        packHolder pckBin = pckTft.createPacket();
        pckBin.merge2beg();
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
    }

    private void replyOack(packTftp pckTft) {
        if (pckTft.bSiz >= 0) {
            pckTft.bSiz = bs;
        }
        pckTft.typ = packTftp.msgOAck;
        pckTft.blk = 0;
        if (debugger.servTftpTraf) {
            logger.debug("tx " + pckTft.dump());
        }
        packHolder pckBin = pckTft.createPacket();
        pckBin.merge2beg();
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
    }

    private void replyRead(packTftp pckTft) {
        switch (pckTft.typ) {
            case packTftp.msgRead:
                blk = 0;
                if (!pckTft.opts) {
                    break;
                }
                if (pckTft.tSiz >= 0) {
                    pckTft.tSiz = siz;
                }
                replyOack(pckTft);
                return;
            case packTftp.msgAck:
                if (((pckTft.blk - 1) & 0xffff) == (blk & 0xffff)) {
                    blk++;
                    break;
                }
                if ((pckTft.blk & 0xffff) == (blk & 0xffff)) {
                    break;
                }
                return;
            default:
                return;
        }
        long pos = blk * bs;
        long len = siz - pos;
        if (len > bs) {
            len = bs;
        }
        if (len < 0) {
            len = 0;
        }
        pckTft = new packTftp();
        pckTft.dat = new byte[(int) len];
        try {
            fil.seek(pos);
            fil.read(pckTft.dat);
        } catch (Exception e) {
            return;
        }
        pckTft.blk = (int) (blk + 1);
        pckTft.typ = packTftp.msgData;
        if (debugger.servTftpTraf) {
            logger.debug("tx " + pckTft.dump());
        }
        packHolder pckBin = pckTft.createPacket();
        pckBin.merge2beg();
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
    }

    private void replyWrite(packTftp pckTft) {
        switch (pckTft.typ) {
            case packTftp.msgWrite:
                blk = 0;
                if (!pckTft.opts) {
                    break;
                }
                replyOack(pckTft);
                return;
            case packTftp.msgData:
                if (((pckTft.blk - 1) & 0xffff) == (blk & 0xffff)) {
                    try {
                        fil.write(pckTft.dat);
                    } catch (Exception e) {
                        return;
                    }
                    blk++;
                    break;
                }
                if ((pckTft.blk & 0xffff) == (blk & 0xffff)) {
                    break;
                }
                return;
            default:
                return;
        }
        pckTft = new packTftp();
        pckTft.blk = (int) blk;
        pckTft.typ = packTftp.msgAck;
        if (debugger.servTftpTraf) {
            logger.debug("tx " + pckTft.dump());
        }
        packHolder pckBin = pckTft.createPacket();
        pckBin.merge2beg();
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
    }

}
