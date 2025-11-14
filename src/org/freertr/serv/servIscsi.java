package org.freertr.serv;

import org.freertr.enc.encScsi;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;
import org.freertr.pack.packIscsi;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * internet small computer systems interface (rfc3720) server
 *
 * @author matecsaba
 */
public class servIscsi extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servIscsi() {
    }

    /**
     * list of files
     */
    public tabGen<encScsi> files = new tabGen<encScsi>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server iscsi .*", cmds.tabulator + "port " + packIscsi.port, null),
        new userFilter("server iscsi .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server iscsi .*", cmds.tabulator + "target .* block 512", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servIscsiConn(pipe, this, id);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        for (int i = 0; i < files.size(); i++) {
            encScsi ntry = files.get(i);
            ntry.getCfg(l, beg + "target " + ntry.name + " ");
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean neg = false;
        if (s.equals(cmds.negated)) {
            neg = true;
            s = cmd.word();
        }
        if (!s.equals("target")) {
            return true;
        }
        encScsi ntry = new encScsi(cmd.word());
        encScsi old = files.add(ntry);
        if (old != null) {
            ntry = old;
        }
        if (neg) {
            ntry.doUpdate(true);
            files.del(ntry);
            return false;
        }
        return ntry.doCfg(cmd);
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "target", "configure one disk");
        l.add(null, false, 2, new int[]{3}, "<str>", "name of disk");
        l.add(null, false, 3, new int[]{4}, "file", "select file to use");
        l.add(null, false, 4, new int[]{4, -1}, "<str>", "name of file");
        l.add(null, false, 3, new int[]{4}, "block", "set block size");
        l.add(null, false, 4, new int[]{-1}, "<num>", "bytes in block");
    }

    public String srvName() {
        return "iscsi";
    }

    public int srvPort() {
        return packIscsi.port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}

class servIscsiConn implements Runnable {

    public servIscsi lower;

    public pipeSide pipe;

    public encScsi file;

    public prtGenConn mySide;

    public servIscsiConn(pipeSide conn, servIscsi parent, prtGenConn id) {
        pipe = conn;
        lower = parent;
        mySide = id;
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        if (debugger.servIscsiTraf) {
            logger.debug("stopped");
        }
    }

    private void addTlvs(packIscsi old) {
        packIscsi nw = new packIscsi();
        nw.clearText();
        nw.addTextIf(old, "HeaderDigest", "None");
        nw.addTextIf(old, "DataDigest", "None");
        nw.addTextIf(old, "AuthMethod", "None");
        nw.addTextIf(old, "MaxRecvDataSegmentLength", "262144");
        nw.addTextIf(old, "DefaultTime2Wait", "0");
        nw.addTextIf(old, "DefaultTime2Retain", "60");
        nw.addTextIf(old, "ErrorRecoveryLevel", "0");
        nw.addTextIf(old, "InitialR2T", "No");
        nw.addTextIf(old, "ImmediateData", "Yes");
        nw.addTextIf(old, "MaxBurstLength", "65536");
        nw.addTextIf(old, "FirstBurstLength", "65536");
        nw.addTextIf(old, "MaxConnections", "1");
        nw.addTextIf(old, "DataPDUInOrder", "Yes");
        nw.addTextIf(old, "DataSequenceInOrder", "Yes");
        old.text = nw.text;
    }

    private void doer() {
        if (debugger.servIscsiTraf) {
            logger.debug("started");
        }
        packHolder pckBin = new packHolder(false, false);
        pckBin.allocHuge(1048576, 1048576);
        for (;;) {
            packIscsi pckRx = new packIscsi();
            pckRx.pipe = pipe;
            pckRx.pack = pckBin;
            if (pckRx.packRecv()) {
                return;
            }
            if (debugger.servIscsiTraf) {
                logger.debug("rx " + pckRx.dumpHdr());
            }
            switch (pckRx.opcode) {
                case packIscsi.opcCscsi:
                    if (pckRx.parseScsiReq()) {
                        return;
                    }
                    byte[] buf = pckBin.getCopy();
                    if (((pckRx.flags & packIscsi.flgWrite) != 0) & (buf.length < 1)) {
                        packIscsi pckDat = new packIscsi();
                        pckDat.pipe = pipe;
                        pckDat.pack = pckBin;
                        if (pckDat.packRecv()) {
                            return;
                        }
                        if (debugger.servIscsiTraf) {
                            logger.debug("rx " + pckDat.dumpHdr());
                        }
                        if (pckDat.parseDataReq()) {
                            return;
                        }
                        buf = pckBin.getCopy();
                        pckRx.datSeq = pckDat.datSeq + 1;
                    }
                    pckRx.cmdSeq++;
                    pckRx.cmdMax = pckRx.cmdSeq;
                    if (file == null) {
                        buf = null;
                    } else {
                        buf = file.doScsi(pckRx.scsi, buf);
                    }
                    if (buf == null) {
                        pckRx.flags = packIscsi.flgFinal;
                        pckRx.status = 1;
                        pckRx.respon = 1;
                        pckRx.createScsiRep();
                        pckRx.packSend();
                        if (debugger.servIscsiTraf) {
                            logger.debug("tx " + pckRx.dumpHdr());
                        }
                        break;
                    }
                    if ((pckRx.flags & packIscsi.flgRead) != 0) {
                        pckBin.clear();
                        pckBin.putCopy(buf, 0, 0, buf.length);
                        pckBin.putSkip(buf.length);
                        pckRx.flags = packIscsi.flgFinal;
                        pckRx.status = 0;
                        pckRx.createDataRep();
                        pckRx.packSend();
                        if (debugger.servIscsiTraf) {
                            logger.debug("tx " + pckRx.dumpHdr());
                        }
                    }
                    pckBin.clear();
                    pckRx.flags = packIscsi.flgFinal;
                    pckRx.status = 0;
                    pckRx.respon = 0;
                    pckRx.createScsiRep();
                    pckRx.packSend();
                    if (debugger.servIscsiTraf) {
                        logger.debug("tx " + pckRx.dumpHdr());
                    }
                    break;
                case packIscsi.opcClogin:
                    if (pckRx.parseLoginReq()) {
                        return;
                    }
                    pckRx.parseText();
                    if (debugger.servIscsiTraf) {
                        logger.debug("rx" + pckRx.dumpTxt());
                    }
                    pckRx.lun |= 1;
                    pckRx.status = 0;
                    pckRx.respon = 0;
                    pckRx.cmdSeq++;
                    pckRx.cmdMax = pckRx.cmdSeq;
                    String s = pckRx.findText("TargetName");
                    if (s != null) {
                        int i = s.indexOf(":");
                        if (i > 0) {
                            s = s.substring(i + 1, s.length());
                        }
                        file = new encScsi(s);
                        file = lower.files.find(file);
                        if (debugger.servIscsiTraf) {
                            logger.debug("opened " + file);
                        }
                    }
                    if (pckRx.findText("sessionType") != null) {
                        addTlvs(pckRx);
                        pckRx.addText("TargetPortalGroupTag", "1");
                        pckRx.createText();
                        pckRx.createLoginRep();
                        pckRx.packSend();
                        if (debugger.servIscsiTraf) {
                            logger.debug("tx " + pckRx.dumpHdr());
                            logger.debug("tx" + pckRx.dumpTxt());
                        }
                        break;
                    }
                    if (pckRx.findText("headerDigest") != null) {
                        addTlvs(pckRx);
                        pckRx.createText();
                        pckRx.createLoginRep();
                        pckRx.packSend();
                        if (debugger.servIscsiTraf) {
                            logger.debug("tx " + pckRx.dumpHdr());
                            logger.debug("tx" + pckRx.dumpTxt());
                        }
                        break;
                    }
                    return;
                case packIscsi.opcCtext:
                    if (pckRx.parseTextReq()) {
                        return;
                    }
                    pckRx.parseText();
                    if (debugger.servIscsiTraf) {
                        logger.debug("rx" + pckRx.dumpTxt());
                    }
                    pckRx.cmdSeq++;
                    pckRx.cmdMax = pckRx.cmdSeq;
                    if (pckRx.findText("sendTargets") != null) {
                        pckRx.clearText();
                        s = "" + mySide.iface.addr;
                        if (!mySide.iface.addr.isIPv4()) {
                            s = "[" + s + "]";
                        }
                        s += ":" + mySide.portLoc + ",1";
                        for (int i = 0; i < lower.files.size(); i++) {
                            pckRx.addText("TargetName", "iqn.2000-01." + cfgAll.hostName + ":" + lower.files.get(i).name);
                            pckRx.addText("TargetAddress", s);
                        }
                        pckRx.createText();
                        pckRx.createTextRep();
                        pckRx.packSend();
                        if (debugger.servIscsiTraf) {
                            logger.debug("tx " + pckRx.dumpHdr());
                            logger.debug("tx" + pckRx.dumpTxt());
                        }
                        break;
                    }
                    return;
                case packIscsi.opcClogout:
                    if (pckRx.parseLogoutReq()) {
                        return;
                    }
                    pckRx.cmdSeq++;
                    pckRx.cmdMax = pckRx.cmdSeq;
                    pckBin.clear();
                    pckRx.createLogoutRep();
                    pckRx.packSend();
                    if (debugger.servIscsiTraf) {
                        logger.debug("tx" + pckRx.dumpHdr());
                    }
                    break;
                default:
                    return;
            }
        }
    }

}
