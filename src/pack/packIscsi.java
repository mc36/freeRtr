package pack;

import java.util.ArrayList;
import java.util.List;

import pipe.pipeSide;
import util.bits;

/**
 * internet small computer systems interface (rfc3720) packet
 *
 * @author matecsaba
 */
public class packIscsi {

    /**
     * port number
     */
    public final static int port = 3260;

    /**
     * size of header
     */
    public final static int size = 48;

    /**
     * no operation
     */
    public final static int opcCnop = 0x00;

    /**
     * scsi command
     */
    public final static int opcCscsi = 0x01;

    /**
     * task management
     */
    public final static int opcCtask = 0x02;

    /**
     * login
     */
    public final static int opcClogin = 0x03;

    /**
     * text
     */
    public final static int opcCtext = 0x04;

    /**
     * scsi data
     */
    public final static int opcCdata = 0x05;

    /**
     * logout
     */
    public final static int opcClogout = 0x06;

    /**
     * snack
     */
    public final static int opcCsnack = 0x10;

    /**
     * no operation
     */
    public final static int opcRnop = 0x20;

    /**
     * scsi response
     */
    public final static int opcRscsi = 0x21;

    /**
     * task management
     */
    public final static int opcRtask = 0x22;

    /**
     * login
     */
    public final static int opcRlogin = 0x23;

    /**
     * text
     */
    public final static int opcRtext = 0x24;

    /**
     * scsi data
     */
    public final static int opcRdata = 0x25;

    /**
     * logout
     */
    public final static int opcRlogout = 0x26;

    /**
     * ready to transfer
     */
    public final static int opcRready = 0x31;

    /**
     * asynchronous message
     */
    public final static int opcRasync = 0x32;

    /**
     * reject
     */
    public final static int opcReject = 0x3f;

    /**
     * no more messages
     */
    public final static int flgFinal = 0x80;

    /**
     * read, input data follows
     */
    public final static int flgRead = 0x40;

    /**
     * write, output data follows
     */
    public final static int flgWrite = 0x20;

    /**
     * packet data
     */
    public packHolder pack;

    /**
     * pipeline to use
     */
    public pipeSide pipe;

    /**
     * opcode value
     */
    public int opcode;

    /**
     * flags value
     */
    public int flags;

    /**
     * response code
     */
    public int respon;

    /**
     * status value
     */
    public int status;

    /**
     * immediate delivery
     */
    public boolean immediately;

    /**
     * logical unit id
     */
    public long lun;

    /**
     * initiator task tag
     */
    public int taskTag;

    /**
     * target transfer tag
     */
    public int xferTag;

    /**
     * buffer offset
     */
    public int bufOfs;

    /**
     * referenced task tag
     */
    public int taskRef;

    /**
     * referenced command sequence
     */
    public int cmdRef;

    /**
     * snack tag
     */
    public int snackTag;

    /**
     * transfer length
     */
    public int xferLen;

    /**
     * command sequence number
     */
    public int cmdSeq;

    /**
     * status sequence number
     */
    public int statSeq;

    /**
     * ready to transfer sequence number
     */
    public int r2tSeq;

    /**
     * command sequence maximum
     */
    public int cmdMax;

    /**
     * data sequence number
     */
    public int datSeq;

    /**
     * bidir residual count
     */
    public int biRes;

    /**
     * residual count
     */
    public int resid;

    /**
     * scsi data
     */
    public byte[] scsi;

    /**
     * tlvs in packet
     */
    public List<String> text;

    /**
     * convert opcode to string
     *
     * @param i opcode
     * @return string
     */
    public static String opcode2string(int i) {
        switch (i) {
            case opcCnop:
                return "noopReq";
            case opcCscsi:
                return "scsiReq";
            case opcCtask:
                return "taskReq";
            case opcClogin:
                return "loginReq";
            case opcCtext:
                return "textReq";
            case opcCdata:
                return "dataReq";
            case opcClogout:
                return "logoutReq";
            case opcCsnack:
                return "snackReq";
            case opcRnop:
                return "noopRep";
            case opcRscsi:
                return "scsiRep";
            case opcRtask:
                return "taskRep";
            case opcRlogin:
                return "loginRep";
            case opcRtext:
                return "textRep";
            case opcRdata:
                return "dataRep";
            case opcRlogout:
                return "logoutRep";
            case opcRready:
                return "r2tRep";
            case opcRasync:
                return "asyncRep";
            case opcReject:
                return "reject";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * receive one packet
     *
     * @return false on success, true on error
     */
    public boolean packRecv() {
        opcode = -1;
        pack.clear();
        if (pack.pipeRecv(pipe, 0, size, 144) != size) {
            pipe.setClose();
            return true;
        }
        opcode = pack.getByte(0);
        immediately = (opcode & 0x40) != 0;
        opcode &= 0x3f;
        int datSiz = pack.msbGetD(4);
        if ((datSiz >>> 24) != 0) {
            pipe.setClose();
            return true;
        }
        if (datSiz < 1) {
            return false;
        }
        int pad = (datSiz & 3);
        if (pad != 0) {
            pad = 4 - pad;
        }
        datSiz += pad;
        if (pack.pipeRecv(pipe, size, datSiz, 144) != datSiz) {
            pipe.setClose();
            return true;
        }
        pack.setDataSize(pack.dataSize() - pad);
        return false;
    }

    /**
     * send one packet
     */
    public void packSend() {
        pack.merge2beg();
        int datSiz = pack.dataSize();
        pack.putByte(0, opcode);
        pack.msbPutD(4, datSiz);
        pack.putSkip(size);
        pack.merge2beg();
        int pad = (datSiz & 3);
        if (pad != 0) {
            pad = 4 - pad;
            pack.putFill(0, pad, 0);
            pack.putSkip(pad);
            pack.merge2end();
        }
        pack.pipeSend(pipe, 0, pack.dataSize(), 3);
    }

    /**
     * dump message
     *
     * @return string
     */
    public String dumpHdr() {
        return "opc=" + opcode2string(opcode) + " flg=" + flags + " rsp=" + respon + " stt=" + status + " lun=" + lun
                + " task=" + taskTag + " snack=" + snackTag + " cmd=" + cmdSeq + " stat=" + statSeq + " scsi="
                + bits.byteDump(scsi, 0, -1) + " xfer=" + xferLen + " cmdMax=" + cmdMax + " dat=" + datSeq + " birs=" + biRes
                + " rsid=" + resid + " tskRef=" + taskRef + " cmdRef=" + cmdRef + " xferTag=" + xferTag + " bufOfs=" + bufOfs
                + " r2t=" + r2tSeq;
    }

    /**
     * dump message
     *
     * @return string
     */
    public String dumpTxt() {
        String s = "";
        for (int i = 0; i < text.size(); i++) {
            s += " " + text.get(i);
        }
        return s;
    }

    /**
     * parse scsi request
     *
     * @return false on success, true on error
     */
    public boolean parseScsiReq() {
        if (opcode != opcCscsi) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferLen = pack.msbGetD(20);
        cmdSeq = pack.msbGetD(24);
        statSeq = pack.msbGetD(28);
        scsi = new byte[16];
        pack.getCopy(scsi, 0, 32, scsi.length);
        pack.getSkip(size);
        return false;
    }

    /**
     * create scsi request
     */
    public void createScsiReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcCscsi;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferLen);
        pack.msbPutD(24, cmdSeq);
        pack.msbPutD(28, statSeq);
        pack.putCopy(scsi, 0, 32, scsi.length);
    }

    /**
     * parse scsi response
     *
     * @return false on success, true on error
     */
    public boolean parseScsiRep() {
        if (opcode != opcRscsi) {
            return true;
        }
        flags = pack.getByte(1);
        respon = pack.getByte(2);
        status = pack.getByte(3);
        taskTag = pack.msbGetD(16);
        snackTag = pack.msbGetD(20);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        datSeq = pack.msbGetD(36);
        biRes = pack.msbGetD(40);
        resid = pack.msbGetD(44);
        pack.getSkip(size);
        return false;
    }

    /**
     * create scsi response
     */
    public void createScsiRep() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRscsi;
        pack.putByte(1, flags);
        pack.putByte(2, respon);
        pack.putByte(3, status);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, snackTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
        pack.msbPutD(36, datSeq);
        pack.msbPutD(40, biRes);
        pack.msbPutD(44, resid);
    }

    /**
     * parse task request
     *
     * @return false on success, true on error
     */
    public boolean parseTaskReq() {
        if (opcode != opcCtask) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        taskRef = pack.msbGetD(20);
        cmdSeq = pack.msbGetD(24);
        statSeq = pack.msbGetD(28);
        cmdRef = pack.msbGetD(32);
        datSeq = pack.msbGetD(36);
        pack.getSkip(size);
        return false;
    }

    /**
     * create task request
     */
    public void createTaskReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcCtask;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, taskRef);
        pack.msbPutD(24, cmdSeq);
        pack.msbPutD(28, statSeq);
        pack.msbPutD(32, cmdRef);
        pack.msbPutD(36, datSeq);
    }

    /**
     * parse task response
     *
     * @return false on success, true on error
     */
    public boolean parseTaskRep() {
        if (opcode != opcRtask) {
            return true;
        }
        flags = pack.getByte(1);
        respon = pack.getByte(2);
        taskTag = pack.msbGetD(16);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        pack.getSkip(size);
        return false;
    }

    /**
     * create task response
     */
    public void createTaskRep() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRtask;
        pack.putByte(1, flags);
        pack.putByte(2, respon);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
    }

    /**
     * parse data request
     *
     * @return false on success, true on error
     */
    public boolean parseDataReq() {
        if (opcode != opcCdata) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetD(20);
        statSeq = pack.msbGetD(28);
        datSeq = pack.msbGetD(36);
        bufOfs = pack.msbGetD(40);
        pack.getSkip(size);
        return false;
    }

    /**
     * create data request
     */
    public void createDataReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcCdata;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferTag);
        pack.msbPutD(28, statSeq);
        pack.msbPutD(36, datSeq);
        pack.msbPutD(40, bufOfs);
    }

    /**
     * parse data response
     *
     * @return false on success, true on error
     */
    public boolean parseDataRep() {
        if (opcode != opcRdata) {
            return true;
        }
        flags = pack.getByte(1);
        status = pack.getByte(3);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetD(20);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        datSeq = pack.msbGetD(36);
        bufOfs = pack.msbGetD(40);
        resid = pack.msbGetD(44);
        pack.getSkip(size);
        return false;
    }

    /**
     * create data response
     */
    public void createDataRep() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRdata;
        pack.putByte(1, flags);
        pack.putByte(3, status);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
        pack.msbPutD(36, datSeq);
        pack.msbPutD(40, bufOfs);
        pack.msbPutD(44, resid);
    }

    /**
     * parse ready to transfer
     *
     * @return false on success, true on error
     */
    public boolean parseR2t() {
        if (opcode != opcRready) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetD(20);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        r2tSeq = pack.msbGetD(36);
        bufOfs = pack.msbGetD(40);
        xferLen = pack.msbGetD(44);
        pack.getSkip(size);
        return false;
    }

    /**
     * create ready to transfer
     */
    public void createR2t() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRready;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
        pack.msbPutD(36, r2tSeq);
        pack.msbPutD(40, bufOfs);
        pack.msbPutD(44, xferLen);
    }

    /**
     * parse async event
     *
     * @return false on success, true on error
     */
    public boolean parseAsync() {
        if (opcode != opcRasync) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        r2tSeq = pack.msbGetD(36);
        bufOfs = pack.msbGetD(40);
        pack.getSkip(size);
        return false;
    }

    /**
     * create async event
     */
    public void createAsync() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRasync;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
        pack.msbPutD(36, r2tSeq);
        pack.msbPutD(40, bufOfs);
    }

    /**
     * parse text request
     *
     * @return false on success, true on error
     */
    public boolean parseTextReq() {
        if (opcode != opcCtext) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetD(20);
        cmdSeq = pack.msbGetD(24);
        statSeq = pack.msbGetD(28);
        pack.getSkip(size);
        return false;
    }

    /**
     * create text request
     */
    public void createTextReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcCtext;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferTag);
        pack.msbPutD(24, cmdSeq);
        pack.msbPutD(28, statSeq);
    }

    /**
     * parse text response
     *
     * @return false on success, true on error
     */
    public boolean parseTextRep() {
        if (opcode != opcRtext) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetD(20);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        pack.getSkip(size);
        return false;
    }

    /**
     * create text response
     */
    public void createTextRep() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRtext;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
    }

    /**
     * parse login request
     *
     * @return false on success, true on error
     */
    public boolean parseLoginReq() {
        if (opcode != opcClogin) {
            return true;
        }
        flags = pack.getByte(1);
        status = pack.getByte(2);
        respon = pack.getByte(3);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetW(20);
        cmdSeq = pack.msbGetD(24);
        statSeq = pack.msbGetD(28);
        pack.getSkip(size);
        return false;
    }

    /**
     * create login request
     */
    public void createLoginReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRlogin;
        pack.putByte(1, flags);
        pack.putByte(2, status);
        pack.putByte(3, respon);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutW(20, xferTag);
        pack.msbPutD(24, cmdSeq);
        pack.msbPutD(28, statSeq);
    }

    /**
     * parse login response
     *
     * @return false on success, true on error
     */
    public boolean parseLoginRep() {
        if (opcode != opcRlogin) {
            return true;
        }
        flags = pack.getByte(1);
        status = pack.getByte(2);
        respon = pack.getByte(3);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        resid = pack.msbGetD(36);
        pack.getSkip(size);
        return false;
    }

    /**
     * create login response
     */
    public void createLoginRep() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRlogin;
        pack.putByte(1, flags);
        pack.putByte(2, status);
        pack.putByte(3, respon);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
        pack.msbPutD(36, resid);
    }

    /**
     * parse logout request
     *
     * @return false on success, true on error
     */
    public boolean parseLogoutReq() {
        if (opcode != opcClogout) {
            return true;
        }
        flags = pack.getByte(1);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetW(20);
        cmdSeq = pack.msbGetD(24);
        statSeq = pack.msbGetD(28);
        pack.getSkip(size);
        return false;
    }

    /**
     * create logout request
     */
    public void createLogoutReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcClogout;
        pack.putByte(1, flags);
        pack.msbPutD(16, taskTag);
        pack.msbPutW(20, xferTag);
        pack.msbPutD(24, cmdSeq);
        pack.msbPutD(28, statSeq);
    }

    /**
     * parse logout response
     *
     * @return false on success, true on error
     */
    public boolean parseLogoutRep() {
        if (opcode != opcRlogout) {
            return true;
        }
        flags = pack.getByte(1);
        respon = pack.getByte(2);
        taskTag = pack.msbGetD(16);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        resid = pack.msbGetW(40);
        biRes = pack.msbGetW(42);
        pack.getSkip(size);
        return false;
    }

    /**
     * create logout response
     */
    public void createLogoutRep() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRlogout;
        pack.putByte(1, flags);
        pack.putByte(2, respon);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
        pack.msbPutW(40, resid);
        pack.msbPutW(42, biRes);
    }

    /**
     * parse snack request
     *
     * @return false on success, true on error
     */
    public boolean parseSnackReq() {
        if (opcode != opcCsnack) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        snackTag = pack.msbGetD(20);
        statSeq = pack.msbGetD(28);
        xferTag = pack.msbGetD(40);
        xferLen = pack.msbGetD(44);
        pack.getSkip(size);
        return false;
    }

    /**
     * create snack request
     */
    public void createSnackReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcCsnack;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, snackTag);
        pack.msbPutD(28, statSeq);
        pack.msbPutD(40, xferTag);
        pack.msbPutD(44, xferLen);
    }

    /**
     * parse reject
     *
     * @return false on success, true on error
     */
    public boolean parseReject() {
        if (opcode != opcReject) {
            return true;
        }
        flags = pack.getByte(1);
        respon = pack.getByte(2);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        datSeq = pack.msbGetD(36);
        pack.getSkip(size);
        return false;
    }

    /**
     * create reject
     */
    public void createReject() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcReject;
        pack.putByte(1, flags);
        pack.putByte(2, respon);
        pack.msbPutD(16, -1);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
        pack.msbPutD(36, datSeq);
    }

    /**
     * parse noop request
     *
     * @return false on success, true on error
     */
    public boolean parseNopReq() {
        if (opcode != opcCnop) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetD(20);
        cmdSeq = pack.msbGetD(24);
        statSeq = pack.msbGetD(28);
        pack.getSkip(size);
        return false;
    }

    /**
     * create nop request
     */
    public void createNopReq() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcCnop;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferTag);
        pack.msbPutD(24, cmdSeq);
        pack.msbPutD(28, statSeq);
    }

    /**
     * parse noop reply
     *
     * @return false on success, true on error
     */
    public boolean parseNopRep() {
        if (opcode != opcRnop) {
            return true;
        }
        flags = pack.getByte(1);
        lun = pack.msbGetQ(8);
        taskTag = pack.msbGetD(16);
        xferTag = pack.msbGetD(20);
        statSeq = pack.msbGetD(24);
        cmdSeq = pack.msbGetD(28);
        cmdMax = pack.msbGetD(32);
        pack.getSkip(size);
        return false;
    }

    /**
     * create nop response
     */
    public void createNopRep() {
        pack.merge2beg();
        pack.putFill(0, size, 0);
        opcode = opcRnop;
        pack.putByte(1, flags);
        pack.msbPutQ(8, lun);
        pack.msbPutD(16, taskTag);
        pack.msbPutD(20, xferTag);
        pack.msbPutD(24, statSeq);
        pack.msbPutD(28, cmdSeq);
        pack.msbPutD(32, cmdMax);
    }

    /**
     * parse tlvs from packet
     */
    public void parseText() {
        text = new ArrayList<String>();
        for (;;) {
            if (pack.dataSize() < 1) {
                return;
            }
            String s = pack.getAsciiZ(0, 1024, 0);
            text.add(s);
            pack.getSkip(s.length() + 1);
        }
    }

    /**
     * create tlvs to packet
     */
    public void createText() {
        pack.clear();
        for (int i = 0; i < text.size(); i++) {
            String s = text.get(i);
            pack.putAsciiZ(0, s.length() + 1, s, 0);
            pack.putSkip(s.length() + 1);
        }
    }

    /**
     * find text
     *
     * @param beg needed tlv
     * @return null if not found, value othervise
     */
    public String findText(String beg) {
        beg = beg.toLowerCase();
        for (int i = 0; i < text.size(); i++) {
            String s = text.get(i);
            int o = s.indexOf("=");
            if (o < 0) {
                continue;
            }
            String a = s.substring(0, o).trim().toLowerCase();
            s = s.substring(o + 1, s.length()).trim();
            if (a.equals(beg)) {
                return s;
            }
        }
        return null;
    }

    /**
     * clear text
     */
    public void clearText() {
        text = new ArrayList<String>();
    }

    /**
     * add one text
     *
     * @param typ type
     * @param val value
     */
    public void addText(String typ, String val) {
        text.add(typ + "=" + val);
    }

    /**
     * add one text if asked
     *
     * @param old old packet
     * @param typ type
     * @param val value
     */
    public void addTextIf(packIscsi old, String typ, String val) {
        if (old.findText(typ) == null) {
            return;
        }
        addText(typ, val);
    }

}
