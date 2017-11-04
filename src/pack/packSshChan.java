package pack;

import util.bits;
import util.debugger;
import util.logger;

/**
 * secure shell connection (rfc4254) protocol
 *
 * @author matecsaba
 */
public class packSshChan {

    /**
     * type of channel
     */
    public String type;

    /**
     * need reply status
     */
    public boolean needReply;

    /**
     * remote channel id
     */
    public int chanRem;

    /**
     * local channel id
     */
    public int chanLoc;

    /**
     * initial window size
     */
    public int window;

    /**
     * max packet size
     */
    public int maxPack;

    /**
     * data buffer
     */
    public byte[] buf;

    private final packSsh lower;

    /**
     * pty request
     */
    public final static String reqPtyReq = "pty-req";

    /**
     * shell request
     */
    public final static String reqShell = "shell";

    /**
     * environment request
     */
    public final static String reqEnv = "env";

    /**
     * create instance
     *
     * @param pack lower layer
     */
    public packSshChan(packSsh pack) {
        lower = pack;
    }

    /**
     * parse channel open
     *
     * @return false on success, true on error
     */
    public boolean chanOpenParse() {
        if (lower.pckTyp != packSsh.typeChanOpen) {
            return true;
        }
        type = lower.stringRead();
        chanRem = lower.pckDat.msbGetD(0);
        window = lower.pckDat.msbGetD(4);
        maxPack = lower.pckDat.msbGetD(8);
        lower.pckDat.getSkip(12);
        if (debugger.secSshTraf) {
            chanOpenDump("rx");
        }
        return false;
    }

    /**
     * create channel open
     */
    public void chanOpenCreate() {
        type = packSsh.sessType;
        chanRem = bits.randomD();
        window = 0x7fffffff;
        maxPack = 1024;
        if (debugger.secSshTraf) {
            chanOpenDump("tx");
        }
        lower.pckTyp = packSsh.typeChanOpen;
        lower.pckDat.clear();
        lower.stringWrite(type);
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.msbPutD(4, window);
        lower.pckDat.msbPutD(8, maxPack);
        lower.pckDat.putSkip(12);
    }

    private void chanOpenDump(String dir) {
        logger.debug(dir + " type=" + type + " chan=" + chanRem + "/" + chanLoc + " win=" + window + " pack=" + maxPack);
    }

    /**
     * parse channel open
     *
     * @return false on success, true on error
     */
    public boolean openDoneParse() {
        if (lower.pckTyp != packSsh.typeOpenConf) {
            return true;
        }
        chanRem = lower.pckDat.msbGetD(0);
        chanLoc = lower.pckDat.msbGetD(4);
        window = lower.pckDat.msbGetD(8);
        maxPack = lower.pckDat.msbGetD(12);
        lower.pckDat.putSkip(16);
        if (debugger.secSshTraf) {
            chanOpenDump("rx");
        }
        return false;
    }

    /**
     * create channel open
     */
    public void openDoneCreate() {
        chanLoc = bits.randomD();
        window = 0x7fffffff;
        maxPack = 1024;
        if (debugger.secSshTraf) {
            chanOpenDump("tx");
        }
        lower.pckTyp = packSsh.typeOpenConf;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.msbPutD(4, chanLoc);
        lower.pckDat.msbPutD(8, window);
        lower.pckDat.msbPutD(12, maxPack);
        lower.pckDat.putSkip(16);
    }

    /**
     * parse channel fail
     *
     * @return false on success, true on error
     */
    public boolean openFailParse() {
        if (lower.pckTyp != packSsh.typeOpenFail) {
            return true;
        }
        chanRem = lower.pckDat.msbGetD(0);
        return false;
    }

    /**
     * create channel fail
     */
    public void openFailCreate() {
        lower.pckTyp = packSsh.typeOpenFail;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.msbPutD(4, 1);
        lower.pckDat.putSkip(8);
        lower.stringWrite("");
        lower.stringWrite("");
    }

    /**
     * parse channel request
     *
     * @return false on success, true on error
     */
    public boolean chanReqParse() {
        if (lower.pckTyp != packSsh.typeChanReq) {
            return true;
        }
        chanRem = lower.pckDat.msbGetD(0);
        lower.pckDat.getSkip(4);
        type = lower.stringRead();
        needReply = lower.pckDat.getByte(0) != 0;
        if (debugger.secSshTraf) {
            chanReqDump("rx");
        }
        return false;
    }

    /**
     * create channel request
     */
    public void chanReqCreatePty() {
        type = reqPtyReq;
        needReply = true;
        if (debugger.secSshTraf) {
            chanReqDump("tx");
        }
        chanReqCreate();
        lower.stringWrite("vt100");
        lower.pckDat.msbPutD(0, 80);
        lower.pckDat.msbPutD(4, 25);
        lower.pckDat.msbPutD(8, 640);
        lower.pckDat.msbPutD(12, 480);
        lower.pckDat.putSkip(16);
        lower.stringWrite("");
    }

    /**
     * create channel request
     */
    public void chanReqCreateShll() {
        type = reqShell;
        needReply = true;
        if (debugger.secSshTraf) {
            chanReqDump("tx");
        }
        chanReqCreate();
    }

    private void chanReqCreate() {
        lower.pckTyp = packSsh.typeChanReq;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.putSkip(4);
        lower.stringWrite(type);
        if (needReply) {
            lower.pckDat.putByte(0, 1);
        } else {
            lower.pckDat.putByte(0, 0);
        }
        lower.pckDat.putSkip(1);
    }

    private void chanReqDump(String dir) {
        logger.debug(dir + " type=" + type + " chan=" + chanRem + " reply=" + needReply);
    }

    /**
     * parse channel success
     *
     * @return false on success, true on error
     */
    public boolean chanSuccParse() {
        if (lower.pckTyp != packSsh.typeChanSucc) {
            return true;
        }
        chanRem = lower.pckDat.msbGetD(0);
        return false;
    }

    /**
     * create channel success
     */
    public void chanSuccCreate() {
        lower.pckTyp = packSsh.typeChanSucc;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.putSkip(4);
    }

    /**
     * parse channel failure
     *
     * @return false on success, true on error
     */
    public boolean chanFailParse() {
        if (lower.pckTyp != packSsh.typeChanErr) {
            return true;
        }
        chanRem = lower.pckDat.msbGetD(0);
        return false;
    }

    /**
     * create channel failure
     */
    public void chanFailCreate() {
        lower.pckTyp = packSsh.typeChanErr;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.putSkip(4);
    }

    /**
     * parse channel data
     *
     * @return false on success, true on error
     */
    public boolean chanDataParse() {
        if ((lower.pckTyp != packSsh.typeChanData) && (lower.pckTyp != packSsh.typeChanExtDat)) {
            return true;
        }
        chanRem = lower.pckDat.msbGetD(0);
        lower.pckDat.getSkip(4);
        buf = lower.bytesRead();
        return false;
    }

    /**
     * create channel data
     */
    public void chanDataCreate() {
        lower.pckTyp = packSsh.typeChanData;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.putSkip(4);
        lower.bytesWrite(buf);
    }

    /**
     * parse window adjust
     *
     * @return false on success, true on error
     */
    public boolean chanWindowParse() {
        if (lower.pckTyp != packSsh.typeChanWin) {
            return true;
        }
        chanRem = lower.pckDat.msbGetD(0);
        window = lower.pckDat.msbGetD(4);
        return false;
    }

    /**
     * create window adjust
     */
    public void chanWindowCreate() {
        lower.pckTyp = packSsh.typeChanWin;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, chanRem);
        lower.pckDat.msbPutD(4, window);
        lower.pckDat.putSkip(8);
    }

}
