package org.freertr.user;

import java.io.File;
import java.io.RandomAccessFile;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.pipe.pipeTerm;
import org.freertr.util.logger;

/**
 * terminal session recorder
 *
 * @author matecsaba
 */
public class userRecord implements Runnable {

    private final userExec orig;

    private final String recn;

    private userRead rdr;

    private userExec exe;

    private userConfig cfg;

    /**
     * screen recorder
     *
     * @param fn filename
     * @param exe exec to use
     */
    public userRecord(String fn, userExec exe) {
        orig = exe;
        recn = fn;
    }

    /**
     * do the work
     */
    public void doWork() {
        orig.pipe.linePut("recording to " + recn);
        RandomAccessFile recf;
        try {
            recf = new RandomAccessFile(new File(recn), "rw");
            recf.setLength(0);
        } catch (Exception e) {
            orig.pipe.linePut("file open error");
            return;
        }
        pipeLine pl = new pipeLine(32768, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        rdr = new userRead(pip, null);
        pip.settingsCopy(orig.pipe);
        pip.setTime(0);
        exe = new userExec(pip, rdr);
        cfg = new userConfig(pip, rdr);
        orig.copy2exec(exe);
        orig.copy2cfg(cfg);
        pip = pl.getSide();
        pip.setTime(0);
        new Thread(this).start();
        pipeTerm trm = new pipeTerm(orig.pipe, pip, recf);
        trm.doTerm();
        exe.pipe.setClose();
        try {
            recf.close();
        } catch (Exception e) {
        }
    }

    public void run() {
        try {
            userLine.doCommands(rdr, exe, cfg);
        } catch (Exception e) {
            logger.traceback(e);
        }
        exe.pipe.setClose();
    }

}
