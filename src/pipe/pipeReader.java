package pipe;

import java.util.ArrayList;
import java.util.List;
import util.bits;
import util.logger;

/**
 * read pipeline line by line
 *
 * @author matecsaba
 */
public class pipeReader implements Runnable {

    private final pipeLine pipP;

    private final pipeSide pipC;

    private final pipeSide pipS;

    private final List<String> res;

    private boolean working = true;

    /**
     * construct reader
     */
    public pipeReader() {
        pipP = new pipeLine(65536, false);
        pipC = pipP.getSide();
        pipS = pipP.getSide();
        res = new ArrayList<String>();
        new Thread(this).start();
    }

    /**
     * set line mode
     *
     * @param mod mode
     */
    public void setLineMode(pipeSide.modTyp mod) {
        pipC.lineRx = mod;
        pipC.lineTx = mod;
        pipS.lineRx = mod;
        pipS.lineTx = mod;
    }

    /**
     * stop work
     */
    public void stopWork() {
        pipC.setClose();
        pipS.setClose();
        pipP.setClose();
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        working = false;
        stopWork();
    }

    /**
     * get user side pipeline
     *
     * @return pipeline to use
     */
    public pipeSide getPipe() {
        return pipC;
    }

    /**
     * wait for termination
     */
    public void waitFor() {
        for (;;) {
            if (!working) {
                return;
            }
            bits.sleep(500);
        }
    }

    /**
     * get collected result
     *
     * @return lines readed
     */
    public List<String> getResult() {
        return res;
    }

    private void doWork() {
        for (;;) {
            String s = pipS.lineGet(0x11);
            res.add(s);
            if (s.length() > 0) {
                continue;
            }
            if (pipS.ready2rx() > 0) {
                continue;
            }
            if (pipS.isClosed() != 0) {
                break;
            }
        }
    }

}
