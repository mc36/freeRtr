package org.freertr.serv;

import java.io.ByteArrayOutputStream;
import org.freertr.pipe.pipeSide;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * class streamer
 *
 * @author matecsaba
 */
public class servHttpStrmC implements Runnable {

    private final servHttpHost cfg;

    /**
     * create instance
     *
     * @param host config to use
     */
    protected servHttpStrmC(servHttpHost host) {
        cfg = host;
    }

    /**
     * start streaming
     */
    protected void doStart() {
        logger.startThread(this);
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        cfg.allowClassC = null;
    }

    private void doWork() {
        if (debugger.servHttpTraf) {
            logger.debug("serving");
        }
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        for (;;) {
            buf.reset();
            try {
                String s = (String) cfg.allowClassM.invoke(cfg.allowClassO, null, null, null, null, null, null, buf);
                if (s == null) {
                    break;
                }
            } catch (Exception e) {
                logger.traceback(e);
                break;
            }
            int i = cfg.allowClassC.size() - 1;
            if (i < 0) {
                break;
            }
            byte[] res = buf.toByteArray();
            for (; i >= 0; i--) {
                pipeSide pip = cfg.allowClassC.get(i);
                if (pip.isClosed() == 0) {
                    pip.nonBlockPut(res, 0, res.length);
                    continue;
                }
                cfg.allowClassC.remove(i);
                pip.setClose();
            }
        }
        for (int i = cfg.allowClassC.size() - 1; i >= 0; i--) {
            cfg.allowClassC.get(i).setClose();
        }
        try {
            cfg.allowClassM.invoke(cfg.allowClassO, null, null, null, null, null, null, null);
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (debugger.servHttpTraf) {
            logger.debug("stopped");
        }

    }

}
