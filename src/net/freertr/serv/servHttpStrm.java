package net.freertr.serv;

import net.freertr.addr.addrIP;
import net.freertr.enc.encUrl;
import net.freertr.pipe.pipeSide;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.version;

/**
 * http streamer
 *
 * @author matecsaba
 */
public class servHttpStrm implements Runnable {

    private final servHttpHost cfg;

    /**
     * create instance
     *
     * @param host config to use
     */
    protected servHttpStrm(servHttpHost host) {
        cfg = host;
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doWork() {
        if (debugger.servHttpTraf) {
            logger.debug("startup");
        }
        bits.sleep(1000);
        encUrl srvUrl = encUrl.parseOne(cfg.streamT);
        addrIP adr = userTerminal.justResolv(srvUrl.server, cfg.streamP.prefer);
        if (adr == null) {
            return;
        }
        pipeSide cnn = cfg.streamP.doConnect(servGeneric.protoTcp, adr, srvUrl.getPort(new servHttp().srvPort()), "http");
        if (cnn == null) {
            return;
        }
        cfg.streamS = cnn;
        if (debugger.servHttpTraf) {
            logger.debug("conned");
        }
        cnn.lineTx = pipeSide.modTyp.modeCRLF;
        cnn.lineRx = pipeSide.modTyp.modeCRtryLF;
        cnn.linePut("GET " + srvUrl.toURL(false, false, true, true) + " HTTP/1.1");
        cnn.linePut("User-Agent: " + version.usrAgnt + " [streaming]");
        cnn.linePut("Host: " + srvUrl.server);
        cnn.linePut("Accept: */*");
        cnn.linePut("Accept-Language: *");
        cnn.linePut("Accept-Charset: *");
        cnn.linePut("Accept-Encoding: identity");
        cnn.linePut("Connection: Close");
        cnn.linePut("");
        for (;;) {
            String s = cnn.lineGet(1);
            if (s == null) {
                break;
            }
            if (s.length() < 1) {
                break;
            }
            if (debugger.servHttpTraf) {
                logger.debug("rx: " + s);
            }
        }
        if (debugger.servHttpTraf) {
            logger.debug("serving");
        }
        for (;;) {
            byte[] buf = new byte[1024];
            int siz = cnn.blockingGet(buf, 0, buf.length);
            if (siz < 1) {
                if (cnn.isClosed() != 0) {
                    break;
                }
                bits.sleep(1000);
                continue;
            }
            int i = cfg.streamC.size() - 1;
            if (i < 0) {
                break;
            }
            for (; i >= 0; i--) {
                pipeSide pip = cfg.streamC.get(i);
                if (pip.isClosed() == 0) {
                    pip.nonBlockPut(buf, 0, siz);
                    continue;
                }
                cfg.streamC.remove(i);
                pip.setClose();
            }
        }
        cnn.setClose();
        for (int i = cfg.streamC.size() - 1; i >= 0; i--) {
            cfg.streamC.get(i).setClose();
        }
        if (debugger.servHttpTraf) {
            logger.debug("stopped");
        }
    }

}
