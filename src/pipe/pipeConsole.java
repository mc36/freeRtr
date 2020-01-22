package pipe;

import util.logger;
import java.io.File;

/**
 * convert console to pipeline
 *
 * @author matecsaba
 */
public class pipeConsole implements Runnable {

    private pipeSide pipe;

    /**
     * attach console to a pipeline
     *
     * @param pip where to attach
     */
    public pipeConsole(pipeSide pip) {
        pipe = pip;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new pipeConsoleDisp(pipe);
        new Thread(this).start();
    }

    /**
     * create a pipeline attached to the console
     *
     * @return the new pipeline
     */
    public static pipeSide create() {
        pipeLine p = new pipeLine(65536, false);
        new pipeConsole(p.getSide());
        pipeSide ps = p.getSide();
        ps.lineTx = pipeSide.modTyp.modeCRLF;
        ps.lineRx = pipeSide.modTyp.modeCRorLF;
        ps.timeout = 0;
        return ps;
    }

    private boolean setRaw(boolean enable) {
        try {
            File sh = new File("/bin/sh");
            if (!sh.exists())
                return false;

            Runtime rtm = Runtime.getRuntime();
            String setting = enable ? "raw" : "cooked";
            String[] cmd = {sh.toString(), "-c", "stty " + setting + " </dev/tty"};
            rtm.exec(cmd).waitFor();
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public void run() {
        boolean isRaw = setRaw(true);

        try {
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                byte buf[] = new byte[1];
                int i = System.in.read(buf);
                if (i < 0) {
                    break;
                }
                pipe.blockingPut(buf, 0, i);
            }
        } catch (Exception e) {
            logger.traceback(e);
        } finally {
            if (isRaw)
                setRaw(false);
        }
        pipe.setClose();
    }

}

class pipeConsoleDisp implements Runnable {

    private pipeSide pipe;

    public pipeConsoleDisp(pipeSide pip) {
        pipe = pip;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                byte buf[] = new byte[1024];
                int siz = pipe.blockingGet(buf, 0, buf.length);
                if (siz < 0) {
                    break;
                }
                String a = "";
                for (int i = 0; i < siz; i++) {
                    a += (char) (buf[i] & 0xff);
                }
                System.out.print(a);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
