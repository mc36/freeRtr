package user;

import cfg.cfgAll;
import pipe.pipeSide;
import util.bits;

/**
 * gps sessions
 *
 * @author matecsaba
 */
public class userGps {

    private final pipeSide pipe;

    /**
     * create instance
     *
     * @param pip pipeline to use
     */
    public userGps(pipeSide pip) {
        pipe = pip;
    }

    /**
     * do tx work
     */
    public void doWorkTx() {
        for (;;) {
            if (pipe.ready2rx() > 0) {
                break;
            }
            bits.sleep(1000);
            String a = bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 5) + ".000";
            pipe.linePut("$GPGGA," + a + ",0000.000000,N,00000.000000,E,1,08,1.4,124.9,M,41.1,M,,");
            pipe.linePut("$GPRMC," + a + ",A,0000.000000,N,00000.000000,E,0.0,357.6,070414,,,A");
        }
    }

    /**
     * do rx work
     */
    public void doWorkRx() {
        for (;;) {
            String s = pipe.lineGet(1);
            if (s == null) {
                break;
            }
            if (s.length() < 1) {
                break;
            }
        }
    }

}
