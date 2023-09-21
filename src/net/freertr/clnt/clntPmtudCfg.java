package net.freertr.clnt;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.ip.ipFwd;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 *
 * @author matecsaba
 */
public class clntPmtudCfg {

    /**
     * pmtud min
     */
    public int pmtudMin;

    /**
     * pmtud max
     */
    public int pmtudMax;

    /**
     * pmtud timeout
     */
    public int pmtudTim;

    public static void getConfig(List<String> lst, clntPmtudCfg cfg, String beg) {
        if (cfg == null) {
            return;
        }
        if (cfg.pmtudTim < 1) {
            return;
        }
        lst.add(beg + cfg.pmtudMin + " " + cfg.pmtudMax + " " + cfg.pmtudTim);
    }

    /**
     * perform configuration tasks
     *
     * @param cfg config to update
     * @param cmd commands to do
     * @param negated if negation needed
     * @return true on error, false on success
     */
    public static boolean doConfig(clntPmtudCfg cfg, cmds cmd, boolean negated) {
        if (negated) {
            cfg.pmtudMin = 0;
            cfg.pmtudMax = 0;
            cfg.pmtudTim = 0;
            return true;
        }
        cfg.pmtudMin = bits.str2num(cmd.word());
        cfg.pmtudMax = bits.str2num(cmd.word());
        cfg.pmtudTim = bits.str2num(cmd.word());
        if (cfg.pmtudMin < cfg.pmtudMax) {
            return false;
        }
        cfg.pmtudMin = 0;
        cfg.pmtudMax = 0;
        cfg.pmtudTim = 0;
        return true;
    }

    public static clntPmtudWrk getWorker(clntPmtudCfg cfg, ipFwd vrf, addrIP rem, addrIP sou) {
        if (cfg == null) {
            return null;
        }
        logger.warn("pmtuding " + rem + " from " + sou);
        pipeLine pl = new pipeLine(65536, true);
        pipeSide ps = pl.getSide();
        clntPmtudWrk pm = new clntPmtudWrk(ps, rem, vrf, sou);
        pm.min = cfg.pmtudMin;
        pm.max = cfg.pmtudMax;
        pm.delay = 100;
        pm.timeout = cfg.pmtudTim;
        return pm;
    }

}
