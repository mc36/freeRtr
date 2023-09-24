package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.ip.ipFwd;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * generic pmtud configuration
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

    /**
     * create instance
     */
    public clntPmtudCfg() {
    }

    /**
     * perform configuration tasks
     *
     * @param cfg config to update
     * @param cmd commands to do
     * @param negated if negation needed
     * @return null if cleared, config on success
     */
    public static final clntPmtudCfg doCfgStr(clntPmtudCfg cfg, cmds cmd, boolean negated) {
        if (cfg == null) {
            cfg = new clntPmtudCfg();
        }
        cfg.pmtudMin = 0;
        cfg.pmtudMax = 0;
        cfg.pmtudTim = 0;
        if (negated) {
            return cfg;
        }
        cfg.pmtudMin = bits.str2num(cmd.word());
        cfg.pmtudMax = bits.str2num(cmd.word());
        cfg.pmtudTim = bits.str2num(cmd.word());
        if (cfg.pmtudMin < cfg.pmtudMax) {
            return cfg;
        }
        cfg.pmtudMin = 0;
        cfg.pmtudMax = 0;
        cfg.pmtudTim = 0;
        return cfg;
    }

    /**
     * perform the work
     *
     * @param cfg configuration to use
     * @param vrf vrf to use
     * @param trg target address
     * @param src source address
     * @return work done, null if error happened
     */
    public static clntPmtudWrk doWork(clntPmtudCfg cfg, ipFwd vrf, addrIP trg, addrIP src) {
        clntPmtudWrk wrk = getWorker(cfg, vrf, trg, src);
        if (wrk == null) {
            return null;
        }
        logger.info("pmtuding " + vrf + " " + trg + " " + src);
        if (wrk.doer() == null) {
            logger.error("failed pmtud " + vrf + " " + trg + " " + src);
            return null;
        }
        logger.info("pmtuded " + vrf + " " + trg + " " + src + " " + wrk);
        return wrk;
    }

    private static clntPmtudWrk getWorker(clntPmtudCfg cfg, ipFwd vrf, addrIP trg, addrIP src) {
        if (cfg == null) {
            return null;
        }
        if (cfg.pmtudTim < 1) {
            return null;
        }
        if (cfg.pmtudMin >= cfg.pmtudMax) {
            return null;
        }
        pipeLine pl = new pipeLine(65536, true);
        pipeSide ps = pl.getSide();
        clntPmtudWrk pm = new clntPmtudWrk(ps, trg, vrf, src);
        pm.min = cfg.pmtudMin;
        pm.max = cfg.pmtudMax;
        pm.delay = 100;
        pm.timeout = cfg.pmtudTim;
        pm.doer();
        return pm;
    }

}
