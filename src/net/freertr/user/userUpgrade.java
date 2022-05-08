package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.cry.cryBase64;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashSha2512;
import net.freertr.cry.cryHashSha3512;
import net.freertr.cry.cryKeyRSA;
import net.freertr.cry.cryUtils;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.uniResLoc;
import net.freertr.util.verCore;
import net.freertr.util.version;

/**
 * process sw upgrade
 *
 * @author matecsaba
 */
public class userUpgrade {

    /**
     * version extension
     */
    public static String verExt = ".ver";
    /**
     * backup extension
     */
    public static String bakExt = ".bak";

    /**
     * update progress indicator, 0=none, 1=upgrade, 2=auto-revert
     */
    public static int inProgress = 0;

    private final static int justSimu = 0x1000000;

    private final cmds cmd;

    private final pipeProgress cons;

    private int forces = 0;

    /**
     * create new instance
     *
     * @param c commands to use
     */
    public userUpgrade(cmds c) {
        cmd = c;
        cons = new pipeProgress(cmd.pipe);
    }

    /**
     * calculate hash on file
     *
     * @param n filename
     * @return calculated hash
     */
    public static String calcFileHash(String n) {
        File f = new File(n);
        cryHashGeneric h = new cryHashSha2512();
        h.init();
        if (cryUtils.hashFile(h, f)) {
            return null;
        }
        String a = cryUtils.hash2hex(h);
        h = new cryHashSha3512();
        h.init();
        if (cryUtils.hashFile(h, f)) {
            return null;
        }
        return a + "-" + cryUtils.hash2hex(h);
    }

    /**
     * calculate hash on text
     *
     * @param l lines
     * @return calculated hash
     */
    public static String calcTextHash(List<String> l) {
        cryHashGeneric h = new cryHashSha2512();
        h.init();
        if (cryUtils.hashText(h, l, pipeSide.modTyp.modeLF)) {
            return null;
        }
        String a = cryUtils.hash2hex(h);
        h = new cryHashSha3512();
        h.init();
        if (cryUtils.hashText(h, l, pipeSide.modTyp.modeLF)) {
            return null;
        }
        return a + "-" + cryUtils.hash2hex(h);
    }

    /**
     * get version file name
     *
     * @return version filename
     */
    protected static String myVerFile() {
        return version.myPathName() + verExt;
    }

    /**
     * get archive base name
     *
     * @return filename without path
     */
    public static String myFileName() {
        return new File(version.getFileName()).getName();
    }

    /**
     * generate release version file
     */
    public void doRelease() {
        cryKeyRSA ky = readUpKey(cmd.word());
        if (ky == null) {
            cmd.error("failed to get key!");
            return;
        }
        userUpgradeBlob blb = new userUpgradeBlob();
        blb.putSelf();
        for (;;) {
            String n = cmd.word();
            if (n.length() < 1) {
                break;
            }
            int f = bits.str2num(cmd.word());
            String h = calcFileHash(n);
            if (h == null) {
                cmd.error(n + " not found");
            }
            blb.files.add(new userUpgradeNtry(h, f, n));
        }
        blb.doSign(ky);
        bits.buf2txt(true, blb.getText(2), myVerFile());
        cmd.error(myVerFile() + " written!");
    }

    /**
     * update version core file
     */
    public void doVerCore() {
        cryKeyRSA kc = readUpKey(cmd.word());
        if (kc == null) {
            cmd.error("failed to get current key!");
            return;
        }
        cryKeyRSA ko = readUpKey(cmd.word());
        if (ko == null) {
            cmd.error("failed to get old key!");
            return;
        }
        final String fn = "net/freertr/util/verCore.java";
        final String sy = "    public final static int year = ";
        final String sm = "    public final static int month = ";
        final String sd = "    public final static int day = ";
        final String sc = "    public final static String pubKeyC = ";
        final String so = "    public final static String pubKeyO = ";
        long tim = bits.getTime();
        String vy = (bits.time2num(cfgAll.timeZoneName, tim, 1) % 100) + ";";
        String vm = bits.time2num(cfgAll.timeZoneName, tim, 2) + ";";
        String vd = bits.time2num(cfgAll.timeZoneName, tim, 3) + ";";
        String vc = "\"" + kc.pemWriteStr(true) + "\";";
        String vo = "\"" + ko.pemWriteStr(true) + "\";";
        List<String> txt = bits.txt2buf(fn);
        if (txt == null) {
            cmd.error(fn + " not found!");
            return;
        }
        int o = 0;
        for (int i = 0; i < txt.size(); i++) {
            String a = txt.get(i);
            if (a.startsWith(sy)) {
                txt.set(i, sy + vy);
                o++;
                continue;
            }
            if (a.startsWith(sm)) {
                txt.set(i, sm + vm);
                o++;
                continue;
            }
            if (a.startsWith(sd)) {
                txt.set(i, sd + vd);
                o++;
                continue;
            }
            if (a.startsWith(sc)) {
                txt.set(i, sc + vc);
                o++;
                continue;
            }
            if (a.startsWith(so)) {
                txt.set(i, so + vo);
                o++;
                continue;
            }
        }
        if (bits.buf2txt(true, txt, fn)) {
            return;
        }
        cmd.error(fn + " patched " + o + " times!");
    }

    /**
     * verify installation
     *
     * @param blb blob to use, null if read from disk
     * @return error count
     */
    public int doVerify(userUpgradeBlob blb) {
        int err = 0;
        cmd.error("server: " + cfgAll.upgradeServer);
        cmd.error("archive: " + version.getFileName());
        cmd.error("version: " + myVerFile());
        if (blb == null) {
            List<String> txt = bits.txt2buf(myVerFile());
            if (txt == null) {
                cmd.error("error reading version info!");
            }
            blb = new userUpgradeBlob();
            String s = blb.fromText(txt, true);
            if (s != null) {
                cmd.error("version info parser: " + s);
                err++;
            }
        }
        cmd.error("release: " + blb.head);
        cmd.error("files: " + blb.getFilelist(true));
        cmd.error("time: " + blb.getTime());
        cmd.error("sign: " + blb.keyed + " key");
        cmd.error("hash: " + blb.getSum(2));
        err += verifyFile(version.getFileName(), blb.jars);
        for (int i = 0; i < blb.files.size(); i++) {
            userUpgradeNtry ntry = blb.files.get(i);
            err += verifyFile(version.getRWpath() + ntry.name, ntry.chk);
        }
        if (err > 0) {
            cmd.error("some tests failed!");
            logger.error("system verification failed!");
        } else {
            cmd.error("all tests passed!");
        }
        return err;
    }

    /**
     * check installation
     *
     * @param blb blob to use, null if read from disk
     * @return error count
     */
    public int doMissing(userUpgradeBlob blb) {
        int err = 0;
        for (int i = 0; i < blb.files.size(); i++) {
            userUpgradeNtry ntry = blb.files.get(i);
            if (new File(version.getRWpath() + ntry.name).exists()) {
                continue;
            }
            cmd.error(ntry.name + " is missing!");
            err++;
        }
        return err;
    }

    private int verifyFile(String fn, String sum) {
        cmd.pipe.strPut(fn);
        String calc = calcFileHash(fn);
        if (calc == null) {
            cmd.pipe.linePut(" is missing!");
            return 1;
        }
        if (calc.compareTo(sum) != 0) {
            cmd.pipe.linePut(" is corrupted!");
            return 2;
        } else {
            cmd.pipe.linePut(" is ok!");
            return 0;
        }
    }

    /**
     * do software upgrade simulation
     */
    public void doSimulate() {
        forces = justSimu;
        doUpgrade();
    }

    /**
     * do software revert
     *
     * @return false on success, true on error
     */
    public static boolean doRevert() {
        String a = version.getFileName();
        if (userFlash.rename(a + bakExt, a, true, false)) {
            logger.error("unable to revert to backup");
            return true;
        }
        return false;
    }

    /**
     * do software backup
     */
    public void doBackup() {
        String a = version.getFileName();
        cons.debugStat(userExec.doneFail(userFlash.copy(a, a + bakExt, true)));
    }

    /**
     * start auto-revert
     */
    public static void startReverter() {
        new Thread(new userUpgradeRevert()).start();
    }

    /**
     * do auto-revert
     */
    protected static void doAutoRevert() {
        String tmp = version.getRWpath() + "rev" + bits.randomD() + ".tmp";
        uniResLoc url = uniResLoc.parseOne(cfgAll.upgradeServer + myFileName());
        url.filExt = verExt;
        userFlash.delete(tmp);
        boolean dl = userFlash.doReceive(pipeDiscard.needAny(null), url, new File(tmp));
        userFlash.delete(tmp);
        if (!dl) {
            String a = "auto-revert cancelled";
            logger.info(a);
            cfgInit.stopRouter(true, -14, a);
            return;
        }
        logger.info("auto-revert was unable to reach server");
        if (doRevert()) {
            return;
        }
        cfgInit.stopRouter(true, 13, "auto-revert finished");
    }

    /**
     * do software upgrade
     */
    public void doUpgrade() {
        if (inProgress != 0) {
            String s = "overlapping upgrades eliminated";
            logger.info(s);
            cons.debugStat(s);
            return;
        }
        inProgress = 1;
        int oldf = forces;
        String s = cmd.word();
        forces = (oldf | bits.str2num(cmd.word())) ^ justSimu;
        try {
            upgraderDoer(s);
        } catch (Exception e) {
            logger.traceback(e);
        }
        forces = oldf;
        inProgress = 0;
    }

    private boolean needStop(int i) {
        return (forces & i) == 0;
    }

    private void upgraderDoer(String server) {
        if (server.length() < 1) {
            server = cfgAll.upgradeServer;
        }
        cons.debugStat("downloading version info");
        String tmp = version.getRWpath() + "upg" + bits.randomD() + ".tmp";
        uniResLoc url = uniResLoc.parseOne(server + myFileName());
        url.filExt = verExt;
        userFlash.delete(tmp);
        userFlash.doReceive(cmd.pipe, url, new File(tmp));
        List<String> txt = bits.txt2buf(tmp);
        userFlash.delete(tmp);
        if (txt == null) {
            cons.debugRes("failed to download version info!");
            return;
        }
        userUpgradeBlob blb = new userUpgradeBlob();
        userUpgradeBlob old = new userUpgradeBlob();
        old.fromText(bits.txt2buf(myVerFile()), true);
        String a = blb.fromText(txt, false);
        if (a != null) {
            cons.debugRes("version info parser: " + a);
            if (needStop(0x1)) {
                return;
            }
        }
        cons.debugRes("old release: " + old.head);
        cons.debugRes("new release: " + blb.head);
        cons.debugRes("old time: " + old.getTime());
        cons.debugRes("new time: " + blb.getTime());
        cons.debugRes("diff: " + bits.timeDump((blb.time - old.time) / 1000));
        cons.debugRes("old files:" + old.getFilelist(true));
        cons.debugRes("new files:" + blb.getFilelist(true));
        userUpgradeBlob diff = blb.copyBytes();
        diff.delFiles(old.files);
        cons.debugRes("extra files:" + diff.getFilelist(false));
        diff = old.copyBytes();
        diff.delFiles(blb.files);
        cons.debugRes("excess files:" + diff.getFilelist(false));
        if (old.time > blb.time) {
            cons.debugRes("no downgrade allowed!");
            if (needStop(0x200)) {
                return;
            }
        }
        if (old.time == blb.time) {
            if (doMissing(blb) < 1) {
                cons.debugRes("no upgrade needed!");
                if (needStop(0x2)) {
                    return;
                }
            }
        }
        if (cfgAll.upgradeConfig) {
            cons.debugRes("saving configuration");
            userReader rdr = new userReader(cmd.pipe, null);
            cmd.pipe.settingsPut(pipeSetting.height, 0);
            userExec exe = new userExec(cmd.pipe, rdr);
            exe.privileged = true;
            String s = exe.repairCommand("write");
            exe.executeCommand(s);
        }
        logger.info("upgrading to " + blb.head);
        if (upgradeFiles(blb, server, tmp, userUpgradeNtry.flgBefore)) {
            if (needStop(0x40)) {
                return;
            }
        }
        int i = upgradeFile(blb.jars, version.getFileName(), server + myFileName(), tmp);
        if (i == 2) {
            if (cfgAll.upgradeScript != null) {
                cons.debugRes("running upgrade script");
                try {
                    cfgAll.upgradeScript.doRound(null);
                } catch (Exception e) {
                    logger.traceback(e);
                }
            }
            cons.debugRes("successfully finished, rebooting!");
            cfgInit.stopRouter(true, 2, "upgrade finished");
            return;
        }
        if (i != 0) {
            if (needStop(0x4)) {
                return;
            }
        }
        if (upgradeFiles(blb, server, tmp, userUpgradeNtry.flgAfter)) {
            if (needStop(0x80)) {
                return;
            }
        }
        diff = old.copyBytes();
        diff.delFiles(blb.files);
        for (i = 0; i < diff.files.size(); i++) {
            userUpgradeNtry ntry = diff.files.get(i);
            a = version.getRWpath() + ntry.name;
            if (needStop(justSimu)) {
                cons.debugStat("should remove " + a);
                continue;
            }
            cons.debugStat("removing " + a);
            userFlash.delete(a);
            if (!cfgAll.upgradeBackup) {
                continue;
            }
            a += bakExt;
            cons.debugStat("removing " + a);
            userFlash.delete(a);
        }
        if (doVerify(blb) > 0) {
            if (needStop(0x10)) {
                return;
            }
        }
        if (needStop(justSimu)) {
            return;
        }
        if (bits.buf2txt(true, blb.getText(2), myVerFile())) {
            cons.debugRes("failed to write version info!");
        }
        cons.debugRes("successfully finished!");
        logger.info("upgrade finished!");
    }

    private boolean upgradeFiles(userUpgradeBlob blb, String server, String tmp, int flg) {
        boolean some = false;
        for (int o = 0; o < blb.files.size(); o++) {
            userUpgradeNtry ntry = blb.files.get(o);
            if ((ntry.flag & flg) == 0) {
                continue;
            }
            int i = upgradeFile(ntry.chk, version.getRWpath() + ntry.name, server + ntry.name, tmp);
            if (i == 2) {
                some = (ntry.flag & userUpgradeNtry.flgData) == 0;
                continue;
            }
            if (i != 0) {
                if (needStop(0x8)) {
                    return true;
                }
            }
        }
        if (!some) {
            cons.debugRes("nothing done in this round");
            return false;
        }
        List<String> scr = new ArrayList<String>();
        for (int o = 0; o < blb.files.size(); o++) {
            userUpgradeNtry ntry = blb.files.get(o);
            if ((ntry.flag & flg) == 0) {
                continue;
            }
            if ((ntry.flag & userUpgradeNtry.flgScript) == 0) {
                continue;
            }
            List<String> res = bits.txt2buf(version.getRWpath() + ntry.name);
            if (res == null) {
                continue;
            }
            scr.addAll(res);
        }
        if (scr.size() < 1) {
            cons.debugRes("no script for this round");
            return false;
        }
        if (needStop(justSimu)) {
            return false;
        }
        cons.debugRes("running upgrade script");
        pipeLine pl = new pipeLine(32768, false);
        pl.getSide();
        pipeSide pip = pl.getSide();
        pip.setTime(120000);
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userScript s = new userScript(pip, "");
        s.allowExec = true;
        s.allowConfig = true;
        s.addLine("set path \"" + version.getRWpath() + "\"");
        s.addLines(scr);
        s.cmdAll();
        pl.setClose();
        return false;
    }

    private cryKeyRSA readUpKey(String s) {
        List<String> l = cfgInit.httpGet(s);
        if (l == null) {
            return null;
        }
        if (l.size() < 2) {
            return null;
        }
        cryKeyRSA k = new cryKeyRSA();
        if (k.pemReadStr(l.get(0), true)) {
            return null;
        }
        if (k.pemReadStr(l.get(1), false)) {
            return null;
        }
        if (k.keyVerify()) {
            return null;
        }
        return k;
    }

    /**
     * upgrade one file
     *
     * @param sumN sum needed
     * @param loc local file name
     * @param rem remote file name
     * @param tmp temp file to use
     * @return status; 0=not needed, 1=failed, 2=done
     */
    protected int upgradeFile(String sumN, String loc, String rem, String tmp) {
        String sumO = calcFileHash(loc);
        if (sumO == null) {
            sumO = "doit";
        }
        if (sumN.equals(sumO)) {
            cons.debugStat("skipping " + loc + " since up to date!");
            return 0;
        }
        if (needStop(justSimu)) {
            cons.debugStat("skipping " + loc + " since just simulating!");
            return 0;
        }
        if (cfgAll.upgradeBackup) {
            cons.debugStat("backing up " + loc);
            userFlash.copy(loc, loc + bakExt, true);
        }
        cons.debugStat("downloading " + loc);
        uniResLoc url = uniResLoc.parseOne(rem);
        userFlash.delete(tmp);
        userFlash.doReceive(cmd.pipe, url, new File(tmp));
        cons.debugStat("upgrading " + loc);
        if (!sumN.equals(calcFileHash(tmp))) {
            cons.debugRes("checksum mismatch, aborting!");
            if (needStop(0x20)) {
                userFlash.delete(tmp);
                return 1;
            }
        }
        if (userFlash.rename(tmp, loc, true, false)) {
            cons.debugRes("failed to rename!");
            return 1;
        }
        if (!sumN.equals(calcFileHash(loc))) {
            cons.debugRes("checksum mismatch after rename!");
            if (needStop(0x100)) {
                return 1;
            }
        }
        return 2;
    }

}

class userUpgradeRevert implements Runnable {

    public void run() {
        if (cfgInit.lastReloadCode != 2) {
            return;
        }
        if (cfgAll.upgradeRevert < 1) {
            return;
        }
        userUpgrade.inProgress = 2;
        for (;;) {
            bits.sleep(1000);
            if (!cfgInit.booting) {
                break;
            }
        }
        if (!cfgAll.upgradeBackup) {
            logger.warn("auto-revert enabled without auto-backup");
        }
        bits.sleep(cfgAll.upgradeRevert);
        try {
            userUpgrade.doAutoRevert();
        } catch (Exception e) {
            logger.traceback(e);
        }
        userUpgrade.inProgress = 0;
    }

}

class userUpgradeNtry {

    public final static int flgBefore = 0x1;

    public final static int flgAfter = 0x2;

    public final static int flgData = 0x4;

    public final static int flgScript = 0x8;

    public final String name;

    public final int flag;

    public final String chk;

    public userUpgradeNtry(String sum, int flg, String nam) {
        name = nam;
        flag = flg;
        chk = sum;
    }

    public String toString() {
        return chk + " " + flag + " " + name;
    }

    public static userUpgradeNtry fromString(String s) {
        cmds cmd = new cmds("upg", s);
        String h = cmd.word();
        int f = bits.str2num(cmd.word());
        return new userUpgradeNtry(h, f, cmd.getRemaining());
    }

}

class userUpgradeBlob {

    public String head;

    public String jars;

    public long time;

    public String sign;

    public String keyed;

    public final List<userUpgradeNtry> files = new ArrayList<userUpgradeNtry>();

    public void putSelf() {
        head = version.headLine;
        jars = userUpgrade.calcFileHash(version.getFileName());
        time = 0;
    }

    public userUpgradeBlob copyBytes() {
        userUpgradeBlob n = new userUpgradeBlob();
        n.head = head;
        n.jars = jars;
        n.time = time;
        n.sign = sign;
        n.keyed = keyed;
        n.addFiles(files);
        return n;
    }

    public int findFile(userUpgradeNtry ntry) {
        for (int i = 0; i < files.size(); i++) {
            if (files.get(i).name.compareTo(ntry.name) == 0) {
                return i;
            }
        }
        return -1;
    }

    public void addFiles(List<userUpgradeNtry> f) {
        for (int i = 0; i < f.size(); i++) {
            userUpgradeNtry ntry = f.get(i);
            if (findFile(ntry) >= 0) {
                continue;
            }
            files.add(ntry);
        }
    }

    public void delFiles(List<userUpgradeNtry> f) {
        for (int i = 0; i < f.size(); i++) {
            userUpgradeNtry ntry = f.get(i);
            int o = findFile(ntry);
            if (o < 0) {
                continue;
            }
            files.remove(o);
        }
    }

    public String getFilelist(int flg) {
        String s = "";
        for (int i = 0; i < files.size(); i++) {
            userUpgradeNtry ntry = files.get(i);
            if ((ntry.flag & flg) == 0) {
                continue;
            }
            s += " " + ntry.name;
        }
        return s.trim();
    }

    public String getFilelist(boolean impl) {
        String a = "";
        if (impl) {
            a += " " + userUpgrade.myVerFile();
        }
        a += " " + getFilelist(userUpgradeNtry.flgBefore);
        if (impl) {
            a += " " + version.getFileName();
        }
        a += " " + getFilelist(userUpgradeNtry.flgAfter);
        return a.trim();
    }

    public String getTime() {
        return bits.time2str(cfgAll.timeZoneName, time, 3);
    }

    public List<String> getText(int level) {
        List<String> l = new ArrayList<String>();
        l.add(head);
        l.add(jars);
        l.add("" + time);
        for (int i = 0; i < files.size(); i++) {
            l.add("" + files.get(i));
        }
        if (level < 1) {
            return l;
        }
        l.add(sign);
        if (level < 2) {
            return l;
        }
        l.add(getSum(1));
        return l;
    }

    public String getSum(int level) {
        return userUpgrade.calcTextHash(getText(level));
    }

    public String fromText(List<String> txt, boolean defs) {
        head = "<bad release>";
        jars = "<bad sum>";
        sign = "<bad sign>";
        time = 0;
        if (txt == null) {
            putSelf();
            if (defs) {
                return null;
            }
            return "not found!";
        }
        int len = txt.size() - 1;
        if (len < 4) {
            return "too small!";
        }
        head = txt.get(0);
        jars = txt.get(1);
        time = bits.str2long(txt.get(2));
        String sum = txt.get(len);
        txt.remove(len);
        if (!sum.equals(userUpgrade.calcTextHash(txt))) {
            return "checksum mismatch!";
        }
        len--;
        sign = txt.get(len);
        txt.remove(len);
        for (int i = 3; i < len; i++) {
            files.add(userUpgradeNtry.fromString(txt.get(i)));
        }
        if (!sum.equals(getSum(1))) {
            return "checksum invalid!";
        }
        byte[] buf = cryBase64.decodeBytes(sign);
        if (buf == null) {
            return "error decoding signature!";
        }
        keyed = "configured";
        if (cfgAll.upgradeOwnKey) {
            return doVrfy(cfgAll.upgradePubKey, buf);
        }
        String res;
        res = doVrfy(cfgAll.upgradePubKey, buf);
        if (res == null) {
            return null;
        }
        res = doVrfy(verCore.pubKeyC, buf);
        if (res == null) {
            keyed = "current";
            return null;
        }
        res = doVrfy(verCore.pubKeyO, buf);
        if (res == null) {
            keyed = "previous";
            return null;
        }
        keyed = "failed";
        return res;
    }

    public String doVrfy(String ks, byte[] buf) {
        try {
            if (ks == null) {
                return "public key not exists!";
            }
            cryKeyRSA ky = new cryKeyRSA();
            if (ky.pemReadStr(ks, true)) {
                return "error reading public key!";
            }
            if (ky.tlsVerify(0, new cryHashSha3512(), getSum(0).getBytes(), buf)) {
                return "signature mismatch!";
            }
            return null;
        } catch (Exception e) {
            logger.traceback(e);
        }
        return "error during verify!";
    }

    public void doSign(cryKeyRSA k) {
        time = bits.getTime();
        sign = cryBase64.encodeBytes(k.tlsSigning(0, new cryHashSha3512(), getSum(0).getBytes()));
    }

}
