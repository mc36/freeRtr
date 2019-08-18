package auth;

import cfg.cfgAll;
import cry.cryBase64;
import cry.cryEncrCTRaes;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import cry.cryHashHmac;
import cry.cryHashSha1;
import cry.cryHashSha2224;
import cry.cryHashSha2256;
import cry.cryOtp;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import serv.servPop3;
import tab.tabGen;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * local user database
 *
 * @author matecsaba
 */
public class authLocal extends authGeneric {

    private tabGen<authLocalEntry> users;

    private List<String> commands;

    private static final String passwdBeg = "$v10$";

    private static final String cryptoBeg = "$w10$";

    private static final String secretBeg = "$V10$";

    /**
     * hide password
     *
     * @param str cleartext password to encode
     * @return encoded password
     */
    public static String passwdHide(String str) {
        if (str == null) {
            return null;
        }
        return passwdBeg + cryBase64.encodeString(str);
    }

    /**
     * encode password
     *
     * @param str cleartext password to encode
     * @return encoded password
     */
    public static String passwdEncode(String str) {
        if (str == null) {
            return null;
        }
        if (cfgAll.passEnc == null) {
            return passwdHide(str);
        }
        cryEncrGeneric c = new cryEncrCTRaes();
        byte[] buf1 = cryHashGeneric.compute(new cryHashSha2256(), cfgAll.passEnc.getBytes());
        byte[] buf2 = cryHashGeneric.compute(new cryHashSha2224(), cfgAll.passEnc.getBytes());
        byte[] buf3 = new byte[c.getKeySize()];
        bits.byteCopy(buf1, 0, buf3, 0, buf3.length);
        int i = c.getBlockSize();
        buf1 = new byte[i];
        bits.byteCopy(buf2, 0, buf1, 0, i);
        c.init(buf3, buf1, true);
        buf1 = str.getBytes();
        buf2 = new byte[i - (buf1.length % i)];
        buf1 = bits.byteConcat(buf1, buf2);
        c.update(buf1, 0, buf1.length);
        return cryptoBeg + cryBase64.encodeBytes(buf1);
    }

    /**
     * decode password
     *
     * @param str encoded password
     * @return cleartext password
     */
    public static String passwdDecode(String str) {
        if (str == null) {
            return null;
        }
        if (str.startsWith(passwdBeg)) {
            str = str.substring(passwdBeg.length(), str.length());
            str = cryBase64.decodeString(str);
            return str;
        }
        if (!str.startsWith(cryptoBeg)) {
            return str;
        }
        str = str.substring(passwdBeg.length(), str.length());
        if (cfgAll.passEnc == null) {
            return null;
        }
        cryEncrGeneric c = new cryEncrCTRaes();
        byte[] buf1 = cryHashGeneric.compute(new cryHashSha2256(), cfgAll.passEnc.getBytes());
        byte[] buf2 = cryHashGeneric.compute(new cryHashSha2224(), cfgAll.passEnc.getBytes());
        byte[] buf3 = new byte[c.getKeySize()];
        bits.byteCopy(buf1, 0, buf3, 0, buf3.length);
        int i = c.getBlockSize();
        buf1 = new byte[i];
        bits.byteCopy(buf2, 0, buf1, 0, i);
        c.init(buf3, buf1, false);
        buf1 = cryBase64.decodeBytes(str);
        if (buf1 == null) {
            return null;
        }
        c.update(buf1, 0, buf1.length);
        int o = -1;
        for (i = buf1.length - 1; i >= 0; i--) {
            if (buf1[i] == 0) {
                o = i;
            }
        }
        if (o < 0) {
            return null;
        }
        buf2 = new byte[o];
        bits.byteCopy(buf1, 0, buf2, 0, buf2.length);
        return new String(buf2);
    }

    /**
     * compress password
     *
     * @param k key to use
     * @param s password to compress
     * @return compressed password
     */
    private static byte[] hashPass(byte[] k, String s) {
        cryHashGeneric h = new cryHashHmac(new cryHashSha2256(), k);
        h.init();
        h.update(s.getBytes());
        return bits.byteConcat(k, h.finish());
    }

    /**
     * test one password
     *
     * @param sec secret to use
     * @param pass password to check
     * @return true if error, false if match
     */
    public static boolean secretTest(byte[] sec, String pass) {
        if (sec == null) {
            return true;
        }
        byte[] buf = new byte[sec[0]];
        bits.byteCopy(sec, 0, buf, 0, buf.length);
        buf = hashPass(buf, pass);
        return bits.byteComp(sec, 0, buf, 0, buf.length) != 0;
    }

    /**
     * decode secret
     *
     * @param s secret to decode
     * @return decoded
     */
    public static byte[] secretDecode(String s) {
        if (s.startsWith(secretBeg)) {
            s = s.substring(5, s.length());
            return cryBase64.decodeBytes(s);
        }
        byte[] buf = new byte[bits.random(6, 8)];
        for (int i = 1; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        buf[0] = (byte) buf.length;
        return hashPass(buf, s);
    }

    /**
     * encode secret
     *
     * @param sec secret to encode
     * @return encoded
     */
    public static String secretEncode(byte[] sec) {
        if (sec == null) {
            return null;
        }
        return secretBeg + cryBase64.encodeBytes(sec);
    }

    /**
     * create new user list
     */
    public authLocal() {
        users = new tabGen<authLocalEntry>();
        commands = new ArrayList<String>();
    }

    /**
     * get name
     *
     * @return name
     */
    public String getCfgName() {
        return "userlist";
    }

    /**
     * get help
     *
     * @param l help
     */
    public void getHelp(userHelping l) {
        l.add("1 2  deluser             delete one user");
        l.add("2 .    <name>            name of user");
        l.add("1 2  allowed             allow one command");
        l.add("2 2,.  <text>            command");
        l.add("1 2  disallowed          disallow one command");
        l.add("2 2,.  <text>            command");
        l.add("1 2  username            create or update user");
        l.add("2 3,.  <name>            name of user, * for any");
        l.add("3 4      password        set password of user");
        l.add("4 4,.      [text]        password of user");
        l.add("3 .      nopassword      clear password of user");
        l.add("3 4      secret          set secret of user");
        l.add("4 4,.      [text]        secret of user");
        l.add("3 .      nosecret        clear secret of user");
        l.add("3 4      otpseed         set seed of user");
        l.add("4 4,.      [text]        seed of user");
        l.add("3 .      nootpseed       clear seed of user");
        l.add("3 4      otppass         set seed of user");
        l.add("4 5        <num>         length of tokencode");
        l.add("5 5,.        [text]        seed of user");
        l.add("3 4      autocommand     set automatic command");
        l.add("4 4,.      [text]        autocommand of user");
        l.add("3 .      noautocommand   clear automatic command");
        l.add("3 4      countdown       set counter");
        l.add("4 .        <num>         login counter");
        l.add("3 .      nocountdown     clear login counter");
        l.add("3 .      anypass         any password will be accepted");
        l.add("3 .      noanypass       just good password will accepted");
        l.add("3 .      noautohangup    leave user after autocommand");
        l.add("3 4      privilege       set privilege level of user");
        l.add("4 .        <priv>        privilege of user");
    }

    /**
     * convert from string
     *
     * @param cmd string
     * @return false on success, true on error
     */
    public boolean fromString(cmds cmd) {
        String a = cmd.word();
        if (a.equals("allowed")) {
            commands.add(cmd.getRemaining().trim());
            return false;
        }
        if (a.equals("disallowed")) {
            commands.remove(cmd.getRemaining().trim());
            return false;
        }
        if (a.equals("deluser")) {
            authLocalEntry ntry = new authLocalEntry();
            ntry.username = cmd.word();
            return users.del(ntry) == null;
        }
        if (!a.equals("username")) {
            return true;
        }
        a = cmd.word().trim();
        if (a.length() < 1) {
            return true;
        }
        authLocalEntry ntry = new authLocalEntry();
        ntry.username = a;
        authLocalEntry old = users.add(ntry);
        if (old != null) {
            ntry = old;
        }
        return ntry.fromString(cmd);
    }

    /**
     * get config
     *
     * @param beg beginning
     * @return confi
     */
    public List<String> getShRun(String beg) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < users.size(); i++) {
            authLocalEntry ntry = users.get(i);
            ntry.getShRun(beg, l);
        }
        for (int i = 0; i < commands.size(); i++) {
            l.add(beg + "allowed " + commands.get(i));
        }
        return l;
    }

    /**
     * create passed result
     *
     * @param ntry entry
     * @param user username
     * @return result
     */
    private authResult createPassed(authLocalEntry ntry, String user) {
        authResult res = new authResult(this, authResult.authSuccessful, user);
        res.autoCommand = ntry.autoCommand;
        res.autoHangup = ntry.autoHangup;
        res.privilege = ntry.privilege;
        return res;
    }

    /**
     * authenticate with password
     *
     * @param user username
     * @param pass password
     * @return result
     */
    public authResult authUserPass(String user, String pass) {
        authLocalEntry ntry = new authLocalEntry();
        ntry.username = user;
        ntry = users.find(ntry);
        if (ntry == null) {
            ntry = new authLocalEntry();
            ntry.username = "*";
            ntry = users.find(ntry);
        }
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (ntry.countdown == 0) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (ntry.countdown >= 0) {
            ntry.countdown--;
        }
        if (ntry.anyPass) {
            return createPassed(ntry, user);
        }
        if (ntry.otpseed != null) {
            List<String> lst = ntry.getOtpPass();
            for (int i = 0; i < lst.size(); i++) {
                if (lst.get(i).equals(pass)) {
                    return createPassed(ntry, user);
                }
            }
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (ntry.secret != null) {
            if (authLocal.secretTest(ntry.secret, pass)) {
                return new authResult(this, authResult.authBadUserPass, user);
            }
            return createPassed(ntry, user);
        }
        if (ntry.password != null) {
            if (!ntry.password.equals(pass)) {
                return new authResult(this, authResult.authBadUserPass, user);
            }
            return createPassed(ntry, user);
        }
        return new authResult(this, authResult.authBadUserPass, user);
    }

    /**
     * authorize command
     *
     * @param user username
     * @param cmd command
     * @return result
     */
    public authResult authUserCommand(String user, String cmd) {
        cmd = cmd.trim().toLowerCase();
        for (int i = 0; i < commands.size(); i++) {
            if (cmd.matches(commands.get(i))) {
                return new authResult(this, authResult.authSuccessful, user);
            }
        }
        return new authResult(this, authResult.authBadUserPass, user);
    }

    /**
     * authenticate with chap
     *
     * @param user username
     * @param id id
     * @param chal challenge
     * @param resp response
     * @return result
     */
    public authResult authUserChap(String user, int id, byte[] chal, byte[] resp) {
        authLocalEntry ntry = new authLocalEntry();
        ntry.username = user;
        ntry = users.find(ntry);
        if (ntry == null) {
            ntry = new authLocalEntry();
            ntry.username = "*";
            ntry = users.find(ntry);
        }
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (ntry.countdown == 0) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (ntry.countdown >= 0) {
            ntry.countdown--;
        }
        if (ntry.anyPass) {
            return createPassed(ntry, user);
        }
        if (ntry.password == null) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        byte[] buf = autherChap.calcAuthHash(id, ntry.password, chal);
        if (bits.byteComp(buf, 0, resp, 0, buf.length) != 0) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        return createPassed(ntry, user);
    }

    /**
     * authenticate with apop
     *
     * @param cookie cookie
     * @param user username
     * @param resp response
     * @return result
     */
    public authResult authUserApop(String cookie, String user, String resp) {
        authLocalEntry ntry = new authLocalEntry();
        ntry.username = user;
        ntry = users.find(ntry);
        if (ntry == null) {
            ntry = new authLocalEntry();
            ntry.username = "*";
            ntry = users.find(ntry);
        }
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (ntry.countdown == 0) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (ntry.countdown >= 0) {
            ntry.countdown--;
        }
        if (ntry.anyPass) {
            return createPassed(ntry, user);
        }
        if (ntry.password == null) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        if (servPop3.calcApop(cookie, ntry.password).compareTo(
                resp.toLowerCase()) != 0) {
            return new authResult(this, authResult.authBadUserPass, user);
        }
        return createPassed(ntry, user);
    }

}

/**
 * one user record
 *
 * @author matecsaba
 */
class authLocalEntry implements Comparator<authLocalEntry> {

    /**
     * name of user
     */
    public String username = "";

    /**
     * password of user
     */
    public String password = null;

    /**
     * secret of user
     */
    public byte[] secret;

    /**
     * one time password
     */
    public byte[] otpseed;

    public boolean anyPass;

    /**
     * command to use on login
     */
    public String autoCommand = "";

    /**
     * terminate session after command
     */
    public boolean autoHangup = false;

    /**
     * privilege of user
     */
    public int privilege = 15;

    /**
     * usage counter
     */
    public int countdown = -1;

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param lst list to append
     */
    public void getShRun(String beg, List<String> lst) {
        beg += "username " + username;
        lst.add(beg);
        beg += " ";
        if (password == null) {
            lst.add(beg + "nopassword");
        } else {
            lst.add(beg + "password " + authLocal.passwdEncode(password));
        }
        if (secret == null) {
            lst.add(beg + "nosecret");
        } else {
            lst.add(beg + "secret " + authLocal.secretEncode(secret));
        }
        if (otpseed == null) {
            lst.add(beg + "nootpseed");
        } else {
            lst.add(beg + "otpseed $v10$" + cryBase64.encodeBytes(otpseed));
        }
        if (autoHangup) {
            lst.add(beg + "autohangup");
        } else {
            lst.add(beg + "noautohangup");
        }
        if (countdown >= 0) {
            lst.add(beg + "countdown " + countdown);
        } else {
            lst.add(beg + "nocountdown");
        }
        if (anyPass) {
            lst.add(beg + "anypass");
        } else {
            lst.add(beg + "noanypass");
        }
        lst.add(beg + "autocommand " + autoCommand);
        lst.add(beg + "privilege " + privilege);
    }

    /**
     * parse commands
     *
     * @param cmd commands
     * @return true if error happened
     */
    public boolean fromString(cmds cmd) {
        String s = cmd.word();
        if (s.length() < 1) {
            return false;
        }
        if (s.equals("password")) {
            password = authLocal.passwdDecode(cmd.getRemaining());
            return false;
        }
        if (s.equals("secret")) {
            secret = authLocal.secretDecode(cmd.getRemaining());
            return false;
        }
        if (s.equals("otpseed")) {
            s = cmd.getRemaining();
            if (!s.startsWith("$v10$")) {
                byte[] buf1 = new byte[1];
                buf1[0] = (byte) bits.str2num(cmd.word());
                otpseed = bits.byteConcat(buf1, cmd.getRemaining().getBytes());
                return false;
            }
            s = s.substring(5, s.length());
            otpseed = cryBase64.decodeBytes(s);
            return false;
        }
        if (s.equals("anypass")) {
            anyPass = true;
            return false;
        }
        if (s.equals("noanypass")) {
            anyPass = false;
            return false;
        }
        if (s.equals("countdown")) {
            countdown = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("nocountdown")) {
            countdown = -1;
            return false;
        }
        if (s.equals("otppass")) {
            byte[] buf1 = new byte[1];
            buf1[0] = (byte) bits.str2num(cmd.word());
            otpseed = bits.byteConcat(buf1,
                    cryOtp.androidPass(cmd.getRemaining()));
            return false;
        }
        if (s.equals("autocommand")) {
            autoCommand = cmd.getRemaining();
            return false;
        }
        if (s.equals("autohangup")) {
            autoHangup = true;
            return false;
        }
        if (s.equals("privilege")) {
            privilege = bits.str2num(cmd.word()) & 0xf;
            return false;
        }
        if (s.equals("noautohangup")) {
            autoHangup = false;
            return false;
        }
        if (s.equals("noautocommand")) {
            autoCommand = "";
            return false;
        }
        if (s.equals("nopassword")) {
            password = null;
            return false;
        }
        if (s.equals("nosecret")) {
            secret = null;
            return false;
        }
        if (s.equals("nootpseed")) {
            otpseed = null;
            return false;
        }
        return true;
    }

    public int compare(authLocalEntry o1, authLocalEntry o2) {
        return o1.username.toLowerCase().compareTo(o2.username.toLowerCase());
    }

    public List<String> getOtpPass() {
        if (otpseed == null) {
            return null;
        }
        int digs = otpseed[0];
        String pref = "";
        if (password != null) {
            pref = "" + password;
        }
        byte[] seed = new byte[otpseed.length - 1];
        bits.byteCopy(otpseed, 1, seed, 0, seed.length);
        List<String> lst = new ArrayList<String>();
        long tim = (bits.getTime() + cfgAll.timeServerOffset) / 1000;
        final int range = 120;
        for (int i = -range; i < range; i += cryOtp.timeInt) {
            String a = cryOtp.calcTotp(seed, tim + i, digs, new cryHashSha1());
            lst.add(pref + a);
        }
        return lst;
    }

}
