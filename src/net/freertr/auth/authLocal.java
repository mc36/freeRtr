package net.freertr.auth;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrEui;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.cfg.cfgAll;
import net.freertr.cry.cryBase64;
import net.freertr.cry.cryEncrCTRaes;
import net.freertr.cry.cryEncrGeneric;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashHmac;
import net.freertr.cry.cryHashSha1;
import net.freertr.cry.cryHashSha2224;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.cry.cryOtp;
import net.freertr.serv.servPop3;
import net.freertr.tab.tabGen;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * local user database
 *
 * @author matecsaba
 */
public class authLocal extends authGeneric {

    private tabGen<authLocalEntry> users;

    private List<String> commands;

    /**
     * password beginning
     */
    protected static final String passwdBeg = "$v10$";

    /**
     * credential beginning
     */
    protected static final String cryptoBeg = "$w10$";

    /**
     * secret beginning
     */
    protected static final String secretBeg = "$V10$";

    /**
     * removed secret
     */
    protected static final String removedEnd = "<removed>$";

    /**
     * hide password
     *
     * @param str cleartext password to encode
     * @param hide hide password
     * @return encoded password
     */
    public static String passwdHide(String str, boolean hide) {
        if (str == null) {
            return null;
        }
        if (hide) {
            return passwdBeg + removedEnd;
        }
        return passwdBeg + cryBase64.encodeString(str);
    }

    /**
     * encode password
     *
     * @param str cleartext password to encode
     * @param hide hide password
     * @return encoded password
     */
    public static String passwdEncode(String str, boolean hide) {
        if (str == null) {
            return null;
        }
        if (hide) {
            return cryptoBeg + removedEnd;
        }
        if ((cfgAll.passEnc == null) && (cfgAll.passEnh == null)) {
            return passwdHide(str, hide);
        }
        cryEncrGeneric c = new cryEncrCTRaes();
        cryHashGeneric h1 = new cryHashSha2256();
        cryHashGeneric h2 = new cryHashSha2224();
        h1.init();
        h2.init();
        if (cfgAll.passEnh != null) {
            h1.update(cfgAll.passEnh.getBytes());
            h2.update(cfgAll.passEnh.getBytes());
        }
        if (cfgAll.passEnc != null) {
            h1.update(cfgAll.passEnc.getBytes());
            h2.update(cfgAll.passEnc.getBytes());
        }
        byte[] buf1 = h1.finish();
        byte[] buf2 = h2.finish();
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
        if ((cfgAll.passEnc == null) && (cfgAll.passEnh == null)) {
            return null;
        }
        cryEncrGeneric c = new cryEncrCTRaes();
        cryHashGeneric h1 = new cryHashSha2256();
        cryHashGeneric h2 = new cryHashSha2224();
        h1.init();
        h2.init();
        if (cfgAll.passEnh != null) {
            h1.update(cfgAll.passEnh.getBytes());
            h2.update(cfgAll.passEnh.getBytes());
        }
        if (cfgAll.passEnc != null) {
            h1.update(cfgAll.passEnc.getBytes());
            h2.update(cfgAll.passEnc.getBytes());
        }
        byte[] buf1 = h1.finish();
        byte[] buf2 = h2.finish();
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
        byte[] buf = new byte[32 + bits.random(0, 16)];
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
     * @param hide hide password
     * @return encoded
     */
    public static String secretEncode(byte[] sec, boolean hide) {
        if (sec == null) {
            return null;
        }
        if (hide) {
            return secretBeg + removedEnd;
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
        l.add(null, "1 2  allowed             allow one command");
        l.add(null, "2 2,.  <text>            command");
        l.add(null, "1 2  username            create or update user");
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < users.size(); i++) {
            lst.add(users.get(i).username);
        }
        l.add(lst, "2 3,.  <name:loc>        name of user, * for any");
        l.add(null, "3 4      password        set password of user");
        l.add(null, "4 4,.      [text]        password of user");
        l.add(null, "3 4      pubkey          set ssh key of user");
        l.add(null, "4 4,.      [text]        public key of user");
        l.add(null, "3 4      secret          set secret of user");
        l.add(null, "4 4,.      [text]        secret of user");
        l.add(null, "3 4      otpseed         set seed of user");
        l.add(null, "4 4,.      [text]        seed of user");
        l.add(null, "3 4      otppass         set seed of user");
        l.add(null, "4 5        <num>         length of tokencode");
        l.add(null, "5 5,.        [text]      seed of user");
        l.add(null, "3 4      autocommand     set automatic command");
        l.add(null, "4 4,.      [text]        autocommand of user");
        l.add(null, "3 4      countdown       set counter");
        l.add(null, "4 .        <num>         login counter");
        l.add(null, "3 .      anypass         any password will be accepted");
        l.add(null, "3 .      anykey          any pubkey will be accepted");
        l.add(null, "3 .      anything        refused auth will be accepted");
        l.add(null, "3 .      autohangup      disconnect user after autocommand");
        l.add(null, "3 4      ipv4addr        specify ipv4 address");
        l.add(null, "4 .        <addr>        address");
        l.add(null, "3 4      ipv4route       specify ipv4 route");
        l.add(null, "4 4,.      [text]        route");
        l.add(null, "3 4      ipv6addr        specify ipv6 address");
        l.add(null, "4 .        <addr>        address");
        l.add(null, "3 4      ipv6ifid        specify ipv6 interface id");
        l.add(null, "4 .        <addr>        address");
        l.add(null, "3 4      ipv6route       specify ipv6 route");
        l.add(null, "4 4,.      [text]        route");
        l.add(null, "3 4      privilege       set privilege level of user");
        l.add(null, "4 .        <priv>        privilege of user");
    }

    /**
     * convert from string
     *
     * @param cmd string
     * @return false on success, true on error
     */
    public boolean fromString(cmds cmd) {
        String a = cmd.word();
        boolean negated = false;
        if (a.equals("no")) {
            negated = true;
            a = cmd.word();
        }
        if (a.equals("allowed")) {
            a = cmd.getRemaining().trim();
            if (negated) {
                commands.remove(a);
            } else {
                commands.add(a);
            }
            return false;
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
        if (negated && (cmd.size() < 2)) {
            users.del(ntry);
            return false;
        }
        return ntry.fromString(negated, cmd);
    }

    /**
     * get config
     *
     * @param beg beginning
     * @return config
     */
    public List<String> getShRun(String beg, int filter) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < users.size(); i++) {
            authLocalEntry ntry = users.get(i);
            ntry.getShRun(beg, l, filter);
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
     * @param pass password
     * @return result
     */
    private authResult createPassed(authLocalEntry ntry, String user, String pass) {
        authResult res = new authResult(this, authResult.authSuccessful, user, pass);
        res.autoCommand = ntry.autoCommand;
        res.autoHangup = ntry.autoHangup;
        res.privilege = ntry.privilege;
        if (ntry.ipv4addr != null) {
            res.ipv4addr = ntry.ipv4addr.copyBytes();
        }
        if (ntry.ipv4route != null) {
            res.ipv4route = ntry.ipv4route;
        }
        if (ntry.ipv6addr != null) {
            res.ipv6addr = ntry.ipv6addr.copyBytes();
        }
        if (ntry.ipv6ifid != null) {
            res.ipv6ifid = ntry.ipv6ifid.copyBytes();
        }
        if (ntry.ipv6route != null) {
            res.ipv6route = ntry.ipv6route;
        }
        return res;
    }

    private authLocalEntry findUser(String user) {
        authLocalEntry ntry = new authLocalEntry();
        ntry.username = user;
        ntry = users.find(ntry);
        if (ntry == null) {
            ntry = new authLocalEntry();
            ntry.username = "*";
            ntry = users.find(ntry);
        }
        if (ntry == null) {
            return null;
        }
        if (ntry.countdown == 0) {
            return null;
        }
        if (ntry.countdown >= 0) {
            ntry.countdown--;
        }
        return ntry;
    }

    /**
     * authenticate with password
     *
     * @param user username
     * @param pass password
     * @return result
     */
    public authResult authUserPass(String user, String pass) {
        authLocalEntry ntry = findUser(user);
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user, pass);
        }
        if (ntry.anyPass) {
            return createPassed(ntry, user, pass);
        }
        if (ntry.otpseed != null) {
            List<String> lst = ntry.getOtpPass();
            for (int i = 0; i < lst.size(); i++) {
                if (lst.get(i).equals(pass)) {
                    return createPassed(ntry, user, pass);
                }
            }
            return new authResult(this, authResult.authBadUserPass, user, pass);
        }
        if (ntry.secret != null) {
            if (authLocal.secretTest(ntry.secret, pass)) {
                return new authResult(this, authResult.authBadUserPass, user, pass);
            }
            return createPassed(ntry, user, pass);
        }
        if (ntry.password != null) {
            if (!ntry.password.equals(pass)) {
                return new authResult(this, authResult.authBadUserPass, user, pass);
            }
            return createPassed(ntry, user, pass);
        }
        return new authResult(this, authResult.authBadUserPass, user, pass);
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
                return new authResult(this, authResult.authSuccessful, user, cmd);
            }
        }
        return new authResult(this, authResult.authBadUserPass, user, cmd);
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
        authLocalEntry ntry = findUser(user);
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (ntry.anyPass) {
            return createPassed(ntry, user, "");
        }
        if (ntry.password == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        byte[] buf = autherChap.calcAuthHash(id, ntry.password, chal);
        if (bits.byteComp(buf, 0, resp, 0, buf.length) != 0) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        return createPassed(ntry, user, "");
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
        authLocalEntry ntry = findUser(user);
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (ntry.anyPass) {
            return createPassed(ntry, user, "");
        }
        if (ntry.password == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (servPop3.calcApop(cookie, ntry.password).compareTo(
                resp.toLowerCase()) != 0) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        return createPassed(ntry, user, "");
    }

    /**
     * check user by username/pubkey
     *
     * @param key public key
     * @param user username
     * @return authentication value
     */
    public authResult authUserPkey(cryKeyGeneric key, String user) {
        authLocalEntry ntry = findUser(user);
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (ntry.anyKey) {
            return createPassed(ntry, user, "");
        }
        if (ntry.pubkey == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (ntry.checkPkey(key)) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        return createPassed(ntry, user, "");
    }

    /**
     * authenticate user by username/pubkey
     *
     * @param key public key
     * @param algo hash algorithm
     * @param algn sign algorithm
     * @param chal challenge
     * @param user username
     * @param resp response received
     * @return authentication value
     */
    public authResult authUserPkey(cryKeyGeneric key, cryHashGeneric algo, String algn, byte[] chal, String user, byte[] resp) {
        authLocalEntry ntry = findUser(user);
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (ntry.anyKey) {
            return createPassed(ntry, user, "");
        }
        if (ntry.checkPkey(key)) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (key.sshVerify(algo, algn, chal, resp)) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        return createPassed(ntry, user, "");
    }

    /**
     * authenticate user by username
     *
     * @param user username
     * @return authentication value
     */
    public authResult authUserNone(String user) {
        authLocalEntry ntry = findUser(user);
        if (ntry == null) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        if (!ntry.nothing) {
            return new authResult(this, authResult.authBadUserPass, user, "");
        }
        return createPassed(ntry, user, "");
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

    /**
     * ssh public key
     */
    public byte[] pubkey;

    /**
     * accept any password
     */
    public boolean anyPass;

    /**
     * accept any public key
     */
    public boolean anyKey;

    /**
     * accept refused authentication
     */
    public boolean nothing;

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
     * ipv4 address
     */
    public addrIPv4 ipv4addr;

    /**
     * ipv4 routes
     */
    public String ipv4route;

    /**
     * ipv6 address
     */
    public addrIPv6 ipv6addr;

    /**
     * ipv6 interface id
     */
    public addrEui ipv6ifid;

    /**
     * ipv6 routes
     */
    public String ipv6route;

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param lst list to append
     */
    public void getShRun(String beg, List<String> lst, int filter) {
        beg += "username " + username;
        lst.add(beg);
        beg += " ";
        if (password != null) {
            lst.add(beg + "password " + authLocal.passwdEncode(password, (filter & 2) != 0));
        }
        if (secret != null) {
            lst.add(beg + "secret " + authLocal.secretEncode(secret, (filter & 2) != 0));
        }
        if (otpseed != null) {
            lst.add(beg + "otpseed " + authLocal.passwdEncode(new String(otpseed), (filter & 2) != 0));
        }
        if (pubkey != null) {
            lst.add(beg + "pubkey " + cryBase64.encodeBytes(pubkey));
        }
        if (autoHangup) {
            lst.add(beg + "autohangup");
        }
        if (countdown >= 0) {
            lst.add(beg + "countdown " + countdown);
        }
        if (anyPass) {
            lst.add(beg + "anypass");
        }
        if (anyKey) {
            lst.add(beg + "anykey");
        }
        if (nothing) {
            lst.add(beg + "anything");
        }
        if (ipv4addr != null) {
            lst.add(beg + "ipv4addr " + ipv4addr);
        }
        if (ipv4route != null) {
            lst.add(beg + "ipv4route " + ipv4route);
        }
        if (ipv6addr != null) {
            lst.add(beg + "ipv6addr " + ipv6addr);
        }
        if (ipv6ifid != null) {
            lst.add(beg + "ipv6ifid " + ipv6ifid);
        }
        if (ipv6route != null) {
            lst.add(beg + "ipv6route " + ipv6route);
        }
        lst.add(beg + "autocommand " + autoCommand);
        lst.add(beg + "privilege " + privilege);
    }

    /**
     * parse commands
     *
     * @param neg negated
     * @param cmd commands
     * @return true if error happened
     */
    public boolean fromString(boolean neg, cmds cmd) {
        String s = cmd.word();
        if (s.length() < 1) {
            return false;
        }
        if (s.equals("password")) {
            if (neg) {
                password = null;
                return false;
            }
            password = authLocal.passwdDecode(cmd.getRemaining());
            return false;
        }
        if (s.equals("secret")) {
            if (neg) {
                secret = null;
                return false;
            }
            secret = authLocal.secretDecode(cmd.getRemaining());
            return false;
        }
        if (s.equals("pubkey")) {
            if (neg) {
                pubkey = null;
                return false;
            }
            pubkey = cryBase64.decodeBytes(cmd.getRemaining());
            return false;
        }
        if (s.equals("otpseed")) {
            if (neg) {
                otpseed = null;
                return false;
            }
            s = cmd.getRemaining();
            if (s.startsWith(authLocal.passwdBeg)) {
                otpseed = null;
                s = authLocal.passwdDecode(s);
                if (s == null) {
                    return false;
                }
                otpseed = s.getBytes();
                return false;
            }
            byte[] buf1 = new byte[1];
            buf1[0] = (byte) bits.str2num(cmd.word());
            otpseed = bits.byteConcat(buf1, cmd.getRemaining().getBytes());
            return false;
        }
        if (s.equals("anypass")) {
            anyPass = !neg;
            return false;
        }
        if (s.equals("anykey")) {
            anyKey = !neg;
            return false;
        }
        if (s.equals("anything")) {
            nothing = !neg;
            return false;
        }
        if (s.equals("countdown")) {
            if (neg) {
                countdown = -1;
                return false;
            }
            countdown = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("otppass")) {
            byte[] buf1 = new byte[1];
            buf1[0] = (byte) bits.str2num(cmd.word());
            otpseed = bits.byteConcat(buf1, cmd.getRemaining().getBytes());
            return false;
        }
        if (s.equals("autocommand")) {
            if (neg) {
                autoCommand = "";
                return false;
            }
            autoCommand = cmd.getRemaining();
            return false;
        }
        if (s.equals("autohangup")) {
            autoHangup = !neg;
            return false;
        }
        if (s.equals("ipv4addr")) {
            if (neg) {
                ipv4addr = null;
                return false;
            }
            ipv4addr = new addrIPv4();
            ipv4addr.fromString(cmd.word());
            return false;
        }
        if (s.equals("ipv4route")) {
            if (neg) {
                ipv4route = null;
                return false;
            }
            ipv4route = cmd.getRemaining();
            return false;
        }
        if (s.equals("ipv6addr")) {
            if (neg) {
                ipv6addr = null;
                return false;
            }
            ipv6addr = new addrIPv6();
            ipv6addr.fromString(cmd.word());
            return false;
        }
        if (s.equals("ipv6ifid")) {
            if (neg) {
                ipv6ifid = null;
                return false;
            }
            ipv6ifid = new addrEui();
            ipv6ifid.fromString(cmd.word());
            return false;
        }
        if (s.equals("ipv6route")) {
            if (neg) {
                ipv6route = null;
                return false;
            }
            ipv6route = cmd.getRemaining();
            return false;
        }
        if (s.equals("privilege")) {
            privilege = bits.str2num(cmd.word()) & 0xf;
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
        final int range = 10;
        for (int i = -range; i < range; i += cryOtp.timeInt) {
            String a = cryOtp.calcTotp(seed, tim + i, cryOtp.timeInt, digs, new cryHashSha1());
            lst.add(pref + a);
        }
        return lst;
    }

    public boolean checkPkey(cryKeyGeneric key) {
        if (pubkey == null) {
            return true;
        }
        byte[] buf = key.sshWriter();
        if (buf.length != pubkey.length) {
            return true;
        }
        if (bits.byteComp(pubkey, 0, buf, 0, buf.length) != 0) {
            return true;
        }
        return false;
    }

}
