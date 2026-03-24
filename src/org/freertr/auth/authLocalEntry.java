package org.freertr.auth;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEui;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgAll;
import org.freertr.cry.cryHashSha1;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.enc.encBase32;
import org.freertr.enc.encBase64;
import org.freertr.enc.encUrl;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * one user record
 *
 * @author matecsaba
 */
public class authLocalEntry implements Comparable<authLocalEntry> {

    /**
     * create instance
     */
    public authLocalEntry() {
    }

    /**
     * times matched
     */
    protected int matches;

    /**
     * last matched
     */
    protected long lastMatch;

    /**
     * name of user
     */
    protected String username = "";

    /**
     * description of user
     */
    protected String description = null;

    /**
     * password of user
     */
    protected String password = null;

    /**
     * secret of user
     */
    protected byte[] secret;

    /**
     * hidden data
     */
    protected byte[] hidata;

    /**
     * one time password
     */
    protected byte[] otpseed;

    /**
     * ssh public key
     */
    protected byte[] pubkey;

    /**
     * accept any password
     */
    protected boolean anyPass;

    /**
     * accept any public key
     */
    protected boolean anyKey;

    /**
     * accept refused authentication
     */
    protected boolean nothing;

    /**
     * command to use on login
     */
    protected String autoCommand = "";

    /**
     * terminate session after command
     */
    protected boolean autoHangup = false;

    /**
     * privilege of user
     */
    protected int privilege = 15;

    /**
     * filter id of user
     */
    protected String filterid;

    /**
     * usage counter
     */
    protected int countdown = -1;

    /**
     * ipv4 address
     */
    protected addrIPv4 ipv4addr;

    /**
     * ipv4 routes
     */
    protected String ipv4route;

    /**
     * ipv6 address
     */
    protected addrIPv6 ipv6addr;

    /**
     * ipv6 interface id
     */
    protected addrEui ipv6ifid;

    /**
     * ipv6 routes
     */
    protected String ipv6route;

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param lst list to append
     * @param filter default filter
     */
    public void getShRun(String beg, List<String> lst, int filter) {
        beg += "username " + username;
        lst.add(beg);
        beg += " ";
        if (description != null) {
            lst.add(beg + "description " + description);
        }
        if (password != null) {
            lst.add(beg + "password " + authLocal.passwdEncode(password, (filter & 2) != 0));
        }
        if (secret != null) {
            lst.add(beg + "secret " + authLocal.secretEncode(secret, (filter & 2) != 0));
        }
        if (otpseed != null) {
            lst.add(beg + "otpseed " + authLocal.passwdEncode(encBase64.encodeBytes(otpseed), (filter & 2) != 0));
        }
        if (hidata != null) {
            lst.add(beg + "hidata " + authLocal.passwdEncode(encBase64.encodeBytes(hidata), (filter & 2) != 0));
        }
        if (pubkey != null) {
            lst.add(beg + "pubkey " + encBase64.encodeBytes(pubkey));
        }
        if (autoHangup) {
            lst.add(beg + "autohangup");
        }
        if (countdown >= 0) {
            lst.add(beg + "countdown " + countdown);
        }
        if (filterid != null) {
            lst.add(beg + "filter " + filterid);
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
        if (s.equals("description")) {
            description = cmd.getRemaining();
            if (neg) {
                description = null;
            }
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
            pubkey = encBase64.decodeBytes(cmd.getRemaining());
            return false;
        }
        if (s.equals("otpseed")) {
            if (neg) {
                otpseed = null;
                return false;
            }
            s = cmd.getRemaining();
            s = authLocal.passwdDecode(s);
            if (s == null) {
                return false;
            }
            otpseed = encBase64.decodeBytes(s);
            return false;
        }
        if (s.equals("hidata")) {
            if (neg) {
                hidata = null;
                return false;
            }
            s = cmd.getRemaining();
            s = authLocal.passwdDecode(s);
            if (s == null) {
                return false;
            }
            hidata = encBase64.decodeBytes(s);
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
            byte[] buf1 = new byte[2];
            buf1[0] = (byte) bits.str2num(cmd.word());
            buf1[1] = (byte) bits.str2num(cmd.word());
            otpseed = bits.byteConcat(buf1, cmd.getRemaining().getBytes());
            return false;
        }
        if (s.equals("otpurl")) {
            return setOtpUrl(cmd.getRemaining());
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
        if (s.equals("filter")) {
            if (neg) {
                filterid = null;
                return false;
            }
            filterid = cmd.getRemaining();
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

    public int compareTo(authLocalEntry o) {
        return username.toLowerCase().compareTo(o.username.toLowerCase());
    }

    /**
     * get otp list
     *
     * @param pwd include password
     * @return list of passwords
     */
    public String getOtpPass(boolean pwd) {
        if (otpseed == null) {
            return null;
        }
        byte[] seed = new byte[otpseed.length - 2];
        bits.byteCopy(otpseed, 2, seed, 0, seed.length);
        long tim = (bits.getTime() + cfgAll.timeServerOffset) / 1000;
        final int digits = otpseed[0];
        final int period = otpseed[1];
        String a = autherOtp.calcTotp(seed, tim, period, digits, new cryHashSha1());
        if (password == null) {
            return a;
        }
        if (!pwd) {
            return a;
        }
        return password + a;
    }

    /**
     * get otp url
     *
     * @return url string
     */
    public String getOtpUrl() {
        if (otpseed == null) {
            return null;
        }
        return "totp:///?secret=" + encBase32.encodeBytes(otpseed, 2, otpseed.length - 2) + "&digits=" + otpseed[0] + "&period=" + otpseed[1];
    }

    /**
     * set otp url
     *
     * @param s string
     * @return false on success, true on error
     */
    public boolean setOtpUrl(String s) {
        encUrl url = encUrl.parseOne(s);
        byte[] buf1 = new byte[2];
        s = url.getParam("digits");
        if (s == null) {
            return true;
        }
        buf1[0] = (byte) bits.str2num(s);
        s = url.getParam("period");
        if (s == null) {
            return true;
        }
        buf1[1] = (byte) bits.str2num(s);
        s = url.getParam("secret");
        if (s == null) {
            return true;
        }
        byte[] buf2 = encBase32.decodeBytes(s);
        if (buf2 == null) {
            return true;
        }
        otpseed = bits.byteConcat(buf1, buf2);
        return false;
    }

    /**
     * check public key
     *
     * @param key key to compare
     * @return false if equals, true if not
     */
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

    /**
     * get menu data
     *
     * @param otp value
     * @return list of stings
     */
    public List<String> toMenu(boolean otp) {
        List<String> res = new ArrayList<String>();
        res.add("nam " + description);
        res.add("pwd " + password);
        res.add("otp " + (otp ? getOtpPass(false) : getOtpUrl()));
        res.add("---");
        if (hidata == null) {
            return res;
        }
        String a = "";
        for (int i = 0; i < hidata.length; i++) {
            int o = hidata[i] & 0xff;
            switch (o) {
                case 0:
                    res.add(a);
                    a = "";
                    break;
                default:
                    a += (char) o;
                    break;
            }
        }
        return res;
    }

    /**
     * set menu data
     *
     * @param lst
     */
    public void fromMenu(List<String> lst) {
        if (lst == null) {
            return;
        }
        password = "";
        otpseed = null;
        int i = 0;
        for (; i < lst.size(); i++) {
            String a = lst.get(i);
            cmds cmd = new cmds("menu", a);
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("---")) {
                break;
            }
            if (a.equals("nam")) {
                description = cmd.getRemaining();
                continue;
            }
            if (a.equals("pwd")) {
                password = cmd.getRemaining();
                continue;
            }
            if (a.equals("otp")) {
                setOtpUrl(cmd.getRemaining());
                continue;
            }
        }
        i++;
        hidata = new byte[0];
        for (; i < lst.size(); i++) {
            byte[] buf = lst.get(i).getBytes();
            buf = bits.byteConcat(buf, new byte[1]);
            hidata = bits.byteConcat(hidata, buf);
        }
        if (hidata.length < 1) {
            hidata = null;
        }
    }

}
