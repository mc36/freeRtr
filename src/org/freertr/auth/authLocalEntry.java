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
    public int matches;

    /**
     * last matched
     */
    public long lastMatch;

    /**
     * name of user
     */
    public String username = "";

    /**
     * description of user
     */
    public String description = null;

    /**
     * password of user
     */
    public String password = null;

    /**
     * secret of user
     */
    public byte[] secret;

    /**
     * hidden data
     */
    public byte[] hidata;

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
     * filter id of user
     */
    public String filterid;

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
            lst.add(beg + "otpseed " + authLocal.passwdEncode(new String(otpseed), (filter & 2) != 0));
        }
        if (hidata != null) {
            lst.add(beg + "hidata " + authLocal.passwdEncode(new String(hidata), (filter & 2) != 0));
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
            s = authLocal.passwdDecode(s);
            if (s == null) {
                return false;
            }
            otpseed = s.getBytes();
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
            hidata = s.getBytes();
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
            encUrl url = encUrl.parseOne(cmd.getRemaining());
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
     * @return list of passwords
     */
    public List<String> getOtpPass() {
        if (otpseed == null) {
            return null;
        }
        String pref = "";
        if (password != null) {
            pref = "" + password;
        }
        byte[] seed = new byte[otpseed.length - 2];
        bits.byteCopy(otpseed, 2, seed, 0, seed.length);
        List<String> lst = new ArrayList<String>();
        long tim = (bits.getTime() + cfgAll.timeServerOffset) / 1000;
        final int range = 10;
        final int digits = otpseed[0];
        final int period = otpseed[1];
        for (int i = -range; i < range; i += otpseed[1]) {
            String a = autherOtp.calcTotp(seed, tim + i, period, digits, new cryHashSha1());
            lst.add(pref + a);
        }
        return lst;
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

}
