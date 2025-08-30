package org.freertr.enc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv6;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servHttp;
import org.freertr.util.bits;

/**
 * uniform resource locator (rfc3986)
 *
 * @author matecsaba
 */
public class encUrl {

    /**
     * create instance
     */
    public encUrl() {
    }

    /**
     * original string
     */
    public String orig;

    /**
     * protocol id
     */
    public String proto;

    /**
     * name of user
     */
    public String username;

    /**
     * password of user
     */
    public String password;

    /**
     * server name
     */
    public String server;

    /**
     * server port
     */
    public int port;

    /**
     * file path
     */
    public String filPath;

    /**
     * file name
     */
    public String filName;

    /**
     * file extension
     */
    public String filExt;

    /**
     * parameters
     */
    public List<encUrlPar> param;

    /**
     * address to string
     *
     * @param adr address
     * @param prt port
     * @return string
     */
    public static String addr2str(addrIP adr, int prt) {
        String a;
        if (adr.isIPv4()) {
            a = "" + adr;
        } else {
            a = "[" + adr + "]";
        }
        if (prt < 1) {
            return a;
        } else {
            return a + ":" + prt;
        }
    }

    /**
     * clear all data
     */
    public void clear() {
        orig = "";
        proto = "";
        server = "";
        port = -1;
        filPath = "";
        filName = "";
        filExt = "";
        param = new ArrayList<encUrlPar>();
        username = "";
        password = "";
    }

    /**
     * copy from other url
     *
     * @param src original url to copy
     */
    public void copyBytes(encUrl src) {
        orig = src.orig;
        proto = src.proto;
        server = src.server;
        port = src.port;
        filPath = src.filPath;
        filName = src.filName;
        filExt = src.filExt;
        param = src.param;
    }

    /**
     * decore percent coding
     *
     * @param s string to decode
     * @return decoded string
     */
    public static String percentUncode(String s) {
        s = s.trim();
        for (;;) {
            int i = s.indexOf("+");
            if (i < 0) {
                break;
            }
            s = s.substring(0, i) + " " + s.substring(i + 1, s.length());
        }
        String r = "";
        for (;;) {
            int i = s.indexOf("%");
            if (i < 0) {
                break;
            }
            if ((i + 3) > s.length()) {
                break;
            }
            String a = s.substring(i + 1, i + 3);
            r = r + s.substring(0, i) + (char) bits.fromHex(a);
            s = s.substring(i + 3, s.length());
        }
        return r + s;
    }

    /**
     * encode percent coding
     *
     * @param s string to encode
     * @return encoded string
     */
    public static String percentEncode(String s) {
        String o = "";
        if (s == null) {
            return o;
        }
        final String e = "%:?#[]@!$&'()*+,;=\\|*";
        for (int ps = 0; ps < s.length(); ps++) {
            char c = s.charAt(ps);
            if (c == 0x20) {
                o += "+";
                continue;
            }
            boolean n;
            n = (c <= 0x20) || (c >= 0x7e);
            if (!n) {
                n = e.indexOf(c) >= 0;
            }
            if (n) {
                o += "%" + bits.toHexB(c).toUpperCase();
            } else {
                o += c;
            }
        }
        return o;
    }

    /**
     * convert string to url
     *
     * @param s string to parse
     */
    public void fromPathname(String s) {
        if (s.startsWith("/")) {
            s = s.substring(1, s.length());
        }
        filPath = s;
        int i = filPath.indexOf("?");
        String pars = "";
        if (i >= 0) {
            pars = filPath.substring(i + 1, filPath.length());
            filPath = filPath.substring(0, i);
        }
        filPath = percentUncode(filPath);
        i = filPath.lastIndexOf("/");
        if (i >= 0) {
            filName = filPath.substring(i + 1, filPath.length());
            filPath = filPath.substring(0, i + 1);
        } else {
            filName = filPath;
            filPath = "";
        }
        i = filName.lastIndexOf(".");
        if (i >= 0) {
            filExt = filName.substring(i, filName.length());
            filName = filName.substring(0, i);
        }
        param.clear();
        if (pars.length() < 1) {
            return;
        }
        for (;;) {
            if (pars.length() < 1) {
                break;
            }
            i = pars.indexOf("&");
            if (i < 0) {
                s = pars;
                pars = "";
            } else {
                s = pars.substring(0, i);
                pars = pars.substring(i + 1, pars.length());
            }
            encUrlPar ntry = new encUrlPar();
            i = s.indexOf("=");
            if (i < 0) {
                ntry.nam = percentUncode(s);
            } else {
                ntry.nam = percentUncode(s.substring(0, i));
                ntry.val = percentUncode(s.substring(i + 1, s.length()));
            }
            param.add(ntry);
        }
    }

    /**
     * convert string to url
     *
     * @param s string to parse
     * @return false on success, true on error
     */
    public boolean fromString(String s) {
        clear();
        orig = "" + s;
        if (s == null) {
            return true;
        }
        if (s.startsWith("/")) {
            fromPathname(s);
            return false;
        }
        int i = s.indexOf(":");
        if (i < 0) {
            filPath = s;
        } else {
            proto = s.substring(0, i);
            server = s.substring(i + 1, s.length());
        }
        if (server.indexOf("//") == 0) {
            server = server.substring(2, server.length());
        }
        i = server.indexOf("/");
        if (i >= 0) {
            filPath = server.substring(i + 1, server.length());
            server = server.substring(0, i);
        }
        i = server.indexOf("@");
        if (i >= 0) {
            username = server.substring(0, i);
            server = server.substring(i + 1, server.length());
            i = username.indexOf(":");
            if (i >= 0) {
                password = username.substring(i + 1, username.length());
                username = username.substring(0, i);
            }
        }
        i = server.indexOf("[");
        int o = server.indexOf("]");
        if ((i >= 0) && (o > i)) {
            s = server.substring(o + 1, server.length());
            server = server.substring(i + 1, o);
        } else {
            s = "";
            i = server.indexOf(":");
            if (i >= 0) {
                s = server.substring(i, server.length());
                server = server.substring(0, i);
            }
        }
        i = s.indexOf(":");
        if (i >= 0) {
            port = bits.str2num(s.substring(i + 1, s.length()).trim());
        }
        proto = percentUncode(proto);
        server = percentUncode(server);
        username = percentUncode(username);
        password = percentUncode(password);
        fromPathname(filPath);
        return false;
    }

    /**
     * add parameter
     *
     * @param nam name
     * @param val value
     */
    public void addParam(String nam, String val) {
        encUrlPar ntry = new encUrlPar();
        ntry.nam = nam;
        ntry.val = val;
        param.add(ntry);
    }

    /**
     * get parameter
     *
     * @param nam name
     * @return value, null if not found
     */
    public String getParam(String nam) {
        for (int i = 0; i < param.size(); i++) {
            encUrlPar ntry = param.get(i);
            if (nam.equals(ntry.nam)) {
                return ntry.val;
            }
        }
        return null;
    }

    /**
     * follow link
     *
     * @param n url to follow
     */
    public void followLink(encUrl n) {
        if (n.server.length() > 0) {
            copyBytes(n);
            return;
        }
        param = n.param;
        if ((n.filName + n.filExt).length() <= 0) {
            return;
        }
        filName = n.filName;
        filExt = n.filExt;
        if (n.filPath.length() <= 0) {
            return;
        }
        if (n.filPath.indexOf("/") == 0) {
            filPath = n.filPath;
            return;
        }
        filPath += n.filPath;
    }

    /**
     * normalize this path
     */
    public void normalizePath() {
        filPath = normalizePath(filPath);
    }

    /**
     * normalize one path
     *
     * @param s path to process
     * @return normalized path
     */
    public static String normalizePath(String s) {
        String b = "";
        for (;;) {
            int i = s.indexOf("/");
            if (i < 0) {
                break;
            }
            String a = s.substring(0, i);
            s = s.substring(i + 1, s.length());
            if (a.length() <= 0) {
                continue;
            }
            if (a.equals("")) {
                continue;
            }
            if (a.equals(".")) {
                continue;
            }
            if (!a.equals("..")) {
                b = b + "/" + a;
                continue;
            }
            i = b.lastIndexOf("/");
            if (i < 0) {
                continue;
            }
            b = b.substring(0, i);
        }
        s = "/" + b + "/" + s;
        for (; s.indexOf("/") == 0;) {
            s = s.substring(1, s.length());
        }
        return s;
    }

    private static String percentEncode2(String s, boolean needed) {
        if (!needed) {
            return s;
        }
        return percentEncode(s);
    }

    /**
     * convert to url
     *
     * @param hostname put hostname
     * @param userpass put password
     * @param params put parameters
     * @param percent percent encode
     * @return url format
     */
    public String toURL(boolean hostname, boolean userpass, boolean params, boolean percent) {
        String a;
        String b;
        String c;
        if (port >= 0) {
            a = ":" + port;
        } else {
            a = "";
        }
        if (new addrIPv6().fromString(server)) {
            b = percentEncode2(server, percent);
        } else {
            b = "[" + server + "]";
        }
        if (userpass && (username.length() > 0)) {
            b = percentEncode2(username, percent) + ":" + percentEncode2(password, percent) + "@" + b;
        }
        c = "";
        if (params) {
            c = toParams();
        }
        if (c.length() > 0) {
            c = "?" + c;
        }
        c = "/" + percentEncode2(filPath, percent) + percentEncode2(filName, percent) + percentEncode2(filExt, percent) + c;
        if (!hostname) {
            return c;
        }
        return percentEncode2(proto, percent) + "://" + b + a + c;
    }

    /**
     * convert to parameters
     *
     * @return pathname format
     */
    public String toParams() {
        String c = "";
        for (int i = 0; i < param.size(); i++) {
            encUrlPar ntry = param.get(i);
            c += "&" + percentEncode(ntry.nam) + "=" + percentEncode(ntry.val);
        }
        if (c.length() > 0) {
            c = c.substring(1, c.length());
        }
        return c;
    }

    /**
     * convert to pathname
     *
     * @return pathname format
     */
    public String toPathName() {
        return filPath + filName + filExt;
    }

    /**
     * convert to pathname
     *
     * @return pathname format
     */
    public String toFileName() {
        return filName + filExt;
    }

    /**
     * convert to email
     *
     * @return email format
     */
    public String toEmail() {
        return username + "@" + server;
    }

    /**
     * get address from braced address
     *
     * @param s address
     * @return address
     */
    public static String fromEmail(String s) {
        int i = s.indexOf('<');
        int o = s.indexOf('>');
        if ((i >= 0) && (o >= 0) && (i < o)) {
            s = s.substring(i + 1, o);
        }
        return s.trim();
    }

    /**
     * convert to string
     *
     * @return string showing this url
     */
    public String dump() {
        String s = "'" + orig + "' '" + proto + "' '" + server + "' " + port + " '" + filPath + "' '" + filName + "' '" + filExt + "'";
        for (int i = 0; i < param.size(); i++) {
            s += " " + param.get(i);
        }
        return s;
    }

    /**
     * convert to string
     *
     * @return lines of string
     */
    public List<String> show() {
        List<String> l = new ArrayList<String>();
        l.add("url=" + toURL(true, true, true, true));
        l.add("url=" + toURL(true, true, true, false));
        l.add("dump=" + dump());
        l.add("filename=" + toFileName());
        l.add("pathname=" + toPathName());
        l.add("email=" + toEmail());
        l.add("proto=" + proto);
        l.add("server=" + server);
        l.add("port=" + port);
        l.add("user=" + username);
        l.add("pass=" + password);
        l.add("path=" + filPath);
        l.add("name=" + filName);
        l.add("extension=" + filExt);
        for (int i = 0; i < param.size(); i++) {
            l.add("" + param.get(i));
        }
        return l;
    }

    /**
     * get port
     *
     * @param def default
     * @return port to connect to
     */
    public int getPort(int def) {
        if (port >= 0) {
            return port;
        }
        if (proto.equals("http")) {
            return servHttp.clearPort;
        }
        if (proto.equals("http2")) {
            return servHttp.clearPort;
        }
        if (proto.equals("https")) {
            return servHttp.securePort;
        }
        return def;
    }

    /**
     * get security
     *
     * @return security to connect with
     */
    public int getSecurity() {
        if (proto.equals("https")) {
            return servGeneric.protoTls;
        }
        return servGeneric.protoTcp;
    }

    /**
     * convert string to url
     *
     * @param url string to convert
     * @return converted url
     */
    public static encUrl parseOne(String url) {
        encUrl res = new encUrl();
        res.fromString(url);
        return res;
    }

}

class encUrlPar {

    public String nam;

    public String val;

    public String toString() {
        return nam + "=" + val;
    }

}
