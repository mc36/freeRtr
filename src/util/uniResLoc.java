package util;

import addr.addrIP;
import addr.addrIPv6;
import java.util.ArrayList;
import java.util.List;
import serv.servHttp;

/**
 * uniform resource locator (rfc3986)
 *
 * @author matecsaba
 */
public class uniResLoc {

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
    public List<uniResLocPar> param;

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
        param = new ArrayList<uniResLocPar>();
        username = "";
        password = "";
    }

    /**
     * copy from other url
     *
     * @param src original url to copy
     */
    public void copyBytes(uniResLoc src) {
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
            int o;
            try {
                o = Integer.parseInt(a, 16);
            } catch (Exception e) {
                o = (byte) '%';
            }
            r = r + s.substring(0, i) + (char) o;
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
        final String e = "%:?#[]@!$&'()*+,;=";
        for (int ps = 0; ps < s.length(); ps++) {
            char c = s.charAt(ps);
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
     * @return false on success, true on error
     */
    public boolean fromString(String s) {
        clear();
        orig = "" + s;
        if (s == null) {
            return true;
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
        i = filPath.indexOf("?");
        String pars = "";
        if (i >= 0) {
            pars = filPath.substring(i + 1, filPath.length());
            filPath = filPath.substring(0, i);
        }
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
        proto = percentUncode(proto);
        server = percentUncode(server);
        username = percentUncode(username);
        password = percentUncode(password);
        filPath = percentUncode(filPath);
        filName = percentUncode(filName);
        filExt = percentUncode(filExt);
        if (pars.length() < 1) {
            return false;
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
            uniResLocPar ntry = new uniResLocPar();
            i = s.indexOf("=");
            if (i < 0) {
                ntry.nam = percentUncode(s);
            } else {
                ntry.nam = percentUncode(s.substring(0, i));
                ntry.val = percentUncode(s.substring(i + 1, s.length()));
            }
            param.add(ntry);
        }
        return false;
    }

    /**
     * add parameter
     *
     * @param nam name
     * @param val value
     */
    public void addParam(String nam, String val) {
        uniResLocPar ntry = new uniResLocPar();
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
            uniResLocPar ntry = param.get(i);
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
    public void followLink(uniResLoc n) {
        if (!n.server.equals("")) {
            copyBytes(n);
            return;
        }
        param = n.param;
        if ((n.filName + n.filExt).equals("")) {
            return;
        }
        filName = n.filName;
        filExt = n.filExt;
        if (n.filPath.equals("")) {
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
            if (a.equals(".")) {
                continue;
            }
            if (a.equals("")) {
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

    /**
     * convert to url
     *
     * @param userpass put password
     * @param params put parameters
     * @return url format
     */
    public String toURL(boolean userpass, boolean params) {
        String a;
        String b;
        String c;
        if (port >= 0) {
            a = ":" + port;
        } else {
            a = "";
        }
        if (new addrIPv6().fromString(server)) {
            b = percentEncode(server);
        } else {
            b = "[" + server + "]";
        }
        if (userpass && (username.length() > 0)) {
            b = percentEncode(username) + ":" + percentEncode(password) + "@" + b;
        }
        c = "";
        if (params) {
            for (int i = 0; i < param.size(); i++) {
                uniResLocPar ntry = param.get(i);
                c += "&" + percentEncode(ntry.nam) + "=" + percentEncode(ntry.val);
            }
            if (c.length() > 0) {
                c = "?" + c.substring(1, c.length());
            }
        }
        return percentEncode(proto) + "://" + b + a + "/" + percentEncode(filPath) + percentEncode(filName) + percentEncode(filExt) + c;
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
        l.add("url=" + toURL(true, true));
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
            return new servHttp().srvPort();
        }
        if (proto.equals("https")) {
            return servHttp.securePort;
        }
        return def;
    }

    /**
     * convert string to url
     *
     * @param url string to convert
     * @return converted url
     */
    public static uniResLoc parseOne(String url) {
        uniResLoc res = new uniResLoc();
        res.fromString(url);
        return res;
    }

}

class uniResLocPar {

    public String nam;

    public String val;

    public String toString() {
        return nam + "=" + val;
    }

}
