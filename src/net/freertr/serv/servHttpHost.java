package net.freertr.serv;

import java.net.URLClassLoader;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authGeneric;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.clnt.clntProxy;
import net.freertr.pipe.pipeSide;
import net.freertr.enc.encUrl;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabListing;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.version;

/**
 * http virtual host
 *
 * @author matecsaba
 */
public class servHttpHost implements Runnable, Comparator<servHttpHost> {

    /**
     * create instance
     *
     * @param h host
     */
    public servHttpHost(String h) {
        host = h;
    }

    /**
     * allow nothing
     */
    public final static int apiBitsNothing = 0;

    /**
     * allow something
     */
    public final static int apiBitsSomething = 0x01;

    /**
     * allow exec commands
     */
    public final static int apiBitsExec = 0x02;

    /**
     * allow config commands
     */
    public final static int apiBitsConfig = 0x04;

    /**
     * allow ip info commands
     */
    public final static int apiBitsIpinfo = 0x08;

    /**
     * convert string to api bits
     *
     * @param cmd commands
     * @return api bits
     */
    public static final int string2apiBits(cmds cmd) {
        int i = servHttpHost.apiBitsNothing;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("exec")) {
                i |= servHttpHost.apiBitsExec;
                continue;
            }
            if (a.equals("config")) {
                i |= 4;
                continue;
            }
            if (a.equals("ipinfo")) {
                i |= 8;
                continue;
            }
        }
        if (i == servHttpHost.apiBitsSomething) {
            return servHttpHost.apiBitsNothing;
        }
        return i | servHttpHost.apiBitsSomething;
    }

    /**
     * convert api bits to string
     *
     * @param i bits to convert
     * @return api bits
     */
    public static final String apiBits2string(int i) {
        if ((i & apiBitsSomething) == 0) {
            return "bug=" + i;
        }
        String s = "";
        if ((i & servHttpHost.apiBitsExec) != 0) {
            s += " exec";
        }
        if ((i & servHttpHost.apiBitsConfig) != 0) {
            s += " config";
        }
        if ((i & servHttpHost.apiBitsIpinfo) != 0) {
            s += " ipinfo";
        }
        return s;
    }

    /**
     * name of server
     */
    public final String host;

    /**
     * asked
     */
    public int askNum;

    /**
     * asked
     */
    public long askTim;

    /**
     * path of root directory
     */
    public String path = null;

    /**
     * path of backup directory
     */
    public String backupPath = null;

    /**
     * number of backups to keep
     */
    public int backupCount = 0;

    /**
     * url to redirect to
     */
    public String redir;

    /**
     * log to syslog
     */
    public boolean logging;

    /**
     * proxy for reconnection
     */
    public clntProxy reconnP;

    /**
     * url to reconnect to
     */
    public String reconnT;

    /**
     * restrict url
     */
    public int subconn;

    /**
     * translate url
     */
    public List<cfgTrnsltn> translate;

    /**
     * proxy for stream
     */
    public clntProxy streamP;

    /**
     * url to stream
     */
    public String streamT;

    /**
     * pipe of stream
     */
    public pipeSide streamS;

    /**
     * content type of stream
     */
    public String streamM;

    /**
     * list of receivers
     */
    public List<pipeSide> streamC;

    /**
     * proxy for multiple access
     */
    public clntProxy multiAccP;

    /**
     * urls to multiple access
     */
    public String multiAccT;

    /**
     * page style
     */
    public List<String> style;

    /**
     * speed limit
     */
    public int speedLimit;

    /**
     * serve index for dirs
     */
    public boolean autoIndex = true;

    /**
     * convert markdown files
     */
    public boolean allowMarkdown = false;

    /**
     * directory listing allowed
     */
    public int allowList;

    /**
     * script search allowed
     */
    public String searchScript;

    /**
     * script running allowed
     */
    public int allowScript;

    /**
     * api calls allowed
     */
    public int allowApi;
    
    /**
     * ip info configuration
     */
    public servHoneyPotCfg ipInfo;

    /**
     * image map decode allowed
     */
    public boolean allowImgMap;

    /**
     * web socket allowed
     */
    public boolean allowWebSck;

    /**
     * image streaming allowed
     */
    public boolean allowMediaStrm;

    /**
     * class running allowed
     */
    public URLClassLoader allowClass;

    /**
     * uploading allowed
     */
    public boolean allowUpload;

    /**
     * webdav allowed
     */
    public boolean allowWebDav;

    /**
     * sstp allowed
     */
    public cfgIfc allowSstp;

    /**
     * anyconnect allowed
     */
    public cfgIfc allowAnyconn;

    /**
     * fortinet allowed
     */
    public cfgIfc allowForti;

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * access list
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> accessList;

    public int compare(servHttpHost o1, servHttpHost o2) {
        return o1.host.toLowerCase().compareTo(o2.host.toLowerCase());
    }

    /**
     * start streaming
     */
    public void reStream() {
        if (streamS != null) {
            if (streamS.isClosed() == 0) {
                return;
            }
        }
        new Thread(this).start();
    }

    public void run() {
        try {
            doStream();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doStream() {
        if (debugger.servHttpTraf) {
            logger.debug("startup");
        }
        bits.sleep(1000);
        encUrl srvUrl = encUrl.parseOne(streamT);
        addrIP adr = userTerminal.justResolv(srvUrl.server, streamP.prefer);
        if (adr == null) {
            return;
        }
        pipeSide cnn = streamP.doConnect(servGeneric.protoTcp, adr, srvUrl.getPort(new servHttp().srvPort()), "http");
        if (cnn == null) {
            return;
        }
        streamS = cnn;
        if (debugger.servHttpTraf) {
            logger.debug("conned");
        }
        cnn.lineTx = pipeSide.modTyp.modeCRLF;
        cnn.lineRx = pipeSide.modTyp.modeCRtryLF;
        cnn.linePut("GET " + srvUrl.toURL(false, false, true, true) + " HTTP/1.1");
        cnn.linePut("User-Agent: " + version.usrAgnt + " [streaming]");
        cnn.linePut("Host: " + srvUrl.server);
        cnn.linePut("Accept: */*");
        cnn.linePut("Accept-Language: *");
        cnn.linePut("Accept-Charset: *");
        cnn.linePut("Accept-Encoding: identity");
        cnn.linePut("Connection: Close");
        cnn.linePut("");
        for (;;) {
            String s = cnn.lineGet(1);
            if (s == null) {
                break;
            }
            if (s.length() < 1) {
                break;
            }
            if (debugger.servHttpTraf) {
                logger.debug("rx: " + s);
            }
        }
        if (debugger.servHttpTraf) {
            logger.debug("serving");
        }
        for (;;) {
            byte[] buf = new byte[1024];
            int siz = cnn.blockingGet(buf, 0, buf.length);
            if (siz < 1) {
                if (cnn.isClosed() != 0) {
                    break;
                }
                bits.sleep(1000);
                continue;
            }
            int i = streamC.size() - 1;
            if (i < 0) {
                break;
            }
            for (; i >= 0; i--) {
                pipeSide pip = streamC.get(i);
                if (pip.isClosed() == 0) {
                    pip.nonBlockPut(buf, 0, siz);
                    continue;
                }
                streamC.remove(i);
                pip.setClose();
            }
        }
        cnn.setClose();
        for (int i = streamC.size() - 1; i >= 0; i--) {
            streamC.get(i).setClose();
        }
        if (debugger.servHttpTraf) {
            logger.debug("stopped");
        }
    }

}
