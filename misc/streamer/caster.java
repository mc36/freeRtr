
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.FileReader;

/**
 * stream caster
 *
 * @author matecsaba
 */
public class caster {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        caster app = new caster();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName() + ".";
            a = app.httpRequest("http://localhost/" + a, "./" + a, "cli",
                    "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf;
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    private static String mime;

    private static packet pack;

    /**
     * do one request
     *
     * @param url url of app
     * @param path path of app
     * @param peer client address
     * @param agent user agent
     * @param user auth data
     * @param par parameters
     * @param buf result buffer, if empty, pathname must present
     * @return [pathname"][file name.]extension
     * @throws Exception if something went wrong
     */
    public String httpRequest(String url, String path, String peer, String agent, String user, String[] par, ByteArrayOutputStream buf) throws Exception {
        if (pack == null) {
            try {
                path = path.substring(0, path.lastIndexOf(".")) + ".cfg";
                BufferedReader f = new BufferedReader(new FileReader(path));
                mime = f.readLine();
                String knd = f.readLine();
                String grp = f.readLine();
                String src = f.readLine();
                String prt = f.readLine();
                f.close();
                pack = packer.receiver(grp, src, prt).string2kind(knd);
            } catch (Exception e) {
                return null;
            }
        }
        if (par != null) {
            buf.write(packer.generateWav());
            return "//stream//" + mime;
        }
        if (buf == null) {
            pack.pck.stopper();
            pack = null;
            return null;
        }
        byte[] res = new byte[consts.payl];
        int siz;
        try {
            siz = pack.readKind(res);
            pack.coder.byteSwap(res, siz);
        } catch (Exception e) {
            return null;
        }
        if (siz < 1) {
            return null;
        }
        buf.write(res);
        return mime;
    }

}
