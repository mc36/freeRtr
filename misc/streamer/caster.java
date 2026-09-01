
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
            buf.write(new byte[]{0x52, 0x49, 0x46, 0x46}); // riff
            write32(buf, -1); // length
            buf.write(new byte[]{0x57, 0x41, 0x56, 0x45}); // wave
            buf.write(new byte[]{0x66, 0x6d, 0x74, 0x20}); // fmt
            write32(buf, 0x10); // chunk
            write16(buf, 1); // codec
            write16(buf, 2); // channels
            write32(buf, consts.rate); // sample rate
            write32(buf, consts.rate * 2 * consts.smpb); // bytes per sec
            write16(buf, 2 * consts.smpb); // alignment
            write16(buf, consts.smpb * 8); // bit depth
            buf.write(new byte[]{0x64, 0x61, 0x74, 0x61}); // data
            write32(buf, -1); // length
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

    private static void write32(ByteArrayOutputStream output, int value) {
        write16(output, value);
        write16(output, value >>> 16);
    }

    private static void write16(ByteArrayOutputStream output, int value) {
        output.write(value);
        output.write(value >>> 8);
    }

}
