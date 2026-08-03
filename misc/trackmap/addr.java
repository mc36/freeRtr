
import java.io.ByteArrayOutputStream;
import java.io.RandomAccessFile;

/**
 * sample http applet
 *
 * @author matecsaba
 */
public class addr {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        addr app = new addr();
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

    private static byte[] style = null;

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
    public String httpRequest(String url, String path, String peer,
            String agent, String user, String[] par, ByteArrayOutputStream buf)
            throws Exception {
        if (style == null) {
            try {
                RandomAccessFile fr = new RandomAccessFile(path.substring(0, path.lastIndexOf(".")) + ".css", "r");
                byte[] red = new byte[(int) fr.length()];
                fr.read(red, 0, red.length);
                fr.close();
                style = red;
            } catch (Exception e) {
                style = new byte[0];
            }
        }
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><style>\n".getBytes());
        buf.write(style);
        buf.write("</style><title>address</title></head><body>\n".getBytes());
        buf.write(peer.getBytes());
        buf.write("\n</body></html>\n".getBytes());
        return "html";
    }

}
