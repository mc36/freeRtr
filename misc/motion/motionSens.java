
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;

/**
 * motion sensor handler
 *
 * @author matecsaba
 */
public class motionSens {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.out.println("usage: sensor <host> <port> <bit> <need> <reget> <url>");
            return;
        }
        System.out.println("connecting");
        Socket sck = new Socket(args[0], motionUtil.str2num(args[1]));
        System.out.println("connected");
        int bit = motionUtil.str2num(args[2]);
        int ned = motionUtil.str2num(args[3]);
        int reg = motionUtil.str2num(args[4]);
        String trg = args[5];
        sck.setSoTimeout(60000);
        InputStream in = sck.getInputStream();
        long lst = 0;
        for (;;) {
            int i = in.read();
            System.out.println("got " + i);
            if (i < 0) {
                break;
            }
            if ((i & bit) == ned) {
                continue;
            }
            if ((motionUtil.getTime() - lst) < reg) {
                System.out.println("skipping api");
                continue;
            }
            lst = motionUtil.getTime();
            System.out.println("calling api");
            try {
                URL testUrl = new URI(trg).toURL();
                URLConnection testConn = testUrl.openConnection();
                testConn.setConnectTimeout(5000);
                testConn.setReadTimeout(5000);
                BufferedReader testReader = new BufferedReader(new InputStreamReader(testConn.getInputStream()));
                testReader.readLine();
                testReader.close();
            } catch (Exception e) {
            }
        }
    }

}
