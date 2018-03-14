
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.Socket;

/**
 * web temperature setter
 *
 * @author matecsaba
 */
public class phoneExecute {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            return;
        }
        phoneExecute a = new phoneExecute();
        a.doer(args[0], args[1]);
    }

    private Socket sck;

    private DataOutputStream out;

    private BufferedReader in;

    private void txLn(String s) throws Exception {
        System.out.println("tx: " + s);
        out.writeBytes(s + "\r\n");
    }

    private void doer(String dev, String cmd) {
        try {
            String xml = "XML=<CiscoIPPhoneExecute><ExecuteItem Priority=\"0\" URL=\"" + cmd + "\"/></CiscoIPPhoneExecute>";
            sck = new Socket(dev, 80);
            out = new DataOutputStream(sck.getOutputStream());
            in = new BufferedReader(new InputStreamReader(sck.getInputStream()));
            txLn("POST /CGI/Execute HTTP/1.1");
            txLn("Host: " + dev);
            txLn("User-Agent: phone-agent/1.0");
            txLn("Accept: */*");
            txLn("Content-Type: application/xml");
            txLn("Content-Length: " + (xml.length() + 2));
            txLn("");
            txLn(xml);
            for (;;) {
                String a = in.readLine();
                if (a == null) {
                    break;
                }
                System.out.println("rx: " + a);
            }
        } catch (Exception e) {
            System.out.println("error sending command!");
        }
    }

}
