
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * sample http applet
 *
 * @author matecsaba
 */
public class phoneBook implements Comparator<String> {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        phoneBook app = new phoneBook();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName();
            a = app.httpRequest("http://localhost/" + a, "./" + a, "cli",
                    "clibrowser", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf;
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    public int compare(String o1, String o2) {
        return o1.toLowerCase().compareTo(o2.toLowerCase());
    }

    /**
     * do one request
     *
     * @param url url of app
     * @param path path of app
     * @param peer client address
     * @param agent user agent
     * @param par parameters
     * @param buf result buffer, if empty, pathname must present
     * @return [pathname"][file name.]extension
     * @throws Exception if something went wrong
     */
    public String httpRequest(String url, String path, String peer,
            String agent, String[] par, ByteArrayOutputStream buf)
            throws Exception {
        String name = null;
        for (int o = 0; o < par.length; o++) {
            String a = par[o];
            int i = a.indexOf("=");
            if (i < 0) {
                break;
            }
            String s = a.substring(i + 1, a.length()).trim();
            a = a.substring(0, i).trim();
            if (a.equals("name")) {
                name = s;
                continue;
            }
        }
        if (name == null) {
            buf.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>".getBytes());
            buf.write("<CiscoIPPhoneInput>".getBytes());
            buf.write("<Title>phonebook search</Title>".getBytes());
            buf.write("<Prompt>enter name</Prompt>".getBytes());
            buf.write(("<URL>" + url + "</URL>").getBytes());
            buf.write("<InputItem><DisplayName>name</DisplayName><QueryStringParam>name</QueryStringParam><InputFlags>A</InputFlags><DefaultValue/></InputItem>".getBytes());
            buf.write("</CiscoIPPhoneInput>".getBytes());
            return "xml";
        }
        name = name.toLowerCase();
        int i = path.lastIndexOf(".");
        path = path.substring(0, i) + ".csv";
        List<String> res = new ArrayList<String>();
        try {
            FileInputStream in = new FileInputStream(path);
            BufferedReader rd = new BufferedReader(new InputStreamReader(in));
            while (rd.ready()) {
                String a = rd.readLine();
                if (a.indexOf(name) < 0) {
                    continue;
                }
                i = a.lastIndexOf(",");
                if (i < 0) {
                    continue;
                }
                String s = a.substring(i + 1, a.length()).replaceAll(" ", "").replaceAll("\\(", "").replaceAll("\\)", "").trim();
                a = a.substring(0, i).trim();
                res.add("<DirectoryEntry><Name>" + a + "</Name><Telephone>" + s + "</Telephone></DirectoryEntry>");
            }
            rd.close();
            Collections.sort(res, new phoneBook());
        } catch (Exception e) {
            return null;
        }
        buf.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>".getBytes());
        buf.write("<CiscoIPPhoneDirectory>".getBytes());
        buf.write("<Title>phonebook search</Title>".getBytes());
        buf.write("<Prompt>select person</Prompt>".getBytes());
        for (i = 0; i < res.size(); i++) {
            buf.write(res.get(i).getBytes());
        }
        buf.write("</CiscoIPPhoneDirectory>".getBytes());
        return "xml";
    }

}
