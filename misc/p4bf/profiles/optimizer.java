
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public class optimizer {

    private optimizer() {
    }

    private static void log(String msg) {
        System.out.println(msg);
    }

    private static List<String> doRead(InputStream is) {
        List<String> res = new ArrayList<String>();
        try {
            BufferedReader rd = new BufferedReader(new InputStreamReader(is));
            while (rd.ready()) {
                res.add(rd.readLine());
            }
            rd.close();
            is.close();
        } catch (Exception e) {
            log("error reading stream!");
        }
        return res;
    }

    private static List<String> doRead(String fn) {
        try {
            return doRead(new FileInputStream(fn));
        } catch (Exception e) {
            log("error reading file " + fn + "!");
            return null;
        }
    }

    private static List<String> doExec(String cmd) {
        log("running " + cmd + "...");
        Process prc;
        try {
            Runtime rtm = Runtime.getRuntime();
            StringTokenizer st = new StringTokenizer(cmd);
            String[] arr = new String[st.countTokens()];
            for (int i = 0; st.hasMoreTokens(); i++) {
                arr[i] = st.nextToken();
            }
            prc = rtm.exec(arr);
            prc.waitFor();
            List<String> res = doRead(prc.getInputStream());
            prc.destroy();
            return res;
        } catch (Exception e) {
            log("error starting process!");
            return null;
        }
    }

    private static void doWrite(OutputStream os, String msg) {
        try {
            BufferedWriter wr = new BufferedWriter(new OutputStreamWriter(os));
            wr.write(msg);
            wr.write("\n");
            wr.flush();
        } catch (Exception e) {
            log("error writing stream!");
        }
    }

    private static void doWrite(String fn, String msg) {
        try {
            FileOutputStream ot = new FileOutputStream(fn, false);
            doWrite(ot, msg);
            ot.close();
        } catch (Exception e) {
            log("error writing file " + fn + "!");
        }
    }

    private static void doWrite(String fn, List<String> msg) {
        try {
            FileOutputStream ot = new FileOutputStream(fn, false);
            for (int i = 0; i < msg.size(); i++) {
                doWrite(ot, msg.get(i));
            }
            ot.close();
        } catch (Exception e) {
            log("error writing file " + fn + "!");
        }
    }

    private static void doDelete(String fn) {
        new File(fn).delete();
    }

    private final static String tempFile = "optimizer.tmp";

    private final static String tempProg = "optimizer";

    private static boolean checkString(List<String> orig, String str) {
        for (int i = 0; i < orig.size(); i++) {
            if (orig.get(i).indexOf(str) >= 0) {
                return true;
            }
        }
        return false;
    }

    private static List<String> doTransform(List<String> orig, int num1, int num2) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < orig.size(); i++) {
            String a = orig.get(i);
            int o = a.indexOf("$p");
            int p = a.indexOf("$s");
            if ((o + p) < 10) {
                res.add(a);
                continue;
            }
            if (o < 0) {
                o = a.length();
            }
            if (p < 0) {
                p = a.length();
            }
            if (p < o) {
                o = p;
            }
            o -= 8;
            String b = a.substring(o, a.length());
            a = a.substring(0, o);
            b = b.replaceAll("\\$p", "" + num1);
            b = b.replaceAll("\\$s", "" + num2);
            doWrite(tempFile, "echo " + b + " | bc");
            b = doExec("bash " + tempFile).get(0);
            res.add(a + b);
        }
        res.add("#define _TABLE_SIZE_P4_");
        doDelete(tempFile);
        return res;
    }

    private static boolean doRound(List<String> orig, int num1, int num2) {
        log("trying with " + num1 + " and " + num2);
        doDelete(tempFile);
        List<String> res = doTransform(orig, num1, num2);
        res.add("#include \"bf_router.p4\"");
        doWrite(tempProg + ".p4", res);
        doExec(System.getenv("SDE") + "/install/bin/bf-p4c -I../p4src/ -Xp4c=\"--disable-parse-depth-limit\" " + tempProg + ".p4");
        boolean succ = new File(tempProg + ".tofino/pipe/tofino.bin").exists();
        doWrite(tempFile, "rm -rf " + tempProg + ".tofino");
        doExec("bash " + tempFile);
        doDelete(tempProg + ".p4");
        log("returning " + succ + " for " + num1 + " and " + num2);
        return succ;
    }

    private static void doFinal(String prof, List<String> orig, int num1, int num2) {
        List<String> res = doTransform(orig, num1, num2);
        res.add(0, "#ifdef " + prof.toUpperCase().replaceAll("-", "_"));
        res.add("#endif");
        doWrite(prof + ".p4", res);
    }

    private final static int step1 = 1024;

    private final static int step2 = 4096;

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String prof = args[0];
        prof = prof.substring(0, prof.lastIndexOf("."));
        List<String> orig = doRead(prof + ".tmpl");
        log("read " + orig.size() + " lines from " + prof);
        int num1 = step1;
        int num2 = step2;
        boolean res;
        if (!checkString(orig, "$p")) {
            log("*** no first value to optimize ***");
        } else {
            num1 = 256 * step1;
            res = false;
            for (;;) {
                if (num1 < step1) {
                    break;
                }
                res = doRound(orig, num1, num2);
                if (res) {
                    break;
                }
                num1 >>>= 1;
            }
            if (!res) {
                log("*** unable to find a working value ***");
                return;
            }
            for (;;) {
                num1 += step1;
                res = doRound(orig, num1, num2);
                if (!res) {
                    break;
                }
            }
            num1 -= step1;
        }
        if (!checkString(orig, "$s")) {
            log("*** no second value to optimize ***");
        } else {
            num2 = 64 * step2;
            res = false;
            for (;;) {
                if (num2 < step2) {
                    break;
                }
                res = doRound(orig, num1, num2);
                if (res) {
                    break;
                }
                num2 >>>= 1;
            }
            if (!res) {
                log("*** unable to find a working value ***");
                return;
            }
            for (;;) {
                num2 += step2;
                res = doRound(orig, num1, num2);
                if (!res) {
                    break;
                }
            }
            num2 -= step2;
        }
        res = doRound(orig, num1, num2);
        if (!res) {
            log("*** profile failed at final verification ***");
            return;
        }
        doFinal(prof, orig, num1, num2);
        log("*** working profile generated successfully ***");
    }

}
