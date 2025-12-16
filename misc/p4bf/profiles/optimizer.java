
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

    private static void log(List<String> lst) {
        for (int i = 0; i < lst.size(); i++) {
            log(lst.get(i));
        }
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
            List<String> err = doRead(prc.getErrorStream());
            log(err);
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

    private static String tempProg = null;

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
            doWrite(tempProg + ".tmp", "echo " + b + " | bc");
            b = doExec("bash " + tempProg + ".tmp").get(0);
            res.add(a + b);
        }
        res.add("#define _TABLE_SIZE_P4_");
        doDelete(tempProg + ".tmp");
        return res;
    }

    private static boolean doCompile(String prof, String pars, List<String> orig, int num1, int num2) {
        log("trying with " + num1 + " and " + num2 + " on " + prof);
        doDelete(tempProg + ".tmp");
        List<String> res = doTransform(orig, num1, num2);
        res.add("#include \"bf_router.p4\"");
        doWrite(tempProg + ".p4", res);
        doExec(System.getenv("SDE_INSTALL") + "/bin/bf-p4c -I. -I../p4src/ " + pars + " " + tempProg + ".p4");
        boolean succ = new File(tempProg + ".tofino/pipe/tofino.bin").exists();
        succ |= new File(tempProg + ".tofino2/pipe/tofino2.bin").exists();
        doWrite(tempProg + ".tmp", "rm -rf " + tempProg + ".tofino*");
        doExec("bash " + tempProg + ".tmp");
        doDelete(tempProg + ".p4");
        log("returning " + succ + " for " + num1 + " and " + num2 + " on " + prof);
        return succ;
    }

    private static void doFinal(String prof, List<String> orig, int num1, int num2) {
        List<String> res = doTransform(orig, num1, num2);
        res.add(0, "#ifdef " + prof.toUpperCase().replaceAll("-", "_"));
        res.add("#endif");
        doWrite(prof + ".p4", res);
    }

    private static int doOptimize(String prof, String pars, List<String> orig, boolean first, int num1, int num2) {
        int low = 0;
        int high = 1024 * 1024;
        for (;;) {
            if ((high - low) < 1024) {
                return low;
            }
            int mid = ((high - low) / 2) + low;
            log("currently low=" + low + " mid=" + mid + " high=" + high);
            boolean res;
            if (first) {
                res = doCompile(prof, pars, orig, mid, num2);
            } else {
                res = doCompile(prof, pars, orig, num1, mid);
            }
            if (res) {
                low = mid;
            } else {
                high = mid;
            }
        }
    }

    private static int[] doParam(String prof, String pars, List<String> orig) {
        log("optimizing for " + pars);
        int num1 = 1024;
        int num2 = 1024;
        if (!checkString(orig, "$p")) {
            log("*** no first value to optimize ***");
        } else {
            num1 = doOptimize(prof, pars, orig, true, num1, num2);
            if (num1 <= 0) {
                log("*** unable to find a working first value ***");
                return null;
            }
        }
        if (!checkString(orig, "$s")) {
            log("*** no second value to optimize ***");
        } else {
            num2 = doOptimize(prof, pars, orig, false, num1, num2);
            if (num2 <= 0) {
                log("*** unable to find a working second value ***");
                return null;
            }
        }
        int[] res = {num1, num2};
        return res;
    }

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        tempProg = "optimizer" + ProcessHandle.current().pid();
        String prof = args[1];
        List<String> pars = doRead(args[1]);
        log("read " + pars.size() + " lines from " + prof);
        prof = args[0];
        prof = prof.substring(0, prof.lastIndexOf("."));
        List<String> orig = doRead(prof + ".tmpl");
        log("read " + orig.size() + " lines from " + prof);
        int nums[] = doParam(prof, pars.get(0), orig);
        if (nums == null) {
            System.exit(1);
        }
        for (int i = 0;;) {
            if (i >= pars.size()) {
                break;
            }
            if (doCompile(prof, pars.get(i), orig, nums[0], nums[1])) {
                i++;
                continue;
            }
            nums[0] -= nums[0] / 100;
            nums[1] -= nums[1] / 100;
            i = 0;
        }
        doFinal(prof, orig, nums[0], nums[1]);
        log("*** working profile generated successfully ***");
    }

}
