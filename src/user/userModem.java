package user;

import addr.addrIP;
import cfg.cfgAll;
import pipe.pipeConnect;
import pipe.pipeSide;
import serv.servGeneric;
import serv.servTelnet;
import util.bits;
import util.version;

/**
 * modem emulation interface
 *
 * @author matecsaba
 */
public class userModem {

    private final pipeSide pipe;

    private boolean echo;

    private boolean quiet;

    private boolean numeric;

    /**
     * create modem interface
     *
     * @param console pipeline to use
     */
    public userModem(pipeSide console) {
        pipe = console;
        doReset();
    }

    /**
     * do reset
     */
    public void doReset() {
        echo = true;
        quiet = false;
        numeric = false;
    }

    /**
     * do modem work
     */
    public void doWork() {
        for (;;) {
            if (doRound()) {
                break;
            }
        }
    }

    private void doResult(int i) {
        if (quiet) {
            return;
        }
        if (numeric) {
            pipe.linePut("" + i);
            return;
        }
        switch (i) {
            case 0:
                pipe.linePut("OK");
                break;
            case 1:
                pipe.linePut("CONNECT");
                break;
            case 2:
                pipe.linePut("RING");
                break;
            case 3:
                pipe.linePut("NO CARRIER");
                break;
            case 4:
                pipe.linePut("ERROR");
                break;
            case 6:
                pipe.linePut("NO DIALTONE");
                break;
            case 7:
                pipe.linePut("BUSY");
                break;
            case 8:
                pipe.linePut("NO ANSWER");
                break;
            case 52:
                pipe.linePut("CARRIER 300");
                break;
            case 67:
                pipe.linePut("COMPRESSION: NONE");
                break;
            case 77:
                pipe.linePut("PROTOCOL: FSK");
                break;
            default:
                pipe.linePut("UNKNOWN: " + i);
                break;
        }
    }

    private boolean doRound() {
        int i;
        if (echo) {
            i = 0x32;
        } else {
            i = 0x11;
        }
        if (pipe.isClosed() != 0) {
            return true;
        }
        String lin = pipe.lineGet(i);
        lin = lin.trim().toLowerCase();
        if (!lin.startsWith("at")) {
            return false;
        }
        lin = lin.substring(2, lin.length()).replaceAll(" ", "");
        for (;;) {
            if (lin.length() < 1) {
                break;
            }
            String cmd = lin.substring(0, 1);
            lin = lin.substring(1, lin.length());
            if (cmd.equals("a")) { // answer
                doResult(3);
                return false;
            }
            if (cmd.equals("d")) { // dial
                if (lin.startsWith("t")) {
                    lin = lin.substring(1, lin.length());
                }
                addrIP trg = new addrIP();
                if (trg.fromString(lin)) {
                    doResult(6);
                    return false;
                }
                pipeSide con = cfgAll.clntConnect(servGeneric.protoTcp, trg, new servTelnet().srvPort(), "modem");
                if (con == null) {
                    doResult(3);
                    return false;
                }
                doResult(1);
                pipeConnect.connect(pipe, con, false);
                for (;;) {
                    if (con.isClosed() != 0) {
                        break;
                    }
                    bits.sleep(100);
                }
                doResult(3);
                return false;
            }
            int par = 0;
            for (;;) {
                if (lin.length() < 1) {
                    break;
                }
                String chr = lin.substring(0, 1);
                i = bits.str2num(chr);
                if (!chr.equals("" + i)) {
                    break;
                }
                lin = lin.substring(1, lin.length());
                par = (par * 10) + i;
            }
            if (cmd.equals("e")) { // echo mode
                echo = par != 0;
                continue;
            }
            if (cmd.equals("h")) { // hang up
                continue;
            }
            if (cmd.equals("i")) { // information
                pipe.linePut(version.headLine);
                continue;
            }
            if (cmd.equals("m")) { // mute
                continue;
            }
            if (cmd.equals("o")) { // online mode
                continue;
            }
            if (cmd.equals("q")) { // quiet mode
                quiet = par != 0;
                continue;
            }
            if (cmd.equals("s")) { // registers
                doResult(4);
                continue;
            }
            if (cmd.equals("v")) { // verbose mode
                numeric = par == 0;
                continue;
            }
            if (cmd.equals("x")) { // result set
                continue;
            }
            if (cmd.equals("z")) { // reset
                if (par == 99) {
                    return true;
                }
                doReset();
                continue;
            }
            doResult(4);
            return false;
        }
        doResult(0);
        return false;
    }

}
