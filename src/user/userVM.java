package user;

import cfg.cfgAll;
import cry.cryHashCrc32;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pipe.pipeSide;
import tab.tabGen;
import util.bits;
import util.logger;
import util.uniResLoc;
import util.version;

/**
 * virtual machine
 *
 * @author matecsaba
 */
public class userVM {

    private static final String rootDir = "/rtr/vm";

    private static final int reg_a = 1;

    private static final int reg_b = 2;

    private static final int reg_c = 3;

    private static final int reg_d = 4;

    private static final int reg_src = 5;

    private static final int reg_trg = 6;

    private static final int reg_cip = 7;

    /**
     * termination flag
     */
    public static final int res_term = 0x40000000;

    private static final int memBound = 65536;

    private boolean allowFileIO;

    private int[] regs;

    private int flags; // 1=above, 2=below, 4=equal

    private String nam;

    private String par;

    private byte[] stackD;

    private int stackP;

    private byte[] codeD;

    private byte[] dataD;

    private int dataP;

    private int dataS;

    private int[] procD;

    private tabGen<userVMfile> files;

    private tabGen<userVMdir> dirs;

    private pipeSide console;

    private String currDir;

    /**
     * do vm work
     *
     * @param cons pipe to use
     * @param fio set true to allow file io
     * @param dir working directory
     * @param name file to load
     * @param param parameters to give
     * @return result code
     */
    public static int doWork(pipeSide cons, boolean fio, String dir,
            String name, String param) {
        userVM vm = new userVM(cons, fio, dir);
        int res;
        try {
            vm.doLoad(name, param);
            res = vm.doProcess();
        } catch (Exception e) {
            cons.linePut("vm error: " + logger.dumpException(e));
            List<String> txt = vm.dump();
            for (int i = 0; i < txt.size(); i++) {
                cons.linePut(txt.get(i));
            }
            res = 1;
        }
        try {
            vm.doFinish();
        } catch (Exception e) {
        }
        return res;
    }

    /**
     * construct vm
     *
     * @param cons pipe to use
     * @param fio set true to allow file io
     * @param dir working directory
     */
    public userVM(pipeSide cons, boolean fio, String dir) {
        console = cons;
        allowFileIO = fio;
        currDir = "/" + uniResLoc.normalizePath(dir + "/");
        regs = new int[8];
    }

    public String toString() {
        return "emulating " + nam;
    }

    /**
     * dump this virtual machine
     *
     * @return dump of virtual machine
     */
    public List<String> dump() {
        List<String> l = new ArrayList<String>();
        l.add("nam='" + nam + "'  par='" + par + "' dir='" + currDir + "'");
        l.add("a=" + regs[reg_a] + "  b=" + regs[reg_b] + "  c=" + regs[reg_c]
                + "  d=" + regs[reg_d]);
        l.add("src=" + regs[reg_src] + "  trg=" + regs[reg_trg] + "  cip="
                + regs[reg_cip]);
        return l;
    }

    /**
     * convert result to error
     *
     * @param i result
     * @return error
     */
    public static int result2error(int i) {
        if ((i & res_term) == 0) {
            return i;
        } else {
            return 0;
        }
    }

    /**
     * convert result to exit code
     *
     * @param i result
     * @return exit code
     */
    public static int result2extcod(int i) {
        i &= ~res_term;
        return i;
    }

    /**
     * convert result to string
     *
     * @param i result
     * @return string
     */
    public static String result2string(int i) {
        if (result2error(i) != 0) {
            return "error code=" + result2error(i);
        }
        return "exit code=" + result2extcod(i);
    }

    private static int getSize(int siz) {
        siz &= 15;
        switch (siz) {
            case 1:
                return 1;
            case 2:
                return 2;
            case 3:
                return 4;
        }
        return 0;
    }

    private static int convType(int d, int sig, int siz, int after) {
        sig &= 1;
        siz &= 15;
        switch (siz) {
            case 1:
                d &= 0xff;
                break;
            case 2:
                d &= 0xffff;
                break;
            case 3:
                d &= 0xffffffff;
                break;
        }
        if (sig != 0) {
            switch (siz) {
                case 1:
                    d = (byte) d;
                    break;
                case 2:
                    d = (short) d;
                    break;
                case 3:
                    // d = d;
                    break;
            }
        }
        if (after != 0) {
            switch (siz) {
                case 1:
                    d &= 0xff;
                    break;
                case 2:
                    d &= 0xffff;
                    break;
                case 3:
                    d &= 0xffffffff;
                    break;
            }
        }
        return d;
    }

    private static String replacePathSep(String s) {
        byte[] buf = s.getBytes();
        for (int i = 0; i < buf.length; i++) {
            switch (buf[i]) {
                case 47:
                    buf[i] = 92;
                    break;
                case 92:
                    buf[i] = 47;
                    break;
            }
        }
        return new String(buf);
    }

    private static String fromUnix(String s) {
        return "c:" + replacePathSep(s);
    }

    private String fromDos(String s) {
        s = replacePathSep(s);
        if (s.length() > 2) {
            if (s.substring(1, 3).equals(":/")) {
                s = s.substring(2, s.length());
            }
        }
        if (!s.startsWith("/")) {
            s = currDir + s;
        }
        return s;
    }

    private String getPascii(int ofs) {
        int i = dataD[ofs];
        byte[] buf = new byte[i];
        bits.byteCopy(dataD, ofs + 1, buf, 0, buf.length);
        return new String(buf);
    }

    private void putPascii(int ofs, String str) {
        final int max = 255;
        if (str.length() > max) {
            str = str.substring(0, max);
        }
        byte[] res = str.getBytes();
        dataD[ofs] = (byte) res.length;
        bits.byteCopy(res, 0, dataD, ofs + 1, res.length);
    }

    private void putAsciiz(int ofs, String str) {
        byte[] res = str.getBytes();
        bits.byteCopy(res, 0, dataD, ofs, res.length);
        bits.msbPutD(dataD, ofs + res.length, 0);
    }

    private int readOne(byte[] data, int ofs, int siz, int msb)
            throws Exception {
        siz &= 15;
        switch (siz) {
            case 1:
                return data[ofs] & 0xff;
            case 2:
                switch (msb) {
                    case 1: // default
                    case 3: // lsb
                        return bits.lsbGetW(data, ofs);
                    case 2: // msb
                        return bits.msbGetW(data, ofs);
                    default:
                        throw new Exception("invalid type");
                }
            case 3:
                switch (msb) {
                    case 1: // default
                    case 3: // lsb
                        return bits.lsbGetD(data, ofs);
                    case 2:
                        return bits.msbGetD(data, ofs);
                    default:
                        throw new Exception("invalid type");
                }
            default:
                throw new Exception("invalid type");
        }
    }

    private void writeOne(byte[] data, int ofs, int siz, int msb, int val)
            throws Exception {
        siz &= 15;
        switch (siz) {
            case 1:
                data[ofs] = (byte) (val & 0xff);
                return;
            case 2:
                switch (msb) {
                    case 1: // default
                    case 3: // lsb
                        bits.lsbPutW(data, ofs, val);
                        return;
                    case 2: // msb
                        bits.msbPutW(data, ofs, val);
                        return;
                    default:
                        throw new Exception("invalid type");
                }
            case 3:
                switch (msb) {
                    case 1: // default
                    case 3: // lsb
                        bits.lsbPutD(data, ofs, val);
                        return;
                    case 2: // msb
                        bits.msbPutD(data, ofs, val);
                        return;
                    default:
                        throw new Exception("invalid type");
                }
            default:
                throw new Exception("invalid type");
        }
    }

    private int getConst(int siz) throws Exception {
        int i = readOne(codeD, regs[reg_cip], siz, 1);
        regs[reg_cip] += getSize(siz);
        return i;
    }

    private void pushOne(int val, int siz) throws Exception {
        writeOne(stackD, stackP, siz, 1, val);
        stackP += getSize(siz);
    }

    private int popOne(int siz) throws Exception {
        stackP -= getSize(siz);
        return readOne(stackD, stackP, siz, 1);
    }

    private int getMemory() throws Exception {
        int i, o, p;
        i = getConst(1);
        p = i & 0x80;
        i = i & 15;
        o = getConst(3);
        i = regs[i];
        if (p == 0) {
            o += i;
        } else {
            o -= i;
        }
        return o;
    }

    private static int convCol(int i) {
        final int[] tab = {0, 4, 2, 6, 1, 5, 3, 7, 8, 12, 10, 14, 9, 13, 11,
            15};
        int fg = i & 0xf;
        int bg = (i >>> 4) & 0xf;
        return tab[fg] | (tab[bg] << 16);
    }

    private int getKey() {
        return getKey(console);
    }

    /**
     * get one key
     *
     * @param pipe pipeline to use
     * @return key readed
     */
    public static int getKey(pipeSide pipe) {
        byte[] buf = new byte[1];
        if (pipe.blockingGet(buf, 0, buf.length) != buf.length) {
            return -1;
        }
        int i = buf[0] & 0xff;
        switch (i) {
            case 127: // delete
                return 0x8003;
            case 8: // backspace
                return 0x8003;
            case 9: // tabulator
                return 0x8002;
            case 13: // enter
                return 0x8004;
            case 27: // escape
                break;
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
            case 10:
            case 11:
            case 12:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
            case 28:
            case 29:
            case 30:
            case 31:
                return 0x0260 | i;
            default: // any key
                return i;
        }
        if (pipe.blockingGet(buf, 0, buf.length) != buf.length) {
            return -1;
        }
        i = buf[0] & 0xff;
        switch (i) {
            case 8: // backspace
                return 0x8403;
            case 9: // tabulator
                return 0x8402;
            case 13: // enter
                return 0x8404;
            case 27: // escape
                return 0x8005;
            case 91: // [
                break;
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
            case 10:
            case 11:
            case 12:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
            case 28:
            case 29:
            case 30:
            case 31:
                return 0x0660 | i;
            default: // any key
                return i | 0x0400;
        }
        String s = "";
        for (;;) {
            if (pipe.blockingGet(buf, 0, buf.length) != buf.length) {
                return -1;
            }
            i = buf[0] & 0xff;
            boolean need2stop = false;
            switch (i) {
                case 0x30:
                case 0x31:
                case 0x32:
                case 0x33:
                case 0x34:
                case 0x35:
                case 0x36:
                case 0x37:
                case 0x38:
                case 0x39:
                    break;
                case 91: // [
                    break;
                default:
                    need2stop = true;
                    break;
            }
            if (need2stop) {
                break;
            }
            s += new String(buf);
        }
        if (s.startsWith("[")) {
            final int[] keys1 = {20, 21, 22, 23, 24};
            switch (i) {
                case 65:
                case 66:
                case 67:
                case 68:
                case 69:
                    return keys1[i - 65] | 0x8000;
                default:
                    return i;
            }
        }
        final int[] keys1 = {12, 13, 15, 14};
        final int[] keys2 = {8, 6, 7, 9, 10, 11};
        final int[] keys3 = {20, 21, 22, 23, 24, 0, 25, 26, 27, 28, 29, 0, 30,
            31};
        switch (i) {
            case 65:
            case 66:
            case 67:
            case 68:
                return keys1[i - 65] | 0x8000;
            case 126:
                i = bits.str2num(s);
                switch (i) {
                    case 1:
                    case 2:
                    case 3:
                    case 4:
                    case 5:
                    case 6:
                        return keys2[i - 1] | 0x8000;
                    case 11:
                    case 12:
                    case 13:
                    case 14:
                    case 15:
                    case 16:
                    case 17:
                    case 18:
                    case 19:
                    case 20:
                    case 21:
                    case 22:
                    case 23:
                    case 24:
                        return keys3[i - 11] | 0x8000;
                    default:
                        return 126;
                }
            default:
                return i;
        }
    }

    /**
     * load one file
     *
     * @param name file to load
     * @param param parameters to give
     * @throws Exception on error
     */
    public void doLoad(String name, String param) throws Exception {
        name = "/" + uniResLoc.normalizePath(name);
        nam = name;
        par = param;
        RandomAccessFile f = new RandomAccessFile(rootDir + name, "r");
        int codeS = (int) f.length();
        codeD = new byte[codeS];
        f.read(codeD, 0, codeS);
        f.close();
        if (getConst(3) != 0x30314d56) {
            throw new Exception("invalid magic number");
        }
        int i = getConst(3);
        if (i != codeS) {
            throw new Exception("invalid size field");
        }
        int stackS = getConst(3);
        dataS = getConst(3) + memBound;
        int procS = getConst(3);
        int procB = getConst(3);
        cryHashCrc32 h = new cryHashCrc32();
        h.init();
        h.update(codeD, 0, codeD.length - 4);
        byte[] buf = h.finish();
        regs[reg_cip] = codeD.length - 4;
        if (getConst(3) != bits.msbGetD(buf, 0)) {
            throw new Exception("invalid checksum");
        }
        regs[reg_cip] = procB;
        stackD = new byte[stackS + memBound];
        dataD = new byte[dataS];
        procD = new int[procS];
        files = new tabGen<userVMfile>();
        dirs = new tabGen<userVMdir>();
    }

    /**
     * run until end
     *
     * @return result code
     * @throws Exception on error
     */
    public int doProcess() throws Exception {
        int i;
        for (;;) {
            i = doOpcode();
            if ((i & res_term) != 0) {
                break;
            }
        }
        return i;
    }

    /**
     * finish the process
     */
    public void doFinish() {
        for (int i = 0; i < files.size(); i++) {
            try {
                files.get(i).fil.close();
            } catch (Exception e) {
            }
        }
    }

    /**
     * run one opcode
     *
     * @return result code
     * @throws Exception on error
     */
    public int doOpcode() throws Exception {
        int form;
        int siz1, siz2;
        int reg1, reg2;
        int val1, val2;
        int opc = getConst(1);
        switch (opc) {
            case 1: // add
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                regs[reg1] += val1;
                return 0;
            case 2: // sub
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                regs[reg1] -= val1;
                return 0;
            case 3: // mul
                siz1 = getConst(1);
                siz2 = siz1 >>> 6;
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                val1 = convType(val1, siz2, siz1, 0);
                val2 = convType(regs[reg1], siz2, siz1, 0);
                regs[reg1] = val2 * val1;
                return 0;
            case 4: // div
                siz1 = getConst(1);
                siz2 = siz1 >>> 6;
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                val1 = convType(val1, siz2, siz1, 0);
                val2 = convType(regs[reg1], siz2, siz1, 0);
                if (val1 == 0) {
                    throw new Exception("division by zero");
                }
                regs[reg1] = val2 / val1;
                return 0;
            case 5: // mod
                siz1 = getConst(1);
                siz2 = siz1 >>> 6;
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                val1 = convType(val1, siz2, siz1, 0);
                val2 = convType(regs[reg1], siz2, siz1, 0);
                if (val1 == 0) {
                    throw new Exception("division by zero");
                }
                regs[reg1] = val2 % val1;
                return 0;
            case 6: // or
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                regs[reg1] |= val1;
                return 0;
            case 7: // xor
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                regs[reg1] ^= val1;
                return 0;
            case 8: // and
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                regs[reg1] &= val1;
                return 0;
            case 9: // not
                siz1 = getConst(1);
                reg1 = getConst(1);
                regs[reg1] = -1 - regs[reg1];
                return 0;
            case 10: // neg
                siz1 = getConst(1);
                reg1 = getConst(1);
                regs[reg1] = -regs[reg1];
                return 0;
            case 11: // shl
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                regs[reg1] <<= val1;
                return 0;
            case 12: // shr
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                regs[reg1] >>>= val1;
                return 0;
            case 13: // push
                siz1 = getConst(1);
                reg1 = getConst(1);
                val1 = regs[reg1];
                pushOne(val1, siz1);
                return 0;
            case 14: // pop
                siz1 = getConst(1);
                reg1 = getConst(1);
                regs[reg1] = popOne(siz1);
                return 0;
            case 15: // comp
                siz1 = getConst(1);
                siz2 = siz1 >>> 6;
                reg1 = getConst(1);
                if ((siz1 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                val1 = convType(val1, siz2, siz1, 0);
                val2 = convType(regs[reg1], siz2, siz1, 0);
                reg1 = 0;
                if (val1 < val2) {
                    reg1 |= 1;
                }
                if (val1 > val2) {
                    reg1 |= 2;
                }
                if (val1 == val2) {
                    reg1 |= 4;
                }
                flags &= 0xffffff8;
                flags |= reg1;
                return 0;
            case 16: // move
                siz1 = getConst(1);
                siz2 = getConst(1);
                reg1 = getConst(1);
                if ((siz2 & 0x80) != 0) {
                    val1 = getConst(3);
                } else {
                    val1 = regs[getConst(1)];
                }
                val1 = convType(val1, siz2 >>> 6, siz2, 0);
                val1 = convType(val1, siz1 >>> 6, siz1, 1);
                regs[reg1] = val1;
                return 0;
            case 17: // movr
                form = getConst(1);
                siz1 = getConst(1);
                siz2 = getConst(1);
                val2 = getMemory();
                reg1 = getConst(1);
                val1 = readOne(dataD, val2, siz2, form);
                val1 = convType(val1, siz2 >>> 6, siz2, 0);
                val1 = convType(val1, siz1 >>> 6, siz1, 1);
                regs[reg1] = val1;
                return 0;
            case 18: // movw
                form = getConst(1);
                siz1 = getConst(1);
                siz2 = getConst(1);
                val2 = getMemory();
                reg1 = getConst(1);
                val1 = regs[reg1];
                val1 = convType(val1, siz2 >>> 6, siz2, 0);
                val1 = convType(val1, siz1 >>> 6, siz1, 1);
                writeOne(dataD, val2, siz1, form, val1);
                return 0;
            case 19: // call
                val1 = getConst(3);
                pushOne(regs[reg_cip], 3);
                regs[reg_cip] = val1;
                return 0;
            case 20: // ret
                regs[reg_cip] = popOne(3);
                return 0;
            case 21: // jump
                regs[reg_cip] = getConst(3);
                return 0;
            case 22: // jmpc
                reg1 = getConst(1);
                val1 = getConst(3);
                if ((flags & reg1) != 0) {
                    regs[reg_cip] = val1;
                }
                return 0;
            case 23: // addrLod
                val2 = getMemory();
                reg1 = getConst(1);
                regs[reg1] = readOne(dataD, val2, 3, 1);
                return 0;
            case 24: // addrSav
                val2 = getMemory();
                reg1 = getConst(1);
                writeOne(dataD, val2, 3, 1, regs[reg1]);
                return 0;
            case 25: // procAddr
                reg1 = getConst(1);
                val1 = getConst(3);
                if (val1 == -1) {
                    val2 = popOne(3);
                    pushOne(val2, 3);
                } else {
                    val2 = procD[val1];
                }
                regs[reg1] = val2;
                return 0;
            case 26: // procAllocBeg
                val1 = getConst(3);
                val2 = procD[val1];
                pushOne(val2, 3);
                val2 = dataP;
                val1 = getConst(3);
                dataP += val1;
                pushOne(val2, 3);
                return 0;
            case 27: // procFree
                val1 = getConst(3);
                val2 = popOne(3);
                procD[val1] = val2;
                val2 = getConst(3);
                dataP -= val2;
                return 0;
            case 28: // codeOfs
                reg1 = getConst(1);
                regs[reg1] = getConst(3);
                return 0;
            case 29: // xchg
                siz1 = getConst(1);
                val2 = getMemory();
                reg1 = getConst(1);
                val1 = regs[reg1];
                regs[reg1] = readOne(dataD, val2, siz1, 1);
                writeOne(dataD, val2, siz1, 1, val1);
                return 0;
            case 30: // setc
                reg2 = getConst(1);
                siz1 = getConst(1);
                reg1 = getConst(1);
                if ((flags & reg2) != 0) {
                    val1 = 1;
                } else {
                    val1 = 0;
                }
                regs[reg1] = val1;
                return 0;
            case 31: // procAllocEnd
                val2 = popOne(3);
                val1 = getConst(3);
                procD[val1] = val2;
                getConst(3);
                return 0;
            case 32: // syscall
                return doSyscall();
            case 33: // cllr
                reg1 = getConst(1);
                pushOne(regs[reg_cip], 3);
                regs[reg_cip] = regs[reg1];
                return 0;
            case 34: // jmpr
                reg1 = getConst(1);
                regs[reg_cip] = regs[reg1];
                return 0;
        }
        regs[reg_cip] -= 1;
        throw new Exception("unknown (" + opc + ") opcode");
    }

    private int doSyscall() throws Exception {
        int opc = getConst(1);
        int val1;
        switch (opc) {
            case 1: // sleep
                bits.sleep(10);
                return 0;
            case 2: // memCopy
                bits.byteCopy(dataD, regs[reg_src], dataD, regs[reg_trg],
                        regs[reg_c]);
                return 0;
            case 3: // codeCopy
                bits.byteCopy(codeD, regs[reg_src], dataD, regs[reg_trg],
                        regs[reg_c]);
                return 0;
            case 4: // terminate
                return (regs[reg_a] & 0xffff) | res_term;
            case 5: // console.write
                console.strPut(new String(dataD, regs[reg_src], regs[reg_c]));
                return 0;
            case 6: // console.read
                if (console.ready2rx() < 1) {
                    if (console.isClosed() != 0) {
                        throw new Exception("console closed");
                    }
                    regs[reg_c] = 0;
                    return 0;
                }
                bits.lsbPutW(dataD, regs[reg_trg], getKey());
                regs[reg_c] = 2;
                return 0;
            case 7: // file.maxName
                regs[reg_a] = 128;
                return 0;
            case 8: // file.myName
                String a = fromUnix(nam);
                regs[reg_c] = a.length();
                putAsciiz(regs[reg_trg], a);
                return 0;
            case 9: // file.myParam
                regs[reg_c] = par.length();
                putAsciiz(regs[reg_trg], par);
                return 0;
            case 10: // file.open
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                if (!new File(a).exists()) {
                    return 0;
                }
                userVMfile fil = new userVMfile(0);
                try {
                    fil.fil = new RandomAccessFile(a, "rw");
                } catch (Exception e) {
                    return 0;
                }
                for (;;) {
                    fil.num = bits.randomD();
                    if (files.add(fil) == null) {
                        break;
                    }
                }
                regs[reg_a] = fil.num;
                regs[reg_b] = 0;
                return 0;
            case 11: // file.read
                regs[reg_b] = 1;
                fil = files.find(new userVMfile(regs[reg_a]));
                if (fil == null) {
                    return 0;
                }
                try {
                    fil.fil.read(dataD, regs[reg_trg], regs[reg_c]);
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 12: // file.write
                regs[reg_b] = 1;
                fil = files.find(new userVMfile(regs[reg_a]));
                if (fil == null) {
                    return 0;
                }
                try {
                    fil.fil.write(dataD, regs[reg_src], regs[reg_c]);
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 13: // file.seek
                regs[reg_b] = 1;
                fil = files.find(new userVMfile(regs[reg_a]));
                if (fil == null) {
                    return 0;
                }
                try {
                    fil.fil.seek(regs[reg_c]);
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 14: // file.getSize
                regs[reg_b] = 1;
                fil = files.find(new userVMfile(regs[reg_a]));
                if (fil == null) {
                    return 0;
                }
                try {
                    regs[reg_c] = (int) fil.fil.length();
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 15: // file.getPos
                regs[reg_b] = 1;
                fil = files.find(new userVMfile(regs[reg_a]));
                if (fil == null) {
                    return 0;
                }
                try {
                    regs[reg_c] = (int) fil.fil.getFilePointer();
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 16: // file.truncate
                regs[reg_b] = 1;
                fil = files.find(new userVMfile(regs[reg_a]));
                if (fil == null) {
                    return 0;
                }
                try {
                    fil.fil.setLength(fil.fil.getFilePointer());
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 17: // file.close
                regs[reg_b] = 1;
                fil = files.del(new userVMfile(regs[reg_a]));
                if (fil == null) {
                    return 0;
                }
                try {
                    fil.fil.close();
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 18: // file.create
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                try {
                    new File(a).createNewFile();
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 19: // file.erase
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                try {
                    if (!new File(a).delete()) {
                        return 0;
                    }
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 20: // dir.current
                putPascii(regs[reg_trg], fromUnix(currDir));
                regs[reg_b] = 0;
                return 0;
            case 21: // dir.change
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = fromDos(getPascii(regs[reg_src]));
                a = "/" + uniResLoc.normalizePath(a + "/");
                if (!new File(rootDir + a).isDirectory()) {
                    return 0;
                }
                currDir = a;
                regs[reg_b] = 0;
                return 0;
            case 22: // dir.setRights
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                if (!new File(a).exists()) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 23: // dir.rename
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                try {
                    new File(a).renameTo(new File(rootDir
                            + fromDos(getPascii(regs[reg_trg]))));
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 24: // dir.makeLink
                regs[reg_b] = 1;
                return 0;
            case 25: // dir.open
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                if (!new File(a).exists()) {
                    return 0;
                }
                userVMdir dir = new userVMdir(0);
                try {
                    dir.lst = new File(a).listFiles();
                } catch (Exception e) {
                    return 0;
                }
                if (dir.lst == null) {
                    return 0;
                }
                for (;;) {
                    dir.num = bits.randomD();
                    if (dirs.add(dir) == null) {
                        break;
                    }
                }
                regs[reg_a] = dir.num;
                regs[reg_b] = 0;
                return 0;
            case 26: // dir.read
                regs[reg_b] = 1;
                dir = dirs.find(new userVMdir(regs[reg_a]));
                if (dir == null) {
                    return 0;
                }
                bits.byteFill(dataD, regs[reg_trg], 64, 0);
                if (dir.pos < dir.lst.length) {
                    File ntry = dir.lst[dir.pos];
                    dir.pos++;
                    int ofs = regs[reg_trg];
                    int rgt = 0;
                    long tim = ntry.lastModified();
                    if (ntry.isDirectory()) {
                        rgt |= 0x83;
                    }
                    if (ntry.canRead()) {
                        rgt |= 0x01;
                    }
                    if (ntry.canWrite()) {
                        rgt |= 0x02;
                    }
                    if (ntry.canExecute()) {
                        rgt |= 0x04;
                    }
                    bits.lsbPutD(dataD, ofs + 0, (int) ntry.length());
                    bits.lsbPutD(dataD, ofs + 4, rgt);
                    bits.lsbPutD(dataD, ofs + 8, 0);
                    bits.lsbPutW(dataD, ofs + 12,
                            bits.time2num(cfgAll.timeZoneName, tim, 1));
                    dataD[ofs + 14] = (byte) bits.time2num(cfgAll.timeZoneName,
                            tim, 2);
                    dataD[ofs + 15] = (byte) bits.time2num(cfgAll.timeZoneName,
                            tim, 3);
                    dataD[ofs + 16] = (byte) bits.time2num(cfgAll.timeZoneName,
                            tim, 4);
                    dataD[ofs + 17] = (byte) bits.time2num(cfgAll.timeZoneName,
                            tim, 5);
                    dataD[ofs + 18] = (byte) bits.time2num(cfgAll.timeZoneName,
                            tim, 6);
                    bits.byteCopy(dataD, ofs + 12, dataD, ofs + 19, 7);
                    putPascii(ofs + 26, ntry.getName());
                }
                regs[reg_b] = 0;
                return 0;
            case 27: // dir.close
                regs[reg_b] = 1;
                dir = dirs.del(new userVMdir(regs[reg_a]));
                if (dir == null) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 28: // dir.create
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                try {
                    if (!new File(a).mkdir()) {
                        return 0;
                    }
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 29: // dir.erase
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                try {
                    if (!new File(a).delete()) {
                        return 0;
                    }
                } catch (Exception e) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 30: // dir.stats
                regs[reg_a] = 0x20000000;
                regs[reg_b] = 0x10000000;
                regs[reg_c] = 0;
                regs[reg_d] = 512;
                return 0;
            case 31: // dir.setDate
                regs[reg_b] = 1;
                if (!allowFileIO) {
                    return 0;
                }
                a = rootDir + fromDos(getPascii(regs[reg_src]));
                if (!new File(a).exists()) {
                    return 0;
                }
                regs[reg_b] = 0;
                return 0;
            case 32: // memReSize
                byte[] buf = dataD;
                dataD = new byte[dataS + regs[reg_c] + memBound];
                if (buf.length < dataD.length) {
                    val1 = buf.length;
                } else {
                    val1 = dataD.length;
                }
                bits.byteCopy(buf, 0, dataD, 0, val1);
                regs[reg_c] = dataD.length - dataS - memBound;
                regs[reg_trg] = dataS;
                return 0;
            case 33: // memInfo
                regs[reg_c] = dataD.length - dataS - memBound;
                regs[reg_trg] = dataS;
                return 0;
            case 34: // console.isKey
                regs[reg_a] = 0;
                if (console.ready2rx() > 0) {
                    regs[reg_a] = 1;
                }
                return 0;
            case 35: // console.size
                regs[reg_a] = 78;
                regs[reg_b] = 24;
                return 0;
            case 36: // console.gotoXY
                userScreen.sendCur(console, regs[reg_a] - 1, regs[reg_b] - 1);
                return 0;
            case 37: // console.setColor
                userScreen.sendCol(console, convCol(regs[reg_a]));
                return 0;
            case 38: // console.clear
                userScreen.sendCls(console);
                return 0;
            case 39: // console.execWait
                a = fromDos(getPascii(regs[reg_src]));
                val1 = userVM.doWork(console, allowFileIO, currDir, a,
                        getPascii(regs[reg_trg]));
                regs[reg_b] = result2error(val1);
                regs[reg_a] = result2extcod(val1);
                return 0;
            case 40: // console.execBckgnd
                regs[reg_b] = 1;
                return 0;
            case 41: // console.execInMe
                regs[reg_b] = 1;
                return 0;
            case 42: // console.getDate
                regs[reg_a] = bits.time2num(cfgAll.timeZoneName, bits.getTime(), 1);
                regs[reg_b] = bits.time2num(cfgAll.timeZoneName, bits.getTime(), 2);
                regs[reg_c] = bits.time2num(cfgAll.timeZoneName, bits.getTime(), 3);
                return 0;
            case 43: // console.getTime
                regs[reg_a] = bits.time2num(cfgAll.timeZoneName, bits.getTime(), 4);
                regs[reg_b] = bits.time2num(cfgAll.timeZoneName, bits.getTime(), 5);
                regs[reg_c] = bits.time2num(cfgAll.timeZoneName, bits.getTime(), 6);
                return 0;
            case 44: // memFillByte
                bits.byteFill(dataD, regs[reg_trg], regs[reg_c], regs[reg_a]);
                return 0;
            case 100: // pipeline.startListen
            case 101: // pipeline.stopListen
            case 102: // pipeline.getIncoming
            case 103: // pipeline.create
            case 104: // pipeline.close
            case 105: // pipeline.info
            case 106: // pipeline.receive
            case 107: // pipeline.send
                regs[reg_b] = 1;
                return 0;
            case 108: // system.getPID
                regs[reg_a] = 1234;
                regs[reg_b] = 4321;
                regs[reg_c] = 3;
                return 0;
            case 109: // system.getUID
                regs[reg_a] = 0;
                regs[reg_b] = 0;
                return 0;
            case 110: // system.sysInfoNum
                regs[reg_a] = 1;
                regs[reg_c] = 0;
                regs[reg_d] = dirs.size() + files.size();
                return 0;
            case 111: // system.sysInfoMem
                regs[reg_a] = 0x20000000;
                regs[reg_c] = 0x10000;
                regs[reg_d] = 0x10000000;
                return 0;
            case 112: // system.sysInfoProc
                regs[reg_a] = 64;
                regs[reg_b] = 1;
                regs[reg_c] = 1;
                regs[reg_d] = 1;
                return 0;
            case 113: // system.procInfoNam
                putPascii(regs[reg_trg] + 0, "");
                putPascii(regs[reg_trg] + 256, "");
                regs[reg_a] = 0;
                regs[reg_c] = 0;
                regs[reg_d] = 0;
                return 0;
            case 114: // system.procInfoNum
                regs[reg_a] = 0;
                regs[reg_c] = 0;
                regs[reg_d] = 0;
                return 0;
            case 115: // system.procInfoRun
                regs[reg_a] = 0;
                regs[reg_b] = 0;
                regs[reg_c] = 0;
                return 0;
            case 116: // system.findProcNum
                regs[reg_a] = 0;
                return 0;
            case 117: // system.findProcNam
                regs[reg_a] = 0;
                return 0;
            case 118: // system.cpuInfo
                regs[reg_a] = 1;
                regs[reg_c] = 8086;
                putAsciiz(regs[reg_trg], "emulator");
                return 0;
            case 119: // system.kernelInfo
                putAsciiz(regs[reg_trg], bits.lst2str(version.shPlat(), "\r\n"));
                return 0;
            case 120: // system.kernelLogo
                putAsciiz(regs[reg_trg], bits.lst2str(version.shLogo(0x08), "\r\n"));
                return 0;
            case 121: // system.procLive
                regs[reg_b] = 0;
                return 0;
            case 122: // system.uptimeInfo
                regs[reg_a] = 0;
                regs[reg_c] = (int) ((bits.getTime() / 10) % 1000000);
                regs[reg_d] = 100;
                return 0;
            case 123: // system.killProc
                regs[reg_b] = 1;
                return 0;
        }
        regs[reg_cip] -= 2;
        throw new Exception("unknown (" + opc + ") syscall");
    }

}

class userVMfile implements Comparator<userVMfile> {

    public int num;

    public RandomAccessFile fil;

    public userVMfile(int i) {
        num = i;
    }

    public int compare(userVMfile o1, userVMfile o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

}

class userVMdir implements Comparator<userVMdir> {

    public int num;

    public File[] lst;

    public int pos;

    public userVMdir(int i) {
        num = i;
    }

    public int compare(userVMdir o1, userVMdir o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

}
