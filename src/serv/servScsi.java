package serv;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.Comparator;
import java.util.List;

import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * scsi server
 *
 * @author matecsaba
 */
public class servScsi implements Comparator<servScsi> {

    /**
     * name of this file
     */
    public final String name;

    private String file;

    private RandomAccessFile hndl;

    private long blkSiz;

    private long filSiz;

    /**
     * create new handler
     *
     * @param n name of handler
     */
    public servScsi(String n) {
        name = "" + n;
        blkSiz = 512;
    }

    public int compare(servScsi o1, servScsi o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return name + ", file=" + file + ", size=" + filSiz + " block=" + blkSiz;
    }

    /**
     * get configuration
     *
     * @param l list to adppend
     * @param beg beginning
     */
    public void getCfg(List<String> l, String beg) {
        l.add(beg + "file " + file);
        l.add(beg + "block " + blkSiz);
    }

    /**
     * do configuration
     *
     * @param cmd command to do
     * @return false on success, true on error
     */
    public boolean doCfg(cmds cmd) {
        String s = cmd.word();
        if (s.equals("file")) {
            file = cmd.getRemaining();
            doUpdate(false);
            return false;
        }
        if (s.equals("block")) {
            blkSiz = bits.str2num(cmd.word());
            doUpdate(false);
            return false;
        }
        return true;
    }

    /**
     * update this target
     *
     * @param close true to close
     */
    public void doUpdate(boolean close) {
        try {
            hndl.close();
        } catch (Exception e) {
        }
        hndl = null;
        filSiz = 0;
        if (close) {
            return;
        }
        try {
            File fh = new File(file);
            if (!fh.isFile()) {
                return;
            }
            hndl = new RandomAccessFile(fh, "rw");
            filSiz = hndl.length() / blkSiz;
        } catch (Exception e) {
            hndl = null;
            return;
        }
    }

    private byte[] doRead(long ps, int ln) {
        synchronized (hndl) {
            byte[] buf;
            try {
                buf = new byte[ln * (int) blkSiz];
                hndl.seek(blkSiz * ps);
                if (hndl.read(buf) != buf.length) {
                    return null;
                }
                return buf;
            } catch (Exception e) {
                return null;
            }
        }
    }

    private void doWrite(long ps, byte[] buf) {
        synchronized (hndl) {
            try {
                hndl.seek(blkSiz * ps);
                hndl.write(buf);
            } catch (Exception e) {
                return;
            }
        }
    }

    /**
     * do cone scsi command
     *
     * @param cmd command to do
     * @param dat data to process
     * @return processed data
     */
    public byte[] doScsi(byte[] cmd, byte[] dat) {
        if (hndl == null) {
            return null;
        }
        byte[] res = null;
        switch (cmd[0] & 0xff) {
            case 0x28: // read10
                int sec = bits.msbGetD(cmd, 2);
                int len = bits.msbGetW(cmd, 7);
                if (debugger.servScsiTraf) {
                    logger.debug("read10 sec=" + sec + " len=" + len);
                }
                res = doRead(sec, len);
                break;
            case 0x2a: // write10
                sec = bits.msbGetD(cmd, 2);
                len = bits.msbGetW(cmd, 7);
                if (debugger.servScsiTraf) {
                    logger.debug("write10 sec=" + sec + " len=" + len);
                }
                doWrite(sec, dat);
                res = new byte[0];
                break;
            case 0x08: // read6
                sec = bits.msbGetD(cmd, 0) & 0x1fffff;
                len = bits.getByte(cmd, 4);
                if (debugger.servScsiTraf) {
                    logger.debug("read6 sec=" + sec + " len=" + len);
                }
                res = doRead(sec, len);
                break;
            case 0x0a: // write6
                sec = bits.msbGetD(cmd, 0) & 0x1fffff;
                len = bits.getByte(cmd, 4);
                if (debugger.servScsiTraf) {
                    logger.debug("write6 sec=" + sec + " len=" + len);
                }
                doWrite(sec, dat);
                res = new byte[0];
                break;
            case 0x1a: // mode sense6
                if (debugger.servScsiTraf) {
                    logger.debug("mode sense6");
                }
                res = new byte[cmd[4] & 0xff];
                res[0] = 0x19; // size of data
                res[1] = 0; // medium type
                res[2] = 0; // device specific
                res[3] = 0; // block descriptor length
                res[4] = 0; // page code
                res[5] = 0; // page length
                break;
            case 0x00: // test unit ready
                if (debugger.servScsiTraf) {
                    logger.debug("test unit ready");
                }
                res = new byte[0];
                break;
            case 0x25: // read capacity10
                if (debugger.servScsiTraf) {
                    logger.debug("read capacity10");
                }
                res = new byte[8];
                bits.msbPutD(res, 0, (int) filSiz);
                bits.msbPutD(res, 4, (int) blkSiz);
                break;
            case 0xa0: // report luns
                if (debugger.servScsiTraf) {
                    logger.debug("report luns");
                }
                res = new byte[16];
                bits.msbPutD(res, 0, 8); // lun list size
                bits.msbPutD(res, 4, 0); // reserved
                bits.msbPutQ(res, 8, 0); // lun id
                break;
            case 0x12: // inquiry
                if (debugger.servScsiTraf) {
                    logger.debug("inquiry");
                }
                res = new byte[cmd[4] & 0xff];
                res[0] = 0; // magnetic disc
                res[1] = 0; // not removable
                res[2] = 2; // version 2 command set
                res[3] = 0; // no queue, no taskmgmt
                res[4] = (byte) (res.length - 4); // additional data length
                res[5] = 0; // no scc
                res[6] = 0; // no qqueue
                res[7] = 0x62; // no cmdqueue
                bits.byteFill(res, 8, 28, 32);
                res[8] = 0x61; // vendor
                res[16] = 0x62; // product
                res[32] = 0x63; // revision
                break;
            default:
                if (debugger.servScsiTraf) {
                    logger.debug("unknown=" + (cmd[0] & 0xff));
                }
                break;
        }
        return res;
    }

}
