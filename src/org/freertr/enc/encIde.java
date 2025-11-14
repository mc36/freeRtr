package org.freertr.enc;

import java.io.File;
import java.io.RandomAccessFile;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * ide server
 *
 * @author matecsaba
 */
public class encIde implements Comparable<encIde> {

    /**
     * name of this file
     */
    public final String name;

    private RandomAccessFile hndl;

    /**
     * block size
     */
    public long blkSiz;

    private long filSiz;

    /**
     * create new handler
     */
    public encIde(String n) {
        name = "" + n;
        blkSiz = 512;
    }

    public int compareTo(encIde o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String toString() {
        return name + ", file=" + name + ", size=" + filSiz + " block=" + blkSiz;
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
            File fh = new File(name);
            if (!fh.isFile()) {
                return;
            }
            hndl = new RandomAccessFile(fh, "rw");
            filSiz = hndl.length() / blkSiz;
            hndl.setLength(filSiz * blkSiz);
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
     * do cone ide command
     *
     * @param cmd command to do
     * @param dat data to process
     * @return processed data
     */
    public byte[] doIde(byte[] cmd, byte[] dat) {
        if (hndl == null) {
            cmd[0] = 0; // normal
            cmd[1] = (byte) 0x80; // crc
            cmd[2] = 0; // sectors
            cmd[3] = (byte) 0x81; // busy error
            return null;
        }
        byte[] res = null;
        switch (cmd[3] & 0xff) {
            case 0x24: // read48
                long sec = bits.lsbGetQ(cmd, 4) & 0xffffffffffffffL;
                int len = cmd[2] & 0xff;
                if (debugger.prtAtaTraf) {
                    logger.debug("read48 sec=" + sec + " len=" + len);
                }
                res = doRead(sec, len);
                cmd[1] = 0; // ok
                cmd[3] = 0x40; // ready
                break;
            case 0x34: // write48
                sec = bits.lsbGetQ(cmd, 4) & 0xffffffffffffffL;
                len = cmd[2] & 0xff;
                if (debugger.prtAtaTraf) {
                    logger.debug("write48 sec=" + sec + " len=" + len);
                }
                doWrite(sec, dat);
                res = new byte[0];
                cmd[1] = 0; // ok
                cmd[3] = 0x40; // ready
                break;
            case 0x20: // read28
                sec = bits.lsbGetD(cmd, 4) & 0xfffffff;
                len = cmd[2] & 0xff;
                if (debugger.prtAtaTraf) {
                    logger.debug("read28 sec=" + sec + " len=" + len);
                }
                res = doRead(sec, len);
                cmd[1] = 0; // ok
                cmd[3] = 0x40; // ready
                break;
            case 0x30: // write28
                sec = bits.lsbGetD(cmd, 4) & 0xfffffff;
                len = cmd[2] & 0xff;
                if (debugger.prtAtaTraf) {
                    logger.debug("write28 sec=" + sec + " len=" + len);
                }
                doWrite(sec, dat);
                res = new byte[0];
                cmd[1] = 0; // ok
                cmd[3] = 0x40; // ready
                break;
            case 0xe7: // flush cache
                res = new byte[0];
                cmd[1] = 0; // ok
                cmd[3] = 0x40; // ready
                break;
            case 0xe5: // check power
                res = new byte[0];
                cmd[1] = 0; // ok
                cmd[2] = (byte) 0xff; // active
                cmd[3] = 0x40; // ready
                break;
            case 0xec: // identify drive
                if (debugger.prtAtaTraf) {
                    logger.debug("inquiry");
                }
                cmd[1] = 0; // ok
                cmd[3] = 0x40; // ready
                bits.lsbPutQ(cmd, 4, 0); // lba
                res = new byte[512];
                bits.lsbPutW(res, 0, 0); // classification
                bits.lsbPutW(res, 2, 0); // cylinders
                bits.lsbPutW(res, 4, 0xc837); // complete
                bits.lsbPutW(res, 6, 0); // heads / cylinder
                bits.lsbPutW(res, 12, 0); // sectors / track
                bits.lsbPutD(res, 20, 0x61626364); // serial number
                bits.byteFill(res, 24, 16, 32); // spaces
                bits.lsbPutW(res, 40, 3); // controller type
                bits.lsbPutW(res, 42, 16); // buffer size
                bits.lsbPutD(res, 46, 0x312e32); // microcode version
                bits.lsbPutD(res, 50, 0x20202020); // spaces
                bits.lsbPutD(res, 54, 0x65666768); // model number
                bits.byteFill(res, 58, 36, 32); // spaces
                bits.lsbPutW(res, 94, 0x8000); // multiple sectors
                bits.lsbPutW(res, 98, 0x200); // lba capabilities
                bits.lsbPutW(res, 100, 0x4000); // valid capabilities
                int i = (int) filSiz;
                if (i > 0xfffffff) {
                    i = 0xfffffff;
                }
                bits.lsbPutD(res, 120, i); // user addressable
                bits.lsbPutW(res, 166, 0x5400); // command set
                bits.lsbPutW(res, 168, 0x4000); // feature set
                bits.lsbPutW(res, 172, 0x1400); // feature enabled
                bits.lsbPutW(res, 174, 0x4000); // feature enabled
                bits.lsbPutQ(res, 200, (int) filSiz); // user addressable
                break;
            default:
                if (debugger.prtAtaTraf) {
                    logger.debug("unknown=" + (cmd[3] & 0xff));
                }
                break;
        }
        if (res != null) {
            return res;
        }
        cmd[0] = 0; // normal
        cmd[1] = 4; // abort
        cmd[2] = 0; // sectors
        cmd[3] = 1; // error
        return null;
    }

}
