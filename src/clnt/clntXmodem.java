package clnt;

import java.io.File;
import java.io.RandomAccessFile;
import pipe.pipeSide;
import util.bits;

/**
 * xmodem client
 *
 * @author matecsaba
 */
public class clntXmodem {

    private pipeSide pipe;

    private RandomAccessFile fr;

    /**
     * create new client
     *
     * @param console console to use
     */
    public clntXmodem(pipeSide console) {
        pipe = console;
    }

    /**
     * clean up state
     */
    public void cleanUp() {
        try {
            fr.close();
        } catch (Exception e) {
        }
    }

    /**
     * download one file
     *
     * @param trg target
     * @return result code
     */
    public boolean download(File trg) {
        try {
            trg.createNewFile();
        } catch (Exception e) {
            return true;
        }
        try {
            fr = new RandomAccessFile(trg, "rw");
        } catch (Exception e) {
            return true;
        }
        pipe.linePut("start xmodem upload!");
        sendByte(21);
        int blk = 1;
        for (;;) {
            byte[] buf;
            for (int re = 0;; re++) {
                if (re > 8) {
                    return true;
                }
                byte[] hdr = recvBytes(1);
                if (hdr == null) {
                    sendByte(21);
                    continue;
                }
                switch (hdr[0]) {
                    case 1: // soh
                        break;
                    case 3: // brk
                        return true;
                    case 4: // eot
                        return false;
                    default:
                        recvFlush();
                        continue;
                }
                hdr = recvBytes(2);
                if (hdr == null) {
                    continue;
                }
                if (((~hdr[1]) & 0xff) != (hdr[0] & 0xff)) {
                    continue;
                }
                buf = recvBytes(128);
                if (buf == null) {
                    continue;
                }
                byte[] sum = recvBytes(1);
                if (sum == null) {
                    continue;
                }
                int o = 0;
                for (int i = 0; i < buf.length; i++) {
                    o += buf[i];
                }
                if ((o & 0xff) != (sum[0] & 0xff)) {
                    continue;
                }
                if ((hdr[0] & 0xff) != blk) {
                    sendByte(6);
                    continue;
                }
                break;
            }
            sendByte(6);
            blk = (blk + 1) & 0xff;
            try {
                fr.write(buf);
            } catch (Exception e) {
                return true;
            }
        }
    }

    /**
     * upload one file
     *
     * @param src target
     * @return result code
     */
    public boolean upload(File src) {
        long siz = 0;
        try {
            fr = new RandomAccessFile(src, "r");
            siz = fr.length();
        } catch (Exception e) {
            return true;
        }
        pipe.linePut("start xmodem download!");
        long blk = 0;
        for (;;) {
            byte[] buf;
            for (int re = 0;; re++) {
                if (re > 8) {
                    return true;
                }
                buf = recvBytes(1);
                if (buf == null) {
                    continue;
                }
                switch (buf[0]) {
                    case 21: // nak
                        break;
                    case 06: // ack
                        blk++;
                        break;
                    case 03: // brk
                        return true;
                    default:
                        recvFlush();
                        continue;
                }
                break;
            }
            buf = new byte[128];
            long pos = blk * buf.length;
            int len = (int) (siz - pos);
            if (len > buf.length) {
                len = buf.length;
            }
            if (len < 1) {
                break;
            }
            try {
                fr.seek(pos);
                fr.read(buf, 0, len);
            } catch (Exception e) {
                return true;
            }
            bits.byteFill(buf, len, buf.length - len, 26);
            byte[] hdr = new byte[3];
            hdr[0] = 1;
            hdr[1] = (byte) ((blk + 1) & 0xff);
            hdr[2] = (byte) ((~hdr[1]) & 0xff);
            int o = 0;
            for (int i = 0; i < buf.length; i++) {
                o += buf[i] & 0xff;
            }
            hdr = bits.byteConcat(hdr, buf);
            buf = new byte[1];
            buf[0] = (byte) (o & 0xff);
            hdr = bits.byteConcat(hdr, buf);
            pipe.blockingPut(hdr, 0, hdr.length);
        }
        byte[] buf = new byte[1];
        buf[0] = 4;
        pipe.blockingPut(buf, 0, buf.length);
        return false;
    }

    private void recvFlush() {
        for (;;) {
            int i = pipe.ready2rx();
            if (i < 1) {
                return;
            }
            byte[] buf = new byte[i];
            pipe.moreGet(buf, 0, i);
            bits.sleep(100);
        }
    }

    private void sendByte(int i) {
        byte[] buf = new byte[1];
        buf[0] = (byte) i;
        pipe.blockingPut(buf, 0, buf.length);
    }

    private byte[] recvBytes(int len) {
        byte[] buf = new byte[len];
        long beg = bits.getTime();
        len = 0;
        for (;;) {
            if ((bits.getTime() - beg) > 5000) {
                return null;
            }
            int o = buf.length - len;
            if (o < 1) {
                break;
            }
            int i = pipe.ready2rx();
            if (i < 1) {
                pipe.notif.sleep(1000);
                continue;
            }
            if (i > o) {
                i = o;
            }
            i = pipe.blockingGet(buf, len, i);
            len += i;
        }
        return buf;
    }

}
