
import net.freertr.cry.cryHashCrc16;
import net.freertr.ifc.ifcPolka;
import net.freertr.util.bits;

public class zzz {

    private static void log(String s) {
        System.out.println(s);
    }

    private static void dumpO(int[] o) {
        for (int i = 0; i < o.length; i++) {
            log("o" + i + "=0x" + bits.toHexW(o[i]));
        }
    }

    public static void main(String[] args) {
        ifcPolka plk = new ifcPolka(3, 32768, 10);
        for (int i = 0; i < plk.crcMax; i++) {
            log("s" + i + "=" + plk.coeffs[i]);
        }
        int[] out = new int[]{1, 3, 5, 7, 8, 9};
        byte[] rou = ifcPolka.encodeRouteId(plk.coeffs, out);
        dumpO(ifcPolka.decodeRouteId(plk.coeffs, rou));
        log("r=" + bits.byteDump(rou, 0, -1));
        cryHashCrc16 crc = new cryHashCrc16(plk.hasher);
        crc.init();
        crc.update(rou, 0, 14);
        int dif = bits.msbGetW(rou, 14);
        int res = crc.getCrc();
        log("crc=" + bits.toHexW(res) + " dif=" + bits.toHexW(dif) + " xor=" + bits.toHexW(dif ^ res));
    }

}
