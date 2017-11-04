package sec;

import auth.authGeneric;
import cry.cryCertificate;
import cry.cryKeyDSA;
import cry.cryKeyECDSA;
import cry.cryKeyRSA;
import pipe.pipeLine;
import pipe.pipeSide;
import serv.servGeneric;

/**
 * negotiate security if needed
 *
 * @author matecsaba
 */
public class secServer {

    /**
     * start secure connection
     *
     * @param pipe pipeline to use
     * @param proto protocol to use
     * @param sample pipe sample
     * @param auther auther to use
     * @param keyrsa rsa key to use
     * @param keydsa dsa key to use
     * @param keyecdsa ecdsa key to use
     * @param certrsa rsa certificate to use
     * @param certdsa dsa certificate to use
     * @param certecdsa ecdsa certificate to use
     * @return secure pipeline, null on error
     */
    public static pipeSide openSec(pipeSide pipe, int proto, pipeLine sample, authGeneric auther, cryKeyRSA keyrsa, cryKeyDSA keydsa, cryKeyECDSA keyecdsa, cryCertificate certrsa, cryCertificate certdsa, cryCertificate certecdsa) {
        if (pipe == null) {
            return null;
        }
        proto &= servGeneric.protoSec;
        if (proto == 0) {
            return pipe;
        }
        switch (proto & servGeneric.protoSec) {
            case 0:
                return pipe;
            case servGeneric.protoSsh:
                secSsh ssh = new secSsh(pipe, pipeLine.doClone(sample, pipe.isBlockMode()));
                ssh.startServer(auther, keyrsa, keydsa, keyecdsa);
                return ssh.getPipe();
            case servGeneric.protoTls:
                secTls tls = new secTls(pipe, pipeLine.doClone(sample, pipe.isBlockMode()), false);
                tls.startServer(keyrsa, keydsa, keyecdsa, certrsa, certdsa, certecdsa);
                return tls.getPipe();
            case servGeneric.protoDtls:
                tls = new secTls(pipe, pipeLine.doClone(sample, pipe.isBlockMode()), true);
                tls.startServer(keyrsa, keydsa, keyecdsa, certrsa, certdsa, certecdsa);
                return tls.getPipe();
            case servGeneric.protoTelnet:
                secTelnet telnet = new secTelnet(pipe, pipeLine.doClone(sample, pipe.isBlockMode()));
                telnet.startServer();
                return telnet.getPipe();
            default:
                return null;
        }

    }

}
