package org.freertr.sec;

import org.freertr.auth.authGeneric;
import org.freertr.cfg.cfgAll;
import org.freertr.cry.cryCertificate;
import org.freertr.cry.cryKeyDSA;
import org.freertr.cry.cryKeyECDSA;
import org.freertr.cry.cryKeyMLDSA;
import org.freertr.cry.cryKeyRSA;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;

/**
 * negotiate security if needed
 *
 * @author matecsaba
 */
public class secServer {

    private secServer() {
    }

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
     * @param keymldsa mldsa key to use
     * @param certrsa rsa certificate to use
     * @param certdsa dsa certificate to use
     * @param certecdsa ecdsa certificate to use
     * @param certmldsa mldsa certificate to use
     * @return secure pipeline, null on error
     */
    public static pipeSide openSec(pipeSide pipe, int proto, pipeLine sample, authGeneric auther, cryKeyRSA keyrsa, cryKeyDSA keydsa, cryKeyECDSA keyecdsa, cryKeyMLDSA keymldsa, cryCertificate certrsa, cryCertificate certdsa, cryCertificate certecdsa, cryCertificate certmldsa) {
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
                ssh.startServer(auther, keyrsa, keydsa, keyecdsa, keymldsa);
                return ssh.getPipe();
            case servGeneric.protoTls:
            case servGeneric.protoDtls:
                boolean dtls = proto == servGeneric.protoDtls;
                secTls tls = new secTls(pipe, pipeLine.doClone(sample, pipe.isBlockMode()), dtls);
                tls.minVer = 0x300 + cfgAll.tlsVerMin;
                tls.maxVer = 0x300 + cfgAll.tlsVerMax;
                tls.startServer(keyrsa, keydsa, keyecdsa, keymldsa, certrsa, certdsa, certecdsa, certmldsa);
                return tls.getPipe();
            case servGeneric.protoRlogin:
                secRlogin rlogin = new secRlogin(pipe, pipeLine.doClone(sample, pipe.isBlockMode()));
                rlogin.startServer(auther);
                return rlogin.getPipe();
            case servGeneric.protoTelnet:
                secTelnet telnet = new secTelnet(pipe, pipeLine.doClone(sample, pipe.isBlockMode()));
                telnet.startServer();
                return telnet.getPipe();
            default:
                return null;
        }

    }

}
