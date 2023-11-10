use crate::common::{Stats, BUF_SIZE};
use crate::config::Config;
use std::error::Error;
use std::io::{ErrorKind, Write};
use std::net::UdpSocket;
use std::sync::{atomic::Ordering, Arc};

#[cfg(any(feature = "crypto", test))]
use orion::aead;

pub struct Sock {
    sock: UdpSocket,
    #[cfg(any(feature = "crypto", test))]
    key: Option<aead::SecretKey>,
}

impl Sock {
    #[cfg(not(any(feature = "crypto", test)))]
    pub fn new(cfg: &Config) -> Result<Self, Box<dyn Error>> {
        let sock = UdpSocket::bind(cfg.laddr)?;
        sock.connect(cfg.raddr)?;
        Ok(Self { sock })
    }

    #[cfg(any(feature = "crypto", test))]
    pub fn new(cfg: &Config) -> Result<Self, Box<dyn Error>> {
        let sock = UdpSocket::bind(cfg.laddr)?;
        sock.connect(cfg.raddr)?;
        let key = if let Some(key) = &cfg.key {
            Some(aead::SecretKey::from_slice(key.as_bytes())?)
        } else {
            None
        };
        Ok(Self { sock, key })
    }

    pub fn get_writer(&mut self) -> Result<UdpSocket, Box<dyn Error>> {
        let sock = self.sock.try_clone()?;
        Ok(sock)
    }

    pub fn sock_loop(&mut self, mut if_tx: std::fs::File, stats: Arc<Stats>) {
        loop {
            let mut buf = [0; BUF_SIZE];
            let read = self.sock.recv(&mut buf);
            if let Ok(read) = read {
                if read == 0 {
                    continue;
                }

                stats.rx_bytes.fetch_add(read, Ordering::Relaxed);
                stats.rx_packets.fetch_add(1, Ordering::Relaxed);

                #[cfg(any(feature = "crypto", test))]
                let (read, buf) = if let Some(key) = &self.key {
                    let buf = aead::open(key, &buf[0..read]);
                    if let Err(e) = buf {
                        eprintln!("Error decrypting incomming message: {e}");
                        continue;
                    }
                    let buf = buf.unwrap();
                    (buf.len(), buf)
                } else {
                    (read, buf.to_vec())
                };

                let send = if_tx.write(&buf[0..read]);
                if let Err(e) = send {
                    eprintln!("Error writing to thread connection: {e}");
                }
            } else {
                let e = read.unwrap_err();
                match e.kind() {
                    // The other side is not up yet
                    ErrorKind::ConnectionRefused => {}
                    _ => eprintln!("Error receiving from UDP socket: {e}"),
                }
            }
        }
    }
}
