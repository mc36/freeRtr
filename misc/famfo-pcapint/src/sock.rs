use crate::common::{Stats, BUF_SIZE};
use crate::config::Config;
use pcap::{Active, Capture};
use std::error::Error;
use std::io::ErrorKind;
use std::net::UdpSocket;
use std::sync::Mutex;
use std::sync::{atomic::Ordering, Arc};

pub struct Sock {
    sock: UdpSocket,
}

impl Sock {
    pub fn new(cfg: &Config) -> Result<Self, Box<dyn Error>> {
        let sock = UdpSocket::bind(cfg.laddr)?;
        sock.connect(cfg.raddr)?;
        Ok(Self { sock })
    }

    pub fn get_writer(&mut self) -> Result<UdpSocket, Box<dyn Error>> {
        let sock = self.sock.try_clone()?;
        Ok(sock)
    }

    pub fn sock_loop(&mut self, if_tx: Arc<Mutex<Capture<Active>>>, stats: Arc<Stats>) {
        let if_tx = if_tx.lock();
        if let Err(e) = if_tx {
            eprintln!("Failed to acquire capture lock: {e}");
            return;
        }
        let mut if_tx = if_tx.unwrap();
        loop {
            let mut buf = [0; BUF_SIZE];
            let read = self.sock.recv(&mut buf);
            if let Ok(read) = read {
                if read == 0 {
                    continue;
                }

                stats.rx_bytes.fetch_add(read, Ordering::Relaxed);
                stats.rx_packets.fetch_add(1, Ordering::Relaxed);

                if let Err(e) = if_tx.sendpacket(&buf[0..read]) {
                    eprintln!("Error sending to interface: {e}");
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
