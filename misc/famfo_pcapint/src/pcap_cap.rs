use crate::common::{Stats, BUF_SIZE};
use crate::config::Config;
use pcap::{Active, Capture};
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};
use std::{error::Error, net::UdpSocket};

#[cfg(any(feature = "crypto", test))]
use orion::aead;

pub struct Pcap {
    cap: Arc<Mutex<Capture<Active>>>,
    #[cfg(any(feature = "crypto", test))]
    key: Option<aead::SecretKey>,
}

impl Pcap {
    pub fn open(cfg: &Config) -> Result<Self, Box<dyn Error>> {
        let cap = Capture::from_device(cfg.iface_name.as_str())?;
        let cap = cap
            .snaplen(BUF_SIZE as i32)
            .promisc(true)
            .immediate_mode(true)
            .open()?;
        cap.direction(pcap::Direction::In)?;
        let cap = Arc::new(Mutex::new(cap));

        #[cfg(not(any(feature = "crypto", test)))]
        return Ok(Self { cap });

        #[cfg(any(feature = "crypto", test))]
        if let Some(key) = &cfg.key {
            let key = aead::SecretKey::from_slice(key.as_bytes())?;
            return Ok(Self {
                cap,
                key: Some(key),
            });
        } else {
            return Ok(Self { cap, key: None });
        }
    }

    pub fn get_cap(&self) -> Arc<Mutex<Capture<Active>>> {
        self.cap.clone()
    }

    pub fn pcap_loop(&mut self, sock_tx: UdpSocket, stats: Arc<Stats>) {
        let cap = self.cap.lock();
        if let Err(e) = cap {
            eprintln!("Failed to acquire capture lock: {e}");
            return;
        }
        let mut cap = cap.unwrap();
        loop {
            if let Ok(packet) = cap.next_packet() {
                stats.tx_bytes.fetch_add(packet.len(), Ordering::Relaxed);
                stats.tx_packets.fetch_add(1, Ordering::Relaxed);

                let buf = packet.to_vec();

                #[cfg(any(feature = "crypto", test))]
                let buf = if let Some(key) = &self.key {
                    let buf = aead::seal(key, &buf);
                    if let Err(e) = buf {
                        eprintln!("Error encrypting outgoing message: {e}");
                        continue;
                    }
                    buf.unwrap()
                } else {
                    buf
                };

                if let Err(e) = sock_tx.send(&buf) {
                    eprintln!("Error while sending to thread connection: {e}");
                }
            }
        }
    }
}
