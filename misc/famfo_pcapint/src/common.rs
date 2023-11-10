use crate::{pcap_cap::Pcap, sock::Sock};
use std::net::UdpSocket;
use std::sync::Mutex;
use std::sync::{atomic::AtomicUsize, Arc};

pub const BUF_SIZE: usize = 16384;

#[derive(Debug)]
pub struct Stats {
    pub rx_packets: AtomicUsize,
    pub rx_bytes: AtomicUsize,
    pub tx_packets: AtomicUsize,
    pub tx_bytes: AtomicUsize,
}

impl Stats {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            rx_packets: 0.into(),
            rx_bytes: 0.into(),
            tx_packets: 0.into(),
            tx_bytes: 0.into(),
        })
    }
}

pub unsafe fn daemonize(
    mut pcap: Pcap,
    mut sock: Sock,
    if_tx: Arc<Mutex<pcap::Capture<pcap::Active>>>,
    sock_tx: UdpSocket,
    rx_stats: Arc<Stats>,
    tx_stats: Arc<Stats>,
) -> Result<(), &'static str> {
    let pid = libc::fork();
    if pid < 0 {
        return Err("Failed to fork process");
    };
    if pid > 0 {
        libc::exit(libc::EXIT_SUCCESS)
    };

    let sid = libc::setsid();
    if sid < 0 {
        return Err("Failed to set SID");
    };

    let path = std::ffi::CString::new("/");
    if path.is_err() {
        return Err("Failed to cerate CString");
    };
    let path = path.unwrap();

    if libc::chdir(path.as_ptr()) < 0 {
        return Err("Failed to chdir to /");
    };

    libc::close(libc::STDIN_FILENO);
    libc::close(libc::STDOUT_FILENO);
    libc::close(libc::STDERR_FILENO);

    std::thread::spawn(move || pcap.pcap_loop(sock_tx, rx_stats));
    sock.sock_loop(if_tx, tx_stats);

    Ok(())
}

#[macro_export]
macro_rules! unwrap_err {
    ($x:expr) => {{
        if let Err(e) = $x {
            eprintln!("{e}");
            return;
        }

        $x.unwrap()
    }};
}
