use crate::common::Stats;
use std::io::{stdin, stdout, Stdin, Stdout, Write};
use std::sync::{atomic::Ordering, Arc};

static HELP: &'static str = concat!(
    "commands:\n",
    "h - this help\n",
    "q - exit process\n",
    "d - display counters\n",
    "c - clear counters\n",
);

pub struct Cmd {
    stdin: Stdin,
    stdout: Stdout,
}

impl Cmd {
    pub fn new() -> Self {
        Self {
            stdin: stdin(),
            stdout: stdout(),
        }
    }

    pub fn cmd_loop(&mut self, stats: Arc<Stats>) {
        let mut buf = String::new();
        loop {
            print!("> ");
            if let Err(e) = self.stdout.flush() {
                eprintln!("Error while flushing stdout: {e}");
            }

            let cmd = self.stdin.read_line(&mut buf);
            if let Err(e) = cmd {
                eprintln!("Error reading from stding: {e}");
            }

            match buf.trim() {
                "q" => {
                    println!("Exiting");
                    return;
                }
                "d" => println!("Interface stats:\n{stats:?}"),
                "c" => {
                    stats.rx_bytes.store(0, Ordering::Release);
                    stats.tx_bytes.store(0, Ordering::Release);
                    stats.rx_packets.store(0, Ordering::Release);
                    stats.tx_packets.store(0, Ordering::Release);
                }
                _ => println!("{HELP}"),
            }

            buf.clear();
        }
    }
}
