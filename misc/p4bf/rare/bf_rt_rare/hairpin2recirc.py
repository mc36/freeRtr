
def hairpin2recirc(self, port):
    if port in self.hairpins:
        return self.recirc_port
    return port
