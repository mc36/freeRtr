import yappi

def writeProfilerRules(self, op_type):
    if op_type == 1:
        yappi.start()
        return
    if op_type != 3:
        return
    yappi.stop()
    threads = yappi.get_thread_stats()
    stats = yappi.get_func_stats()
    stats.save(path="profiler.ystat", type="ystat");
    stats.save(path="profiler.pstat", type="pstat");
    stats.save(path="profiler.callgrind", type="callgrind");
