import os, sys, grpc
import logging, linecache, inspect
from threading import Thread
from time import sleep
import ipaddress

import argparse
import socket
import mib, shutil, re, subprocess

# INTEL SDE client path
SDE = os.environ.get("SDE", "~/bf-sde-9.8.0")
SDE_INSTALL = os.environ.get("SDE_INSTALL", SDE + "/install")
BF_RUNTIME_LIB = SDE_INSTALL + "/lib/python3.9/site-packages/tofino/"
BSP_FILE_PATH = SDE_INSTALL + "/lib/libpltfm_mgr.so"

# set our lib path
sys.path.append(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), "./", BF_RUNTIME_LIB)
)
sys.path.append(
    os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "./", BF_RUNTIME_LIB + "bfrt_grpc"
    )
)
import bfrt_grpc.client as gc
import bfrt_grpc.bfruntime_pb2 as bfrt_pb2

# logger
PROGRAM_NAME = os.path.basename(sys.argv[0])
log_level = logging.WARNING
logger = logging.getLogger(PROGRAM_NAME)

if not len(logger.handlers):
    logger.addHandler(logging.StreamHandler())
    logger.setLevel(log_level)


def _Exception():
    exc_type, exc_obj, tb = sys.exc_info()
    f = tb.tb_frame
    lineno = tb.tb_lineno
    filename = f.f_code.co_filename
    linecache.checkcache(filename)
    line = linecache.getline(filename, lineno, f.f_globals)
    return 'EXCEPTION IN ({}, LINE {} "{}"): {}'.format(
        filename, lineno, line.strip(), exc_obj
    )


def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif v.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        raise argparse.ArgumentTypeError("Boolean value expected.")


def is_any_thread_alive(threads):
    return True in [t.is_alive() for t in threads]


def graceful_exit(bf_client, sck):
    os._exit(0)
    bf_client.interface.tear_down_stream()
    sck.close()
    sys.exit(0)


def inet_ntoa(ip_addr):
    return str(ipaddress.ip_address(ip_addr))


def mac_ntoa(mac_addr):
    return ":".join(re.findall("..", "%012x" % mac_addr))
