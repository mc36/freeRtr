from setuptools import setup, find_packages

setup(
    name="bf_forwarder",
    version="20.6.23",
    scripts = [ "bf_forwarder.py", "switchdctl.py", ],
    py_modules = [ "mib" ],
    packages = find_packages(),
    install_requires = [
        "yappi"
    ]
)
