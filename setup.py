from setuptools import setup, find_packages

setup(
    name="qafny_impl",
    version="0.1",
    description="Qafny implementation",
    package_dir={"": "src"},
    packages=find_packages(where="src"),
)