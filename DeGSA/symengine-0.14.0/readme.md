v.tsarev:

build order:

pre-requisite:
apt-get install cmake libgmp-dev

building (inside symengine directory):
mkdir build && cd build
cmake ..
make
sudo make install

then run tests:
ctest
