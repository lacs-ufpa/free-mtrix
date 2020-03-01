```
// zeromq3-x is a git submodule
// the following assumes you are in the dependency folder

sudo apt-get install libtool-bin autoconf automake   
git clone https://github.com/zeromq/zeromq3-x.git   
cd zeromq3-x   
./autogen.sh   
./configure --enable-static   
make   
sudo make install   
sudo ldconfig   
```
