### to build gnucobol with bdb

```sh
wget http://sourceforge.net/projects/open-cobol/files/gnu-cobol/3.0/gnucobol-3.0-rc1.tar.gz

tar xvf gnucobol-3.0-rc1.tar.gz

cd gnu-cobol-3.0-rc1

export CPPFLAGS="-I/usr/local/include"

export LDFLAGS="-L/usr/local/lib -Wl,-R,/usr/local/lib -Wl,--enable-new-dtags"

./configure --with-debug --with-vbisam

make

make check

sudo make install

sudo ldconfig
```
