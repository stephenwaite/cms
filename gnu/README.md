* to build gnucobol with bdb

$ wget http://sourceforge.net/projects/open-cobol/files/gnu-cobol/3.0/gnucobol-3.0-rc1.tar.gz
$ tar xvf gnucobol-3.0.tar.gz
$ cd gnu-cobol-3.0
$export CPPFLAGS="-I/usr/local/BerkeleyDB.18.1/include"
$export LDFLAGS="-L/usr/local/BerkeleyDB.18.1/lib -Wl,-R,/usr/local/BerkeleyDB.18.1/lib -Wl,--enable-new-dtags"
$ ./configure
$ make
$ make check
$ sudo make install
$ sudo ldconfig