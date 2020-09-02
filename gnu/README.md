### to build gnucobol with vbisam

```sh
git clone https://github.com/opensourcecobol/opensource-cobol/tree/develop/vbisam

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
## now for ocesql
```
cd ~/src
git clone git@github.com:opensourcecobol/Open-COBOL-ESQL.git
cd Open-COBOL-ESQL/
export CPPFLAGS+=" -I/usr/include/postgresql "
./configure
make AUTOCONF=: AUTOHEADER=: AUTOMAKE=: ACLOCAL=:
sudo make install AUTOCONF=: AUTOHEADER=: AUTOMAKE=: ACLOCAL=:
```
## precompile
```
ocesql FETCHTBL.cbl fetchtbl.cob
cobc -x fetchtbl.cob ../dblib/ocdb.o ../dblib/ocdblog.o ../dblib/ocdbutil.o ../dblib/ocesql.o -locesql
```
