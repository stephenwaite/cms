### to build gnucobol with vbisam

```sh
https://sourceforge.net/projects/vbisam/files/vbisam2/

extract and make a couple changes in vbisam-2.0/vbisam.h
`vbisam_off_t` to `off_t`

wget http://sourceforge.net/projects/open-cobol/files/gnu-cobol/3.0/gnucobol-3.0-rc1.tar.gz

tar xvf gnucobol-3.0-rc1.tar.gz

cd gnu-cobol-3.0-rc1

env CPPFLAGS='-I/home/stee/vbisam-2.0/' LDFLAGS='-L/home/stee/vbisam-2.0/libvbisam' ./configure --with-debug --with-vbisam

make

make check

make test

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
