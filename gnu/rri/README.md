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

####for emailauthssnfile
#####handle alt keys
unload to wsid
sort -o wsid.1 -t~ -k 1.7,1.14 wsid
sort -o wsid.2 -t~ -k 1.15,1.38 wsid
sort -o wsid.3 -t~ -k 1.39,1.68 wsid
sort -o wsid.4 -t~ -k 1.69,1.88 wsid
sort -o wsid.5 -t~ -k 1.89,1.96 wsid
sort -o wsid.6 -t~ -k 1.97,1.105 wsid
####for rri
##### on stockbridge
make-unl
##### on destination server
get-unl-rri
rload-x
rload-y
rload-z

####for sidw indexed files
#####on stockbridge
make-unl
##### on destination server
get-unl-sidw
rload-00