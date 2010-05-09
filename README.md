Introduction
============

chillax is a [CouchDB](http://couchdb.apache.org) abstraction layer for Common Lisp licensed under the MIT license. The original author of chillax is [Josh Marchán](http://github.com/sykopomp); [Ian McEwen](http://github.com/ianmcorvidae) subsequently worked/is working on CLOS bindings, since the original version was written for [Sheeple](http://github.com/sykopomp/sheeple).

chillax also includes a CouchDB view server, which can be made with make-chillax-server.lisp. Currently-supported by make-chillax-server.lisp are sbcl and ccl.

YASON Problem
=============

Note that, at least on some systems, it's necessary to edit YASON's parse.lisp file (line 72) to include #\Return. Sorry for any inconvenience.
