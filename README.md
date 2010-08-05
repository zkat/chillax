Introduction
============

chillax is a [CouchDB](http://couchdb.apache.org) abstraction layer for Common Lisp licensed under
the MIT license. The original author of chillax is [Josh Marchán](http://github.com/sykopomp);
[Ian McEwen](http://github.com/ianmcorvidae) subsequently worked/is working on CLOS bindings, since
the original version was written for [Sheeple](http://github.com/sykopomp/sheeple).

chillax also includes a CouchDB view server, which can be made with
make-chillax-server.lisp. Currently-supported by make-chillax-server.lisp are sbcl and ccl.

YASON Problem
=============

Note that, at least on some systems, versions of [YASON](http://github.com/hanshuebner/Yason) prior
to commit
[00b9a5c06b7c4113a48518a1f136637efb4458b9](http://github.com/hanshuebner/Yason/commit/00b9a5c06b7c4113a48518a1f136637efb4458b9)
will not work (in this commit, #\Return was added to the list of whitespace characters
recognized). Using these versions instead of 0.1 is recommended anyway for performance reasons.
