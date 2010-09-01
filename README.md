# Quickstart example

Make sure CouchDB is installed, and currently running. This example assumes that the server is
running in localhost, using the default 5984 port. Yason's alist encoder/decoder is used below to
make replies readable.

    CL-USER> (require 'chillax)
    CL-USER> (in-package :chillax)
    CHILLAX> (defparameter *server* (make-instance 'yason-server :object-as-alist-p t
                                                   :parse-object-key-fun
                                                   (lambda (string) (intern string *package*))))
    *SERVER*
    CHILLAX> (defparameter *db* (ensure-db *server* "test"))
    *DB*
    CHILLAX> (all-documents *db*)
    ((|rows|) (|offset| . 0) (|total_rows| . 0))
    CHILLAX> (put-document *db* "my_test_doc" '((name . "Kat") (favorite-language . "common-lisp")))
    ((|rev| . "1-3c964cc898c03c48903a91d90b24a269")
     (|id| . "my_test_doc") (|ok| . T))
    CHILLAX> (get-document *db* "my_test_doc")
    ((FAVORITE-LANGUAGE . "common-lisp")
     (NAME . "Kat")
     (|_rev| . "1-3c964cc898c03c48903a91d90b24a269")
     (|_id| . "my_test_doc"))
    CHILLAX> (delete-document *db* (cdr (assoc '|_id| *)) (cdr (assoc '|_rev| *)))
    ((|rev| . "2-2221fc2b97c1fac1a82ba07d2835ac80")
     (|id| . "my_test_doc")
     (|ok| . T))

# Introduction

Chillax is a [CouchDB](http://couchdb.apache.org) abstraction layer for Common Lisp licensed under
the MIT license. The original author of chillax is [Kat Marchán](http://github.com/zkat);
[Ian McEwen](http://github.com/ianmcorvidae) subsequently worked on CLOS bindings, since
the original version was written for [Sheeple](http://github.com/zkat/sheeple).

Chillax also includes a CouchDB view server, which can be made with
make-chillax-server.lisp. Currently supported by make-chillax-server.lisp are sbcl and ccl.

Chillax includes several systems:

* chillax.asd - This is a 'DWIM' system. It uses Yason to parse/encode JSON data. If you don't know
  what you want, this is probably what you want.
* chillax.core.asd - Core API and protocols for servers, databases, documents, and
  design-docs.
* chillax.yason.asd - Implementation of the server protocol using Yason's JSON parser.
* chillax.utils.asd - Some handy utilities.
* chillax.view-server.asd - The Chillax view server. This only depends on chillax.utils.

# Core API

The basic Chillax API is very straightforward: It is a thin, lispy layer on top of CouchDB's RESTful
API. Its main purpose is to provide Lisp functions to all of CouchDB's API calls, while translating
certain things into Lisp data and concepts. For example, Chillax takes care of checking CouchDB's
HTTP response codes for sanity. When error codes are returned, Chillax will signal Lisp
corresponding Lisp conditions.

Additionally, Chillax is able to use any representation for CouchDB documents, provided the core
Chillax protocols are implemented for that representation, i.e, you can use hash tables, alists, or
instances of classes as your 'documents', and Chillax will automatically serialize/deserialize JSON
communications with CouchDB according to the protocol's implementation.

Note that, since Chillax really is just a thin layer, it abstracts very little beyond the
above. Most Chillax functions will simply return the de-JSON-ified CouchDB responses, and leave you
to digging through the response objects for the data you need.

For a full listing of Chillax functions, refer to src/core/packages.lisp

# Core Protocol

You can use the core protocol to customize behavior of Chillax. Writing your own implementation for
this protocol is relatively simple and painless, and it allows you to do things such as use your own
custom JSON encoder/decoder, if Yason doesn't fit your needs.

The details of using this protocol are currently not documented, but you can look at src/yason.lisp
for an example. The protocol is fairly small and straightforward to implement.

For a full listing of protocol functions, refer to src/core/packages.lisp, and to the various
related files for actual doc strings.

# View Server

The Chillax View Server allows you to write views in full-fledged Common Lisp. In order to use the
server, you must create a binary using code loaded by chillax.view-server.asd, and make
chillax-server::run-server the toplevel function.

Once you put the binary in a place visible to the CouchDB process, add the following to
/etc/couchdb/local.ini (or wherever your local.ini is located):

    [query_servers]
    common-lisp = /path/to/chillax-view-server

Add the common-lisp entry if there's already a query_servers entry.

Once you've done this, you can start making queries directly in Lisp!

    CHILLAX> (invoke-temporary-view *db* :language "common-lisp"
                                         :map (prin1-to-string '(lambda (doc) (emit doc 1))))

You can load anything you want into the view server image before dumping it, customize which package
it executes views in, etc. The source code is fairly short and easy to digest. It's designed to be
customizable for whatever your needs are.

# Yason Problem

Note that, at least on some systems, versions of [Yason](http://github.com/hanshuebner/Yason) prior
to commit
[00b9a5c06b7c4113a48518a1f136637efb4458b9](http://github.com/hanshuebner/Yason/commit/00b9a5c06b7c4113a48518a1f136637efb4458b9)
will not work (in this commit, #\Return was added to the list of whitespace characters
recognized). Using these versions instead of 0.1 is recommended anyway for performance reasons.
