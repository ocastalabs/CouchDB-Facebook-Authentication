COUCH_ROOT = /usr/local
COUCHDB_ERLANG_LIB = $(COUCH_ROOT)/lib/couchdb/erlang/lib/couch-1.3.0a-f07c75f-git
COUCHDB_LOCALD = $(COUCH_ROOT)/etc/couchdb/local.d
COUCHDB_INIT_SCRIPT = /etc/init.d/couchdb

any:	compile

install:
	sudo cp fb_auth.beam $(COUCHDB_ERLANG_LIB)/ebin
	sudo cp fb_auth.ini $(COUCHDB_LOCALD)
	sudo chown couchdb:couchdb $(COUCHDB_LOCALD)/fb_auth.ini
	sudo chmod 660 $(COUCHDB_LOCALD)/fb_auth.ini
	sudo $(COUCHDB_INIT_SCRIPT) restart

compile:clean
	erlc -I $(COUCHDB_ERLANG_LIB)/include fb_auth.erl

clean:
	rm -f *.beam
