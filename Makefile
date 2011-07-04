COUCH_ROOT = /opt/couchbase-server
COUCHDB_ERLANG_LIB = $(COUCH_ROOT)/lib/couchdb/erlang/lib/couch-1.0.2
COUCHDB_LOCALD = $(COUCH_ROOT)/etc/couchdb/local.d


any:
	@echo "Targets are 'test', 'compile' and 'install'"

test: clean
	# make sure test compilation code is not mistaken for real compiled codeA
	# by puting the beam in mocks/
	erlc -DTEST -I mocks/ -o mocks/ fb_auth.erl
	(cd mocks && erlc *.erl)
	(cd mocks && erl -pa mocks/ -pa . -noshell -eval "eunit:test([fb_auth], [{report,{eunit_surefire,[{dir,\".\"}]}}])." -s erlang halt)

install: compile
	sudo cp fb_auth.beam $(COUCHDB_ERLANG_LIB)/ebin/
	sudo cp fb_auth.ini $(COUCHDB_LOCALD)
	sudo chown root:couchdb $(COUCHDB_LOCALD)/fb_auth.ini
	sudo chmod 660 $(COUCHDB_LOCALD)/fb_auth.ini
	sudo /etc/init.d/couchdb restart

compile: clean
	erlc -I $(COUCHDB_ERLANG_LIB)/include/ fb_auth.erl

clean:
	rm -f *.beam
	rm -f mocks/*.beam

