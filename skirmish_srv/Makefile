all:	compile

compile:
	rebar co

clean:
	rebar clean
	rm -rf test_reports/*

test: clean compile
	ERL_FLAGS="-config config/test.config -pa ../resource_srv/ebin/" rebar eunit skip_deps=true; mkdir -p test_reports; mv .eunit/*.xml test_reports/; mv .eunit/*.html test_reports/;


doc:
	@rm -rf api_docs
	@mkdir api_docs
	rebar doc
