all:	compile

compile:
	rebar3 compile

clean:
	rebar3 clean
	rm -rf test_reports/*

test: clean compile
	ERL_FLAGS="-config config/test.config" rebar3 eunit; mkdir -p test_reports; mv .eunit/*.xml test_reports/; mv .eunit/*.html test_reports/;


doc:
	@rm -rf api_docs
	@mkdir api_docs
	rebar3 edoc
