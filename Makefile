REBAR = $(shell command -v rebar || echo ./rebar)
DEPS_PLT=./.deps_plt
DEPS=erts kernel stdlib

.PHONY: all get-deps compile cpp_compile cpp_clean clean dialyze xref

all: get-deps cpp_compile compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

cpp_compile:
	$(MAKE) -C cpp_src

cpp_clean:
	$(MAKE) -C cpp_src clean

#test: compile
#	@ERL_AFLAGS="-config test/vegrandis_tests.app.config" $(REBAR) eunit skip_deps=true

clean: cpp_clean
	@$(REBAR) clean

$(DEPS_PLT):
	@echo Building $(DEPS_PLT)
	dialyzer --build_plt \
	  --output_plt $(DEPS_PLT) \
	  --apps $(DEPS)
#-r deps \

dialyze: compile $(DEPS_PLT)
	dialyzer --fullpath \
		--src src \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs \
		-r ebin \
		--plt $(DEPS_PLT)

xref:
	@$(REBAR) xref

doc: compile
	./scripts/hackish_make_docs.sh