MAIN := gideon
TEST := ../tests/tests.scm
LIBS := ./gideon-lib/cmds.scm ./gideon-lib/prompts.scm ./gideon-lib/system.scm ./gideon-lib/term.scm
BUILD := guild compile -L ./
BUILD_NO_DEPS := guild compile

.PHONY: all test

all: $(MAIN) cmds system term prompts
	$(BUILD) $(MAIN)

test: cmds system term prompts
	$(BUILD) $(TEST)
	guile -L . $(TEST)
cmds: system prompts term ./gideon-lib/cmds.scm
	$(BUILD) ./gideon-lib/cmds.scm

system: term ./gideon-lib/system.scm
	$(BUILD) ./gideon-lib/system.scm

prompts: ./gideon-lib/prompts.scm
	$(BUILD_NO_DEPS) ./gideon-lib/prompts.scm

term: ./gideon-lib/term.scm
	$(BUILD_NO_DEPS) ./gideon-lib/term.scm

