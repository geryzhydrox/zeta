MAIN := ./src/gideon
TEST := ./tests/tests.scm
LIBS := ./src/gideon-lib/cmds.scm ./src/gideon-lib/prompts.scm ./src/gideon-lib/system.scm ./src/gideon-lib/term.scm
BUILD := guild compile -L ./
BUILD_NO_DEPS := guild compile

.PHONY: all test

all: $(MAIN) cmds system term prompts
	$(BUILD) $(MAIN)

test: cmds system term prompts
	$(BUILD) $(TEST)
	guile -L . $(TEST)
cmds: system prompts term ./src/gideon-lib/cmds.scm
	$(BUILD) ./src/gideon-lib/cmds.scm

system: term ./src/gideon-lib/system.scm
	$(BUILD) ./src/gideon-lib/system.scm

prompts: ./src/gideon-lib/prompts.scm
	$(BUILD_NO_DEPS) ./src/gideon-lib/prompts.scm

term: ./src/gideon-lib/term.scm
	$(BUILD_NO_DEPS) ./src/gideon-lib/term.scm

