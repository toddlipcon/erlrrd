include ../support/include.mk

ERLC_FLAGS += -I../deps/eunit/include

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
