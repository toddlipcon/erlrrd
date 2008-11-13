all: dirs deps
	(cd src;$(MAKE))

deps:
	cd deps/eunit && $(MAKE)

dirs: ebin doc
	mkdir -p ebin doc
clean:
	(cd src;$(MAKE) clean)
