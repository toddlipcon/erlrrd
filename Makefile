all: dirs build-deps
	(cd src;$(MAKE))

build-deps:
	cd deps/eunit && $(MAKE)

dirs:
	mkdir -p ebin doc
clean:
	(cd src;$(MAKE) clean)
