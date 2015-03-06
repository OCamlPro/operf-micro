include Makefile.conf

FILES= \
  cycles.c micro_bench_run.ml micro_bench_types.ml \
  micro_bench_types.mli time_stamp_counter.ml \
  benchmarks

all:
	$(MAKE) -C builder all

install_benchs:
	cp -R benchmarks $(SHARE)/$(PKG)

install: all install_benchs
	opam-installer --prefix=$(PREFIX) $(PKG).install

uninstall_benchs:
	rm -r $(SHARE)/$(PKG)/benchmarks

uninstall: uninstall_benchs
	opam-installer --prefix=$(PREFIX) -u $(PKG).install

clean:
	$(MAKE) -C builder clean

depend:
	$(MAKE) -C builder depend

.PHONY: all install install-builder install-data depend clean
