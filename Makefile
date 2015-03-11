include Makefile.conf

FILES= \
  cycles.c micro_bench_run.ml micro_bench_types.ml \
  micro_bench_types.mli time_stamp_counter.ml \
  benchmarks

all:
	$(MAKE) -C builder all

install_benchs:
	mkdir -p $(SHARE)/$(PKG)/
	cp -a $(FILES) $(SHARE)/$(PKG)/

install: all install_benchs
	cp -a builder/builder.opt $(BIN)/operf-micro

uninstall_benchs:
	rm -rf $(SHARE)/$(PKG)/benchmarks

uninstall: uninstall_benchs
	rm -f $(BIN)/operf-micro
	rm -rf $(SHARE)/$(PKG)

distclean: clean
	rm -f Makefile.conf

clean:
	$(MAKE) -C builder clean

depend:
	$(MAKE) -C builder depend

.PHONY: all install install-builder install-data depend clean\
	uninstall_benchs uninstall
