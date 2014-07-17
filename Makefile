
include Makefile.conf

FILES= \
  cycles.c micro_bench_run.ml micro_bench_types.ml \
  micro_bench_types.mli time_stamp_counter.ml \
  benchmarks

all:
	$(MAKE) -C builder all

install: install-builder install-data

install-builder:
	$(MAKE) -C builder install

install-data:
	mkdir -p $(DATADIR)
	cp -a $(FILES) $(DATADIR)

clean:
	$(MAKE) -C builder clean

depend:
	$(MAKE) -C builder depend

.PHONY: all install install-builder install-data depend clean
