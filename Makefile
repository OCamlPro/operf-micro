include Makefile.conf

FILES= \
  cycles.c micro_bench_run.ml micro_bench_types.ml \
  micro_bench_types.mli time_stamp_counter.ml \
  benchmarks

OPERF_MICRO:=operf-micro
OPERF_MICRO_SRC:=tools/$(OPERF_MICRO)
OPERF_MICRO_SHARE:=share/$(OPERF_MICRO)

all:
	$(MAKE) -C $(OPERF_MICRO_SRC) all
	$(MAKE) -C $(OPERF_MICRO_SHARE) all

install: all
	$(MAKE) -C $(OPERF_MICRO_SRC) install
	$(MAKE) -C $(OPERF_MICRO_SHARE) install

clean: clean
	$(MAKE) -C $(OPERF_MICRO_SRC) clean
	$(MAKE) -C $(OPERF_MICRO_SHARE) clean

uninstall:
	$(MAKE) -C $(OPERF_MICRO_SRC) uninstall
	$(MAKE) -C $(OPERF_MICRO_SHARE) uninstall

distclean: clean
	rm -f Makefile.conf

depend:
	$(MAKE) -C $(OPERF_MICRO_SRC) depend

.PHONY: all install depend clean uninstall distclean
