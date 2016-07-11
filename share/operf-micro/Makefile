
ROOTDIR:=../..
include $(ROOTDIR)/Makefile.conf

FILES:= \
  cycles.c micro_bench_run.ml micro_bench_types.ml \
  micro_bench_types.mli time_stamp_counter.ml \
  benchmarks

all: merlin

install:
	mkdir -p $(SHARE)/$(PKG)/
	cp -a $(FILES) $(SHARE)/$(PKG)/

uninstall:
	rm -rf $(SHARE)/$(PKG)

clean:
	rm -f micro_bench_types.cmi

merlin: micro_bench_types.cmi

%.cmi: %.mli
	ocamlc -c $<
