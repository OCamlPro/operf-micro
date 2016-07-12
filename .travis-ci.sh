
OPAM_PKGS="base-bytes base-unix base-bigarray base-threads"

export OPAMYES=1
if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade
else
    opam init
fi
if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`

opam install $OPAM_PKGS

opam remove operf-micro
opam pin remove operf-micro
opam pin add operf-micro .

export OCAMLRUNPARAM=b

operf-micro check share/operf-micro/benchmarks/

opam remove operf-micro
opam pin remove operf-micro
