PKGNAME=lkanren

MKDIR ?= mkdir -vp
CP    ?= cp

SRC = -I src
# PKGS = -pkgs ""
CFLGS = -cflags "-g"
LFLGS = -lflags "-g"

OCB_FLAGS = -use-ocamlfind $(CFLGS) $(LFLGS) $(PKGS) $(SRC)
OCB = ocamlbuild $(OCB_FLAGS)

BYTE_TARGETS = lkanren.cma
NATIVE_TARGETS = lkanren.cmxa

.PHONY: all lkanren test clean # install uninstall

all: lkanren

clean:
	rm -rf ./_build *.byte *.native

lkanren:
	$(OCB) $(BYTE_TARGETS) $(NATIVE_TARGETS)

test: lkanren
	$(OCB) -I test -pkgs "oUnit" TestMain.native

######################## Installation related stuff ##########################
# INSTALL_TARGETS = META \
# 	_build/relcppmem.cmi \
# 	_build/relcppmem.cmo \
# 	_build/relcppmem.cmx \
# 	_build/relcppmem.cma \
# 	_build/relcppmem.cmxa \
# 	_build/relcppmem.o \
# 	_build/relcppmem.a \
# 	_build/camlp5/pa_cppmem.cmi \
# 	_build/camlp5/pa_cppmem.cmo
#
# BUNDLEDIR = _build/bundle/$(PKGNAME)
#
# define MAKE_BUNDLE_RULE
# $(BUNDLEDIR)/$(notdir $(1)): $(1)
# 	cp $(1) $(BUNDLEDIR)
# MAKE_BUNDLE_TARGETS += $(BUNDLEDIR)/$(notdir $(1))
#
# endef
# $(foreach i,$(INSTALL_TARGETS),$(eval $(call MAKE_BUNDLE_RULE,$(i)) ) )
#
# rmbundledir:
# 	@$(RM) -r $(BUNDLEDIR)
#
# $(BUNDLEDIR):
# 	@$(MKDIR) $@
#
# bundle: rmbundledir $(BUNDLEDIR) $(MAKE_BUNDLE_TARGETS)
#
# install: bundle
# 	ocamlfind install $(PKGNAME) $(BUNDLEDIR)/*
#
# uninstall:
# 	ocamlfind remove $(PKGNAME)
