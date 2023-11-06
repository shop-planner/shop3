MAKEINFO?=makeinfo

.PHONY: clean

all: html pdf info

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
$(info $$mkfile_path is [${mkfile_path}])
current_dir := $(dir $(mkfile_path))
$(info $$current_dir is [${current_dir}])
root_dir := $(shell dirname $(realpath ${current_dir}/../))
$(info $$root_dir is [${root_dir}])
export CL_SOURCE_REGISTRY=${root_dir}/jenkins/ext//:${root_dir}/shop3//:

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr
	rm -f include-stamp

LISP_SOURCE_DIRS := .. ../common ../explicit-stack-search ../io ../looping-tasks ../minimal-subtree ../pddl \
    ../plan-grapher ../planning-engine ../planning-tree ../shop-theorem-prover.api ../theorem-prover \
    ../unification ../hddl
LISP_SOURCES := $(foreach dir,$(LISP_SOURCE_DIRS), $(wildcard $(dir)/*.lisp))
LISP_ASDS := $(foreach dir,$(LISP_SOURCE_DIRS), $(wildcard $(dir)/*.asd))

include-stamp: $(LISP_SOURCES} $(LISP_ASDS)
	sbcl --no-userinit --eval '(require :asdf)' \
	--eval '(progn (require :sb-texinfo)(asdf:load-system :shop3 :force :all) (require :shop3-thmpr-api) (require :shop3/plan-grapher))' \
	--eval '(sb-texinfo:generate-includes "include/" (list :shop3 :shop3.unifier :shop3.common :shop3.theorem-prover :shop3.theorem-prover-api :shop3.plan-grapher :shop-user :hddl-translator) :base-package :shop3 :extra-symbols (list (quote shop3.unifier::binding) (quote shop3.common::insert-atom) (quote shop3.common::remove-atom) (quote shop3.common::atom-in-state-p) (quote shop3.common::state-all-atoms-for-predicate) (quote shop3.common::state-candidate-atoms-for-goal)))' \
	--eval '(quit)'
	touch include-stamp

# document-package-sample.texinfo: ../*.lisp ../*.asd
# 	sbcl \
# 	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :sb-texinfo))' \
# 	--eval '(sb-texinfo:document-package :sb-texinfo :output-file "document-package-sample.texinfo")' \
# 	--eval '(quit)'

%.html: %.texinfo style.css include-stamp
	${MAKEINFO} --html --output=shop3-manual.html --no-split --css-include=style.css $<

%.pdf: %.texinfo include-stamp
	texi2dvi --output=shop3-manual.pdf --pdf $<

%.info: %.texinfo include-stamp
	${MAKEINFO} --output shop3-manual.info --no-split $<

html: shop3-manual.html

pdf: shop3-manual.pdf

info: shop3-manual.info
