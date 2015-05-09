# $Id: Makefile,v 1.8 2008/08/11 07:14:57 dvd Exp $

# Makefile for the CGI questionnaire interpreter

HC=ghc -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances

all: qrf idx sydiff

qrf: CGI.lhs CSV.lhs Compute.lhs Ana.lhs Acti.lhs Alter.lhs Traverse.lhs Form.lhs HTML.lhs Parse.lhs Synet.lhs Test.lhs XML.lhs qrf.lhs
	${HC} --make $@.lhs -o $@

idx: qrf idx.lhs
	${HC} --make $@.lhs -o $@

sydiff: idx sydiff.lhs
	${HC} --make $@.lhs -o $@

explanation: qrf explanation.lhs
	${HC} --make $@.lhs -o $@

thematree: explanation Gene.lhs thematree.lhs
	${HC} --make $@.lhs -o $@
