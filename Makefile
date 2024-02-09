# lab4/Makefile

all: ppc-arm

LIB = ../lib
TOOLS = ../tools
OCAMLC = @$(TOOLS)/ocamlwrap ocamlc

PPC = mach.cmo optree.cmo dict.cmo tree.cmo lexer.cmo \
	parser.cmo check.cmo target.cmo regs.cmo simp.cmo \
	share.cmo jumpopt.cmo tran.cmo tgen.cmo main.cmo

ppc-arm: $(LIB)/common.cma $(PPC)
	$(OCAMLC) -g $^ -o $@ 

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

%.cmi: %.mli
	@$(OCAMLC) $(MLFLAGS) -c -g $<

%.cmo: %.ml $(TOOLS)/nodexp
	$(OCAMLC) $(MLFLAGS) -c -g -pp "$(TOOLS)/nodexp" $<

MLFLAGS = -I ../lib


# TESTING

test: force
	@echo "Say..."
	@echo "  'make test0' to compare assembly code"
	@echo "  'make test1' to test using QEMU or natively"
	@echo "  'make test2' to test using a remote RPi"
	@echo "  'make test3' to test using ECSLAB remotely"

TESTSRC := $(shell ls test/*.p)
OPT = -O2

SCRIPT1 = -e '1,/^(\*\[\[/d' -e '/^]]\*)/q' -e p
SCRIPT2 = -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p

ARMGCC = arm-linux-gnueabihf-gcc -marm -march=armv6+fp

ARCH := $(shell uname -m)
QEMU-armv6l = env
QEMU-armv7l = env
QEMU-aarch64 = env
QEMU := $(QEMU-$(ARCH))
ifndef QEMU
    QEMU := qemu-arm
endif

# test0 -- compile tests and diff object code
test0 : $(TESTSRC:test/%.p=test0-%)

test0-%: force
	@echo "*** Test $*.p"
	./ppc-arm $(OPT) test/$*.p >b.s
	sed -n $(SCRIPT1) test/$*.p | diff -u -b - b.s
	@echo

# test1 -- compile tests and execute with QEMU
test1 : $(TESTSRC:test/%.p=test1-%)

test1-%: pas0.o force
	@echo "*** Test $*.p"
	./ppc-arm $(OPT) test/$*.p >b.s
	$(ARMGCC) b.s pas0.o -static -o b.out 
	$(QEMU) ./b.out >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

pas0.o: pas0.c
	$(ARMGCC) -c $< -o $@

# test2 -- compile tests and execute using remote or local RPi
test2 : $(TESTSRC:test/%.p=test2-%)

test2-%: force
	@echo "*** Test $*.p"
	./ppc-arm $(OPT) test/$*.p >b.s
	$(TOOLS)/pibake b.s >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

# test3 -- ditto but using qemu on ecs.ox
test3 : $(TESTSRC:test/%.p=test3-%)

test3-%: $(TOOLS)/ecsbake force
	@echo "*** Test $*.p"
	./ppc-arm $(OPT) test/$*.p >b.s
	$(TOOLS)/ecsbake pas0.c b.s >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

promote: $(TESTSRC:test/%.p=promote-%)

promote-%: force
	./ppc-arm $(OPT) test/$*.p >b.s
	sed -f promote.sed test/$*.p >test/$*.new
	mv test/$*.new test/$*.p


# REMOTE DEPENDENCIES

$(LIB)/common.cma:
	$(MAKE) -C $(@D) $(@F)

$(TOOLS)/nodexp:
	$(MAKE) -C $(@D) $(@F)


# DEPENDENCIES

MLGEN = parser.mli parser.ml lexer.ml

ML = $(MLGEN) optree.ml tgen.ml tran.ml simp.ml share.ml jumpopt.ml \
	check.ml check.mli dict.ml dict.mli lexer.mli \
	mach.ml mach.mli main.ml optree.mli tgen.mli tree.ml \
	tree.mli tran.mli target.mli target.ml \
	simp.mli share.mli regs.mli regs.ml jumpopt.mli

depend: $(ML) $(TOOLS)/nodexp force
	(sed '/^###/q' Makefile; echo; ocamldep -pp $(TOOLS)/nodexp $(ML)) >new
	mv new Makefile


# CLEANUP

clean: force
	rm -f ppc-arm b.out b.s b.test
	rm -f $(MLGEN) *.cmi *.cmo *.o *.output

force:

###

check.cmo : \
    tree.cmi \
    optree.cmi \
    mach.cmi \
    lexer.cmi \
    dict.cmi \
    check.cmi
check.cmx : \
    tree.cmx \
    optree.cmx \
    mach.cmx \
    lexer.cmx \
    dict.cmx \
    check.cmi
check.cmi : \
    tree.cmi
dict.cmo : \
    optree.cmi \
    mach.cmi \
    dict.cmi
dict.cmx : \
    optree.cmx \
    mach.cmx \
    dict.cmi
dict.cmi : \
    optree.cmi \
    mach.cmi
jumpopt.cmo : \
    optree.cmi \
    jumpopt.cmi
jumpopt.cmx : \
    optree.cmx \
    jumpopt.cmi
jumpopt.cmi : \
    optree.cmi
lexer.cmo : \
    parser.cmi \
    optree.cmi \
    dict.cmi \
    lexer.cmi
lexer.cmx : \
    parser.cmx \
    optree.cmx \
    dict.cmx \
    lexer.cmi
lexer.cmi : \
    parser.cmi \
    optree.cmi \
    dict.cmi
mach.cmo : \
    mach.cmi
mach.cmx : \
    mach.cmi
mach.cmi :
main.cmo : \
    tree.cmi \
    tran.cmi \
    tgen.cmi \
    parser.cmi \
    mach.cmi \
    lexer.cmi \
    check.cmi
main.cmx : \
    tree.cmx \
    tran.cmx \
    tgen.cmx \
    parser.cmx \
    mach.cmx \
    lexer.cmx \
    check.cmx
optree.cmo : \
    optree.cmi
optree.cmx : \
    optree.cmi
optree.cmi :
parser.cmo : \
    tree.cmi \
    optree.cmi \
    lexer.cmi \
    dict.cmi \
    parser.cmi
parser.cmx : \
    tree.cmx \
    optree.cmx \
    lexer.cmx \
    dict.cmx \
    parser.cmi
parser.cmi : \
    tree.cmi \
    optree.cmi \
    dict.cmi
regs.cmo : \
    target.cmi \
    regs.cmi
regs.cmx : \
    target.cmx \
    regs.cmi
regs.cmi : \
    target.cmi
share.cmo : \
    regs.cmi \
    optree.cmi \
    mach.cmi \
    share.cmi
share.cmx : \
    regs.cmx \
    optree.cmx \
    mach.cmx \
    share.cmi
share.cmi : \
    optree.cmi
simp.cmo : \
    optree.cmi \
    simp.cmi
simp.cmx : \
    optree.cmx \
    simp.cmi
simp.cmi : \
    optree.cmi
target.cmo : \
    optree.cmi \
    target.cmi
target.cmx : \
    optree.cmx \
    target.cmi
target.cmi : \
    optree.cmi
tgen.cmo : \
    tree.cmi \
    tran.cmi \
    target.cmi \
    simp.cmi \
    share.cmi \
    regs.cmi \
    optree.cmi \
    mach.cmi \
    lexer.cmi \
    jumpopt.cmi \
    dict.cmi \
    tgen.cmi
tgen.cmx : \
    tree.cmx \
    tran.cmx \
    target.cmx \
    simp.cmx \
    share.cmx \
    regs.cmx \
    optree.cmx \
    mach.cmx \
    lexer.cmx \
    jumpopt.cmx \
    dict.cmx \
    tgen.cmi
tgen.cmi : \
    tree.cmi
tran.cmo : \
    target.cmi \
    regs.cmi \
    optree.cmi \
    tran.cmi
tran.cmx : \
    target.cmx \
    regs.cmx \
    optree.cmx \
    tran.cmi
tran.cmi : \
    optree.cmi
tree.cmo : \
    optree.cmi \
    dict.cmi \
    tree.cmi
tree.cmx : \
    optree.cmx \
    dict.cmx \
    tree.cmi
tree.cmi : \
    optree.cmi \
    dict.cmi
