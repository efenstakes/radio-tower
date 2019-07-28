SHELL := /bin/sh
.SUFFIXES :
.SUFFIXES : .erl .hrl .app .rel

# define variables
BEAM_DIR := ./ebin
SRC_DIR := ./src
INCLUDE_DIR := ./include
REL_DIR := ./release

ERLS := $(wildcard ./src/*.erl)
BEAMS := $(subst ./src, ./ebin, $(patsubst %.erl, %.beam, $(ERLS))) 
#$(ERLS: .erl=.beam)

ERLC := erlc
ERLC_FLAGS := -o $(BEAM_DIR)

VPATH := $(BEAM_DIR) $(SRC_DIR) $(INCLUDE_DIR)



.PHONY : all generate_docs clean realclean release relclean

all: $(BEAMS)

$(BEAM_DIR)/%.beam : %.erl
	$(ERLC) $(ERLC_FLAGS) $<

$(BEAMS): $(INCLUDE_DIR)/*.hrl



generate_docs :
	cd doc; erl -s doc_gen -s erlang halt -noshell -noinput 


clean:
	-rm -r $(BEAM_DIR)/*.beam

realclean:
	-rm -r $(BEAM_DIR)/*.beam $(BEAM_DIR)/*.rel $(BEAM_DIR)/*.app

release: relclean
	-cp -r $(BEAM_DIR) ./scripts $(REL_DIR)/tmp

relclean:
	-rm -r $(REL_DIR)/tmp/*.beam $(REL_DIR)/tmp/*.rel $(REL_DIR)/tmp/*.app $(REL_DIR)/tmp/*.gz $(REL_DIR)/tmp/*.boot $(REL_DIR)/tmp/*.script

# other help tests
.PHONY : eflags erls beams 

eflags:
	@echo $(ERLC_FLAGS)

erls:
	@echo $(ERLS)

beams:
	@echo $(BEAMS)

