#! ./bin/sh
SHELL := /bin/sh

## set the extensions we deal with
##
.SUFFICES :
.SUFFICES : .erl .beam .hrl

## set search paths for diff kinds of files
##
vpath  %.h  ./include
vpath  %.beam  ./ebin
vpath  %.erl  ./src


## set vars
ERLC := erlc
SOURCEDIR := ./src
BEAMDIR := ./ebin
INCLUDEDIR := ./include
REL_DIR := ./releases

EFILES := $(wildcard $(SOURCEDIR)/*.erl)
BEAMS := $(patsubst %.erl, %.beam, $(subst $(SOURCEDIR)/, , $(EFILES))) 

DBDEPENDENT := dsm.erl 

.PHONY : all documentation clean realclean release relclean

## define default goal
all : $(BEAMS)
	
## compile erl files
## state that each depend on a corresponding .erl file
## and that the .beam files sh be saved in ../ebin dir
%.beam : %.erl
	$(ERLC) -o $(BEAMDIR) $< 

## all erl files depend on commons.hrl header file
$(BEAMS) : $(INCLUDEDIR)/*.hrl 
## $(DBDEPENDENT) : $(INCLUDEDIR)/grd_dsm.hrl

documentation:
	cd doc && erl -s doc_gen -s erlang halt -noshell -noinput

release : relclean
	-cp $(BEAMDIR)/*.beam  scripts/* $(REL_DIR)/tmp

## delete all beam files in ebin directory
clean :
	-rm $(BEAMDIR)/*.beam

## delete all files in ebin directory
realclean :
	-rm $(BEAMDIR)/*.beam $(BEAMDIR)/*.app $(BEAMDIR)/*.rel

relclean :
	-rm -r $(REL_DIR)/tmp/*.beam $(REL_DIR)/tmp/*.app $(REL_DIR)/tmp/*.rel $(REL_DIR)/tmp/*.gz

.PHONY : erls beams

## convenience to list .erl files
erls :
	@echo esource files are $(EFILES)

## list all beam files
beams :
	@echo beam files are $(BEAMS)

