CXX = u++
CXXFLAGS = -g -multi -Wall -Wno-unused-label -MMD ${OPT}

#########################################################

OBJECTS = $(patsubst ./%.cc, %.o, $(shell find . -maxdepth 1 -type f -name '*.cc'))
DEPENDS = ${OBJECTS:.o=.d}
EXEC = wordcount

#########################################################

.PHONY : all clean

all : ${EXEC}

${EXEC} : $(OBJECTS)
	${CXX} ${CXXFLAGS} $^ -o $@

${OBJECTS} : Makefile

#########################################################

clean :
	rm -f ${EXEC} *.o *.d
    
-include ${DEPENDS}