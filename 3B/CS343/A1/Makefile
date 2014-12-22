CXX = u++					# compiler
CXXFLAGS = -g -Wall -Wno-unused-label -MMD	# compiler flags
MAKEFILE_NAME = ${firstword ${MAKEFILE_LIST}}	# makefile name

OBJECTS0 = q1exception.o			# optional build of given program
EXEC0 = exception

OBJECTS1 = q1flags.o
EXEC1 = flags

OBJECTS2 = q1globalstatusflag.o
EXEC2 = globalstatusflag

OBJECTS3 = q1returncodes.o
EXEC3 = returncodes

OBJECTS00 = q2resumption.o			# optional build of given program
EXEC00 = resumption

OBJECTS4 = q2noresumption.o
EXEC4 = noresumption

OBJECTS5 = q3.o 
EXEC5 = utf8

OBJECTS = ${OBJECTS1} ${OBJECTS2} ${OBJECTS3} ${OBJECTS4} ${OBJECTS5}
DEPENDS = ${OBJECTS:.o=.d}
EXECS = ${EXEC1} ${EXEC2} ${EXEC3} ${EXEC4} ${EXEC5}

#############################################################

.PHONY : all clean

all : ${EXECS}					# build all executables

q1%.o : q1%.cc					# change compiler 1st executable
	g++ ${CXXFLAGS} -c $< -o $@

${EXEC0} : ${OBJECTS0}
	g++ ${CXXFLAGS} $^ -o $@

${EXEC1} : ${OBJECTS1}
	g++ ${CXXFLAGS} $^ -o $@

${EXEC2} : ${OBJECTS2}
	g++ ${CXXFLAGS} $^ -o $@

${EXEC3} : ${OBJECTS3}
	g++ ${CXXFLAGS} $^ -o $@

q2%.o : q2%.cc					# change compiler 2nd executable
	g++ ${CXXFLAGS} -c $< -o $@

${EXEC00} : ${OBJECTS00}
	g++ ${CXXFLAGS} $^ -o $@

${EXEC4} : ${OBJECTS4}
	g++ ${CXXFLAGS} $^ -o $@

${EXEC5} : ${OBJECTS5}
	${CXX} $^ -o $@

#############################################################

${OBJECTS} : ${MAKEFILE_NAME}			# OPTIONAL : changes to this file => recompile

-include ${DEPENDS}				# include *.d files containing program dependences

clean :						# remove files that can be regenerated
	rm -f *.d *.o ${EXEC0} ${EXEC00} ${EXECS}
