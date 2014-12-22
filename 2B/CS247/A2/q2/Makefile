CXX = g++ 			
CXXFLAGS = -g -Wall -MMD
OBJECTS = $(patsubst ./%.cpp, %.o, $(shell find . -type f -name '*.cpp'))
DEPENDS = ${OBJECTS:.o=.d}
EXEC = Passwords

${EXEC} : ${OBJECTS}
	${CXX} ${CXXFLAGS} ${OBJECTS} -o ${EXEC}

clean :
	rm -rf ${DEPENDS} ${OBJECTS} ${EXEC}

-include ${DEPENDS}	# reads the .d files and reruns dependencies
