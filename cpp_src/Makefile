CXX=g++
RM=rm -f
INC=-I$(ERL_INCLUDE)
CPPFLAGS=-std=c++11 -Wall -Wextra -Wpedantic -fPIC -O2 $(INC)
LDFLAGS=-fPIC
LDLIBS=
SHARED_OBJECT=../priv/vegrandis_nif.so

SRCS=vegrandis_native_var.cpp vegrandis_erlang_var.cpp vegrandis_atomic_var.cpp vegrandis_common.cpp vegrandis.cpp
OBJS=$(subst .cpp,.o,$(SRCS))

.PHONY: all clean

all: $(SHARED_OBJECT)

$(SHARED_OBJECT): $(OBJS)
	$(CXX) $(LDFLAGS) -shared -o $(SHARED_OBJECT)  $(OBJS) $(LDLIBS)

clean:
	$(RM) $(OBJS)
	$(RM) $(SHARED_OBJECT)
