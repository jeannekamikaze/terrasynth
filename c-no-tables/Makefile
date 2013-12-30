CFLAGS = -O2

gradient: main.o gradient.o
	$(CXX) $^ -o $@

%.o: %.cc
	$(CXX) $(CFLAGS) -c $?

clean:
	@rm -f gradient *.o
