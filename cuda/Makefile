CFLAGS = -DDEBUG

gradient: main.o gradient-cuda
	nvcc *.o -o $@

gradient-cuda: gradient.cu
	nvcc -c $?

%.o: %.cc
	$(CXX) $(CFLAGS) -c $?

clean:
	@rm -f gradient *.o
