CFLAGS = -O2
NVCC_FLAGS = -O2

gradient: main.o gradient-cuda
	nvcc *.o -o $@

gradient-cuda: gradient.cu
	nvcc $(NVCC_FLAGS) -c $?

%.o: %.cc
	$(CXX) $(CFLAGS) -c $?

clean:
	@rm -f gradient *.o
