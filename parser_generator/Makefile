DC=dmd
OBJS=definition.o expression.o main.o

.SUFFIXES: .d .o

parser: $(OBJS) comp_d/libcomp_d.a
	$(DC) -L-Lcomp_d -L-lcomp_d -of="parser" $^

.d.o:
	$(DC) -c $<
