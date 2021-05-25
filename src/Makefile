SRCFYPP =\
	cfftb1.fypp \
	cfftf1.fypp \
	cffti1.fypp \
	cosqb1.fypp \
	cosqf1.fypp \
	dcosqb.fypp \
	dcosqf.fypp \
	dcosqi.fypp \
	dcost.fypp \
	dcosti.fypp \
	dfftb.fypp \
	dfftf.fypp \
	dffti.fypp \
	dsinqf.fypp \
	dsinqb.fypp \
	dsinqi.fypp \
	dsint.fypp \
	dsinti.fypp \
	dzfftb.fypp \
	dzfftf.fypp \
	dzffti.fypp \
	ezfft1.fypp \
	fftpack_api.fypp \
	passb.fypp \
	passb2.fypp \
	passb3.fypp \
	passb4.fypp \
	passb5.fypp \
	passf.fypp \
	passf2.fypp\
	passf3.fypp \
	passf4.fypp \
	passf5.fypp \
	radb2.fypp \
	radb3.fypp \
	radb4.fypp \
	radb5.fypp \
	radbg.fypp \
	radf2.fypp \
	radf3.fypp \
	radf4.fypp \
	radf5.fypp \
	radfg.fypp \
	rfftb1.fypp \
	rfftf1.fypp \
	rffti1.fypp \
	sint1.fypp \
	zfftb.fypp \
	zfftf.fypp \
	zffti.fypp

SRC = fftpack_kinds.f90 \
      $(SRCGEN)

LIB = libfftpack4.a


SRCGEN = $(SRCFYPP:.fypp=.f90)
OBJS = $(SRC:.f90=.o)
MODS = $(OBJS:.o=.mod)
SMODS = $(OBJS:.o=*.smod)

.PHONY: all clean

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

clean:
	$(RM) $(LIB) $(OBJS) $(MODS) $(SMODS) $(SRCGEN)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

$(SRCGEN): %.f90: %.fypp common.fypp
	fypp $(FYPPFLAGS) $< $@

# Fortran module dependencies
cfftb1.o: \
	fftpack_kinds.o\
	passb2.o \
	passb3.o \
	passb4.o \
	passb5.o
cfftf1.o: \
	fftpack_kinds.o
cffti1.o: \
	fftpack_kinds.o
cosqb1.o: \
	fftpack_kinds.o
cosqf1.o: \
	fftpack_kinds.o
dcosqb.o: \
	fftpack_kinds.o
dcosqf.o: \
	fftpack_kinds.o
dcosqi.o: \
	fftpack_kinds.o
dcost.o: \
	fftpack_kinds.o
dcosti.o: \
	fftpack_kinds.o
dfftb.o: \
	fftpack_kinds.o
dfftf.o: \
	fftpack_kinds.o
dffti.o: \
	fftpack_kinds.o
dsinqb.o: \
	fftpack_kinds.o
dsinqf.o: \
	fftpack_kinds.o
dsinqi.o: \
	fftpack_kinds.o
dsint.o: \
	fftpack_kinds.o
dsinti.o: \
	fftpack_kinds.o
dzfftb.o: \
	fftpack_kinds.o
dzfftf.o: \
	fftpack_kinds.o
dzffti.o: \
	fftpack_kinds.o
ezfft1.o: \
	fftpack_kinds.o
passb.o: \
	fftpack_kinds.o
passb2.o: \
	fftpack_kinds.o
passb3.o: \
	fftpack_kinds.o
passb4.o: \
	fftpack_kinds.o
passb5.o: \
	fftpack_kinds.o
passf.o: \
	fftpack_kinds.o
passf2.o: \
	fftpack_kinds.o
passf3.o: \
	fftpack_kinds.o
passf4.o: \
	fftpack_kinds.o
passf5.o: \
	fftpack_kinds.o
radb2.o: \
	fftpack_kinds.o
radb3.o: \
	fftpack_kinds.o
radb4.o: \
	fftpack_kinds.o
radb5.o: \
	fftpack_kinds.o
radbg.o: \
	fftpack_kinds.o
radf2.o: \
	fftpack_kinds.o
radf3.o: \
	fftpack_kinds.o
radf4.o: \
	fftpack_kinds.o
radf5.o: \
	fftpack_kinds.o
radfg.o: \
	fftpack_kinds.o
rfftb1.o: \
	fftpack_kinds.o
rfftf1.o: \
	fftpack_kinds.o
rffti1.o: \
	fftpack_kinds.o
sint1.o: \
	fftpack_kinds.o
zfftb.o: \
	fftpack_kinds.o
zfftf.o: \
	fftpack_kinds.o
zffti.o: \
	fftpack_kinds.o
fftpack_api.o: \
	cfftb1.o \
	cfftf1.o \
	cffti1.o \
	cosqb1.o \
	cosqf1.o \
	dcosqb.o \
	dcosqf.o \
	dcosqi.o \
	dcost.o \
	dcosti.o \
	dfftb.o \
	dfftf.o \
	dffti.o \
	dsinqf.o \
	dsinqb.o \
	dsinqi.o \
	dsint.o \
	dsinti.o \
	dzfftb.o \
	dzfftf.o \
	dzffti.o \
	ezfft1.o \
	fftpack_kinds.o \
	passb.o \
	passb2.o \
	passb3.o \
	passb4.o \
	passb5.o \
	passf.o \
	passf2.o\
	passf3.o \
	passf4.o \
	passf5.o \
	radb2.o \
	radb3.o \
	radb4.o \
	radb5.o \
	radbg.o \
	radf2.o \
	radf3.o \
	radf4.o \
	radf5.o \
	radfg.o \
	rfftb1.o \
	rfftf1.o \
	rffti1.o \
	sint1.o \
	zfftb.o \
	zfftf.o \
	zffti.o