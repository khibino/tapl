HC = ghc
HFLAGS = -Wall

libTaPL_sources = \
	Arithmetic/Syntax.hs \
	Lambda/Bruijn.hs \
	Lambda/Syntax/UnTyped.hs \
##	Lambda/Syntax/Simple.hs


libTaPL_objs = $(libTaPL_sources:.hs=.o)
libTaPL_a = libTaPL.a

libTaPL_pic_objs = $(libTaPL_sources:.hs=.pic.o)
libTaPL_so = libTaPL.so


ghc_version = $(shell ghc -V | sed 's/.*version \([0-9.]*\)\($$\|[^0-9.]\)/\1/')
ghc_major_version = $(shell echo $(ghc_version) | sed 's/^\([0-9]*\)\([^0-9].*\)\?$$/\1/')
ghc_minor_version = $(shell echo $(ghc_version) | sed 's/^[0-9]*\.\([0-9]*\)\([^0-9].*\)\?$$/\1/')

# external_libraries = \
# 	template-haskell \
# 	utf8-string \
# 	regex-posix \
# 	regex-base \
# 	containers \
# 	bytestring

external_libraries =

#ifeq ($(ghc_minor_version), 8)
#external_libraries += time packedstring
#endif

external_library_options = \
	$(shell \
		for l in $(external_libraries) ; do \
			libpaths=$$(ghc-pkg field $$l library-dirs \
				| sed 's@^library-dirs:@@' \
				| sed 's@ ,\?@ -L@g'); \
			libs=$$(ghc-pkg field $$l hs-libraries \
				| sed 's@^hs-libraries:@@' \
				| sed 's@ ,\?@ -l@g'); \
			echo $$libpaths $$libs; \
		done)

library_options += $(external_library_options)

hs_a_libs = \
	$(libTaPL_a)

hs_so_libs = \
	$(libTaPL_so)

programs = \
#	programs/

targets = $(hs_a_libs) $(programs)


%.o: %.hs
	$(HC) $(HFLAGS) -c -o $@ $<

%.pic.o: %.hs
	$(HC) $(HFLAGS) -fPIC -c -o $@ $<

%: %.hs $(hs_a_libs)
	$(HC) $(HFLAGS) -o $@ $(library_options) $(hs_a_libs) $<

all: $(targets)

info:
	@echo ghc_version=$(ghc_version)
	@echo ghc_major_version=$(ghc_major_version)
	@echo ghc_minor_version=$(ghc_minor_version)
	@echo external_libraries=$(external_libraries)
	@echo library_options=$(library_options)
	@echo libTaPL_sources=$(libTaPL_sources)

clean:
	find . \( -name '*.hi' -o -name '*.o' \) -exec rm -f {} \;
	rm -f $(targets)

lint:
	hlint --utf8 --color Arithmetic programs

build: clean all lint

$(libTaPL_a): $(libTaPL_objs)
	ar -r $@ $^
