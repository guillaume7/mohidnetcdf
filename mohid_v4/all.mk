OBJS = $(SRCS:.$(F)=.$(O))

.PHONY: all

all: $(TARGET)

$(TARGET) : $(OBJS) $(LIBS)
	@$(CC) $(LFLAGS) -o $@ $^ $(LLFLAGS)
	@echo Finished building $@.

%.$(O) : %.$(F)
	@$(CC) $(CCFLAGS) $(INCS) $<
	@echo $* .................. [OK]

