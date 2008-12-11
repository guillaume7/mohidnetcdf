OBJS = $(SRCS:.$(F)=.$(O))

.PHONY: all

all: $(TARGET)

$(TARGET) : $(OBJS)
	@$(AR) $@ $^
	@echo Finished building $@.

%.$(O) : %.$(F)
	@$(CC) $(CCFLAGS) $(INCS) $<
	@echo $* .................. [OK]

