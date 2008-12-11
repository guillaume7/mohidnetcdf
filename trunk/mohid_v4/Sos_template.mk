#--------Users: fill these lines with your sourcesafe data-------------
SERVER  := 
USER    := 
PASS    := 
SOSHOME := 
PROJECT := $(basename $(TARGET))
GET     := soscmd -command GetFile \
           -server $(SERVER):8890 \
           -name $(USER) -password $(PASS) \
           -database "W:\SourceSafe\Mohid_v4\srcsafe.ini" \
           -project $(PROJECT) \
           -soshome $(SOSHOME) \
           -file 
S       :=sos
SMK     :=smk
MK      :=mk
#--------End of sourcesafe data-------------

.PHONY : sos $(FILES)

sos: $(FILES) $(METAFILES)
        -$(GET) Makefile

$(FILES):
        -$(GET) $(@:.$(S)=.$(F))
        @echo $(@:.$(S)=.$(F)) ................. [Fetched]

$(METAFILES):
        -$(GET) $(@:.$(SMK)=.$(MK))
        @echo $(@:.$(SMK)=.$(MK))............... [Fetched]

