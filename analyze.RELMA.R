RELMA.mapping                <- read.csv("~/Project/EHR/data.from.martin/RELMA/20210602_LAM_results.csv", stringsAsFactors=FALSE)
local.code.to.be.mapped.1000 <- read.csv("~/Project/EHR/output/select.SH.local.code.R.output/local.code.to.be.mapped.1000.csv", stringsAsFactors=FALSE)
tmp                          <- merge(x=RELMA.mapping,y =local.code.to.be.mapped.1000,by.x='TESTCODE',by.y='LocalCode' )
sum(tmp$LOINC == tmp$LoincCode) / nrow(tmp)
