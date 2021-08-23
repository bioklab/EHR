require(plyr)
require(dplyr)


components.to.loinc <- read.delim("~/Project/EHR/data.from.martin/SH/20210511_components_to_loinc_all.txt", stringsAsFactors=FALSE)

mimic.1000          <- read.csv("~/Project/EHR/data.from.martin/MIMIC/LoincCodesWith1000ValuesMimic.csv", stringsAsFactors=FALSE) %>% as.data.frame
mimic.1000$numvalue <- as.numeric(mimic.1000$Value)
mimic.1000          <- mimic.1000[complete.cases(mimic.1000),]
tmp1                <- ddply(mimic.1000,.(LoincCode),nrow)
ref.loinc.code      <- tmp1$LoincCode[tmp1$V1 >= 1000] %>% unique

flag          <- (components.to.loinc$LoincCode %in% ref.loinc.code) & (components.to.loinc$Count >= 1000)
df            <- components.to.loinc[flag,]
write.csv(x=df,file = 'output/select.SH.local.code.R.output/local.code.to.be.mapped.1000.csv',quote=TRUE,row.names=FALSE)






##################### Trash ##########################################

# LAM_results   <- read.csv("~/Project/EHR/data.from.martin/20210602_LAM_results.csv", stringsAsFactors=FALSE)
# rs            <- merge(x=df,y = LAM_results,by.x= 'LocalCode', by.y = 'TESTCODE')
# accuracy.1000 <- sum(rs$LoincCode == rs$LOINC) / nrow(rs)

# flag           <- (components.to.loinc$LoincCode %in% ref.loinc.code) & (components.to.loinc$Count >= 10000)
# df             <- components.to.loinc[flag,]
# rs             <- merge(x=df,y = LAM_results,by.x= 'LocalCode', by.y = 'TESTCODE')
# accuracy.10000 <- sum(rs$LoincCode == rs$LOINC) / nrow(rs)
# write.csv(x=df,file = 'output/select.SH.local.code.R.output/local.code.to.be.mapped.10000.csv',quote=TRUE,row.names=FALSE)




# common.code.vec         <- intersect(tmp1$LoincCode[tmp1$V1 >= 1000], tmp2$LoincCode[tmp2$V1 >= 1000]) # use common codes to query
# flag                    <- components.to.loinc$LoincCode %in% common.code.vec
# 
# 
# 
# flag                    <- match(x=code,table = components.to.loinc$LoincCode)
# df <- components.to.loinc[flag,]
# View(df)
# 
# easy.code <- assign.df$query[assign.df$flag == 1]
# scale.matrix <- apply(dist.matrix[easy.code,],1,scale) %>% t
# colnames(scale.matrix) <- colnames(dist.matrix)
# z.score.vec <- foreach(o = easy.code,.combine='c') %do% {
#    scale.matrix[o,o]  
# }
# 
# 
# 
# 
# xx <- merge(x=df,y = LAM_results,by.x= 'LocalCode', by.y = 'TESTCODE')
# 
# ff <- ddply(xx,.(LoincCode),function(x) sum(x$LoincCode == x$LOINC))
# sum(ff$V1 >0) / nrow(ff)
