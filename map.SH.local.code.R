require(data.table)
require(plyr)
require(dplyr)
require(foreach)
require(kdensity)
require(ROCit)
require(ggplot2)
source('code/util.R')
source('~/Project/Cancer2CellLine/client-side/code/for.figure/ggplot.style.R')

# MIMIC.file.list     <- c("data.from.martin/MIMIC/LoincCodesWith1000ValuesMimic.csv",
#                          "data.from.martin/MIMIC/20210610LoincCodeWithNValuesMimic/20210610_LoincCodesWith100ValuesMimic_01.csv",
#                          "data.from.martin/MIMIC/20210610LoincCodeWithNValuesMimic/20210610_LoincCodesWith100ValuesMimic_02.csv",
#                          "data.from.martin/MIMIC/20210610LoincCodeWithNValuesMimic/20210610_LoincCodesWith100ValuesMimic_03.csv",
#                          "data.from.martin/MIMIC/20210610LoincCodeWithNValuesMimic/20210610_LoincCodesWith100ValuesMimic_04.csv",
#                          "data.from.martin/MIMIC/20210610LoincCodeWithNValuesMimic/20210610_LoincCodesWith100ValuesMimic_05.csv"
#                          )
tmp <- list.files('data.from.martin/MIMIC/20210629 LoincCodeWithNValues/')[c(-1,-2)]                        

MIMIC.file.list <- paste('data.from.martin/MIMIC/20210629 LoincCodeWithNValues/',tmp,sep='')
MIMIC.file.list <- c("data.from.martin/MIMIC/LoincCodesWith1000ValuesMimic.csv",MIMIC.file.list)




SH.file          <- "data.from.martin/20210603LocalCodesToBeMapped/20210603data_local.code.to.be.mapped.1000.csv"
data             <- fread(input = SH.file,header = TRUE,stringsAsFactors = FALSE) %>% as.data.frame
data$numvalue    <- as.numeric(data$Value)
data             <- data[complete.cases(data),]
query.local.code <- unique(data$LocalCode)
query.data       <- foreach(o = query.local.code) %do% {
  data$numvalue[data$LocalCode == o]  
}
names(query.data) <- query.local.code

mapping.rs.list <- foreach(MIMIC.file = MIMIC.file.list) %do% {
    data           <- fread(input = MIMIC.file,header = TRUE,stringsAsFactors = FALSE) %>% as.data.frame
    data$numvalue  <- as.numeric(data$Value)
    data           <- data[complete.cases(data),]
    dict.loinc.code <- unique(data$LoincCode)
    dict.data <- foreach(o = dict.loinc.code) %do% {
        data$numvalue[data$LoincCode == o]  
    }
    names(dict.data) <- dict.loinc.code


    # SH.file          <- "data.from.martin/20210603LocalCodesToBeMapped/20210603data_local.code.to.be.mapped.1000.csv"
    # data             <- fread(input = SH.file,header = TRUE,stringsAsFactors = FALSE) %>% as.data.frame
    # data$numvalue    <- as.numeric(data$Value)
    # data             <- data[complete.cases(data),]
    # query.local.code <- unique(data$LocalCode)
    # query.data       <- foreach(o = query.local.code) %do% {
    #     data$numvalue[data$LocalCode == o]  
    # }
    # names(query.data) <- query.local.code


    dist.matrix <- foreach(x = query.data,.combine='rbind') %do% {
        dist.vec <- foreach(o = dict.data,.combine='c') %do% {
            compute.distance.between.eCDF(x,o)
        }
        scale(dist.vec) %>% c
    }
    rownames(dist.matrix) <- names(query.data)
    colnames(dist.matrix) <- names(dict.data)


    min.dist.code        <- apply(dist.matrix,1, function(x) names(dict.data)[which(x == min(x))[1]]  )
    assign.df            <- data.frame(query = rownames(dist.matrix), match = min.dist.code)
    assign.df$query      <- as.character(assign.df$query)
    assign.df$match      <- as.character(assign.df$match)


    ss <- assign.df$match %>% as.character()
    tt <- assign.df$query %>% as.character()
    v <- foreach(i = 1:length(tt),.combine='c') %do% {
        dist.matrix[tt[i], ss[i]]
    }
    assign.df$distance <- v


    local.code.to.be.mapped.1000 <- read.csv("~/Project/EHR/output/select.SH.local.code.R.output/local.code.to.be.mapped.1000.csv", stringsAsFactors=FALSE)
    tmp                          <- merge(x=assign.df,y =local.code.to.be.mapped.1000,by.x='query',by.y='LocalCode' )
    assign.df                    <- tmp[,c('query','match','distance','LoincCode')]
    assign.df$label              <- ifelse(assign.df$match == assign.df$LoincCode,1,0)
    assign.df


# #ggplot(assign.df,aes(x=factor(label), y= distance)) + geom_boxplot()
# #wilcox.test(assign.df$distance[assign.df$label == 1], assign.df$distance[assign.df$label == 0] )
# 
# x <- rocit(score = -1 * assign.df$distance, class = assign.df$label)
# #plot(x)
# #Y.index <- x$TPR - x$FPR
# #cut.off <- x$Cutoff[which(Y.index == max(Y.index))]
# 
# 
# df <- assign.df[assign.df$distance < -1 * 5,]
# precision5 <- sum(df$label) / nrow(df)
# df <- assign.df[assign.df$distance < -1 * 4,]
# precision4 <- sum(df$label) / nrow(df)
# data.frame(AUC=x$AUC,precision5 = precision5,precision4 = precision4)
}

save(file ='output/map.SH.to.local.code.R.output/map.SH.to.local.code.RData',list=c('mapping.rs.list'))


source('~/Project/Cancer2CellLine/client-side/code/for.figure/ggplot.style.R')

load('output/map.SH.to.local.code.R.output/map.SH.to.local.code.RData')
assign.df <- mapping.rs.list[[1]]
x         <- rocit(score = -1 * assign.df$distance, class = assign.df$label)
fig       <- plot(x,legend = FALSE,YIndex = FALSE)
draw.df   <- data.frame(y = fig$`TPR/Sensitivity/Recall`, x= fig$`FPR/1-Specificity`)
ggplot(draw.df,aes(x=x,y=y)) + geom_line(lwd=3) + ggplot.style + theme(axis.text  = element_text( size=35, face="bold")) +
geom_abline(slope = 1, intercept = 0,linetype="dashed") + xlab('FPR') + ylab('TPR')
ggsave(file = '~/OneDrive - Michigan State University/Project/EHR/Figures/ROC.pdf', width = 20, height = 20)

pr.df <- foreach(assign.df = mapping.rs.list,.combine='rbind') %do% {
cut.off.vec <- seq(-5,-2,by = 0.05)
rs <- foreach(cut.off = cut.off.vec,.combine = 'rbind') %do% {
    flag <- assign.df$distance <= cut.off
    pr   <-  sum(assign.df$label[flag]) / sum(flag)
    data.frame(cut.off = cut.off, pr = pr,percentage.of.mapped = sum(flag) / nrow(assign.df))
}
rs
}

draw.df <- ddply(pr.df,.(cut.off), function(x) data.frame(pr = mean(x$pr),percentage.of.mapped = mean(x$percentage.of.mapped)    ))
ggplot(draw.df,aes(x= cut.off,y=pr)) + geom_point(size=5) + geom_line() + ggplot.style + ylim(0,1)  + ylab('precision')  
ggplot(draw.df,aes(x= percentage.of.mapped,y=pr,col = cut.off)) + geom_point(size=5) + geom_line() + ggplot.style + 
  ylim(0,1)  + ylab('precision')+ 
  theme(axis.text  = element_text( size=40, face="bold"), 
        legend.text  = element_text( size=15, face="bold"), 
        legend.title  = element_text( size=0, face="bold")  
        )  + geom_point(data = data.frame(x= 1, y = 0.4285714), aes(x=x,y=y),size = 7, col='red',shape=17)


ggsave(file = '~/OneDrive - Michigan State University/Project/EHR/Figures/PR.pdf', width = 20, height = 20)



assign.df <- mapping.rs.list[[1]]
assign.df$text.label <- ifelse(assign.df$label == 1,'correct','wrong')
ggplot(assign.df,aes(x=factor(text.label),y=distance)) + 
  geom_boxplot(lwd=2,outlier.shape = NA) + 
  ggplot.style + geom_jitter(size=4) +
  theme(axis.text  = element_text( size=45, face="bold")) + xlab('') + ylab('Z-score')
ggsave(file = '~/OneDrive - Michigan State University/Project/EHR/Figures/distance.boxplot.pdf', width = 20, height = 20)


auc.vec <- sapply(mapping.rs.list,function(x)  rocit(score = -1 * x$distance, class = x$label)$'AUC')
draw.df <- data.frame(y = auc.vec)
ggplot(draw.df,aes(x='AUC',y=y)) + geom_boxplot(lwd=3) + ggplot.style + 
  theme(axis.text  = element_text( size=45, face="bold")) + geom_jitter(size=5) +
  xlab('') + ylab('')
ggsave(file = '~/OneDrive - Michigan State University/Project/EHR/Figures/AUC.boxplot.pdf', width = 20, height = 20)

  
###################################################################################
# de.duplication.assign.df <- ddply(assign.df,.(LoincCode), function(x) x[1,])
# 
# 
# ggplot(de.duplication.assign.df,aes(x=factor(label), y= distance)) + geom_boxplot()
# wilcox.test(de.duplication.assign.df$distance[de.duplication.assign.df$label == 1], de.duplication.assign.df$distance[de.duplication.assign.df$label == 0] )
# 
# x <- rocit(score = -1 * de.duplication.assign.df$distance, class = de.duplication.assign.df$label)
# plot(x)
# Y.index <- x$TPR - x$FPR
# cut.off <- x$Cutoff[which(Y.index == max(Y.index))]
# 
# df <- de.duplication.assign.df[de.duplication.assign.df$distance < -1 * 5,]
# precision <- sum(df$label) / nrow(df)
# precision
#  
tmp <- foreach(df = mapping.rs.list,.combine='rbind') %do% {
  df
}
