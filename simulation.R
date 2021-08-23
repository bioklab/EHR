require(dplyr)
require(foreach)
require(ggplot2)

num.of.distribtuion <- 100
distribution.df <- data.frame(miu  = runif(num.of.distribtuion,min =0, max = 10),
                             sigma = runif(num.of.distribtuion,min =0, max = 5)
                 )

distribution.df$name <- paste('LOINC',1:num.of.distribtuion,sep = '_')

compute.distance.between.eCDF <- function(x,y) {
  m <- length(x)
  n <- length(y)
  log.scale.value <- (log(m+n) - log(m) - log(n)) * 0.5
  scale.value     <- exp(log.scale.value)
  (ks.test(x,y)$statistic) / scale.value
}


run.simulation <- function(sample.cnt,distribution.df) {

    LOINC.data <- foreach(i = 1:num.of.distribtuion) %do% {
        miu   <- distribution.df$miu[i]
        sigma <- distribution.df$sigma[i]
        rnorm(sample.cnt,mean=miu,sd = sigma)
    }

    match.df <- foreach(i = 1:nrow(distribution.df),.combine='rbind') %do% {
        miu   <- distribution.df$miu[i]
        sigma <- distribution.df$sigma[i]
        lab.test.data <- rnorm(sample.cnt,mean=miu,sd = sigma)
        distance.vec  <- sapply(LOINC.data,function(x) compute.distance.between.eCDF(x,lab.test.data))
        data.frame(distribution.index = i,recover.index = which(distance.vec == min(distance.vec))[1])
    }
    sum(match.df$distribution.index == match.df$recover.index)/nrow(distribution.df)
}

simulation.rs <- foreach(sample.cnt= c(100,500,1000,2000,3000),.combine='rbind') %do% {
    accuracy <- sapply(1:50,function(x) run.simulation(sample.cnt = sample.cnt,distribution.df = distribution.df))
    data.frame(sample.cnt = sample.cnt,accuracy=accuracy)
}
ggplot(simulation.rs,aes(x=sample.cnt %>% factor,y=accuracy)) + geom_boxplot()
save(file = 'output/simulation.R.output/simulation.RData',list = c('simulation.rs'))
