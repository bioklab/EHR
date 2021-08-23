compute.distance.between.eCDF <- function(x,y) {
    # m <- length(x)
    # n <- length(y)
    # log.scale.value <- (log(m+n) - log(m) - log(n)) * 0.5
    # scale.value     <- exp(log.scale.value)
    # (ks.test(x,y)$statistic) / scale.value
  
    (ks.test(x,y)$statistic) 
}



compute.distance.between.eCDF.with.batch.correction <- function(x,y) {
    delta           <- median(y) - median(x)
    (ks.test(x,y - delta)$statistic) 
}


compute.distance.between.mad <- function(x,y) {
  delta           <- abs(mad(y) - mad(x)) + abs(median(y) - median(x))
  delta
  
}

compute.distance.between.eCDF.wilcox.test <- function(x,y) {
    wilcox.test(x,y)$statistic
}

# compute.distance.between.eCDF.v2 <- function(x,y) {
#     q1 <- quantile(x,probs = seq(from=0,to = 1,by = 0.01))
#     q2 <- quantile(y,probs = seq(from=0,to = 1,by = 0.01))
#     abs(q1- q2) %>% median
# 
# }