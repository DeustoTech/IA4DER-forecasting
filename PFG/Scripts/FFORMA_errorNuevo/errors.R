library(doFuture)
plan(multicore)

mape <- foreach(i=list.files("out",full.names="T"),.combine=rbind) %dofuture% {
  d <- t(read.csv(i,row.names=1))

  r <- d[,"real"]
  z <- r != 0

  out <- apply(d[,-1],2,function(x) {100*median(abs(x[z]-r[z])/r[z],na.rm=T)})
  return(out)
}

rmse <- foreach(i=list.files("out",full.names="T"),.combine=rbind) %dofuture% {
  d <- t(read.csv(i,row.names=1))
  r <- d[,"real"]

  out <- apply(d[,-1],2,function(x) {sqrt(median((r-x)^2,na.rm=T))})
  return(out)
}

summary(mape)
summary(rmse)

boxplot(mape,outline=F)
boxplot(rmse,outline=F)
