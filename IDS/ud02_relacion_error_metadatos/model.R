library(data.table)
library(nlsr)
library(pracma)

MODELS <- c("Linear","Weibull", "Exponential", "Gamma", "Rayleigh", "Log Normal",
            "Inverse","Inverse2","Rational(0,1)","Rational(0,3)")
COLOUR <- rainbow(length(MODELS))

a <- fread("Grid_error_160424.csv")

o1  <- boxplot(a$mape_ens, plot=FALSE)$out    ## get outliers for mape_ens
o2  <- boxplot(a$POT_CON,  plot=FALSE)$out    ## get outliers for mape_ens
a[which(a$mape_ens %in% o1),"mape_ens"] <- NA ## remove outliers
a[which(a$POT_CON  %in% o2),"POT_CON"]  <- NA ## remove outliers
a[a$mape_ens == 0,"mape_ens"] <- NA           ## remove outliers
a[a$POT_CON  == 0,"POT_CON"]  <- NA           ## remove outliers
a[a$SUM      == 0,"SUM"]      <- NA           ## remove outliers

b <- a[!is.na(a$mape_ens) & !is.na(a$SUM),c("SUM","mape_ens")]   ## make a copy
b <- as.data.frame(b[order(b$SUM),])                             ## order to have nice prints
c <- reshape::rescaler.default(b, type = "range")                ## scale to test convergence

data <- b                                     ## To easily change the data to adjust to

E  <-list(A=5,B=23,C=1e-5,D=1,E=1)            ## Initial points for iterative parameter optimization
W  <-list(A=1,B=1e-3,C=1e-3,D=1e3,E=1)
G  <-list(A=1,B=1,C=1,D=1,E=1)
R  <-list(A=1,B=1,C=1,D=1,E=1)
L  <-list(A=1,B=1e3,C=1e-6,D=1e-5,E=1)
I1 <-list(A=0,B=1e-3,C=1,D=1e-3,E=1)
I2 <-list(A=0,B=1e-3,C=1,D=1e-3,E=1)

l  <- lm(mape_ens~SUM,data=data)                                             ## linear model

e  <- nlxb(formula=mape_ens~B*exp(-C*(SUM-D)),           start=E,data=data)  ## Exponential model
w  <- nlxb(formula=mape_ens~B*SUM^2*exp(-(C*(SUM-D))^2), start=W,data=data)  ## Weibull model
g  <- nlxb(formula=mape_ens~B*SUM^E*exp(-C*SUM),         start=G,data=data)  ## Gamma model
re <- nlxb(formula=mape_ens~B*SUM*exp(-C^2*(SUM-D)^2),   start=R,data=data)  ## Rayleigh model
ln <- nlxb(formula=mape_ens~B/SUM*exp(-C^2*log(SUM-D)^2),start=L,data=data)  ## Log normal model

i1 <- nlxb(formula=mape_ens~B/(SUM-D),                   start=I1,data=data) ## Inverse model
i2 <- nlxb(formula=mape_ens~B/(SUM-D)^2,                 start=I2,data=data) ## Inverse Square model
r1 <- rationalfit(data[[1]],data[[2]],d1=0,d2=1)                             ## Rational(0,1) model
r2 <- rationalfit(data[[1]],data[[2]],d1=0,d2=3)                             ## Rational(0,3) model


plot(data)
#plot(data,xlim=c(0,1000000))
lines(data[,1],as.numeric(predict(l, newdata=data)),col=COLOUR[1],lwd=3)
lines(data[,1],as.numeric(predict(e, newdata=data)),col=COLOUR[3],lwd=3)
lines(data[,1],as.numeric(predict(w, newdata=data)),col=COLOUR[2],lwd=3)
lines(data[,1],as.numeric(predict(g, newdata=data)),col=COLOUR[4],lwd=3)
lines(data[,1],as.numeric(predict(re,newdata=data)),col=COLOUR[5],lwd=3)
lines(data[,1],as.numeric(predict(ln,newdata=data)),col=COLOUR[6],lwd=3)
lines(data[,1],as.numeric(predict(i1,newdata=data)),col=COLOUR[7],lwd=3)
lines(data[,1],as.numeric(predict(i2,newdata=data)),col=COLOUR[8],lwd=3)
lines(data[,1],polyval(r1$p1,data[,1])/polyval(r1$p2,data[,1]),col=COLOUR[9] ,lwd=3)
lines(data[,1],polyval(r2$p1,data[,1])/polyval(r2$p2,data[,1]),col=COLOUR[10],lwd=3)
legend("topright",legend=MODELS,fill=COLOUR,col=COLOUR)


# a <- fread("CUPS_21032024.csv")
# b <- fread("features.csv")
#
# out <- boxplot(a$mape_ens, plot=FALSE)$out   ## get outliers above for mape_ens
# aux <- a$mape_ens                            ## make a copy of mape_ens
# aux[which(aux %in% out)] <- NA               ## remove outliers above
# aux <- 1/aux                                 ## inverse
# out <- boxplot(aux, plot=FALSE)$out          ## get outliers below for 1/mape_ens
# aux[which(aux %in% out)] <- NA               ## remove outliers below
# a$mape_ens_inv <- aux
#
# lm(mape_ens_inv~CAN_POT_CTD-1,data=a[is.finite(a$mape_ens_inv),])


pars <- expand.grid(A=seq(0.001, 10, len=10),
                    B=seq(0.001, 10, len=10),
                    C=seq(0.001, 10, len=10),
                    D=seq(0.001, 10, len=10),
                    E=seq(0.001, 10, len=10))

# #library(survival)
# library(fitdistrplus)
#
#
#
# control <- list(warnOnly = TRUE)
#
# nls(mape_ens~SSweibull(SUM, Asym, Drop, lrc, pwr),data=b,algorithm="plinear")
#
#
# weibullFit(a[!is.na(a$mape_ens) & !is.na(a$POT_CON),c("mape_ens","POT_CON")],
#            primaryGroup = "mape_ens", secondaryGroup = "POT_CON")
#
# fw <- fitdist(dat$cum.per.plant, "weibull")
#
# nls(mape_ens ~ dweibull(SUM,shape,scale),
#     start = c(shape=1,scale=1), data=b[1:100])
#
#
# ymodel <- WeibullReg(Surv(mape_ens,rep(T,length(mape_ens)))~SUM,data=b[1:100])
# ymodel <- survreg(Surv(mape_ens,rep(T,length(mape_ens)))~SUM,
#                   data=b[1:100],model=T,x=T)
# ymodel <- flexsurvreg(Surv(mape_ens,rep(T,length(mape_ens)))~SUM,
#                       data=b[1:100],model=T,x=T,dist="weibull")
#
#
# res <- nls2(mape_ens ~ D*((A/B) * ((SUM/B)^(A-1)) * exp(- (SUM/B)^A)) + C,
#             data=b,start=pars, algorithm='brute-force')
#
# res <- nls2(mape_ens~SSweibull(SUM, A,B,C,D),data=b[1:10],start=pars,algorithm='brute-force')
#
#
# res <- nls2(mape_ens ~ A + B * exp(- (C*SUM^D)),
#             data=b[1:100],start=pars, algorithm='plinear-brute')
#
# res <- nls2(mape_ens ~ A + B * exp(- (C*SUM^D)),
#             data=c[1:100],start=pars, algorithm='plinear-brute')
#
#
# res <- nls2(mape_ens ~ 1 + I(B*exp(- (C*SUM^D))),
#             data=c[1:100,],start=pars, algorithm='brute-force')
# plot(c[1:100,])
# lines(c[1:100,1],predict(res))
