#library(arrow)
#library(iterators)
#library(lubridate)

library(data.table)
library(stringr)
library(doFuture)
library(outliers)
library(EnvStats)
library(zoo)

plan(multisession)

MINDATA  <- 10
PRINTPDF <- FALSE
ALPHA    <- 0.01
QMAX     <- 0.75
PERIODS  <- c("Daily","Monthly","Weekly")
FEATURES <- c("ENTROPY","AVG","SD","VAR","Q1","MEDIAN","Q3","TOTAL",
              "T2.0_LLANO","T2.0_VALLE","T_SOLAR_PICO","T_SOLAR_SPICO",
              "T2.0_PICO","T_SOLAR_SLLANO")

LIM <- fread("p-valores.csv")

dir.create("results/", showWarnings = F, recursive = T)
for (k in FEATURES)
{
  LA  <- as.numeric(LIM[grepl(k,variable),"avr"])
  LQ  <- as.numeric(LIM[grepl(k,variable),"Q2"])

  for (p in PERIODS)
  {
    dir.create(paste0("fig/",k,"/",p), showWarnings = F, recursive = T)

    DATA <- fread(paste0("SOLAR/Variation/",p,"/",p,"_",k,".csv"))
    DATA <- DATA[,which(unlist(lapply(DATA, function(x)!all(is.na(x))))),with=FALSE]
    DATA <- zoo(DATA)

    if (PRINTPDF)
    {
      pdf(paste0("fig/",k,"/",p,"/boxplots.pdf"),width=100)
        plot(DATA[,0001:0200])
        plot(DATA[,0201:0400])
        plot(DATA[,0401:0600])
        plot(DATA[,0601:0800])
        plot(DATA[,0801:1000])
        plot(DATA[,1001:1200])
        plot(DATA[,1201:1400])
        plot(DATA[,1401:1600])
        plot(DATA[,1601:1798])
      dev.off()
    }

    B <- foreach(NAME = names(DATA),.combine = rbind,.errorhandling = "remove",.options.future = list(seed = TRUE)) %do% {
      i   <- zoo(diff(DATA[,NAME]))

      ZERO <- sum(i != 0,na.rm=T)
      NNA  <- sum(!is.na(i))
      SD   <- sd(i,na.rm=T)

      if(ZERO >= MINDATA & NNA  >= MINDATA & is.numeric(SD) & SD != 0)
      {
        if (PRINTPDF) {pdf(paste0("fig/",k,"/",p,"/",NAME,".pdf")); plot(i); dev.off()}

        ECDF <- ecdf(i)
        AO   <- boxplot(i,plot=FALSE)
        GT   <- grubbs.test(sample(as.numeric(i[!is.na(i)]),30,replace=TRUE),type=11)
        DT   <- dixon.test( sample(as.numeric(i[!is.na(i)]),30,replace=TRUE),type=11)
        RT   <- rosnerTest( as.numeric(i[!is.na(i)]),alpha=ALPHA,k=2,warn=FALSE)

        AVR  <- ifelse(length(which(i>=LA)>=1),which(i>=LA)[1],0)
        Q2   <- ifelse(length(which(i>=LQ)>=1),which(i>=LQ)[1],0)
        ECDFA<- ifelse(ECDF(LA)<=QMAX         ,which(i>=quantile(i,probs=QMAX,na.rm=T))[1],0)
        ECDFQ<- ifelse(ECDF(LQ)<=QMAX         ,which(i>=quantile(i,probs=QMAX,na.rm=T))[1],0)
        OUT  <- ifelse(length(AO$out)>=1      ,which(i %in% AO$out)[1],0)
        GT   <- ifelse(GT$p.value<=ALPHA      ,which(i>=as.numeric(stringr::str_split_1(GT$alternative," ")[3]))[1],0)
        DT   <- ifelse(DT$p.value<=ALPHA      ,which(i>=as.numeric(stringr::str_split_1(DT$alternative," ")[3]))[1],0)
        RT   <- ifelse(RT$n.outlier!=0        ,RT$all.stats[RT$all.stats$Outlier == TRUE,"Obs.Num"][1],0)
        ENS  <- ifelse(names(which.max(table(c(AVR!=0,Q2!=0,ECDFA!=0,ECDFQ!=0,OUT!=0,GT!=0,DT!=0,RT!=0)))),1,0)

        data.frame(
          CUPS  = NAME,
          ZERO  = ZERO,
          NNA   = NNA,
          SD    = SD,
          AVR   = AVR,
          Q2    = Q2,
          ECDFA = ECDFA,
          ECDFQ = ECDFQ,
          OUT   = OUT,
          GT    = GT,
          DT    = DT,
          RT    = RT,
          ENS   = ENS
        )
      } else {
        data.frame(
          CUPS  = NAME,
          ZERO  = ZERO,
          NNA   = NNA,
          SD    = SD,
          AVR   = NA,
          Q2    = NA,
          ECDFA = NA,
          ECDFQ = NA,
          OUT   = NA,
          GT    = NA,
          DT    = NA,
          RT    = NA,
          ENS   = NA
        )
      }
    }

    fwrite(B,file=paste0("results/forecast-",p,"-",k,".csv"))
  }
}

#PROBANDO DIFERENTES HIPERPARAMETROS
ALPHAS   <- seq(0.01, 0.05, by = 0.01)  # Variando ALPHA
QMAXS    <- seq(0.7, 0.9, by = 0.1) 

for (ALPHA in ALPHAS) {
  for (QMAX in QMAXS) {
    dir.create(paste0("results/ALPHA_", formatC(ALPHA, format = "f", digits = 2), "_QMAX_", formatC(QMAX, format = "f", digits = 2)), showWarnings = FALSE, recursive = TRUE)
    
    for (k in FEATURES) {
      for (p in PERIODS) {
        dir.create(paste0("fig/ALPHA_", formatC(ALPHA, format = "f", digits = 2), "_QMAX_", formatC(QMAX, format = "f", digits = 2), "/", k, "/", p), showWarnings = FALSE, recursive = TRUE)
        DATA <- fread(paste0("SOLAR/Variation/", p, "/", p, "_", k, ".csv"))
        DATA <- DATA[, which(unlist(lapply(DATA, function(x) !all(is.na(x))))), with = FALSE]
        DATA <- zoo(DATA)
        
        if (PRINTPDF) {
          pdf(paste0("fig/ALPHA_", formatC(ALPHA, format = "f", digits = 2), "_QMAX_", formatC(QMAX, format = "f", digits = 2), "/", k, "/", p, "/boxplots.pdf"), width = 100)
          for (i in seq(1, ncol(DATA), by = 200)) {
            if (i + 199 <= ncol(DATA)) {
              plot(DATA[, i:(i + 199)])
            } else {
              plot(DATA[, i:ncol(DATA)])
            }
          }
          dev.off()
        }
        
        B <- foreach(NAME = names(DATA), .combine = rbind, .errorhandling = "remove", .options.future = list(seed = TRUE)) %do% {
          i <- zoo(diff(DATA[, NAME]))
          
          ZERO <- sum(i != 0, na.rm = TRUE)
          NNA  <- sum(!is.na(i))
          SD   <- sd(i, na.rm = TRUE)
          
          if (ZERO >= MINDATA & NNA >= MINDATA & is.numeric(SD) & SD != 0) {
            if (PRINTPDF) {
              pdf(paste0("fig/ALPHA_", formatC(ALPHA, format = "f", digits = 2), "_QMAX_", formatC(QMAX, format = "f", digits = 2), "/", k, "/", p, "/", NAME, ".pdf"))
              plot(i)
              dev.off()
            }
            
            ECDF <- ecdf(i)
            AO   <- boxplot(i, plot = FALSE)
            GT   <- grubbs.test(sample(as.numeric(i[!is.na(i)]), 30, replace = TRUE), type = 11)
            DT   <- dixon.test(sample(as.numeric(i[!is.na(i)]), 30, replace = TRUE), type = 11)
            RT   <- rosnerTest(as.numeric(i[!is.na(i)]), alpha = ALPHA, k = 2, warn = FALSE)
            
            AVR  <- ifelse(length(which(i >= LA)) >= 1, which(i >= LA)[1], 0)
            Q2   <- ifelse(length(which(i >= LQ)) >= 1, which(i >= LQ)[1], 0)
            ECDFA<- ifelse(ECDF(LA) <= QMAX, which(i >= quantile(i, probs = QMAX, na.rm = TRUE))[1], 0)
            ECDFQ<- ifelse(ECDF(LQ) <= QMAX, which(i >= quantile(i, probs = QMAX, na.rm = TRUE))[1], 0)
            OUT  <- ifelse(length(AO$out) >= 1, which(i %in% AO$out)[1], 0)
            GT   <- ifelse(GT$p.value <= ALPHA, which(i >= as.numeric(stringr::str_split(GT$alternative, " ")[[1]][3]))[1], 0)
            DT   <- ifelse(DT$p.value <= ALPHA, which(i >= as.numeric(stringr::str_split(DT$alternative, " ")[[1]][3]))[1], 0)
            RT   <- ifelse(RT$n.outlier != 0, RT$all.stats[RT$all.stats$Outlier == TRUE, "Obs.Num"][1], 0)
            ENS  <- ifelse(names(which.max(table(c(AVR != 0, Q2 != 0, ECDFA != 0, ECDFQ != 0, OUT != 0, GT != 0, DT != 0, RT != 0)))), 1, 0)
            
            data.frame(
              CUPS  = NAME,
              ZERO  = ZERO,
              NNA   = NNA,
              SD    = SD,
              AVR   = AVR,
              Q2    = Q2,
              ECDFA = ECDFA,
              ECDFQ = ECDFQ,
              OUT   = OUT,
              GT    = GT,
              DT    = DT,
              RT    = RT,
              ENS   = ENS
            )
          } else {
            data.frame(
              CUPS  = NAME,
              ZERO  = ZERO,
              NNA   = NNA,
              SD    = SD,
              AVR   = NA,
              Q2    = NA,
              ECDFA = NA,
              ECDFQ = NA,
              OUT   = NA,
              GT    = NA,
              DT    = NA,
              RT    = NA,
              ENS   = NA
            )
          }
        }
        
        fwrite(B, file = paste0("results/ALPHA_", formatC(ALPHA, format = "f", digits = 2), "_QMAX_", formatC(QMAX, format = "f", digits = 2), "/forecast-", p, "-", k, ".csv"))
      }
    }
  }
}
