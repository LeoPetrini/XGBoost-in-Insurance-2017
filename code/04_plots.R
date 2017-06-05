library(readr)
library(ggplot2)

cas_df_time= read.csv('C:\\Users\\E3850004\\Desktop\\git\\cas_df_time2.csv')
nocas_df_time= read.csv('C:\\Users\\E3850004\\Desktop\\git\\noncas_df_time2.csv')

colnames(cas_df_time)


cas_df= read.csv('C:\\Users\\E3850004\\Desktop\\git\\cas_df_preds2.csv')
nocas_df= read.csv('C:\\Users\\E3850004\\Desktop\\git\\noncas_df_preds2.csv')

dat <- data.frame(x = c(cas_df_time[["time_xgb_cs_train"]],
                        cas_df_time[["time_gam_cs_train"]]),yy = rep(letters[1:2],each = 10))



ggplot(dat,aes(x=x)) + 
  geom_histogram(data=subset(dat, yy== 'a'),fill = "red", alpha = 0.2, bins = 10) +
  geom_histogram(data=subset(dat, yy == 'b'),fill = "blue", alpha = 0.2, bins = 10) +
   geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2, bins = 10)


#### TIME

mean(nocas_df_time$time_xgb_tweedie)/(mean(nocas_df_time$time_gam_nb)+mean(nocas_df_time$time_gam_cs))

mean(nocas_df_time$time_xgb_tweedie_train)/(mean(nocas_df_time$time_gam_nb_train)+mean(nocas_df_time$time_gam_cs_train))


mean(nocas_df_time$time_xgb_cs)/mean(nocas_df_time$time_gam_cs)
mean(nocas_df_time$time_xgb_nb)/mean(nocas_df_time$time_gam_nb)

mean(nocas_df_time$time_xgb_cs_train)/mean(nocas_df_time$time_gam_cs_train)
(mean(nocas_df_time$time_xgb_nb_train)/mean(nocas_df_time$time_gam_nb_train))

### TIME

mean(cas_df_time$time_xgb_tweedie)/(mean(cas_df_time$time_gam_nb)+mean(cas_df_time$time_gam_cs))

mean(cas_df_time$time_xgb_tweedie_train)/(mean(cas_df_time$time_gam_nb_train)+mean(cas_df_time$time_gam_cs_train))


mean(cas_df_time$time_xgb_cs)/mean(cas_df_time$time_gam_cs)
mean(cas_df_time$time_xgb_nb)/mean(cas_df_time$time_gam_nb)

mean(cas_df_time$time_xgb_cs_train)/mean(cas_df_time$time_gam_cs_train)
mean(cas_df_time$time_xgb_nb_train)/mean(cas_df_time$time_gam_nb_train)

### PREDS


### PREDS



###

ScoreFunc <- function(PIx,Sx,mx){
  Rx = Sx/PIx
  n = length(PIx)
  origorder = (1:n)
  PSRmat <- data.frame(cbind(PIx,Sx,Rx,mx,origorder))
  PSRmatOrder <- PSRmat[order(Rx),]  #  Sort by relativity
  #  PREMIUM, LOSS DFs
  DFPrem = cumsum(PSRmatOrder$PIx)/sum(PIx)
  DFLoss = cumsum(PSRmatOrder$mx)/sum(mx)
  Bound = PSRmatOrder$Rx*DFPrem*(sum(PIx)/sum(mx))
  #  Bound = ((1:n)/n)*DFPrem*(sum(PIx)/sum(mx))
  #  GINI CALC
  DFPremdiff = DFPrem[2:n]-DFPrem[1:(n-1)]
  DFPremavg  = (DFPrem[2:n]+DFPrem[1:(n-1)])/2
  DFLossavg  = (DFLoss[2:n]+DFLoss[1:(n-1)])/2
  (Gini = 2*crossprod(DFPremdiff,DFPremavg-DFLossavg))
  #  PROJECTION CALC
  meany = mean(mx)
  meanpi = mean(PIx)
  h1 = 0.5* (mean(mx)*PSRmatOrder$PIx*DFLoss +
               PSRmatOrder$mx*meanpi*(1-DFPrem) )
  #  STANDARD ERROR CALC
  h1bar = mean(h1)
  sigmah = var(h1)
  sigmahy = cov(h1,PSRmatOrder$mx)
  sigmahpi = cov(h1,PSRmatOrder$PIx)
  sigmay = var(mx)
  sigmapi = var(PIx)
  sigmaypi = cov(PSRmatOrder$mx,PSRmatOrder$PIx)
  temp1 = 4*sigmah + (h1bar/meany)^2*sigmay + (h1bar/meanpi)^2*sigmapi -
    4*(h1bar/meany)*sigmahy -4 *(h1bar/meanpi)*sigmahpi +
    2*(h1bar^2/(meany*meanpi))*sigmaypi
  sigmaGini = 4*temp1/(meany^2*meanpi^2)
  stderrGini = sqrt(sigmaGini/n)
  check = var(PIx-Sx)
  Gini = Gini*(check != 0)
  stderrGini = stderrGini*(check != 0)
  h1mat <- data.frame(cbind(h1,PSRmatOrder$origorder))
  colnames(h1mat) = c("h1","origorder")
  h1matOrder <- h1mat[order(h1mat$origorder),]  #  Sort by original order
  Retmat <- data.frame(cbind(DFPrem,DFLoss,Bound))
  RetmatGini<-list(Retmat,Gini,stderrGini,h1matOrder$h1)
  names(RetmatGini)<-c("Retmat","Gini","stderrGini","h1matOrder")
  return(RetmatGini)
}

# Plot Cas
ix_t = grep("cost_xgbt", colnames(nocas_df))
ix_x = setdiff( grep("cost_xgb", colnames(nocas_df)), ix_t)
ix_g = grep("cost_gam", colnames(nocas_df))


perf_t<-with(cas_df, ScoreFunc(PIx = rep(1,dim(cas_df)[1]),Sx = rowMeans(cas_df[,ix_t]), mx = true_amount))
perf_x<-with(cas_df,ScoreFunc(PIx = rep(1,dim(cas_df)[1]),Sx = rowMeans(cas_df[,ix_x]), mx = true_amount))
perf_g<-with(cas_df,ScoreFunc(PIx = rep(1,dim(cas_df)[1]),Sx = rowMeans(cas_df[,ix_g]), mx = true_amount))

posizioni<-findInterval(seq(0,1,.05), perf_t$Retmat$DFPrem)

temp = data.frame(DFPrem=0, DFLoss=0, Bound=0)

giniPlot<-ggplot(data=rbind(temp,perf_t$Retmat[posizioni,])) +
  
  geom_point(aes(x=DFPrem, y=DFLoss), col="Blue") +
  geom_point(aes(x=c(0,perf_x$Retmat$DFPrem[posizioni]),
                   y=c(0,perf_x$Retmat$DFLoss[posizioni])), shape=3, color="Red")+
  geom_point(aes(x=c(0,perf_g$Retmat$DFPrem[posizioni]),
                 y=c(0,perf_g$Retmat$DFLoss[posizioni])), shape=4, color="Green")+
  
  geom_line(aes(x=DFPrem, y=DFLoss), color="Blue") +
  geom_line(aes(x=c(0,perf_x$Retmat$DFPrem[posizioni]),
                  y=c(0,perf_x$Retmat$DFLoss[posizioni])), color="Red") +
  geom_line(aes(x=c(0,perf_g$Retmat$DFPrem[posizioni]),
                y=c(0,perf_g$Retmat$DFLoss[posizioni])), color="Green")+
  
  scale_x_continuous(name="Cumulative share of Insured", limits=c(0,1)) +
  scale_y_continuous(name="Cumulative share of Loss", limits=c(0,1)) +
  geom_abline() +labs(title="CAS Data: Pure Premium Gini Plot")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
  


# giniPlot<-giniPlot + scale_colour_manual(values=c("setosa"="Red", "versicolor"="Green",
#                                                   "virginica"="Blue"))

ggsave(filename = "gini_cas.png",plot=giniPlot)

# Plot NO Cas

ix_t = grep("cost_xgbt", colnames(nocas_df))
ix_x = setdiff( grep("cost_xgb", colnames(nocas_df)), ix_t)
ix_g = grep("cost_gam", colnames(nocas_df))


perf_t<-with(nocas_df, ScoreFunc(PIx = rep(1,dim(nocas_df)[1]),Sx = rowMeans(nocas_df[,ix_t]), mx = true_amount))
perf_x<-with(nocas_df,ScoreFunc(PIx = rep(1,dim(nocas_df)[1]),Sx = rowMeans(nocas_df[,ix_x]), mx = true_amount))
perf_g<-with(nocas_df,ScoreFunc(PIx = rep(1,dim(nocas_df)[1]),Sx = rowMeans(nocas_df[,ix_g]), mx = true_amount))

posizioni<-findInterval(seq(0,1,.05), perf_t$Retmat$DFPrem)

temp = data.frame(DFPrem=0, DFLoss=0, Bound=0)

giniPlot<-ggplot(data=rbind(temp,perf_t$Retmat[posizioni,])) +
  
  geom_point(aes(x=DFPrem, y=DFLoss), col="Blue") +
  geom_point(aes(x=c(0,perf_x$Retmat$DFPrem[posizioni]),
                 y=c(0,perf_x$Retmat$DFLoss[posizioni])), shape=3, color="Red")+
  geom_point(aes(x=c(0,perf_g$Retmat$DFPrem[posizioni]),
                 y=c(0,perf_g$Retmat$DFLoss[posizioni])), shape=4, color="Green")+
  
  geom_line(aes(x=DFPrem, y=DFLoss), color="Blue") +
  geom_line(aes(x=c(0,perf_x$Retmat$DFPrem[posizioni]),
                y=c(0,perf_x$Retmat$DFLoss[posizioni])), color="Red") +
  geom_line(aes(x=c(0,perf_g$Retmat$DFPrem[posizioni]),
                y=c(0,perf_g$Retmat$DFLoss[posizioni])), color="Green")+
  
  scale_x_continuous(name="Cumulative share of Insured", limits=c(0,1)) +
  scale_y_continuous(name="Cumulative share of Loss", limits=c(0,1)) +
  geom_abline() +labs(title="Private Data: Pure Premium Gini Plot")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))





ggsave(filename = "gini_nocas.png",plot=giniPlot)

#


normalizedGini(aa = nocas_df$true_amount , pp = rowMeans(nocas_df[,ix_t])) # 0.3891004
normalizedGini(aa = nocas_df$true_amount , pp = rowMeans(nocas_df[,ix_x])) # 0.4339567
normalizedGini(aa = nocas_df$true_amount , pp = rowMeans(nocas_df[,ix_g])) # 0.3255676

normalizedGini(aa = cas_df$true_amount , pp = rowMeans(cas_df[,ix_t])) # 0.2547678
normalizedGini(aa = cas_df$true_amount , pp = rowMeans(cas_df[,ix_x])) # 0.2912337
normalizedGini(aa = cas_df$true_amount , pp = rowMeans(cas_df[,ix_g])) # 0.2348684

