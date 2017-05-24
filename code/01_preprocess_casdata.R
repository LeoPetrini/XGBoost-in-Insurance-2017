data(freMTPLfreq)
data(freMTPLsev)

sum(freMTPLsev$PolicyID %in% freMTPLfreq$PolicyID)
sum(freMTPLfreq$ClaimNb)

#filters
keyVars<-c("PolicyID")

dbpolicies<-as.data.table(freMTPLfreq, key=keyVars)
dbclaims<-as.data.table(freMTPLsev, key=keyVars)
#creo il claim amt capped

pct99=quantile(dbclaims$ClaimAmount,probs = .99)
excessRatio=sum(dbclaims$ClaimAmount)/sum(pmin(pct99,dbclaims$ClaimAmount))
dbclaims<-mutate(dbclaims,
                 claimAmtCpd=pmin(pct99,ClaimAmount)*excessRatio
                 )


dbclaims_grouped = group_by(dbclaims, PolicyID) %>% 
  summarise(ClaimAmount = sum(ClaimAmount), claim_nb = n(), claimAmtCpd = sum(claimAmtCpd))


dbpolicies<-mutate(dbpolicies, log_exposure = log(Exposure))

xvarCont<-c("CarAge", "DriverAge" ,"Density")
xvarCat<-c("Power", "Brand", "Gas", "Region")

dbclaims_grouped<-dbclaims_grouped[dbpolicies[,c(keyVars,xvarCat,xvarCont), with=FALSE], on=keyVars, nomatch=0L]

dbtweedie <- left_join(x =dbpolicies  , y = dbclaims_grouped, by = keyVars)


dbtweedie[is.na(dbtweedie)] <- 0

dbtweedie <- mutate(dbtweedie, tweedie_cost=claimAmtCpd/Exposure)


save(list=c("dbclaims_grouped","dbpolicies", "dbtweedie", "xvarCont","xvarCat"),file="./data/datasets_casdata.RData")


