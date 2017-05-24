#load claims and policy data
dbpolicies<-fread(input="./data/PG_2017_YEAR0.csv")
dbclaims<-fread(input="./data/PG_2017_CLAIMS_YEAR0.csv")

#load demographic infos
demography<-read_excel(path="./data/MDB-INSEE-V2.xlsx")

#filters
dbpolicies[pol_usage=='AllTrips',pol_usage:='Professional']
dbpolicies<-dbpolicies[!is.na(vh_age)]
dbclaims<-dbclaims[claim_amount>0]
keyVars<-c("id_client","id_vehicle")

dbpolicies<-as.data.table(dbpolicies, key=keyVars)
dbclaims<-as.data.table(dbclaims, key=keyVars)
dbpolicies<-mutate(dbpolicies,
                   pol_coverage=factor(pol_coverage),
                   pol_pay_freq=factor(pol_pay_freq, levels=c("Yearly","Biannual","Quarterly","Monthly"),ordered=TRUE),
                   pol_usage=factor(pol_usage),
                   drv_drv2=factor(drv_drv2),
                   drv_sex1=factor(drv_sex1),
                   vh_fuel=factor(vh_fuel),
                   vh_make_new=fct_lump(vh_make,n=10),
                   vh_make_new=fct_relevel(vh_make_new,"RENAULT"),
                   vh_type=factor(vh_type,levels = c("Tourism","Commercial")),
                   exposure=1
)


xvarCont<-c("pol_bonus","pol_duration","drv_age1","drv_age_lic1","vh_age","vh_cyl","vh_din","vh_speed","vh_value","vh_weight")
xvarCat<-c("pol_coverage","pol_pay_freq","pol_payd","pol_usage","drv_sex1","vh_make_new","vh_type")

dbclaimsFull<-dbclaims[dbpolicies[,c(keyVars,xvarCat,xvarCont),with=FALSE],on=keyVars,nomatch=0L]
dbclaimsFull<-mutate(dbclaimsFull,
                     id_policy=paste( id_client,id_vehicle,sep="-"))

temp=dbclaims[,.(claim_nb=sum(claim_nb), claim_amount=sum(claim_amount)),by=keyVars]

db4ModelFull<-left_join(x=dbpolicies,y=temp, by=keyVars)
db4ModelFull<-db4ModelFull[is.na(claim_nb),claim_nb:=0]
db4ModelFull<-db4ModelFull[is.na(claim_amount),claim_amount:=0]

# uniqueKeys<-unique(db4ModelFull$id_policy)
# trainKeys<-sample(x=uniqueKeys,size = 80000,replace = FALSE)
# temp<-setdiff(uniqueKeys,trainKeys)
# validKeys<-temp[1:10000]
# testKeys<-setdiff(temp, validKeys)

# save(list=c("db4ModelFull","dbclaimsFull","trainKeys","testKeys","validKeys","xvarCont","xvarCat"),file="./data/datasets.RData")
save(list=c("db4ModelFull","dbclaimsFull","xvarCont","xvarCat"),file="./data/datasets2.RData")


