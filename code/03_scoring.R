#--------------------------------
# Spline GLM (GAM) and XGBOost scoring
#--------------------------------

workDir<-  "/home/rstudio/Actuarial Pricing Game"
setwd(workDir)

# Load Inputs: 
source('./code/03_init_scoring.R')
load(file = "./models/xgb_CS_casdata.RData")
load(file = "./models/xgb_NB_casdata.RData")
load(file = "./models/xgb_tweedie_casdata.RData")

load(file = "./models/xgb_CS.RData")
load(file = "./models/xgb_NB.RData")
load(file = "./models/xgb_tweedie.RData")

# Scoring using CASDataset

output_cas <- scoring_gam_xgb_offset(db = dbtweedie, xvarCat = xvarCat, xvarCont = xvarCont,
                       yvarNB = "ClaimNb", yvarCS = "claimAmtCpd", 
                       yvar_tweedie ="tweedie_cost", offset ="log_exposure",
                       weight_tweedie = "Exposure" , xgb_CS = xgb_NB_casdata ,
                       xgb_NB = xgb_NB_casdata, xgb_tweedie = xgb_tweedie_casdata , nfolds=10)


df1 = output_cas[[1]]
df2 = output_cas[[2]]
write.csv(df1, "cas_df_time.csv")
write.csv(df2, "cas_df_preds.csv")


# Final GAM Metrics 
# RMSE:  3852.589 LOGLOSS:  0.1632816 GINI : 0.2413772 
# Final XGB Metrics 
# RMSE:  1980.291 LOGLOSS:  0.1665413 GINI : 0.302344 GINI_TWEEDIE:  0.2527188


# Scoring using our data

db4ModelFull$log_exposure =  log(db4ModelFull$exposure)
output_noncas <- scoring_gam_xgb_offset(db = db4ModelFull, xvarCat = xvarCat, xvarCont = xvarCont,
                                 yvarNB = "claim_nb", yvarCS = "claim_amount", 
                                 yvar_tweedie ="claim_amount", offset ="log_exposure",
                                 weight_tweedie = "exposure" , xgb_CS = xgb_NB,
                                 xgb_NB = xgb_NB, xgb_tweedie = xgb_tweedie , nfolds=10)

df1_noncas = output_noncas[[1]]
df2_noncas = output_noncas[[2]]
write.csv(df1_noncas, "noncas_df_time.csv")
write.csv(df2_noncas, "noncas_df_preds.csv")

# Final GAM Metrics 
# RMSE:  3852.589 LOGLOSS:  0.1632816 GINI : 0.2413772 
# Final XGB Metrics 
# RMSE:  1980.291 LOGLOSS:  0.1665413 GINI : 0.302344 GINI_TWEEDIE:  0.2527188

