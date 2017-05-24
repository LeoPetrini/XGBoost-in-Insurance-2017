#--------------------------------
# Setting up
#--------------------------------
db4ModelFull$log_exposure =  log(db4ModelFull$exposure)

yvarNB = "claim_nb"
yvarCS = "claim_amount"
offset = "log_exposure"

yvar_tweedie = "claim_amount" # exposure is 1
weight_tweedie = "exposure"

xvars = c(xvarCont, xvarCat)

dbtweedie = db4ModelFull
dbpolicies = db4ModelFull
dbclaims_grouped = filter_(db4ModelFull , paste(yvarCS, ">",0) )

full_NB_xgb <- prepare_db_xgboost(dbtweedie, xvars, yvarNB, offset_var = offset )
full_CS_xgb <- prepare_db_xgboost(dbclaims_grouped, xvars, yvarCS)
full_tweedie_xgb <- prepare_db_xgboost(dbtweedie, xvars, yvar_tweedie, weight_var = weight_tweedie)

#--------------------------------
# Bayesian Grid Configuration
#--------------------------------

Bayes_kappa <-  4
init_bayes <- 20
iter_bayes <- 60
rounds <- 700
rounds_eta <- 2000

xgb_NB_bounds_list <- list(min_child_weight= c(0L, 50L),
                           max_depth= c(6L, 14L), 
                           gamma= c(0, 1),
                           alpha=c(0, 1),
                           lambda = c(0, 4),
                           subsample=c(0.4,0.95), 
                           colsample_bytree=c(0.5,1),
                           max_delta_step=c(0,5))


xgb_CS_bounds_list <- list(min_child_weight= c(0L, 100L),
                           max_depth= c(4L, 16L), 
                           gamma= c(0, 1),
                           alpha=c(0, 1),
                           lambda = c(0, 4),
                           subsample=c(0.4,0.95), 
                           colsample_bytree=c(0.5,1),
                           max_delta_step=c(0,1))

xgb_CS_bounds_list <- list(min_child_weight= c(0L, 100L),
                           max_depth= c(4L, 16L), 
                           gamma= c(0, 1),
                           alpha=c(0, 1),
                           lambda = c(0, 4),
                           subsample=c(0.4,0.95), 
                           colsample_bytree=c(0.5,1),
                           max_delta_step=c(0,1))


#--------------------------------
# Parameters Optimisation (Claim Number)
#--------------------------------

Results_bayes_NB <- BayesianOptimization(xgb_bayes_NB, bounds = xgb_NB_bounds_list, init_points = init_bayes,
                                         n_iter = iter_bayes, acq = "ucb", kappa = Bayes_kappa, verbose = TRUE)

train_ix_end =round(nrow(dbpolicies)*0.9)
dbpolicies = dbpolicies[sample(nrow(dbpolicies)),]

train_NB_xgb <- prepare_db_xgboost(dbpolicies[1:train_ix_end,], xvars, yvarNB, offset_var = offset )
val_NB_xgb <- prepare_db_xgboost(dbpolicies[(train_ix_end+1):nrow(dbpolicies),], xvars, yvarNB, offset_var = offset )
watchlistNB <- list(train = train_NB_xgb, test = val_NB_xgb)


XGB_NB_big <- xgb.train(params = list(subsample = Results_bayes_NB$Best_Par["subsample"],
                                      colsample_bytree = Results_bayes_NB$Best_Par["colsample_bytree"],
                                      max_depth = Results_bayes_NB$Best_Par["max_depth"],
                                      min_child_weight= Results_bayes_NB$Best_Par["min_child_weight"],
                                      gamma= Results_bayes_NB$Best_Par["gamma"],
                                      lambda= Results_bayes_NB$Best_Par["lambda"],
                                      alpha= Results_bayes_NB$Best_Par["alpha"],
                                      max_delta_step= Results_bayes_NB$Best_Par["max_delta_step"],
                                      booster = "gbtree",  eta =0.1 ),
                        data = train_NB_xgb, nround = rounds_eta, watchlist = watchlistNB, print_every_n = 100,
                        objective = "count:poisson", eval_metric="poisson-nloglik",
                        early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

XGB_NB_med <- xgb.train(params = list(subsample = Results_bayes_NB$Best_Par["subsample"],
                                      colsample_bytree = Results_bayes_NB$Best_Par["colsample_bytree"],
                                      max_depth = Results_bayes_NB$Best_Par["max_depth"],
                                      min_child_weight= Results_bayes_NB$Best_Par["min_child_weight"],
                                      gamma= Results_bayes_NB$Best_Par["gamma"],
                                      lambda= Results_bayes_NB$Best_Par["lambda"],
                                      alpha= Results_bayes_NB$Best_Par["alpha"],
                                      max_delta_step= Results_bayes_NB$Best_Par["max_delta_step"],
                                      booster = "gbtree",  eta =0.05 ),
                        data = train_NB_xgb, nround = rounds_eta, watchlist = watchlistNB, print_every_n = 300,
                        objective = "count:poisson", eval_metric="poisson-nloglik",
                        early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

XGB_NB_small <- xgb.train(params = list(subsample = Results_bayes_NB$Best_Par["subsample"],
                                        colsample_bytree = Results_bayes_NB$Best_Par["colsample_bytree"],
                                        max_depth = Results_bayes_NB$Best_Par["max_depth"],
                                        min_child_weight= Results_bayes_NB$Best_Par["min_child_weight"],
                                        gamma= Results_bayes_NB$Best_Par["gamma"],
                                        lambda= Results_bayes_NB$Best_Par["lambda"],
                                        alpha= Results_bayes_NB$Best_Par["alpha"],
                                        max_delta_step= Results_bayes_NB$Best_Par["max_delta_step"],
                                        booster = "gbtree",  eta =0.01 ),
                          data = train_NB_xgb, nround = rounds_eta, watchlist = watchlistNB, print_every_n = 600, 
                          objective = "count:poisson", eval_metric="poisson-nloglik",
                          early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

# Choose final setting

iter.big= which.min(XGB_NB_big$evaluation_log$test_poisson_nloglik) 
metric.big= min(XGB_NB_big$evaluation_log$test_poisson_nloglik) 

iter.med= which.min(XGB_NB_med$evaluation_log$test_poisson_nloglik) 
metric.med= min(XGB_NB_med$evaluation_log$test_poisson_nloglik) 

iter.small= which.min(XGB_NB_small$evaluation_log$test_poisson_nloglik) 
metric.small= min(XGB_NB_small$evaluation_log$test_poisson_nloglik) 

# Save final model
best=(as.data.frame(which.min(c("big"=metric.big,"med"=metric.med,"small"=metric.small))))
objectName=paste("xgb",'NB', sep="_")
gridName=paste("bayesGrid", 'NB', sep="_")
File_name=paste("models/", objectName,".RData",sep="")

if(best==1)  assign(x = objectName, value=XGB_NB_big)
if(best==2)  assign(x = objectName, value=XGB_NB_med)
if(best==3)  assign(x = objectName, value=XGB_NB_small)

assign(x=gridName, value=Results_bayes_NB)

save(list=c(objectName,gridName), file=File_name)


#--------------------------------
# Parameters Optimisation (Claim Severity)
#--------------------------------

Results_bayes_CS <- BayesianOptimization(xgb_bayes_CS, bounds = xgb_CS_bounds_list, init_points = init_bayes,
                                         n_iter = iter_bayes, acq = "ucb", kappa = Bayes_kappa, verbose = TRUE)

train_ix_end =round(nrow(dbclaims_grouped)*0.9)
dbclaims_grouped = dbclaims_grouped[sample(nrow(dbclaims_grouped)),]

train_CS_xgb <- prepare_db_xgboost(dbclaims_grouped[1:train_ix_end,], xvars, yvarCS)
val_CS_xgb <- prepare_db_xgboost(dbclaims_grouped[(train_ix_end+1):nrow(dbclaims_grouped),], xvars, yvarCS)
watchlistCS <- list(train = train_CS_xgb, test = val_CS_xgb)

XGB_CS_big <- xgb.train(params = list(subsample = Results_bayes_CS$Best_Par["subsample"],
                                      colsample_bytree = Results_bayes_CS$Best_Par["colsample_bytree"],
                                      max_depth = Results_bayes_CS$Best_Par["max_depth"],
                                      min_child_weight= Results_bayes_CS$Best_Par["min_child_weight"],
                                      gamma= Results_bayes_CS$Best_Par["gamma"],
                                      lambda= Results_bayes_CS$Best_Par["lambda"],
                                      alpha= Results_bayes_CS$Best_Par["alpha"],
                                      max_delta_step= Results_bayes_CS$Best_Par["max_delta_step"],
                                      booster = "gbtree",  eta =0.1 ),
                        data = train_CS_xgb, nround = rounds_eta, watchlist = watchlistCS, print_every_n = 100,
                        objective = "reg:gamma", eval_metric="rmse",
                        early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

XGB_CS_med <- xgb.train(params = list(subsample = Results_bayes_CS$Best_Par["subsample"],
                                      colsample_bytree = Results_bayes_CS$Best_Par["colsample_bytree"],
                                      max_depth = Results_bayes_CS$Best_Par["max_depth"],
                                      min_child_weight= Results_bayes_CS$Best_Par["min_child_weight"],
                                      gamma= Results_bayes_CS$Best_Par["gamma"],
                                      lambda= Results_bayes_CS$Best_Par["lambda"],
                                      alpha= Results_bayes_CS$Best_Par["alpha"],
                                      max_delta_step= Results_bayes_CS$Best_Par["max_delta_step"],
                                      booster = "gbtree",  eta =0.05 ),
                        data = train_CS_xgb, nround = rounds_eta, watchlist = watchlistCS, print_every_n = 300,
                        objective = "reg:gamma", eval_metric="rmse",
                        early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

XGB_CS_small <- xgb.train(params = list(subsample = Results_bayes_CS$Best_Par["subsample"],
                                        colsample_bytree = Results_bayes_CS$Best_Par["colsample_bytree"],
                                        max_depth = Results_bayes_CS$Best_Par["max_depth"],
                                        min_child_weight= Results_bayes_CS$Best_Par["min_child_weight"],
                                        gamma= Results_bayes_CS$Best_Par["gamma"],
                                        lambda= Results_bayes_CS$Best_Par["lambda"],
                                        alpha= Results_bayes_CS$Best_Par["alpha"],
                                        max_delta_step= Results_bayes_CS$Best_Par["max_delta_step"],
                                        booster = "gbtree",  eta =0.01 ),
                          data = train_CS_xgb, nround = rounds_eta, watchlist = watchlistCS, print_every_n = 600, 
                          objective = "reg:gamma", eval_metric="rmse",
                          early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

# Choose final setting

iter.big= which.min(XGB_CS_big$evaluation_log$test_rmse) 
metric.big= min(XGB_CS_big$evaluation_log$test_rmse) 

iter.med= which.min(XGB_CS_med$evaluation_log$test_rmse) 
metric.med= min(XGB_CS_med$evaluation_log$test_rmse) 

iter.small= which.min(XGB_CS_small$evaluation_log$test_rmse) 
metric.small= min(XGB_CS_small$evaluation_log$test_rmse) 

# Save final model
best=(as.data.frame(which.min(c("big"=metric.big,"med"=metric.med,"small"=metric.small))))
objectName=paste("xgb",'CS', sep="_")
gridName=paste("bayesGrid", 'CS', sep="_")
File_name=paste("models/",objectName,".RData",sep="")

if(best==1)  assign(x = objectName, value=XGB_CS_big)
if(best==2)  assign(x = objectName, value=XGB_CS_med)
if(best==3)  assign(x = objectName, value=XGB_CS_small)

assign(x=gridName, value=Results_bayes_CS)

save(list=c(objectName,gridName), file=File_name)

#--------------------------------
# Parameters Optimisation (Tweedie)
#--------------------------------

Results_bayes_tweedie <- BayesianOptimization(xgb_bayes_tweedie, bounds = xgb_CS_bounds_list, init_points = init_bayes,
                                              n_iter = iter_bayes, acq = "ucb", kappa = Bayes_kappa, verbose = TRUE)

train_ix_end =round(nrow(dbtweedie)*0.9)
dbtweedie = dbtweedie[sample(nrow(dbtweedie)),]

train_tweedie_xgb <- prepare_db_xgboost(dbtweedie[1:train_ix_end,], xvars, yvar_tweedie, weight_var = weight_tweedie)
val_tweedie_xgb <- prepare_db_xgboost(dbtweedie[(train_ix_end+1):nrow(dbtweedie),], xvars, yvar_tweedie, weight_var = weight_tweedie)
watchlist_tweedie<- list(train = train_tweedie_xgb, test = val_tweedie_xgb)

XGB_tweedie_big <- xgb.train(params = list(subsample = Results_bayes_tweedie$Best_Par["subsample"],
                                           colsample_bytree = Results_bayes_tweedie$Best_Par["colsample_bytree"],
                                           max_depth = Results_bayes_tweedie$Best_Par["max_depth"],
                                           min_child_weight= Results_bayes_tweedie$Best_Par["min_child_weight"],
                                           gamma= Results_bayes_tweedie$Best_Par["gamma"],
                                           lambda= Results_bayes_tweedie$Best_Par["lambda"],
                                           alpha= Results_bayes_tweedie$Best_Par["alpha"],
                                           max_delta_step= Results_bayes_tweedie$Best_Par["max_delta_step"],
                                           tweedie_variance_power = 1.5,
                                           booster = "gbtree",  eta =0.1 ),
                             data = train_tweedie_xgb, nround = rounds_eta, watchlist = watchlist_tweedie, print_every_n = 100,
                             objective = "reg:tweedie", eval_metric="tweedie-nloglik@1.5",
                             early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

XGB_tweedie_med <- xgb.train(params = list(subsample = Results_bayes_tweedie$Best_Par["subsample"],
                                           colsample_bytree = Results_bayes_tweedie$Best_Par["colsample_bytree"],
                                           max_depth = Results_bayes_tweedie$Best_Par["max_depth"],
                                           min_child_weight= Results_bayes_tweedie$Best_Par["min_child_weight"],
                                           gamma= Results_bayes_tweedie$Best_Par["gamma"],
                                           lambda= Results_bayes_tweedie$Best_Par["lambda"],
                                           alpha= Results_bayes_tweedie$Best_Par["alpha"],
                                           max_delta_step= Results_bayes_tweedie$Best_Par["max_delta_step"],
                                           booster = "gbtree",  eta =0.05 ),
                             data = train_tweedie_xgb, nround = rounds_eta, watchlist = watchlist_tweedie, print_every_n = 300,
                             objective = "reg:tweedie", eval_metric="tweedie-nloglik@1.5",
                             early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

XGB_tweedie_small <- xgb.train(params = list(subsample = Results_bayes_tweedie$Best_Par["subsample"],
                                             colsample_bytree = Results_bayes_tweedie$Best_Par["colsample_bytree"],
                                             max_depth = Results_bayes_tweedie$Best_Par["max_depth"],
                                             min_child_weight= Results_bayes_tweedie$Best_Par["min_child_weight"],
                                             gamma= Results_bayes_tweedie$Best_Par["gamma"],
                                             lambda= Results_bayes_tweedie$Best_Par["lambda"],
                                             alpha= Results_bayes_tweedie$Best_Par["alpha"],
                                             max_delta_step= Results_bayes_tweedie$Best_Par["max_delta_step"],
                                             booster = "gbtree",  eta =0.01 ),
                               data = train_tweedie_xgb, nround = rounds_eta, watchlist = watchlist_tweedie, print_every_n = 600, 
                               objective = "reg:tweedie", eval_metric="tweedie-nloglik@1.5",
                               early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE) 

# Choose final setting

iter.big= which.min(XGB_tweedie_big$best_score) 
metric.big= min(XGB_tweedie_big$best_iteration) 

iter.med= which.min(XGB_tweedie_med$best_score) 
metric.med= min(XGB_tweedie_med$best_iteration) 

iter.small= which.min(XGB_tweedie_small$best_score) 
metric.small= min(XGB_tweedie_small$best_iteration) 

# Save final model
best=(as.data.frame(which.min(c("big"=metric.big,"med"=metric.med,"small"=metric.small))))
objectName=paste("xgb",'tweedie', sep="_")
gridName=paste("bayesGrid", 'tweedie', sep="_")
File_name=paste("models/",objectName,".RData",sep="")

if(best==1)  assign(x = objectName, value=XGB_tweedie_big)
if(best==2)  assign(x = objectName, value=XGB_tweedie_med)
if(best==3)  assign(x = objectName, value=XGB_tweedie_small)

assign(x=gridName, value=Results_bayes_tweedie)

save(list=c(objectName,gridName), file=File_name)

