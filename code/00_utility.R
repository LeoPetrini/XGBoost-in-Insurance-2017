stringaSpline<-function(var) {
  out<-paste("s(",var,")",sep="")
  return(out)
}

normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) 
    accum.losses <- temp.df$actual / total.losses
    gini.sum <- cumsum(accum.losses - null.losses) 
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}

prepare_db_xgboost<-function(df,x_vars, y_var, offset_var, weight_var, na_code) {
  #force df as data frame
  df<-as.data.frame(df)
  previous_na_action <- options('na.action')
  options(na.action='na.pass')
  
  #definisco le variabili da prendere
  supplementaryVars<-character()
  if (!missing(offset_var)) supplementaryVars<-c(supplementaryVars,offset_var)
  if (!missing(weight_var)) supplementaryVars<-c(supplementaryVars,weight_var)
  
  vars2Keep<-c(x_vars,y_var,supplementaryVars)
  
  df<-df[,vars2Keep]
  
  # Matrici sparse
  sparse_all<-sparse.model.matrix(object = ~.-1., data = df)
  options(na.action=previous_na_action$na.action)
  
  # only predictors cols
  predictors_cols<-setdiff(colnames(sparse_all),c(y_var,supplementaryVars))
  
  #creo la xgbmatrix, eventualmente ponderata / na
  
  if (!missing(na_code)) {
    if (missing(weight_var)) {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var], missing = na_code )
    } else {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var],weight = sparse_all[,weight_var], missing = na_code )
    }
  } else {
    if (missing(weight_var)) {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var])
    } else {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var],weight = sparse_all[,weight_var])
    }
  }
  
  # adding possible offset
  
  if(!missing(offset_var)) {
    setinfo(db_xgb_out,"base_margin",sparse_all[,offset_var])
  }
  
  return(db_xgb_out)
}


xgb_bayes_NB <- function(min_child_weight, max_depth, subsample, colsample_bytree, gamma, alpha, lambda, max_delta_step) {
  
  fit_round <- xgb.cv(params = list(max_depth = max_depth, subsample = subsample, colsample_bytree = colsample_bytree,
                                    min_child_weight=min_child_weight,gamma=gamma, alpha=alpha,  lambda = lambda,
                                    max_delta_step=max_delta_step, booster = "gbtree", eta=0.1),
                      data = full_NB_xgb, nround = rounds, objective = "count:poisson",
                      eval_metric="poisson-nloglik", early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE,
                      nfold=5, prediction = TRUE)
  
  predictions_round<- fit_round$pred
  
  metric_round <- min(fit_round$evaluation_log$test_poisson_nloglik_mean)
  
  list(Score = -metric_round, Pred = predictions_round)
}

xgb_bayes_CS <- function(min_child_weight, max_depth, subsample, colsample_bytree,gamma, alpha, lambda, max_delta_step) {
  
  fit_round <- xgb.cv(params = list(max_depth = max_depth, subsample = subsample, colsample_bytree = colsample_bytree,
                                    min_child_weight=min_child_weight,gamma=gamma, alpha=alpha, lambda = lambda,
                                    max_delta_step=max_delta_step, booster = "gbtree", eta=0.1),
                      data = full_CS_xgb, nround = rounds, objective = "reg:gamma",eval_metric="rmse",
                      early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE, 
                      nfold=5, prediction = TRUE)
  
  predictions_round<- fit_round$pred
  
  metric_round <- min(fit_round$evaluation_log$test_rmse_mean)
  
  list(Score = -metric_round, Pred = predictions_round)
}

xgb_bayes_tweedie <- function(min_child_weight, max_depth, subsample, colsample_bytree,gamma, alpha, lambda, max_delta_step) {
  
  fit_round <- xgb.cv(params = list(max_depth = max_depth, subsample = subsample, colsample_bytree = colsample_bytree,
                                    min_child_weight=min_child_weight,gamma=gamma, alpha=alpha, lambda = lambda,
                                    max_delta_step=max_delta_step, booster = "gbtree", eta=0.1),
                      data = full_tweedie_xgb, nround = rounds, objective = "reg:tweedie", eval_metric="tweedie-nloglik@1.5",
                      early_stopping_rounds = 20, maximize = FALSE, verbose = TRUE, 
                      nfold=5, prediction = TRUE)
  
  predictions_round<- fit_round$pred
  
  metric_round <- min(fit_round$evaluation_log$`test_tweedie_nloglik@1.5_mean`)
  
  list(Score = -metric_round, Pred = predictions_round)
}


scoring_gam_xgb_offset<-function(db, xvarCat, xvarCont,  yvarNB, yvarCS,
                                 yvar_tweedie, offset, weight_tweedie, xgb_CS, xgb_NB, xgb_tweedie, nfolds=10){
  
  pred_numeric_spline <- paste(as.character(sapply(X = xvarCont,stringaSpline)),collapse = "+")
  pred_factor_formula <- paste(as.character(xvarCat),collapse = "+")
  bam_formula_rhs <- paste(pred_numeric_spline,pred_factor_formula,sep =  "+")
  bam_formula_NB <- paste(yvarNB,"~",bam_formula_rhs,"+offset(",offset,")",sep="")
  bam_formula_CS <- paste(yvarCS,bam_formula_rhs,sep="~")
  
  #Randomly shuffle the data
  
  db_severity = filter_(db , paste(yvarCS, ">",0) )
  
  set.seed(2017)
  trainCS <- db_severity[sample(nrow(db_severity)),]
  set.seed(2017)
  trainNB <- db[sample(nrow(db)),]
  
  #Create 10 equally size folds
  foldsCS <- cut(seq(1,nrow(trainCS)), breaks=nfolds, labels=FALSE)
  foldsNB <- cut(seq(1,nrow(trainNB)), breaks=nfolds, labels=FALSE)
  
  cum_rmse = 0
  cum_logloss = 0
  cum_gini = 0
  preds_full = c(seq(0,nrow(trainNB)))
  
  cum_gini_xgb = 0
  cum_rmse_xgb = 0
  cum_logloss_xgb = 0
  preds_full_xgb = c(seq(0,nrow(trainNB)))
  
  cum_gini_xgbt = 0
  preds_full_xgbt = c(seq(0,nrow(trainNB)))
  
  xvars <- c(xvarCat, xvarCont)
  
  full_NB_xgb <- prepare_db_xgboost(trainNB, xvars, yvarNB, offset_var= offset)
  full_CS_xgb <- prepare_db_xgboost(trainCS, xvars, yvarCS)
  full_tweedie_xgb <- prepare_db_xgboost(trainNB, xvars, yvar_tweedie, weight_var = weight_tweedie)
  
  df <- data.frame(time_xgb_tweedie = numeric(), time_xgb_nb = numeric(), time_xgb_cs = numeric(),
                   time_gam_nb = numeric(), time_gam_cs = numeric(),
                   time_xgb_tweedie_train = numeric(), time_xgb_nb_train = numeric(), time_xgb_cs_train = numeric(), 
                   time_gam_cs_train = numeric(), time_gam_nb_train = numeric(),
                   
                   rmse_temp = numeric(), rmse_temp_xgb = numeric(),
                   logloss_temp = numeric(), logloss_temp_xgb = numeric(),
                   gini_temp = numeric(), gini_temp_xgb = numeric(),
                   gini_temp_xgbt = numeric())
  
  df_preds <- data.frame(true_amount= trainNB[[yvarCS]], true_nb = trainNB[[yvarNB]], offset = trainNB[[offset]])
  
  #Perform 10 fold cross validation
  for(i in 1:nfolds){
    
    cat("Fold ",i," initiation . . .","\n")
    
    #Fold Segmentation 
    ix_CS <- which(foldsCS==i,arr.ind=TRUE)
    ix_NB <- which(foldsNB==i,arr.ind=TRUE)
    
    testDataCS <- trainCS[ix_CS, ]
    trainDataCS <- trainCS[-ix_CS, ]
    
    testDataNB <- trainNB[ix_NB, ]
    trainDataNB <- trainNB[-ix_NB, ]
    
    trainNB_xgb <- prepare_db_xgboost(trainDataNB, xvars, yvarNB, offset)
    oofNB_xgb <- prepare_db_xgboost(testDataNB, xvars, yvarNB)
    
    trainCS_xgb <- prepare_db_xgboost(trainDataCS, xvars, yvarCS)
    oofCS_xgb <- prepare_db_xgboost(testDataCS, xvars, yvarCS)
    
    train_tweedie_xgb <- prepare_db_xgboost(trainDataNB, xvars, yvar_tweedie, weight_var = weight_tweedie)
    oof_tweedie_xgb <- prepare_db_xgboost(testDataNB, xvars, yvar_tweedie, weight_var = weight_tweedie)
    
    watchlistNB <- list(train = trainNB_xgb, test = oofNB_xgb)
    watchlistCS <- list(train = trainCS_xgb, test = oofCS_xgb)
    watchlist_tweedie <- list(train = train_tweedie_xgb, test = oof_tweedie_xgb)
    
    # XGBoost model training
    
    print("Training XGB_CS")
    
    ptm <- proc.time()
    XGB_CS <- xgb.train(params = list(subsample = xgb_CS$params$subsample,
                                      colsample_bytree = xgb_CS$params$colsample_bytree,
                                      max_depth = xgb_CS$params$max_depth,
                                      min_child_weight= xgb_CS$params$min_child_weight,
                                      gamma= xgb_CS$params$gamma,
                                      lambda= xgb_CS$params$lambda,
                                      alpha= xgb_CS$params$alpha,
                                      max_delta_step= xgb_CS$params$max_delta_step,
                                      booster = "gbtree",  eta = xgb_CS$params$eta ),
                        data = trainCS_xgb, watchlist = watchlistCS , nround = xgb_CS$best_iteration, print_every_n = 100,
                        objective = "reg:gamma", eval_metric="rmse",
                        early_stopping_rounds = 20, maximize = FALSE, verbose = FALSE)
    
    time_xgb_cs_train <- proc.time() - ptm
    time_xgb_cs_train
    
    print("Training XGB_NB")
    
    ptm <- proc.time()
    XGB_NB<- xgb.train(params = list(subsample = xgb_NB$params$subsample,
                                     colsample_bytree = xgb_NB$params$colsample_bytree,
                                     max_depth = xgb_NB$params$max_depth,
                                     min_child_weight= xgb_NB$params$min_child_weight,
                                     gamma= xgb_NB$params$gamma,
                                     lambda= xgb_NB$params$lambda,
                                     alpha= xgb_NB$params$alpha,
                                     max_delta_step= xgb_NB$params$max_delta_step,
                                     booster = "gbtree",  eta = xgb_NB$params$eta ),
                       data = trainNB_xgb, watchlist = watchlistNB, nround = xgb_NB$best_iteration,  print_every_n = 100,
                       objective = "count:poisson", eval_metric="poisson-nloglik",
                       early_stopping_rounds = 20, maximize = FALSE, verbose = FALSE)
    
    time_xgb_nb_train <- proc.time() - ptm
    time_xgb_nb_train
    
    print("Training XGB_tweedie")
    
    ptm <- proc.time()
    XGB_tweedie<- xgb.train(params = list(subsample = xgb_tweedie$params$subsample,
                                          colsample_bytree = xgb_tweedie$params$colsample_bytree,
                                          max_depth = xgb_tweedie$params$max_depth,
                                          min_child_weight= xgb_tweedie$params$min_child_weight,
                                          gamma= xgb_tweedie$params$gamma,
                                          lambda= xgb_tweedie$params$lambda,
                                          alpha= xgb_tweedie$params$alpha,
                                          max_delta_step= xgb_tweedie$params$max_delta_step,
                                          booster = "gbtree",  eta = xgb_tweedie$params$eta ),
                            data = train_tweedie_xgb, watchlist = watchlist_tweedie, nround = xgb_tweedie$best_iteration,  print_every_n = 100,
                            objective = "reg:tweedie", eval_metric="tweedie-nloglik@1.5",
                            early_stopping_rounds = 20, maximize = FALSE, verbose = FALSE)
    
    time_xgb_tweedie_train <- proc.time() - ptm
    time_xgb_tweedie_train
    
    # GAM model training
    
    
    test_CS_GAM = testDataCS[,c(yvarCS,xvarCat, xvarCont), with=FALSE] 
    train_CS_GAM = trainDataCS[,c(yvarCS,xvarCat, xvarCont), with=FALSE] 
    test_NB_GAM = testDataNB[,c(yvarNB,xvarCat, xvarCont, offset), with=FALSE] 
    train_NB_GAM = trainDataNB[,c(yvarNB,xvarCat, xvarCont, offset), with=FALSE] 
    
    print("Training GAM_NB")
    
    ctrl <- list(nthreads=7)
    cl <- makeCluster(7)
    
    ptm <- proc.time()
    
    gamNB<-bam(formula = as.formula(bam_formula_NB),data = train_NB_GAM, family = poisson(link="log"),
               chunk.size=10000, cluster=cl, select = TRUE)
    
    time_gam_nb_train <- proc.time() - ptm
    time_gam_nb_train
    
    stopCluster(cl)
    
    ctrl <- list(nthreads=7)
    cl <- makeCluster(7)
    
    ptm <- proc.time()
    
    print("Training GAM_CS")
    
    gamCS<-bam(formula = as.formula(bam_formula_CS),data = train_CS_GAM, family = Gamma(link="log"),
               cluster=cl, select = TRUE)
    
    time_gam_cs_train <- proc.time() - ptm
    time_gam_cs_train
    
    
    # GAM prediction: oof and total data
    print("Predicting GAM")
    
    
    temp_nb = predict(gamNB, type="response", newdata=testDataNB)
    temp_amount = predict(gamCS, testDataCS)
    
    logloss_temp = poissonLogLoss(predicted = temp_nb, actual = testDataNB[[yvarNB]])
    rmse_temp = rmse(temp_amount, testDataCS[[yvarCS]])
    
    cum_logloss = cum_logloss + logloss_temp
    cum_rmse = cum_rmse + rmse_temp
    
    ptm <- proc.time()
    preds_full_amount =  predict(gamCS, trainNB[,-c(yvarCS, yvarNB),with=FALSE])
    time_gam_cs <- proc.time() - ptm
    
    ptm <- proc.time()
    preds_full_nb =  predict(gamNB, type = "response", newdata=trainNB)
    time_gam_nb <- proc.time() - ptm
    
    preds_full = preds_full_amount * preds_full_nb
    
    gini_temp = normalizedGini(aa = trainNB[[yvarNB]] * trainNB[[yvarCS]], pp= preds_full) 
    cum_gini = cum_gini + gini_temp
    
    stopCluster(cl)
    
    # XGB prediction: oof and total data
    print("Predicting XGB")
    
    temp_nb_xgb = predict(XGB_NB, oofNB_xgb)
    temp_amount_xgb = predict(XGB_CS, oofCS_xgb)
    
    logloss_temp_xgb = poissonLogLoss(predicted = temp_nb_xgb, actual = testDataNB[[yvarNB]])
    rmse_temp_xgb = rmse(temp_amount_xgb, trainNB[[yvarCS]])
    
    cum_logloss_xgb = cum_logloss_xgb + logloss_temp_xgb
    cum_rmse_xgb = cum_rmse_xgb + rmse_temp_xgb
    
    ptm <- proc.time()
    preds_full_amount_xgb =  predict(XGB_CS, full_NB_xgb)
    time_xgb_cs <-proc.time() - ptm
    
    ptm <- proc.time()
    preds_full_nb_xgb =  predict(XGB_NB, full_NB_xgb)
    time_xgb_nb <- proc.time() - ptm
    
    preds_full_xgb = preds_full_amount_xgb * preds_full_nb_xgb
    
    gini_temp_xgb = normalizedGini(aa = trainNB[[yvarNB]] * trainNB[[yvarCS]], pp= preds_full_xgb) 
    cum_gini_xgb = cum_gini_xgb + gini_temp_xgb
    
    # XGB prediction: oof and total data (tweedie)
    
    temp_tweedie = predict(XGB_tweedie, oof_tweedie_xgb)
    
    ptm <- proc.time()
    preds_full_tweedie_xgb =  predict(XGB_tweedie, full_tweedie_xgb)
    time_xgb_tweedie <- proc.time() - ptm
    
    gini_temp_xgbt = normalizedGini(aa = trainNB[[yvarCS]], pp= preds_full_tweedie_xgb * trainNB[[weight_tweedie]]) 
    cum_gini_xgbt = cum_gini_xgbt + gini_temp_xgbt
    
    df_temp <- data.frame(time_xgb_tweedie = time_xgb_tweedie[3], time_xgb_nb = time_xgb_nb[3], time_xgb_cs = time_xgb_cs[3],
                          time_gam_nb = time_gam_nb[3], time_gam_cs = time_gam_cs[3],
                          time_xgb_tweedie_train = time_xgb_tweedie_train[3], time_xgb_nb_train = time_xgb_nb_train[3], time_xgb_cs_train = time_xgb_cs_train[3], 
                          time_gam_cs_train = time_gam_cs_train[3], time_gam_nb_train = time_gam_nb_train[3],
                          
                          rmse_temp = rmse_temp, rmse_temp_xgb = rmse_temp_xgb,
                          logloss_temp = logloss_temp, logloss_temp_xgb = logloss_temp_xgb,
                          gini_temp = gini_temp, gini_temp_xgb = gini_temp_xgb,
                          gini_temp_xgbt = gini_temp_xgbt)
    
    df_preds_temp <- data.frame(cost_xgb = preds_full_xgb ,
                                cost_xgbt = preds_full_tweedie_xgb*trainNB[[weight_tweedie]] ,
                                cost_gam =  preds_full )
    
    df <- rbind(df,df_temp)
    df_preds <- cbind(df_preds, df_preds_temp)
    
    cat("FOLD", i, "GAM | XGBoost", "\n",
        "RMSE: ", rmse_temp,"|", rmse_temp_xgb, "\n",
        "LOGLOSS: ", logloss_temp,"|", logloss_temp_xgb, "\n",
        "GINI: ", gini_temp,"|", gini_temp_xgb, "\n",
        "GINI_TWEEDIE: ", "None ","|", gini_temp_xgbt, "\n")
  }
  
  cat("Final GAM Metrics","\n",
      "RMSE: ",cum_rmse/nfolds, "LOGLOSS: ",cum_logloss/nfolds, "GINI :", cum_gini/nfolds, "\n")
  
  cat("Final XGB Metrics","\n",
      "RMSE: ",cum_rmse_xgb/nfolds, "LOGLOSS: ",cum_logloss_xgb/nfolds, "GINI :", cum_gini_xgb/nfolds,
      "GINI_TWEEDIE: ", cum_gini_xgbt/nfolds)
  
  return(list(df, df_preds))
}


