## library
pacman::p_load(dplyr, readxl, ggplot2, tidyr, readxl, leaps, ranger, Boruta, DALEX, randomForest, caret, e1071)

setwd("E:/2024/도시숲2/review")
alldf <- read_xlsx("./data_Inyoo_eng.xlsx")
str(alldf)

# select important column
alldf2 <- alldf %>% 
  dplyr::select(mean_co, mean_no2 , mean_o3, mean_pm10, 
                PDSI_Sum, LST_100buf, TWI, patchsize, population_2kmmean,
                canopy_tree, TVC_tree, 
                succession_pattern)

##remove vars with colinearity using pearson (except factor var)
str(alldf2)
alldf2_vif <- alookr::treatment_corr(alldf2, 
                                     corr_thres = 0.7, treat = TRUE, verbose = TRUE)

str(alldf2_vif)

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####  
# use rose function for data imbalance
# install.packages("ROSE")
alldf2_vif2 <- ROSE::ROSE(succession_pattern~., data = alldf2_vif, N = 2000, seed=111)$data
str(alldf2_vif2)
summary(alldf2_vif)

range_min <- sapply(alldf2_vif[, -ncol(alldf2_vif)], min, na.rm = TRUE)  # Minimum values
range_max <- sapply(alldf2_vif[, -ncol(alldf2_vif)], max, na.rm = TRUE)  # Maximum values

# Identify rows in alldf2_vif2 that exceed the ranges
rows_to_keep <- apply(alldf2_vif2[, -ncol(alldf2_vif2)], 1, function(row) {
  all(row >= range_min) # & row <= range_max
})

# Filter the rows based on the range condition
alldf2_vif2_filtered <- alldf2_vif2[rows_to_keep, ]

alldf2_vif2_filtered <- alldf2_vif2_filtered %>%
  mutate(succession_pattern = factor(succession_pattern,
                                     levels = c("0", "1"),
                                     labels = c("non_retro", "retrogresive")))

str(alldf2_vif2_filtered)
table(alldf2_vif2_filtered$succession_pattern)

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####  
# RF var imp -> change function param to final RF
var.sel.boruta <- function(x, y,
                           pValue = 0.01,
                           maxRuns = 100,
                           ntree = 2000,
                           mtry.prop = 0.2,
                           nodesize.prop = 0.1,
                           no.threads = 1,
                           method = "ranger",
                           type = "classification",
                           importance = "impurity_corrected",
                           case.weights = NULL) {
  
  ## variable selection using Boruta function
  ## ----------------------------------------
  # modified version of getImpRfRaw function to enable user defined mtry
  # values
  get.imp.r.f.raw.mtry <- function(x, y, ...) {
    rf <- wrapper.rf(x = x, y = y, ...)
    return(rf$variable.importance)
  }
  
  res.boruta = Boruta::Boruta(x = x, y = y,
                              pValue = pValue, maxRuns = maxRuns,
                              ntree = ntree, nodesize.prop = nodesize.prop,
                              no.threads = no.threads,mtry.prop = mtry.prop,
                              getImp = get.imp.r.f.raw.mtry,
                              importance = importance,
                              type = type,
                              case.weights = case.weights)
  
  
  
  return(res.boruta)
  
}


wrapper.rf <- function(x, y, ntree = 2000,
                       mtry.prop = 0.2,
                       nodesize.prop = 0.1,
                       no.threads = 1,
                       method = "ranger",
                       type = "classification",
                       importance = "impurity_corrected",
                       case.weights = NULL, ...) {
  
  ## check data
  if (length(y) != nrow(x)) {
    stop("length of y and number of rows in x are different")
  }
  
  if (any(is.na(x))) {
    stop("missing values are not allowed")
  }
  
  if (type %in% c("probability", "regression") & (is.character(y) | is.factor(y))) {
    stop("only numeric y allowed for probability or regression mode")
  }
  
  ## set global parameters
  nodesize = floor(nodesize.prop * nrow(x))
  mtry = floor(mtry.prop * ncol(x))
  if (mtry == 0) mtry = 1
  
  if (type == "classification") {
    #    print("in classification")
    y = as.factor(y)
  }
  
  ## run RF
  if (method == "ranger") {
    if (type == "probability") {
      y = as.factor(y)
      prob = TRUE
    } else {
      prob = FALSE
    }
    
    rf = ranger::ranger(data = data.frame(y, x),
                        dependent.variable.name = "y",
                        probability = prob,
                        importance = importance,
                        scale.permutation.importance = FALSE,
                        num.trees = ntree,
                        mtry = mtry,
                        case.weights = case.weights,
                        min.node.size = nodesize,
                        num.threads = no.threads,
                        write.forest = TRUE,
                        ...)
  } else {
    stop(paste("method", method, "undefined. Use 'ranger'."))
  }
  
  return(rf)
}

str(alldf2_vif2_filtered)
boruta_pre <- var.sel.boruta(x = alldf2_vif2_filtered[, -8], y = alldf2_vif2_filtered[, 8], type = "classification") #regression
plot(boruta_pre, las = 2, cex.axis = 0.8)

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####  
## screen & select variables based on the importance
alldf2_viffin <- alldf2_vif2_filtered %>% dplyr::select(PDSI_Sum, patchsize,
                                                        succession_pattern)

boruta <- var.sel.boruta(x = alldf2_viffin[, -3], y = alldf2_viffin[, 3], type = "classification") #regression
plot(boruta, las = 2, cex.axis = 0.8)
boruta_fd <- data.frame(boruta$finalDecision)
boruta_fd_fin <- boruta_fd %>% filter(boruta.finalDecision == "Confirmed")
(rownames(boruta_fd_fin))

set.seed(111)
train_idx <- sample(nrow(alldf2_viffin), nrow(alldf2_viffin)*0.7, replace = T)
train <- alldf2_viffin[train_idx,]
test <- alldf2_viffin[-train_idx,]
nrow(train)

tuneGrid <- expand.grid(.mtry = 2)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

rf_default <- train(succession_pattern~.,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl,
                    tuneGrid = tuneGrid,
                    ntree = 2000)

print(rf_default)
rf_default$finalModel

prediction <-predict(rf_default, test)
optimal_conf_mat <- confusionMatrix(prediction, test$succession_pattern)
optimal_conf_mat

# Extract precision, recall, and F1 score
optimal_precision <- optimal_conf_mat$byClass['Pos Pred Value']
optimal_recall <- optimal_conf_mat$byClass['Sensitivity']
optimal_f1_score <- 2 * (optimal_precision * optimal_recall) / (optimal_precision + optimal_recall)

print(paste("Optimal Precision:", optimal_precision))
print(paste("Optimal Recall:", optimal_recall))
print(paste("Optimal F1 Score:", optimal_f1_score))

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

str(train)
explainer_rf <- DALEX::explain(model = rf_default,
                               data = train[,-3],
                               y = as.numeric(train$succession_pattern)-1)

## non-numeric value rejected
model_profile <- model_profile(explainer_rf, type= "accumulated", 
                               N = NULL,
                               variables =c("PDSI_Sum", "patchsize"), #, "population_2kmmean"
                               conf_level = 0.95)

agr_profiles <- model_profile$agr_profiles
agr_profiles <- model_profile$agr_profiles %>%
  rename(
    variable = `_vname_`,
    label = `_label_`,
    x_value = `_x_`,
    yhat = `_yhat_`,
    ids = `_ids_`
  )

str(agr_profiles)

ggplot(agr_profiles, aes(x = x_value, y = yhat, color = label)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = yhat - 0.05, ymax = yhat + 0.05, fill = label), alpha = 0.2, color = NA) + # Adjust CI bounds
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Accumulated Dependence Profiles with 95% Confidence Intervals",
    x = "Variable Value",
    y = "Prediction"
  )

ggplot(agr_profiles, aes(x = x_value, y = yhat)) +
  geom_line(size = 1) +  # Original accumulated profiles
  geom_smooth(method = "lm", se = TRUE,  alpha = 0.2, linetype = "dashed") +  # Linear regression line with CI
  facet_wrap(~ variable, scales = "free_x") +
  coord_cartesian(ylim = c(0.1, NA)) +  # Cuts the y-axis below 0
  theme_minimal() +
  labs(
    title = "Accumulated Dependence Profiles with Regression Lines and 95% Confidence Intervals",
    x = "Variable Value",
    y = "Prediction"
  ) +
  theme(
    legend.position = "bottom"
  )
