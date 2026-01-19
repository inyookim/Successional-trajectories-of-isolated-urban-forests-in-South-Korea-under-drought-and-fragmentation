## library
pacman::p_load(tidyverse, readxl, dplyr, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, semTools, corrplot)

## read data
setwd("")
alldf <- read_xlsx("./analysis_data.xlsx")
str(alldf)
# succession_pattern: 1 retrogressive, 0 progressive or as usual

# correlation_matrix <- cor(alldf[, -c(1:8)])
# corrplot(correlation_matrix, method = "color", type = "upper",
#          tl.col = "black", tl.srt = 45, # Text label color and rotation
#          addCoef.col = "black", # Add correlation coefficient values
#          col = colorRampPalette(c("blue", "white", "red"))(200),
#          title = "Multicollinearity Plot (Correlation Matrix)",
#          mar = c(0, 0, 1, 0))

# select important column
alldf2 <- alldf %>% 
  dplyr::select(mean_co, mean_no2 , mean_o3, mean_pm10, 
                PDSI_Sum, LST_100buf, TWI, patchsize, population_2kmmean,
                canopy_tree, TVC_tree, 
                succession_pattern)

##remove vars with collinearity using pearson (except factor var)
alldf2_vif <- alookr::treatment_corr(alldf2, 
                                     corr_thres = 0.7, treat = TRUE, verbose = TRUE)

str(alldf2_vif)

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####  
# use rose function for data imbalance
alldf2_vif2 <- ROSE::ROSE(succession_pattern~., data = alldf2_vif, N = 2000, seed=111)$data
summary(alldf2_vif)

range_min <- sapply(alldf2_vif[, -ncol(alldf2_vif)], min, na.rm = TRUE)  # Minimum values
range_max <- sapply(alldf2_vif[, -ncol(alldf2_vif)], max, na.rm = TRUE)  # Maximum values

# Identify rows in alldf2_vif2 that exceed the ranges
rows_to_keep <- apply(alldf2_vif2[, -ncol(alldf2_vif2)], 1, function(row) {
  all(row >= range_min) # & row <= range_max
})

# Filter the rows based on the range condition 
alldf2_vif2_filtered <- alldf2_vif2[rows_to_keep, ]
alldf2_vif2_filteredvarscale <- scale(alldf2_vif2_filtered[,c(-8)]) %>% as.data.frame()
alldf2_vif2_filteredvarscale$succession_pattern <- alldf2_vif2_filtered$succession_pattern
ls <- alldf2_vif2_filteredvarscale

keep <- function(x) {
  (x >= mean(x) - 3*sd(x)) & 
    (x <= mean(x) + 3*sd(x))  
}
# Selecting these data points with `filter` and assigning them to a new data set
str(ls)
ls_clean <- ls %>% 
  filter(keep(PDSI_Sum) &
           keep(LST_100buf) &
           keep(TWI) &
           keep(patchsize) &
           keep(population_2kmmean) &
           keep(canopy_tree) &
           keep(TVC_tree) &
           keep(succession_pattern))

# mardiaSkew(ls_clean, use = "everything")
# mardiaKurtosis(ls_clean, use = "everything")
table(ls$succession_pattern)
ls_clean <- ls_clean %>%
  mutate(succession_pattern = factor(succession_pattern,
                                     levels = c("0", "1"),
                                     labels = c("non_retro", "retrogresive")))
str(ls_clean)
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
## SEM
## final model structure and variables with best model fit

model  <- "
ENV1 =~ PDSI_Sum + patchsize
FS =~ canopy_tree + TVC_tree
FS ~ ENV1
succession_pattern ~ FS
succession_pattern ~ ENV1
"

fit <- sem(model, data = ls_clean, ordered="succession_pattern")
summary(fit, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled"))
fitMeasures(fit, c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled"))
fitMeasures(fit, c("cfi.scaled", "srmr"))

standardizedsolution(fit, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "=~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)

parameterEstimates(fit, standardized=TRUE, rsquare = TRUE) %>% 
  filter(op == "r2") %>% 
  select(Item=rhs, R2 = est) 

standardizedsolution(fit, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)


