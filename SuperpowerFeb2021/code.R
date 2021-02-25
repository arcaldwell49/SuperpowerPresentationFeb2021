## Script Information -------------
##
## Script name: Code for Februrary 2021 Superpower Presentation
##
## Purpose of script: Give examples of using 
##
## Author: Dr. Aaron R. Caldwell
##
## Date Created: 2020-02-24
##
## 
## Email: aaron.r.caldwell2.ctr@mail.mil
##
## Notes: All figures should be reproduced. 
##   All data did not make it in the manuscript.
##    Therefore, feel free to explore!
##


library(Superpower)
library(tidyverse)
library(simr)
library(simstudy)

# Three way ANOVA ---------------
# With 2x2x2 designs, 
# the names for paired comparisons can become very long. 
# So here the sample size abbreviate terms
# Size, Color, and Cognitive Load, have values:
# b = big, s = small, g = green, 
# r = red, pres = present, abs = absent.  
labelnames <- c("Size", "b", "s", "x", "Color", "g", "r", 
                "Load", "pres", "abs") #
design_result <- ANOVA_design(design = "3b*2b*2b", 
                              n = 15, 
                              mu = c(20, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 20), 
                              
                              sd = 20, 
                              labelnames = labelnames) 

# Power based on exact simulations
exact_result <- ANOVA_exact2(design_result,
                             verbose = FALSE)
# Extrapolate
plot_power(design_result)


#https://arcstats.io/shiny/justify/

# ANOVA Compromise ----------------

design_result <- ANOVA_design(design = "3b*2w",
                              n = 12,
                              mu = c(1, 2, 2, 3, 3, 4),
                              r = .65,
                              sd = 3,
                              plot = FALSE)
plot(design_result)
example = ANOVA_compromise(design_result,
                           emm = TRUE,
                           emm_comp = "a",
                           error = "minimal")
example$aov_comp

example = ANOVA_compromise(design_result,
                           emm = TRUE,
                           emm_comp = "a",
                           error = "balance")
example$aov_comp

example$aov_plotlist


# simstudy and simr -----------------

# Generate design


# Define school level variance
gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 3, 
                      id = "idSchool")
gen.school <- defData(gen.school, varname = "nClasses", dist = "noZeroPoisson", 
                      formula = 3)
# Generate School data
dtSchool <- genData(20, gen.school)
# Randomly assign treatment
dtSchool <- trtAssign(dtSchool, n = 2)

# Define class level variance
gen.class <- defDataAdd(varname = "c0", dist = "normal", formula = 0, variance = 2)
# Number of students per class
gen.class <- defDataAdd(gen.class, varname = "nStudents", dist = "noZeroPoisson", 
                        formula = 20)
# Generate Clusters
dtClass <- genCluster(dtSchool, "idSchool", numIndsVar = "nClasses", level1ID = "idClass")
dtClass <- addColumns(gen.class, dtClass)

head(dtClass, 10)

# Get outcome (test scores) defined
gen.student <- defDataAdd(varname = "test", dist = "normal", 
                          formula = "50 + s0 + c0 + 8 * trtGrp", variance = 2)
dtStudent <- genCluster(dtClass, cLevelVar = "idClass", numIndsVar = "nStudents", 
                        level1ID = "idChild")
# Generate Data
dtStudent <- addColumns(gen.student, dtStudent)

# Check model
lmer1 = lme4::lmer(test ~ trtGrp + (1 | idSchool) + (1 | idSchool:idClass), 
                   data = dtStudent)
# Make simulation lmer
model1 = makeLmer(test ~ trtGrp + (1 | idSchool) + (1 | idSchool:idClass),
                  fixef = c(50,8), # Intercept followed by slope (effect of treatment)
                  VarCorr = list("idSchool:idClass" = 2,
                                 "idSchool" = 3), # Provide variance components in list
                  sigma = 2, # Residual Standard Deviation
                  data=dtStudent) # give the dataset to generate from
# run at total sample size
model_power = powerSim(model1, 
                       nsim=150)

print(model_power)

# Run at different sample sizes
# Will take some time to run!
# power_many = powerCurve(model1, along = "idSchool", breaks= c(6,12,16,20))


# mu_from_design -------------

model_matrix_from_design <- function(x) {
  
  
  design <- expand.grid(x)
  design_fomula <- as.formula(paste0("~", paste(names(design), collapse = "*")))
  
  design_contrasts <- lapply(x, function(x) "contr.sum")
  design_contrasts <- model.matrix(
    design_fomula
    , data = design
    , contrasts = design_contrasts
  )[, -1]
  
  colnames(design_contrasts) <- attr(terms(design_fomula), "term.labels")
  rownames(design_contrasts) <- apply(
    design
    , 1
    , function(x) paste(paste0(names(x), x), collapse = "_")
  )
  
  design_contrasts
}




mu_from_design <- function(x, f) {
  model_matrix <- model_matrix_from_design(x)
  model_matrix  = model_matrix[sort(rownames(model_matrix)), ]
  model_matrix_subset <- model_matrix[, names(f)]
  mu <- t(t(model_matrix_subset) * unlist(f))
  
  rowSums(mu)
}

### Testing

f <- 0.1481313

mu_from_design(
  x = list(a = c("1", "2"), b = c("1", "2"))
  , f = list("a:b" = f)
)

mu_from_design(
  x = list(a = c("1", "2"), b = c("1", "2"))
  , f = list("b" = f, "a:b" = f)
)

design <- list(a = c("1", "2"), b = c("1", "2"))

design_string <- "2b*2b"
f <- list("a" = 0.1481313/2,"a:b" = 0.1481313)
n <- 75
r <- 0.5
alpha_level <- 0.05

mu <- mu_from_design(x = design, f = f)

model_matrix = model_matrix_from_design(design)
m = model_matrix

model_matrix  = m[sort(rownames(m)), ]
model_matrix_subset <- model_matrix[, names(f)]
rowSums(t(t(model_matrix_subset) * unlist(f)))

power_design <- ANOVA_design(
  design = design_string,
  n = n,
  mu = as.numeric(mu),
  sd = 1,
  plot = TRUE
)

plot(power_design)

power <- ANOVA_exact(
  power_design,
  alpha_level = alpha_level, 
  verbose = FALSE, 
  emm = TRUE
)

power_design2 <- ANOVA_design(
  design = design_string,
  n = n,
  mu = c(0.1481313/2,0.1481313/2+0.1481313,
         0,0),
  sd = 1,
  plot = TRUE
)

plot(power_design2)

power <- ANOVA_exact2(
  power_design2,
  alpha_level = alpha_level, 
  verbose = FALSE, 
  emm = TRUE
)



power$main_results


