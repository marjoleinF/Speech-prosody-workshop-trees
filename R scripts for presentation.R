## First, make sure that the following packages are installed in R, or 
## download and install load using the default approach: 
## gamm4, splines, Formula, lme4, partykit, mgcv, sandwich, merDeriv
## graphics, stats, utils, Formula, vcd, lattice, lmerTest, devtools
## ggplot2 ggpubr

## Install the workhop's key libraries (currently development versions 
## due to recent changes, will be available through standard CRAN 
## repository during workshop)"
library("devtools")
install_github("r-forge/partykit/pkg/glmertree")
install_github("marjoleinF/gamtree")
## If you get a warning about other packages needing updating, type 1, 2 or 3, 
## does not really make a difference

## Load libraries
library("ggplot2")
library("ggpubr")
library("lmerTest")
library("gamtree")
library("glmertree")
library("Formula")



##################################################
##
## Example 1: Data from Gardner et al. (2021) 
## 
## https://doi.org/10.1371/journal.pone.0252602
##

## Load data and do some visualization
load("gardner.rda")
ggdensity(df$SilPerWord, xlab = "Silence Per Word Per Turn",
          add = "mean", ylab="Density")
ggqqplot(df$SilPerWord)
plot(df$NFP.bi, xlab = "Presence of filled pause in a turn", ylab = "Frequency") 

## Fit a tree predicting presence of a filled pause, with a few continuous predictors
## Tree fitting will take a few minutes!
gt1 <- glmertree(NFP.bi ~ 1 | (1 |Speaker_Number) | NVarbs.bi + Turn.Duration.C + 
                   CharPerWord.C + WordPerSec.C, minsize = 750,
                 data = df, verbose = TRUE, family = "binomial")
plot(gt1, which = "tree", gp = gpar(cex=.7))
plot(gt1, which = "ranef")
coef(gt1)

## Fit a tree with many more predictors (binary ones) added
## Tree fitting will take a few minutes!
V.bi <- c(paste0("V0", 1:9, ".bi"), paste0("V", 10:20, ".bi"))
V.bi
ff <- as.Formula(paste("NFP.bi ~ 1 | (1 |Speaker_Number) | NVarbs.bi + 
                        Turn.Duration.C + CharPerWord.C + WordPerSec.C + ", 
                       paste0(V.bi, collapse = "+")))
## The error above can safely be ignored
ff
gt2 <- glmertree(ff, minsize = 500, data = df, verbose = TRUE, family = "binomial")
plot(gt2, which = "tree", gp = gpar(cex=.7))
plot(gt2, which = "ranef")
coef(gt2)





##################################################
##
## Example 2: Data from Wieling (2018) 
## 
## https://doi.org/10.1016/j.wocn.2018.03.002
##

## Load data and do some inspection
load("full.rda")
summary(full)

## Select subset of observations
words <- c("tent","tenth")
dat <- droplevels(full[full$Word %in% c("tent","tenth"),])

## Fit parametric spline tree
sp <- splinetree(Pos ~ ns(Time, df = 10) | (1|Trial) | Word + Lang, data = dat,
                 cluster = Trial, verbose = TRUE)
plot(sp, gp = gpar(cex = .7))
coef(sp)

## Fit a penalized spline tree (will take quite some time!)
dat_small <- dat[seq(1L, nrow(dat), by = 4L), ]
gt <- gamtree(Pos ~ s(Time, k = 10) | Word + Lang, data = dat_small,
              cluster = dat_small$Trial, verbose = TRUE)
plot(gt, which = "tree")
par(mfrow = c(2, 2))
plot(gt, which = "terms")
