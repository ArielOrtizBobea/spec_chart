#===============================================================================
# Description: Reproducible example to plot a specification chart
# Author: Ariel Ortiz-Bobea (ao332@cornell.edu)
# Version: March 10, 2020
# Note: if you have comments or suggestions on how to substantially improve this
# function, feel to contact me. If you just want to say "thank you", that also
# works. If you are too shy, then I can live with you just reading and citing
# a few of my papers. ;) Enjoy!
#===============================================================================

#===============================================================================
# 1). Preliminary ---------
#===============================================================================

# Clean workspace
  rm(list=ls())

# Install and load necessary packages
  wants <- c("plm") # contains the Hedonic dataset
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has])
  sapply(wants, function(i) require(i, character.only=TRUE))

# Load specification chart function
  source("spec_chart_function.R")

# Load Hedonic data from Harrison, D. and D.L. Rubinfeld (1978): “Hedonic
# housing prices and the demand for clean air”, Journal of Environmental
# Economics and Management, 5(1), pp. 81--102.
# Link to paper: https://www.sciencedirect.com/science/article/pii/0095069678900062
  data(Hedonic)
  Hedonic$townid <- as.factor(Hedonic$townid) # convert to factor

#===============================================================================
# 2). Regressions ---------
#===============================================================================

# 1. Baseline regression (see page 20 of the paper, tabel VII)
  xs <- c("rm","age","dis","rad","tax","ptratio","blacks","lstat","crim","zn","indus","chas","nox")
  f <- paste("mv ~",paste(xs, collapse=" + "))
  reg <- lm(as.formula(f), data=Hedonic)
  summary(reg)

# 2. Create list of models with 12 out of 13 controls
  vars0 <- list(c("nox"),xs)
  index <- combn(1:length(xs), length(xs)-1)
  index <- index[,index[nrow(index),] %in% match("nox",xs)] # keep only models with nox
  vars1 <- lapply(1:ncol(index), function(i) xs[index[,i]])
  vars2 <- lapply(vars1, function(x) c(x,"townid"))
  vars <- c(vars0, vars1, vars2)
  rm(vars1,vars2,index)
  flist <- lapply(vars, function(x) paste("mv ~",paste(x, collapse=" + ")))

# 2. Run models and export key info
  regs1 <- lapply(flist, function(f) {
    print(f)
    # Run model
    reg <- lm(as.formula(f), Hedonic)
    # Export
    data.frame(coef=coef(reg)[["nox"]], se=sqrt(diag(vcov(reg)))[["nox"]], r2=summary(reg)$r.squared)
  })
  regs1 <- data.frame(do.call("rbind",regs1))

# 3. Prepare data for plotting
  regs2 <- lapply(vars, function(x) c(xs,"townid") %in% x)
  regs2 <- data.frame(do.call("rbind",regs2))
  names(regs2) <- c(xs,"townid")
  data <- cbind(regs1,regs2)
  rm(regs1, regs2)
  data$nox <- NULL # remove indicator for coefficient of interest present in all models
  # remove R2 from table but keep it as a separate object
  r2 <- data$r2
  data$r2  <- NULL

# 4. Labels
  head(data)
  ?Hedonic # help on the descriptions of variables
  # Enter labels in order they appear in table
  labels <- list("Physical controls:" = c("Average number of rooms","Proportion built pre-1940"),
                 "Neighborhood controls:" = c("Distance to employment","Accessibility to highways","Property tax rate","Pupil/teacher ratio","Proportion of blacks","Proportion lower status","Crime rate","Proportion of 25k lots","Proportion of no–retail"),
                 "Dummies:" = c("Bounds the Charles River?","Town FE"))

#===============================================================================
# 3). Plots ---------
#===============================================================================

# Looks better when there is an outer margins
  par(oma=c(1,0,1,1))

# Most basic plot
  schart(data)

# Add labels without groups
  schart(data, unlist(labels))

# Add labels with groups
  schart(data, labels)

# Labels can be aligned differently, but needs some additional work
  schart(data, labels, adj=c(0,0)) # falls on top of plot region
  schart(data, labels, adj=c(0,0), offset=c(5.5,5)) # add offet

# Change fonts style
  schart(data, labels, fonts=c(2,2)) # bold and bold
  schart(data, labels, fonts=c(2,3)) # bold and italic

# Font size
  schart(data, labels, cex=1.2)

# Y-label
  schart(data, labels, ylab="The coefficient I really care about")

# You can also group models
  schart(data, labels, n=10) # by 10
  schart(data, labels, n=c(2,3,5,10,6)) # or any aribitrary number

# You can also sort models by the magnitude of the coefficients
  schart(data, labels, order="increasing")
  schart(data, labels, order="decreasing")
  schart(data, labels, order="asis") # default in the original order

# You can also highlight specific models, great for "baseline" models
  schart(data, labels, order="asis", highlight=2) # baseline model in the paper
  schart(data, labels, order="asis", highlight=2:4) # you can do more than 1

# Note how sorting keeps track of the model
  schart(data, labels, order="asis", highlight=2)
  schart(data, labels, order="increasing", highlight=2)

# The symbols can be cutumized
  ?pch # help on chosing symbols
  schart(data, labels, highlight=2, pch.dot=c(21,21,21,21)) # change all symbols to circles
  schart(data, labels, highlight=2, pch.dot=c(22,21,21,21)) # change one to squares
  schart(data, labels, highlight=2, pch.dot=c(21,22,21,21)) # change a different one to squares
  schart(data, labels, highlight=2, pch.dot=c(21,22,21,22)) # change a different one to squares

# You can also change the colors of these symbols
  schart(data, labels, highlight=2, col.dot=c("black","grey","white","red"),
         bg.dot=c("black","grey","white","red"))

  schart(data, labels, highlight=2, col.dot=c("black","grey","white","red"),
         bg.dot=c("white","white","white","white"))

# You can also change the color of estimate chart
  schart(data, labels, highlight=2, col.est=c("grey80","royalblue"))

# An then combine these color changes
  schart(data, labels, highlight=2, col.est=c("grey80","royalblue"), col.dot=c("grey60","grey95","grey95","royalblue"))

# Naturally, you can change the symbols related to the estimate chart
  schart(data, labels, highlight=2, pch.est=16)
  schart(data, labels, highlight=2, lwd.symbol=1)
  schart(data, labels, highlight=2, lwd.symbol=2, lwd.est=2)

# Because the lines are using the arrow() function, you can add little tick marks
  schart(data, labels, highlight=2, lwd.symbol=2, lwd.est=2, length=.03)
  schart(data, labels, highlight=2, lwd.symbol=2, lwd.est=2, length=.02)

# The default confidence interval are 95%, but this can be changed
  schart(data, labels, ci=c(.95)) # default
  schart(data, labels, ci=c(.9))
  schart(data, labels, ci=c(.99))

# The function can take 2 different confidence intervals
  schart(data, labels, highlight=2, ci=c(.95,.99))

# And you can change the colors, obviously
  schart(data, labels, highlight=4, ci=c(.95,.99), col.est=c("black","red"), col.est2=c("grey","orange"))

# In case your standard errors are asymetric, you can bring your own confidence intervals
# Here is one arbitrary example
  data2 <- data
  data2$se <- NULL
  data2$l1 <- data2$coef - 2*data$se
  data2$h1 <- data2$coef + 1*data$se
  index.ci <- match(c("l1","h1"), names(data2))
  schart(data2, labels, highlight=4, index.ci=index.ci, col.est=c("black","red"), col.est2=c("grey","orange"))

# Same but with 2 confidence intervals
  data2$l2 <- data2$coef - 3*data$se
  data2$h2 <- data2$coef + 1.5*data$se
  index.ci <- match(c("l1","h1","l2","h2"), names(data2))
  schart(data2, labels, highlight=4, index.ci=index.ci, col.est=c("black","red"), col.est2=c("grey","orange"))

# An useful feature is being able to change the height of the bottom/top panel
# This might be useful when you have many models.
  schart(data, labels, heights=c(.25,1))

# It is sometimes useful to add "reference" lines other than 0
  mu  <- mean(data$coef) # mean of all coefficients
  sd  <- sd(data$coef) # std deviation of all coefficients
  schart(data, labels, ref=c(0,mu, mu+sd, mu-sd))
  schart(data, labels, ref=c(0,mu, mu+sd, mu-sd), lty.ref=c(2,3,3,3))
  schart(data, labels, ref=c(0,mu, mu+sd, mu-sd), lty.ref=c(2,3,3,3), lwd.ref=c(2,1,1,1))
  schart(data, labels, ref=c(0,mu, mu+sd, mu-sd), lty.ref=c(2,3,3,3), lwd.ref=c(2,1,1,1), col.ref=c(1:4))

# A final touch is adding a colored band
  schart(data, labels, band.ref=c(mu+sd, mu-sd), col.band.ref="grey90")

# ... which you can combine with a reference line
  schart(data, labels, band.ref=c(mu+sd, mu-sd), col.band.ref="grey90", ref=c(0,mu), lty.ref=c(2,1), col.ref=c("black","grey70"))

# Note that the top panel is plotted last, meaning that one can use other R
# commands to add features to the top panel

# One can add a legend
  schart(data, labels)
  legend("bottomright", lwd=1:2, col=1:2, c("hey 1", "hey 2"), inset=.02)

# One can add tick marks to axes
  schart(data, labels, ylim=c(-0.02,0.01))
  ticks <- seq(-0.02,0.01,.001)
  axis(side=2, at=ticks, labels=NA, tck=-.01)
  axis(side=4, at=ticks, labels=NA, tck=-.01)

# One could also add information about model fit to this chart
  ylim <- c(-0.028,0.01)
  schart(data, labels, ylim=ylim) # make some room at the bottom
  abline(h=-0.02)
  lapply(1:length(r2), function(i) {
    rect(xleft=i-.4, ybottom=min(ylim), xright=i+.4, ytop=min(ylim)+r2[i]/150, border=NA, col="royalblue")
  })
  text(x=mean(1:nrow(data)), y=-.02-.001, "Model R2", col="royalblue", font=2)

# The end
