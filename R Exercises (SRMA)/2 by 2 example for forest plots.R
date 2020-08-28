
## 3.2 Forest plots and L'abbe plots using metafor package
##########################################################

# https://rdrr.io/cran/metafor/man/forest.default.html
# http://www.metafor-project.org/doku.php/plots:cumulative_forest_plot

## Example code from the second website: 
library(metafor)      ## in case you haven't already run this

### decrease margins so the full space is used
par(mar=c(4,4,1,2))   

## Examine the dataset being used: 
print(dat.bcg)
help(dat.bcg)

## Relevant Details From the Help File:

## R has stored a meta-analysis dataset for you that can be used to test the conventions of the metafor() package. This data comes from the study Colditz et al. (1994), titled "Efficacy of BCG vaccine in the prevention of tuberculosis: Meta-analysis of the published literature". The goal of the meta-analysis was to examine the overall effectiveness of the BCG vaccine for preventing tuberculosis and to examine moderators that may potentially influence the size of the effect.
## The 13 studies in this meta analysis dataset provide data from each of the 13 studies in terms of 2x2 tables in the form: 
## tneg = Number of TB Negative observations (treatment group - vaccinated)
## tpos = Number of TB Positive observations (treatment group - vaccinated)
## cpos = Number of TB Negative observations (control group - unvaccinated)
## cneg = Number of TB Positive observations (control group - unvaccinated)
## This allows you to calculate risk ratios for your final output.
## Remember the risk ratio formula:
## (incidence in exposed) / (incidence in unexposed)
## or... [(tpos/(tneg+tpos)) / (cpos/(cneg+cpos))]

## Additional information
## Format
## The data frame contains the following additional relevant columns: 
## trial = Trial
## author = Author name(s) 
## year = Publication year
## ablat = Numeric absolute latitude where the study was conducted 
## alloc = character method of treatment allocation (random, alternate, or systematic)


## Now lets look into the escalc() formula using "help(escalc)"
### First, lets calculate (log) risk ratios and corresponding sampling variances using the escalc() canned function for the Risk Ratio formula:
dat.bcg1 <- escalc(measure="RR",     ## the effect size or outcome measure type, RR = log risk ratio
                   ai=tpos,          ## upper left cell (positive in treatment group)
                   bi=tneg,          ## upper right cell (negative in treatment group)
                   ci=cpos,          ## lower left cell (positive in control group)
                   di=cneg,          ## lower right cell (negative in control group)
                   data=dat.bcg)

## Notice after we've run this, it creates a yi (vector of the specified effect size or outcomes) and a vi (vector of conrresponding sample variantes) and adds these to the other values from the dataset dat.bcg: 
print(dat.bcg1)


### Now, use the dat dataframe to fit random-effects models
res <- rma(yi, vi, data=dat.bcg1, slab=paste(author, year, sep=", "))

print(res)

### cumulative meta-analysis (in the order of publication year)
tmp <- cumul(res, order=order(dat.bcg1$year))

forest(tmp)

### cumulative forest plot
forest(tmp, xlim=c(-4,2), at=log(c(0.125, 0.25, 0.5, 1, 2)), 
       atransf=exp, digits=c(2,3), cex=0.75)

### switch to bold font
par(cex=0.75, font=2)

### and add in the column headings to the plot
text(-4, 15, "Author(s) and Year",  pos=4)
text( 2, 15, "Risk Ratio [95% CI]", pos=2)

