#remove envery thing by using this command 
rm(list=ls())
#load augmentedRCBD
library(augmentedRCBD)
#load the file from import database section and assign it as a and view the structure by str() command 
#make block(replications) and treatments as factors by using as.factor command
a=DATA_SHEET_3_
str(a)
a$Block=as.factor(a$Block)
a$Treatment=as.factor(a$Treatment)
str(a)

# than assigning the output into output object by using augumentedRCBD.bulk function command
# in data section assign your imported data, block = block and treatments = treatments 
#in traits section mention the traits in inverted comma's
#in check.col section mention the color names withrespect to checks, like if you have two checks than mentions two colors
#rest run everything as defualt
output = augmentedRCBD.bulk(data = a, block = "Block",
                           treatment = "Treatment", traits = c("DTF", "DTM","PH","NN","RL","ERL","NOR","NOC","TW","PY"),
                           checks = NULL, alpha = 0.05, describe = TRUE,
                           freqdist = TRUE, gva = TRUE,
                           check.col = c("brown", "darkcyan"),
                           console = TRUE)

#saving the corrected means into a separate file as csv and it will get saved into present directory
write.csv(output$Means,'adjusted.csv')
#saving the whole report and the path it saved is shown in console window after running analysis
report.augmentedRCBD.bulk(bout, file.path(tempdir(), "augmentedRCBD bulk output.docx"))


######calculate correlations#################
rm(list=ls())
library(metan)
#import data of currated mean which extracted from oUtput
a=corr_coef(DATA_SHEET[,2:11])
plot(a)
view(a)
write.csv(a$cor,file ="corr.csv", row.names = TRUE)
#file is saved in your working directory

##############path analysis######
library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
#load all the packages
#assign a model with dependent variable with independent variables like PY is dependent and rest all is independent variables

model = 'PY ~ DTF + DTM + ERL + NN + NOC + PH + RL + TW'
#run as default and in data mention the data name you upoaded
fit <- cfa(model, data = DATA_SHEET)

s=summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit, 'std', layout = 'circle')
s$PE
view(s$PE)


#ridged path analysis for direct and indirect effects######

#load biotools package
library(biotools)
#import currate means form the output analysis
#and see the class and assign as matrix by using as.matrix function
class(x)
x=as.matrix(x)
#run as default but the x in parenthess shows the data file 
path=pathanalysis(x,10, collinearity = FALSE)
#save the output
write.csv(path$coef,file = "path.csv",row.names = TRUE)

##################*******miscellaneous*****############################

 