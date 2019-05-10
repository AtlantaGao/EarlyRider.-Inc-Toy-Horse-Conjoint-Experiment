############ Preparation works ############
rm(list=ls())
############ Part1: Design Tasks for Respondents and Create Conjoint Data ############
# set parameters
# 200 survey respondents

levels.design= c(2,2,2,2) # 4 vars, each var has 2 levels
set.seed(123)             # set seeds when generating random numbers

# 2 segments (b1 for seg1, b2 for seg2)
#b1 = c(4.5,1.75,3.3,1.4,-1.4)  # vector of coefficients for seg1
#b2 = c(1.2,4.5,1.3,3.4,1.4)    # vector of coefficients for seg2
#bs = cbind(b1,b2)
#K = ncol(bs)      # number of columns 

# create profiles
# install.packages("AlgDesign")
library(AlgDesign)
full.design <- gen.factorial(levels.design,factors="all") # generate a full factorial design
# factor="all" indicates that all of the variables are factors (i.e., categorical variables)
# full.design contains all the combinations: 2x2x2x2=16 combinations of attributes

full.design
class(full.design)
class(full.design$X1)
levels(full.design$X1)  # get levels of factor var X1 in full.design
nprofiles = nrow(full.design) # total number of all possible profiles
full.design.m = as.matrix(full.design)  # convert full.design to a matrix 
full.design.m
# want to create 0/1 dummy variables, need to convert elements into numeric
full.design.n = as.numeric(full.design.m)
design.mat = matrix(full.design.n, nprofiles)-1   
# -1 means every element in the matrix -1 so that we have 0/1 dummies
design.mat   # a numeric matrix with 0/1

# use optFederov() to provide the optimal subset of 16 profiles
fract.design <- optFederov(frml=~X1+X2+X3+X4,data=design.mat,nTrials=12,approximate=FALSE); 
#select the best design given main effects are all that is to be estimated
# need to include ~ before X1 in the formula.
# data: the candidate list describing the variables.
# nTrials is the number of cells in the study (tasks in the conjoint analysis out of 48 tasks)
# approximate=FALSE means an exact design will be calculated.
fract.design$design
# what we are interested is $design: shows you the combs that optFederov() selects
fract.design$design
fract.design$rows # the row numbers selected from the original complete design combs


####### Part2: Regression Analysis for getting part-worth estimates #######
rm(list=ls())
# load the data set
filenm = "MKT412R - Final Analysis Case Data"
load(paste(filenm,".Rdata",sep=""))
ls()                # list the objects in R environment and observe them

Newatts = c("Price","size","motion","style")
colnames(desmat) = Newatts    # assign names to columns in desmat
desmat
cbind(ID,ratings,desmat)[1:16,]

summary(ratings)    # ratings is dependent var for regression
summary(desmat)     # desmat is the "design matrix" for the study
class(desmat)

# aggregate regression that pools over all respondents to find avg preferences 
summary(lm(ratings~desmat))
# only have one set of coefficients

# by apriori segment age
summary(lm(ratings~desmat*ageD))    # add age interaction
# note if significant. can run separately for each segment

summary(lm(ratings~desmat,subset=ageD==1)) # older kids
summary(lm(ratings~desmat,subset=ageD==0)) # young kids
# can compare coefficients of same attributes for different segments
# Similarly do a priori segmentation by gender
summary(lm(ratings~desmat*genderD))
summary(lm(ratings~desmat,subset=genderD==1)) #female  
summary(lm(ratings~desmat,subset=genderD==0)) # male 

# estimate regression for each individual separately
desmatf = cbind(rep(1,nrow(desmat)),desmat) # add column for constant

# construct an empty matrix to store part-worth of each individual
partworths = matrix(nrow=sampsize,ncol=ncol(desmatf))
head(partworths)
dim(partworths)
# for each individual run the regression
# save the individual coefficients to the corresponding cells in partworth matrix
for(i in 1:sampsize){ 
  partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
colnames(partworths) = c("Intercept","Price","Size","Motion","Style")
head(partworths)

partworths   # we have 1000 sets of coefficients estimates


####### Part3: Cluster Analysis of Utility Partworths #######
library(cluster)
library(fpc)
set.seed(123456)   # set random number seed before doing cluster analysis


toclust = partworths    # select partworths to do clustering
pm1 = pamk(toclust,scaling=TRUE)   # determine the optimal number of clusters
# scaling=T: scaling the variables by dividing variables by their root-mean-square
pm1$nc   # the optimal number of clusters

# vitually see the performance with number of clusters
# create a matrix wss to store within-group sum of squares
wss = matrix(NA, nrow=15, ncol=1)# try from 1 cluster to the 15 clusters) 
for (i in 1:15) wss[i] <- sum(kmeans(toclust,
                                     centers=i,nstart=2)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

km1 = kmeans(toclust,3,iter.max = 20, nstart=3)
# iter.max: maximum number of iterations
# nstart: attempts multiple initial configurations and reports on the best one. 
# nstart=2 generates 2 initial random centroids and choose the best one for the algorithm. 


percsize = paste(1:3," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
percsize
pie(km1$size,labels=percsize)

clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0); #plot clusters against principal components

plotcluster(toclust, km1$cluster)  #plot against discriminant functions ()
km1$centers  # look at how the 3 segments differ in attributes
design.mat


km2 = kmeans(toclust,4,iter.max = 20, nstart=4)


percsize2 = paste(1:4," = ",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
percsize2
pie(km2$size,labels=percsize2)

clusplot(toclust, km2$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0); #plot clusters against principal components

plotcluster(toclust, km1$cluster)  #plot against discriminant functions ()
km2$centers  # look at how the 3 segments differ in attributes

km3 = kmeans(toclust,2,iter.max = 20, nstart=2)


percsize3 = paste(1:2," = ",format(km3$size/sum(km3$size)*100,digits=2),"%",sep="")
percsize3
pie(km3$size,labels=percsize3)

clusplot(toclust, km3$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0); #plot clusters against principal components

plotcluster(toclust, km3$cluster)  #plot against discriminant functions ()
km3$centers  # look at how the 3 segments differ in attributes
design.mat

############## part4: Creation of Fitted and Actual Ratings ############
# predicting missing cells (preparing for market simulation)
# repeat individual level partworths for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5)
head(partworths.full,33)

pratings = rowSums(desmatf*partworths.full) # predictiveratin:intercept+x1*beta1+x2*beta2+...
finalratings = ifelse(is.na(ratings),pratings,ratings)# we want to keep the original 12 current
#ratings for real, but we need to make up the rest 4 to do the predictive one 
# if ratings=NA, fill in the missing rating with predicted rating
# if ratings!=NA, use the existing rating
head(ratings)
head(finalratings)

############## part5: Market Simulation ############
# a scenario is a set of products, each with a set of levels.
# create a vector with the indexes for the product profiles from thedesmat
# Suppose our competitor offers product 1 ($45, 18", Bouncing, Racing Horse)
# We offer product 9, ($45, 18", Bouncing, Glamour Horse)
#

summary(lm(finalratings~desmat*ageD))    # add age interaction
# note if significant. can run separately for each segment

summary(lm(finalratings~desmat,subset=ageD==1)) # older kids
summary(lm(finalratings~desmat,subset=ageD==0)) # young kids
# can compare coefficients of same attributes for different segments
# Similarly do a priori segmentation by gender
summary(lm(finalratings~desmat*genderD))
summary(lm(finalratings~desmat,subset=genderD==1)) #female  
summary(lm(finalratings~desmat,subset=genderD==0)) # male 

#88888888888888888888888888888
# After 1 product (segmentation info from km1$centers)
scen0=c(7,5,13)


scen1 = c(7,4) # segment1
scen2 = c(7,5) #segment 2
scen3 = c(7,15) # segment 3
# After 2 product
scen4 = c(7,4,5)
scen5 = c(7,4,15)
scen6 = c(7,5,15)
design.mat
# After 3 product
scen7 = c(7,4,5,15)
#Competitor lower price
scen8 = c(8,4)
scen9 = c(8,5)
scen10 = c(8,15)
scen11 = c(8,4,5)
scen12 = c(8,4,15)
scen13 = c(8,5,15)
scen14 = c(8,4,5,15)

#************** the same price sensitive segmentation*************************
# After 1 product (segmentation info from km1$centers)
scen15 = c(7,4) 
scen16= c(7,6) 
scen17= c(7,16) 
# After 2 product
scen18= c(7,4,6)
scen19= c(7,4,16)
scen20= c(7,6,16)
# After 3 product
scen21= c(7,4,6,16)
#Competitor lower price
scen22= c(8,4)
scen23= c(8,6)
scen24 = c(8,16)
scen25 = c(8,4,6)
scen26 = c(8,4,16)
scen27 = c(8,6,16)
scen28 = c(8,4,6,16)
#9999999999999999999999

# tranform final ratings into matrix
finalratings
simDecInput = matrix(finalratings,nrow=nprofiles) # 16 rows as profiles and samplesize as columns

# inputmat is the ratings matrix with rows as profiles and cols as ratings
# scen is the list of products in the market for the scenario (these are rows in inputmat)
simDec = function(inputmat,scen){
  inmkt = inputmat[scen,]
  max = apply(inmkt,2,max)
  firstChoices = (inmkt==rep(max, each=length(scen)))
  shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
  rowMeans(shares)
}

inputmat = simDecInput[1:5, 1:8]   # select first 5 products and 8 individuals
print(inputmat, digits = 4)
scen = c(1,3)   # select 1st and 3rd product profiles
inmkt = inputmat[scen,]
print(inmkt, digits = 4) # contains the 1st and 3rd rows of inputmat
max = apply(inmkt, 2, max)  # for each column(individual) in inmkt, get the highest rating
print(max, digits = 4)
firstChoices = (inmkt==rep(max, each=length(scen))) # returns TRUE if the product is first choice
firstChoices
rep(max, each=length(scen))  # repeats max vector in order to be conformable with inmkt matrix
inmkt==rep(max,each=length(scen)) # this compares matrix inmkt and vector created by rep().
shares = firstChoices/rep(colSums(firstChoices),each=length(scen)) 
# this code converts the TRUE/FALSE in the "firstChoices" matrix into 1 and 0 respectively.
shares
rowMeans(shares)

##################################################################################3
simDec0 = simDec(simDecInput,scen0)
simDec0
simDec1 = simDec(simDecInput,scen1)
simDec1
simDec2 = simDec(simDecInput,scen2)
simDec2 
simDec3 = simDec(simDecInput,scen3)
simDec3
simDec4 = simDec(simDecInput,scen4)
simDec4
simDec5 = simDec(simDecInput,scen5)
simDec5
simDec6 = simDec(simDecInput,scen6)
simDec6
simDec7 = simDec(simDecInput,scen7)
simDec7 
simDec8 = simDec(simDecInput,scen8)
simDec8
simDec9 = simDec(simDecInput,scen9)
simDec9
simDec10 = simDec(simDecInput,scen10)
simDec10
simDec11 = simDec(simDecInput,scen11)
simDec11
simDec12 = simDec(simDecInput,scen12)
simDec12
simDec13 = simDec(simDecInput,scen13)
simDec13
simDec14 = simDec(simDecInput,scen14)
simDec14
simDec15 = simDec(simDecInput,scen15)
simDec15
simDec16= simDec(simDecInput,scen16)
simDec16
simDec17= simDec(simDecInput,scen17)
simDec17
simDec18= simDec(simDecInput,scen18)
simDec18
simDec19= simDec(simDecInput,scen19)
simDec19
simDec20= simDec(simDecInput,scen20)
simDec20
simDec21= simDec(simDecInput,scen21)
simDec21
simDec22= simDec(simDecInput,scen22)
simDec22
simDec23= simDec(simDecInput,scen23)
simDec23
simDec24= simDec(simDecInput,scen24)
simDec24
simDec25 = simDec(simDecInput,scen25)
simDec25
simDec26 = simDec(simDecInput,scen26)
simDec26
simDec27 = simDec(simDecInput,scen27)
simDec27
simDec28 = simDec(simDecInput,scen28)
simDec28







