library(mcmcplots)
library(coda)

setwd("C:/Users/Austin/fire-risk/RawOutput")

tcorRoom <- read.csv('tcorRoom.csv',header=FALSE)

tcorBldg <- read.csv('tcorBldg.csv',header=FALSE)

tcorBeyond <- read.csv('tcorBeyond.csv',header=FALSE)

AoRoom <- read.csv('AoRoom.csv',header=FALSE)

AoBldg <- read.csv('AoBldg.csv',header=FALSE)

AoBeyond <- read.csv('AoBeyond.csv',header=FALSE)

thetaRoom <- read.csv('thetaRoom.csv',header=FALSE)

thetaBldg <- read.csv('thetaBldg.csv',header=FALSE)

thetaBeyond <- read.csv('thetaBeyond.csv',header=FALSE)

gibbsfire <- cbind(tcorRoom,tcorBldg,tcorBeyond,AoRoom,AoBldg,AoBeyond,thetaRoom,thetaBldg,thetaBeyond)

names(gibbsfire) <- c('tcorRoom','tcorBldg','tcorBeyond','AoRoom','AoBldg','AoBeyond','thetaRoom','thetaBldg','thetaBeyond')
gibbsfire$spreadavgtcor <- (283/395)*gibbsfire$tcorRoom+(103/395)*gibbsfire$tcorBldg+(9/395)*gibbsfire$tcorBeyond
roomdam <- 283*10314.42
bldgdam <- 103*63119.22
beyonddam <- 9*84532.68
alldam <- roomdam+bldgdam+beyonddam
gibbsfire$dollaravgtcor <- (roomdam/alldam)*gibbsfire$tcorRoom+(bldgdam/alldam)*gibbsfire$tcorBldg+(beyonddam/alldam)*gibbsfire$tcorBeyond
gibbsfire <- as.mcmc(gibbsfire)

codamenu()