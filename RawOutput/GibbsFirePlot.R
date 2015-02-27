library(mcmcplots)
library(coda)

setwd("C:/Users/Austin/fire-risk/RawOutput")

tcorRoom <- read.csv('tcorRoom.csv',header=FALSE)

tcorBldg <- read.csv('tcorBldg.csv',header=FALSE)

tcorBeyond <- read.csv('tcorBeyond.csv',header=FALSE)

ARoom <- read.csv('ARoom.csv',header=FALSE)

ABldg <- read.csv('ABldg.csv',header=FALSE)

ABeyond <- read.csv('ABeyond.csv',header=FALSE)

alphRoom <- read.csv('alphRoom.csv',header=FALSE)

alphBldg <- read.csv('alphBldg.csv',header=FALSE)

alphBeyond <- read.csv('alphBeyond.csv',header=FALSE)

gibbsfire <- cbind(tcorRoom,tcorBldg,tcorBeyond,ARoom,ABldg,ABeyond,alphRoom,alphBldg,alphBeyond)

names(gibbsfire) <- c('tcorRoom','tcorBldg','tcorBeyond','ARoom','ABldg','ABeyond','alphRoom','alphBldg','alphBeyond')

