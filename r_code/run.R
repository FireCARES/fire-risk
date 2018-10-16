devtools::install("pkgFireCARES")
ret <- pkgFireCARES::full_analysis()
save(ret, file='predictions.Rda')
write.csv(ret$prediction, file='predictions.csv')
