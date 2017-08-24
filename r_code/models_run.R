models.run <- matrix( c("npt.final",   "lr"
                        "npt.final.L", "lr"
                        "mr.final",    "mr"
                        "hr.final",    "hr"                                                ),
                      byrow=TRUE,
                      ncol=2 )
models.run <- as.data.frame(models.run)
names(models.run) <- c("lst", "risk")