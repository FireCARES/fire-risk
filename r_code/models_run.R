models.run <- matrix( c("lr", "npt.final",
                        "lr", "npt.final.L",
                        "mr", "mr.final",
                        "hr", "hr.final"     ),
                      byrow=TRUE,
                      ncol=2 )
models.run <- as.data.frame(models.run)
names(models.run) <- c("risk", "lst")