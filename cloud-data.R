Target <- matrix(c(0, 6, 14, 6, 12, -8, 2, 6, 10, 12,
                   -12, -10, -6, -2, 0, 2, 4, 4, 10, 10),
                 nrow = 10, ncol = 2)
colnames(Target) <- paste0("x", 1:2)
rownames(Target) <- paste0("j", 1:10)

save(list = "Target", file = paste0("data/Target.rda"))

rm(list = ls())
