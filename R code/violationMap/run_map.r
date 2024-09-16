# ---- Import functions ----
source("/Users/grisv/GitHub/Manifest/R code/violationMap/violation_map.r")

R1 = 0.6
R2 = 100
R3 = 0.35
PR = 0.8

vmap = violationMap(0.5, 1.0, 40, 0.25, 0.5, 40, 0.5, 1.0, 40, R1, R2, R3)

colnames(vmap) = c("M1", "M2", "M3", "SBV")
write.csv(vmap, "violationMap.csv", row.names = F)
