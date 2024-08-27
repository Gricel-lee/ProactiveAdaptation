# ---- Import functions ----
source("/Users/grisv/GitHub/Manifest/R code/violation_map.r")
source("/Users/grisv/GitHub/Manifest/R code/aux_functions.r")


#####################################
#          Design time              #
#####################################
# time window to obtain trend
len <- 10 
# requirement bounds
R1 <- 0.6
R2 <- 100
R3 <- 0.35



# ---- Section: Generate violation map ---- #
vmap <- vMap(0.5, 1.0, 20, 0.25, 0.5, 20, 0.5, 1.0, 20, R1, R2, R3)
# save
colnames(vmap) <- c("M1", "M2", "M3", "SBV")
write.csv(vmap, "violationMap.csv", row.names = FALSE)
# read  (if generated on separate file)
vmap <- read.csv("violationMap.csv", header = TRUE)


# ---- Section: Data Preparation ---- #
#import data
daycol <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv") # used to get column names across all data # nolint
day <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv", col.names = seq_len(ncol(daycol))) # nolint
light <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_filtered.csv", col.names = seq_len(ncol(day))) # nolint
grip <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/degrading_grip_filtered.csv", col.names = 1:ncol(day))	 # nolint
lg <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_degrading_grip_filtered.csv", col.names = 1:ncol(day)) # nolint

# Plot data
this <- day  # select data to plot, e.g. day, light, grip, lg
plot(this[, 1], this[, 2], xlab = "Time", ylab = "Lighting", type = "l") # light
plot(this[, 1], this[, 3], xlab = "Time", ylab = "Floor friction", type = "l") # floor # nolint
plot(this[, 1], this[, 4], xlab = "Time", ylab = "Gripper", type = "l") # gripper # nolint



# ---- Section: Expected environmental values and design their limits ---- #

# Expected environmental values (means)
historicmeans <- colMeans(day)[2:4]  #day = normal environmental conditions

# Limits for expected env. values: (4*sigma1,4*sigma2,4*sigma3)
mlim1 <- 4.0 * sqrt(var(abs(day[, 2] - historicmeans[1])))
mlim2 <- 4.0 * sqrt(var(abs(day[, 3] - historicmeans[2])))
mlim3 <- 4.0 * sqrt(var(abs(day[, 4] - historicmeans[3])))
lims <- c(mlim1, mlim2, mlim3)

# Plot expected light and limits
plot(day[, 1], day[, 2], xlab = "Time", ylab = "Lighting", type = "l", ylim = c(historicmeans[1]-1.5*mlim1, historicmeans[1]+1.5*mlim1)) # nolint
abline(h = historicmeans[1] + mlim1, col = "purple")
abline(h = historicmeans[1] - mlim1, col = "purple")
abline(h = historicmeans[1], col = "green")

# Plot expected floor friction and limits
plot(day[, 1], day[, 3], xlab = "Time", ylab = "Floor friction", type = "l", ylim = c(historicmeans[2]-1.5*mlim2, historicmeans[2]+1.5*mlim2)) # nolint
abline(h = historicmeans[2] + mlim2, col = "purple")
abline(h = historicmeans[2] - mlim2,  col = "purple")
abline(h = historicmeans[2], col = "green")

# Plot expected gripper friction and limits
plot(day[, 1], day[, 4], xlab = "Time", ylab = "Gripper", type = "l", ylim = c(historicmeans[3]-1.5*mlim3, historicmeans[3]+1.5*mlim3)) # nolint
abline(h = historicmeans[3] + mlim3, col = "purple")
abline(h = historicmeans[3] - mlim3,  col = "purple")
abline(h = historicmeans[3], col = "green")


# ---- Section: Expected trend changes and design their limits ---- #

# Get trends in normal environmental conditions
historictrends <- gettrends(day, len)

# Change on trends
l <- dim(historictrends)[1]
change <- historictrends[2:l, 2:4] - historictrends[1:(l - 1), 2:4]

# Limits for expected change in trends
clim1 <- 3.0 * sqrt(var(change[, 1]))
clim2 <- 3.0 * sqrt(var(change[, 2]))
clim3 <- 3.0 * sqrt(var(change[, 3]))
changelims <- c(clim1, clim2, clim3)

# Plot (trend) change in light trend and limits
plot(historictrends[2:l,1], change[,1], xlab = "Time", ylab = "Lighting trend changes ", type = "l", ylim = c(-2*clim1, 2*clim1))
abline(h = clim1, col = "purple")
abline(h = -clim1, col = "purple")
abline(h = 0, col = "green")

# Plot (trend) change in floor friction trend and limits
plot(historictrends[2:l,1], change[,2], xlab = "Time", ylab = "Floor friction trend changes ", type = "l", ylim = c(-2*clim2, 1.5*clim2))
abline(h = clim2, col = "purple")
abline(h = -clim2, col = "purple")
abline(h = 0, col = "green")

# Plot (trend) change in gripper trend and limits
plot(historictrends[2:l,1], change[,3], xlab = "Time", ylab = "Gripper trend changes ", type = "l", ylim = c(-2*clim3, 2*clim3))
abline(h = clim3, col = "purple")
abline(h = -clim3, col = "purple")
abline(h = 0, col = "green")


# ---- Section: Generate synthetic failure data ---- #
#### SELECT HERE ###########
d <- grip     # select failuring data to augment, e.g. light, grip, lg

# Generate synthetic failure data
# a) get last "len" datapoints to beginning of normal day data
start <- dim(day)[1] - len + 1
end <- dim(day)[1]
extra <-  day[start:end, ] # last rows of normal day

# b) augment failing data
dataplus <- rbind(extra, d)
dataplus[, 1] <- 60 * c(0:(nrow(dataplus) - 1)) # adjust time

# Plot synthetic data + limits
plot(dataplus[, 1], dataplus[, 2], xlab = "Time", ylab = "Lighting ", type = "l") # nolint
abline(h = historicmeans[1] - mlim1, col = "purple")
abline(h = historicmeans[1] + mlim1, col = "purple")
abline(h = 0, col = "green")

plot(dataplus[, 1], dataplus[, 3], xlab = "Time", ylab = "Floor friction ", type = "l") # nolint
abline(h = historicmeans[2] - mlim2, col = "purple")
abline(h = historicmeans[2] + mlim2, col = "purple")
abline(h = 0, col = "green")

plot(dataplus[, 1], dataplus[, 4], xlab = "Time", ylab = "Gripper ", type = "l")
abline(h = historicmeans[3] - mlim3, col = "purple")
abline(h = historicmeans[3] + mlim3, col = "purple")
abline(h = 0, col = "green")





#####################################
#          Runtime time              #
#####################################

# monitoring timestep is currently the same for all measurements
timestep <- 60
# maximum time possible
maxtime <- 6000000
# get msteps from violation map calculation
msteps <- c(0.0250, 0.0125, 0.0250)
# minimum time allowed to predicted violation
mintime <- 600


# ---- Section: Predict violations and check trends ---- #
out <- checktrends(dataplus, vmap, timestep, historicmeans, lims,
                   changelims, len, maxtime, mintime, msteps)
write.csv(out, "outputlg.csv", row.names = FALSE)

#sbv <- checkSBV(dataplus, safezone)
sbv <- checkSBV(dataplus, vmap)