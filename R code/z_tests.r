
# ---- Import functions ----
source("/Users/grisv/GitHub/Manifest/R code/violation_map.r")
source("/Users/grisv/GitHub/Manifest/R code/aux_functions.r")

#####################################
#          Design time              #
#####################################
len <- 10 # time window to obtain trend
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
day <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv", header = TRUE) # nolint
light <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_filtered.csv", header = TRUE) # nolint
grip <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/degrading_grip_filtered.csv", header = TRUE)	 # nolint
lg <- read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_degrading_grip_filtered.csv", header = TRUE) # nolint

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
d <- lg     # select failuring data to augment, e.g. light, grip, lg

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




assume vmap:
env1 env2 env3 problem 
0.02 0.03 0.04 e/b

vmap2: only some columns of vmap

dist2: (-1 columns compared to vmap)
0.01 0.02 0.03
____________________

violationtime(cp, vmap, trends, tstep, maxtime)

vmap = define it as only the points that are edge or boundary
_v = cp
Input
\overline{v} <- cp
trends = current trends at time t
tstep = 60 sec
maxtime = 6000000


# get distance to every problem (edge or boundary) point

# leave points that are close or in same direction
for i in vmap:
    # get distance
    dist = i[1,colnum(vmap)-1] - cp
    # leave points that are close or in same direction
    if (dist < 0.00001) or (sign(dist) == sign(trends)): # 0.00001 hyperparameter # nolint
      vmap2.append(i)
      dist2.append(dist)

if len(vmap2) < 0:
    return (0,'none')
# if more than one point in same direction & close, save as new x,y,z

if len(vmap2) == 1:
    closest = vmap2[0]

if len(vmap2) > 1:
    norm_problem = (vmap2[1]* trends[1] + vmap2[2]*trends[2] + ...)/(R*lenVector)
    norm_trends = sqrt(trends[1]^2 + trends[2]^2 + ...)
    # angle between trends and possible problem points
    angles = acos( trends . vmap2[:-1] / (norm_problem*norm_trends) )
    # sort
    sorted_angles = sort(angle)
    # get points have shortest angle
    shortest_angle_index = sorted_angles[sorted_angles== min(sorted_angles)].index 
    # and shortest distance
    dist = norm_problem[shortest_angle_index]
    shortest_dist_index = ...
    # save point
    closest = vmap2[shortest_dist_index]


# get displacement
displacement = DISPLACEMENT(closest, cp)
velocity = sqr(trend^2)/tstep





    shortest_dist_index = norm_problem[shortest_angle_index] == min(norm_problem[shortest_angle_index])
    index(shortest_dist)

    for i in sorted_angles:
        if vmap2[i]-env[i] > 0:
            possible_vmap.append(vmap2[i])
        else:
            possible_vmap.append(vmap2[i] + 0.1)
  




  x = vmap2[0] - cp
  y = vmap2[1] - cp
  z = vmap2[2] - cp
  keep = vmap2
  betype = "0"
  timepred = 0
  if len(vmap2) > 1:
    B = sqrt(x^2 + y^2 + z^2)
    C = (x* trends[1] + y*trends[2] + z*trends[3])/(R*B)
    # deal with precision problems
    C[C > 1] = 1
    C[C < -1] = -1
    angle = acos(C) 
    angle = (angle + 2*pi) % (2*pi)
    sorted =  sort.int(angle, index.return = TRUE)
    # could be multiple points with the same angle
    allsame = sorted$ix[sorted$x == sorted$x[1]]
    # find the point with the shortest distance in direction of change
    ix = allsame[B[allsame] == min(B[allsame])]
    keepx = keep[ix, 1]
    keepy = keep[ix, 2]
    keepz = keep[ix, 3]
    betype = betype[ix]
    timepred = B[ix]/R
    if timepred > maxtime:
      timepred = maxtime
    type = betype[ix]
    t_pred = timepred
    problem = type
  else:
    t_pred = 0
    problem = 0
else:
  t_pred = 0
  problem = 0





vmapClose = vmap[dist < 0.00001]  # hyperparameter
# leave points in same direction
vmapSameDir = vmap[sign(dist) == sign(trends)]
vmap = concat(vmapClose, vmapSameDir)


for i in vmap:
  if any vmap[i]-env[i] > 0:
    possible_vmap.append(vmap[i])
  else:
    possible_vmap.append(vmap[i] + 0.1)
possible_vmap = 
for i in env_i:




return (t_pred,problem)