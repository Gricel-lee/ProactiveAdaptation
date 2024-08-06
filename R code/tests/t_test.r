###############################
# ---- Section: Functions ----

# ---- Calculate trends from data
gettrends <- function(data, len){ # nolint
    full = dim(data)[1]
    trends = matrix(nrow = (full-len), ncol = 4)
    x = c(1:len)
    for (i in (len+1):full){
         trends[(i-len),1] = data[(i-len),1]
         for (j in 1:3){
             model = lm(data[(i-len):(i-1), (j+1)] ~ x)
             trends[(i-len),(j+1)] =  model$coefficients[2]
         }
    } # nolint
    return(trends)
}

# ---- Calculate time to violation
violationtime <- function(s, vmap, trends, tstep, maxtime){
	 border = vmap[which(vmap[,4] == 0),1:3]
	 edge = vmap[which(vmap[,4] == 2),1:3]
            borderedge = rbind(border, edge)
	R = sqrt(trends[1]^2 + trends[2]^2 + trends[3]^2)
            x = borderedge[,1]-s[1]
            y = borderedge[,2]-s[2]
            z = borderedge[,3]-s[3]
            betype = c(rep("b", dim(border)[1]), rep("e", dim(edge)[1]))
            # don't consider border/edge points in wrong direction
            indsx = which( (abs(x) <  0.00001) | (sign(x) == sign(trends[1])))
            indsy = which( (abs(y) <  0.00001) | (sign(y) == sign(trends[2])))
            indsz = which( (abs(z) <  0.00001) | (sign(z) == sign(trends[3])))
            inds = intersect(indsx, intersect(indsy, indsz)) 
            type = "0"     
            timepred = 0  
            if (length(inds) > 0) {
                   x = x[inds]
                   y = y[inds]
                   z = z[inds]
                   keep = borderedge[inds,]
                   betype = betype[inds]
                   if (length(inds) > 1) {
                        B = sqrt(x^2 + y^2 + z^2)
	             C = (x* trends[1] + y*trends[2] + z*trends[3])/(R*B)
	             # deal with precision problems
                        C[which(C > 1)] = 1
                        C[which(C < -1)] = -1
 	             angle = acos(C) 
                        angle = (angle + 2*pi) %% (2*pi)          
                        sorted =  sort.int(angle, index.return = T)
                        # could be multiple points with the same angle 
                        allsame = sorted$ix[which(sorted$x == sorted$x[1])]
                         # find the point with the shortest distance in direction of change
                         ix = allsame[which(B[allsame] == min(B[allsame]))]
                         keepx = keep[ix,1]
                         keepy = keep[ix,2]
                         keepz = keep[ix,3]
                         type = betype[ix]
                   } else {
                         keepx = keep[1]
                         keepy = keep[2]
                         keepz = keep[3]    
                         type = betype[inds]       
                   }
                   displacement = sqrt((keepx-s[1])*(keepx-s[1])+ (keepy-s[2])*(keepy-s[2])+ (keepz-s[3])*(keepz-s[3]))
                   velocity = sqrt(trends[1]*trends[1] + trends[2]*trends[2] +  trends[3]* trends[3])/tstep        
                   timepred = maxtime                  
                   if (velocity > 0) timepred = displacement/velocity              
                   timepred = floor(timepred + 0.5)
            }
	return(list(timepred, type))
}

# ---- Check neighbouring points - function to check time from neighbouring violation map points 
checknay <- function(cp, trends, tstep, keept, vmap,  maxtime, n, msteps){
     nt = matrix(0, nrow = ((2*n+1)^3), ncol = 4)
     ind = 1
     for (j in c(-n,0,n)) {
            for (k in c(-1,0,n)) {
                    for (l in c(-n,0,n)) {
                           nt[ind, 1] = j
                           nt[ind, 2] = k
                           nt[ind, 3] = l
                           np = c((cp[1] + j*msteps[1]), (cp[2] + k*msteps[2]), (cp[3] + l*msteps[3]))
                           nt[ind, 4] = violationtime(np, vmap, trends, tstep, maxtime)[[1]]
                           ind = ind + 1
                    }  
            }  
     }  
     s = sort.int(nt[,4], index.return = T, decreasing = TRUE)
     ntsorted = nt[s$ix,]
     return(ntsorted[which(ntsorted[,4] >= (keept + timestep)),])
}

# ---  Main function to predict violations and check trends
checktrends <- function(data, vmap, tstep, hmeans, hlims, changelims, len, maxtime, mintime, msteps){
    
    full_length <- dim(data)[1]

    # ---- Initialisation 
    keeptrends <- rep(0, 3)  # previous saved trends
    trends <- rep(0, 3)      # current trends
    trendchanges <- rep(0, 3)# changes in trends
    keept <- maxtime         # time to violation
    time <- data[,1]         # time vector
    x <- c(1:len)            # x values for trend calculation
    problem <- 0            # flag for problem
    output <- rep(0,10)    # output matrix
    
    # ---- Monitoring loop
    for (i in (len+1):full_length){ # for each monitoring point
         for (j in 1:3){            # calculate all trends
             model <- lm(data[(i-len):(i-1), (j+1)] ~ x)
             trends[j] <-  model$coefficients[2]
             trendchanges[j] <- abs(trends[j] - keeptrends[j])
         }
         # check whether absolute difference between actual and expected values exceed limits
        if ((abs(data[i, 2] - hmeans[1]) > hlims[1]) | (abs(data[i, 3] - hmeans[2]) > hlims[2]) 
                 |(abs(data[i, 4] - hmeans[3]) > hlims[3]))  { 
                  change <- rep("none",3) #tracks what environmental variables change, e.g., if all: ['lighting', 'floor friction', 'gripper']
                  # which trend(s) change
                  if (abs(data[i, 2] - hmeans[1]) > hlims[1]) change[1] <- "lighting " 
                  if (abs(data[i, 3] - hmeans[2]) > hlims[2])  change[2] <- "floor friction "
                  if (abs(data[i, 4] - hmeans[3]) > hlims[3])  change[3] <- "gripper "
                  # check whether this is a continuing trend
                  if ((trendchanges[1] < changelims[1]) & (abs(trendchanges[2]) < changelims[2]) &(abs(trendchanges[3]) < changelims[3]))  {
                           keept <- keept-tstep
                           print(c(time[i], "same trends", "predicted violation in ", keept, "seconds." ))
                           output <- rbind(output, c(data[i,1:4], "same trend", keept, change, " "))
                           if ((keept - timestep) < mintime){
                               # need to respond now, so check neighbours
                               nay <- checknay(cp, trends, tstep, keept, vmap,  maxtime, 1, msteps)
                               if (nrow(nay) > 0) {
                                     for (j in 1:nrow(nay)) { print(nay[j,]) }
                               }
                                     else { print("no adaptation possible.") }
                         }
                  }
                  else {
                         # calculate time to violation/edge of parameter space from current point
                         cp <- c(data[i, 2], data[i, 3], data[i, 4])
                         t <- violationtime(cp, vmap, trends, tstep, maxtime)
                         if (t[[1]] < maxtime) {
                               keept <- t[[1]]
                               phrase <- "predicted violation in "
                               if (t[[2]] == "e") { phrase = "Could reach unknown parameter space in "} # nolint
                               print(c(time[i], " new trend", phrase, keept, "seconds." ))
                               output <- rbind(output, c(data[i,1:4], "new trend", keept, change, t[[2]]))
                               keeptrends <- trends
                               if ((keept - timestep) < mintime){
                                     # need to respond now, so check neighbours
                                     nay <- checknay(cp, trends, tstep, keept, vmap,  maxtime, 1, msteps)
                                     if (nrow(nay) > 0) {
                                              for (j in 1:nrow(nay)) { print(nay[j,]) }
                                     }
                                     else { print("no adaptation possible.") }
                               }
                         }
                         else { print(c(time[i], "new trend providing maximum time" , keept)) }
                  }
         }             
         else {
                       # otherwise all ok
                       print(c(time[i], "No problem" )) 
         }
    }
    return(output[-1,])
}


# ---- Determine whether each data point is safe or not
checkSBV <- function(data, vmap){
  v <- rep(100, dim(data)[1])
  for (i in 1:dim(data)[1]){
    dmin <- 100
    for (j in 1: dim(vmap)[1]){
        dis <- sqrt((data[i,2]-vmap[j,1])*(data[i,2]-vmap[j,1])+ (data[i,3]-vmap[j,2])*(data[i,3]-vmap[j,2])+   (data[i,4]-    vmap[j,3])*(data[i,4]-vmap[j,3]))  # nolint
        if (dis < dmin){
          dmin <- dis
          keep <- j
        }
    }
    v[i] <- vmap[keep, 4]
  }
  return(v)
}


#####################################
# ---- Section: Design time, initialisation----

# Time window to obtain trend
len <- 10

# Read violation map      # vmap <- read.csv("violationMap.csv", header = TRUE)
vmap <- read.csv("safezone.csv", header = TRUE)


#####################################
# ---- Section: Data Preparation ----
day <- read.csv("/Users/grisv/GitHub/Manifest/R code/sample_day_filtered.csv") # used to get column names

day <- read.csv("/Users/grisv/GitHub/Manifest/R code/sample_day_filtered.csv", col.names = seq_len(ncol(day))) # nolint
light <- read.csv("/Users/grisv/GitHub/Manifest/R code/faulty_light_filtered.csv", col.names = seq_len(ncol(day))) # nolint
grip <- read.csv("/Users/grisv/GitHub/Manifest/R code/degrading_grip_filtered.csv", col.names = 1:ncol(day))	 # nolint
lg <- read.csv("/Users/grisv/GitHub/Manifest/R code/faulty_light_degrading_grip_filtered.csv", col.names = 1:ncol(day)) # nolint

# Plot data
this <- day  # select data to plot, e.g. day, light, grip, lg
plot(this[, 1], this[, 2], xlab = "Time", ylab = "Lighting", type = "l") # light
plot(this[, 1], this[, 3], xlab = "Time", ylab = "Floor friction", type = "l") # floor # nolint
plot(this[, 1], this[, 4], xlab = "Time", ylab = "Gripper", type = "l") # gripper # nolint



#####################################
# ---- Section: Expected environmental values and design their limits ----

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


###############################
# ---- Section: Expected trend changes and design their limits ----

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








###############################
# ---- Section: Generate synthetic failure data ----
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
plot(dataplus[, 1], dataplus[, 2], xlab = "Time", ylab = "Lighting ", type = "l")
abline(h = historicmeans[1] - mlim1, col = "purple")
abline(h = historicmeans[1] + mlim1, col = "purple")
abline(h = 0, col = "green")

plot(dataplus[, 1], dataplus[, 3], xlab = "Time", ylab = "Floor friction ", type = "l")
abline(h = historicmeans[2] - mlim2, col = "purple")
abline(h = historicmeans[2] + mlim2, col = "purple")
abline(h = 0, col = "green")

plot(dataplus[, 1], dataplus[, 4], xlab = "Time", ylab = "Gripper ", type = "l")
abline(h = historicmeans[3] - mlim3, col = "purple")
abline(h = historicmeans[3] + mlim3, col = "purple")
abline(h = 0, col = "green")


###############################
# ---- Section: Test get trends on failing data ----

# get new trends and trend changes
newtrends = gettrends (dataplus, len)
newlen = dim(newtrends)[1]
newchange = newtrends[2:newlen,2:4]- newtrends[1:(newlen-1),2:4]

# check trend change limits on new data	
plot(newtrends[2:newlen,1], newchange[,1], xlab = "Time", ylab = "Lighting change", type = "l")
abline(h = clim1, col = "purple")
abline(h = -clim1, col = "purple")
abline(h = 0, col = "green")

# plot(newtrends[2:newlen,1], newchange[,2], xlab = "Time", ylab = "Friction change", type = "l")
# abline(h = clim2, col = "purple")
# abline(h = -clim2, col = "purple")
# abline(h = 0, col = "green")

# plot(newtrends[2:newlen,1], newchange[,3], xlab = "Time", ylab = "Gripper change", type = "l")
# abline(h = clim3, col = "purple")
# abline(h = -clim3, col = "purple")
# abline(h = 0, col = "green")

print(clim3)


