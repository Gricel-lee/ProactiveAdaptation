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
violationtime <- function(cp, vmap, trends, tstep, maxtime){
  # get border and edge points from violation map
  border <- vmap[which(vmap[, 4] == 0), 1:3]
  edge <- vmap[which(vmap[, 4] == 2), 1:3]
  borderedge <- rbind(border, edge)

  # distance to border/edge points
  x <- borderedge[, 1] - cp[1]
  y <- borderedge[, 2] - cp[2]
  z <- borderedge[, 3] - cp[3]
  #creates a character vector concatenating sequences of "b" and "e"s
  betype <- c(rep("b", dim(border)[1]), rep("e", dim(edge)[1]))
  # consider border/edge points in same direction or too close
  indsx <- which((abs(x) <  0.00001) | (sign(x) == sign(trends[1])))
  indsy <- which((abs(y) <  0.00001) | (sign(y) == sign(trends[2])))
  indsz <- which((abs(z) <  0.00001) | (sign(z) == sign(trends[3])))
  inds <- intersect(indsx, intersect(indsy, indsz))
  # initialise vars
  type <- "e"
  timepred <- 0

  # if more than one point in same direction & close, save as new x,y,z
  if (length(inds) > 0) {
    # a) save distance and possible problem points
    #distances in x,y,z:
    x <- x[inds]
    y <- y[inds]
    z <- z[inds]
    # possible problem points
    keep <- borderedge[inds, ] #points
    betype <- betype[inds] #problem
    
    if (length(inds) > 1) {
      # b) get angle between trends and possible problem points
      # magnitude of vectors
      distance2_problem <- sqrt(x^2 + y^2 + z^2)
      norm_trends <- sqrt(trends[1]^2 + trends[2]^2 + trends[3]^2)
      # dot product normalised
      cos_angles <- (x * trends[1] + y * trends[2] + z * trends[3]) /
        (norm_trends * distance2_problem)
      # deal with precision problems (cosine calculated between -1 and 1)
      cos_angles[which(cos_angles > 1)] <- 1
      cos_angles[which(cos_angles < -1)] <- -1
      # get angle (ensure it is within the range [0, 2 * pi))
      angles <- (acos(cos_angles) + 2 * pi) %% (2 * pi)

      # c) sort angles
      sort_angles <-  sort.int(angles, index.return = TRUE)

      # get points with min angle (could be multiple)
      min_angles_indx <- sort_angles$ix[which(sort_angles$x == sort_angles$x[1])]

      # For debugging:
      #print(min_angles_indx)
      #readline()

      # find the point with the shortest distance (among points with shortest angle) # nolint
      min_dist_indx <- min_angles_indx[which(distance2_problem[ min_angles_indx ] == min(distance2_problem[ min_angles_indx ]))] # nolint
      keepx <- keep[min_dist_indx, 1]
      keepy <- keep[min_dist_indx, 2]
      keepz <- keep[min_dist_indx, 3]
      type <- betype[min_dist_indx]
    }
    else {
      keepx <- keep[1]
      keepy <- keep[2]
      keepz <- keep[3]
      type <- betype[inds]
    }
    displacement <- sqrt((keepx - cp[1]) * (keepx - cp[1]) +
                           (keepy - cp[2]) * (keepy - cp[2]) +
                           (keepz - cp[3]) * (keepz - cp[3]))
    # trends are in environmental units per time unit
    trends_combined <- sqrt(trends[1]*trends[1] + trends[2]*trends[2] +  trends[3]* trends[3]) # nolint
    speed <- trends_combined / tstep # divided by tstep as trends in env. are obtained by increments of tstep (not per 1 time unit)
    
    # if speed is not zero, calculate time to reach the point
    if (speed > 0) timepred <- displacement / speed
    timepred <- floor(timepred + 0.5) # round to nearest integer

    # if not changing speed, set to maxtime
    if (speed == 0) timepred <- maxtime

  }
  return(list(timepred, type))
}






# ---- Check neighbouring points - function to check time from neighbouring violation map points 
checknay <- function(cp, trends, tstep, keept, vmap,  maxtime, n, msteps){
     nt <- matrix(0, nrow = ((2*n+1)^3), ncol = 4)
     ind <- 1
     for (j in c(-n,0,n)) {
            for (k in c(-1,0,n)) {
                    for (l in c(-n,0,n)) {
                           nt[ind, 1] <- j
                           nt[ind, 2] <- k
                           nt[ind, 3] <- l
                           np <- c((cp[1] + j*msteps[1]), (cp[2] + k*msteps[2]), (cp[3] + l*msteps[3]))
                           nt[ind, 4] <- violationtime(np, vmap, trends, tstep, maxtime)[[1]]
                           ind <- ind + 1
                    }  
            }  
     }  
     s <- sort.int(nt[,4], index.return = T, decreasing = TRUE)
     ntsorted = nt[s$ix,]
     return(ntsorted[which(ntsorted[,4] >= (keept + timestep)),])
}




# ---  Main function to predict violations and check trends
checktrends <- function(data, vmap, tstep, hmeans, hlims,
                        changelims, len, maxtime, mintime, msteps) {
  full_length <- dim(data)[1]
  # ---- Initialisation
  keeptrends <- rep(0, 3)  # previous saved trends
  trends <- rep(0, 3)      # current trends
  trendchanges <- rep(0, 3)# changes in trends
  keept <- maxtime         # time to violation
  time <- data[, 1]         # time vector
  x <- c(1:len)            # x values for trend calculation
  problem <- 0            # flag for problem
  output <- rep(0, 10)    # output matrix
  edge_bound <- -9999999       # edge or boundary, 'e' or 'b'

  # ---- Monitoring loop
  for (i in (len + 1):full_length){ # for each monitoring point

    # calculate all trends (done at each time step; for simplicity, done here)
    for (j in 1:3){
      model <- lm(data[(i-len):(i-1), (j+1)] ~ x)
      trends[j] <-  model$coefficients[2]
      trendchanges[j] <- abs(trends[j] - keeptrends[j])
    }
    # a) check whether absolute difference between actual and expected values exceed limits
    if ((abs(data[i, 2] - hmeans[1]) > hlims[1])
        || (abs(data[i, 3] - hmeans[2]) > hlims[2])
        || (abs(data[i, 4] - hmeans[3]) > hlims[3])) {
        change <- rep("none",3) #tracks what environmental variables change, e.g., if all: ['lighting', 'floor friction', 'gripper']
        # which trend(s) change
        if (abs(data[i, 2] - hmeans[1]) > hlims[1]) change[1] <- "lighting "
        if (abs(data[i, 3] - hmeans[2]) > hlims[2]) change[2] <- "floor friction "
        if (abs(data[i, 4] - hmeans[3]) > hlims[3]) change[3] <- "gripper "
        # a.1) check whether this is a continuing trend
        if ((trendchanges[1] < changelims[1])
            && (abs(trendchanges[2]) < changelims[2])
            && (abs(trendchanges[3]) < changelims[3])) {
        keept <- keept - tstep
        print(c(time[i], "same trends", "predicted violation in ", keept, "seconds." ))
        output <- rbind(output, c(data[i,1:4], "same trend", keept, change, edge_bound))
        # adapt
        if ((keept - timestep) < mintime){
          # need to respond now, so check neighbours
          nay <- checknay(cp, trends, tstep, keept, vmap,  maxtime, 1, msteps)
          if (nrow(nay) > 0) {
            for (j in 1:nrow(nay)) {print(nay[j,])}
          }
          else {print("no adaptation possible.")}
        }
      }
      # a.2) if any environmental measurement has a new trend
      else {
        # calculate time to violation/edge of parameter space from current point
        cp <- c(data[i, 2], data[i, 3], data[i, 4])
        # check time to violation
        t <- violationtime(cp, vmap, trends, tstep, maxtime)
        
        edge_bound <- t[[2]] # "e" or "b"

        if (t[[1]] < maxtime) {
          keept <- t[[1]]
          phrase <- "predicted violation in "
          if (edge_bound == "e") { phrase = "Could reach unknown parameter space in "} # nolint
          print(c(time[i], " new trend", phrase, keept, "seconds." ))
          output <- rbind(output, c(data[i,1:4], "new trend", keept, change, edge_bound))
          keeptrends <- trends
          # adapt
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
    # b) within limits
    else {
      # otherwise all ok
      print(c(time[i], "No problem"))
      output <- rbind(output, c(data[i,1:4], "no problem", -9999999, -9999999, -9999999, -9999999, edge_bound))
    }
  }


# Add a new column with 0 if the 6th column is smaller than 700
# tv <- 700
# output$new_column <- ifelse(output[, 6] < tv, 0, NA)

# Assign the column names to the data frame
colnames(output) <- c("time", "m1", "m2", "m3", "trend",
                    "time2problem", "changeLight", "changeFloor",
                    "changeGripper", "edgeORboundary")



return(output[-1, ])
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
