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
checktrends <- function(data, vmap, tstep, hmeans, hlims,
                        changelims, len, maxtime, mintime, msteps) {
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
