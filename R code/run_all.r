#Check for out of system's domain -- if already out of edge
out_of_edge <- function(s,printnum){
     type = "v"
     if (s[1] <= 0.5 || s[1] >= 1) {
          type = "e"
          if (printnum == 1) print(c(type, "this is an edge point"))
     }
     else if (s[2]<=0.25 || s[2]>=0.5) {
          type = "e"
          if (printnum == 1) print(c(type, "this is an edge point"))
     }
     else if (s[3]<=0.5 || s[3]>=1) {
          type = "e"
          if (printnum == 1) print(c(type, "this is an edge point"))
     }
     return(type)
}

# function to calculate time to violation 
violationtime <- function(s, vmap, trends, tstep, maxtime, msteps, printnum){

     type = out_of_edge(s,printnum)
     if (type == "e") {
          return(list(0, type))
     }
     else{

	border = vmap[which(vmap[,4] == 0),1:3]
	edge = vmap[which(vmap[,4] == 2),1:3]
            borderedge = rbind(border, edge)
	R = sqrt(trends[1]^2 + trends[2]^2 + trends[3]^2)
            x = borderedge[,1]-s[1]
            y = borderedge[,2]-s[2]
            z = borderedge[,3]-s[3]
            betype = c(rep("b", dim(border)[1]), rep("e", dim(edge)[1]))
            type = "v"     
            timepred = 0  
            # before considering a move, check whether already on a border/edge point
            ind0 = which((abs(x) < (msteps[1]/2)) & (abs(y) < (msteps[2]/2))  & (abs(z) < (msteps[3]/2)))
            if (length(ind0) > 0) {
               type = betype[ind0]
               if (printnum == 1) print(c(type, "this is a boundary/edge point"))
            }
            else {
                # don't consider border/edge points in wrong direction
                indsx = which( (abs(x) <  0.00001) | (sign(x) == sign(trends[1])))
                indsy = which( (abs(y) <  0.00001) |(sign(y) == sign(trends[2])))
                indsz = which( (abs(z) <  0.00001) |(sign(z) == sign(trends[3])))
                inds = intersect(indsx, intersect(indsy, indsz)) 
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
                else {
                     if (printnum == 1) print(c(type, "this is a violation point"))
               }
            }
            
            # if time to violation is zero and was not an edge point, this is a violation point
            if(timepred == 0 && type == "v") {
                 type = "b"
                 if (printnum == 1) print(c(type, "this is an violation point (by default)"))
            }

            return(list(timepred, type))
     }
}

plot_initial_data <- function(data) {
  # Plot data
  this <- day  # select data to plot, e.g. day, light, grip, lg
  plot(this[, 1], this[, 2], xlab = "Time", ylab = "Lighting", type = "l")#light
  #plot(this[, 1], this[, 3], xlab = "Time", ylab = "Floor friction", type = "l") # floor # nolint
  #plot(this[, 1], this[, 4], xlab = "Time", ylab = "Gripper", type = "l") # gripper # nolint
}

# function to calculate trends from data
gettrends <- function(data, len){
    full = dim(data)[1]    
    trends = matrix(nrow = (full-len), ncol = 4)
    x = c(1:len)
    for (i in (len+1):full){
         trends[(i-len),1] = data[(i-len),1]
         for (j in 1:3){
             model = lm(data[(i-len):(i-1), (j+1)] ~ x)
             trends[(i-len),(j+1)] =  model$coefficients[2]
         }
    }
    return(trends)
}

##########################
pausing <- function(){
     # Pause execution until the user presses Enter/q/n
     if (pause){
          res <- readline(prompt = "Press [Enter] to continue, [n] to stop pausing, [q] to quit...")
          if (res == "q") {
               stop("User stopped execution")}
          if (res == "n") {
               pause <<- FALSE # global variable
          }
     }
}
##########################


# function to check time from neighbouring violation map points 
checknay <- function(cp, trends, tstep, keept, vmap,  maxtime, msteps, t_adapt){

     # increase neighbours by adding up to n increments in each direction
     n =1
      
     ## Select which adaptations possible, incr/decr[-1,1] in each env
     nlight = c(0,1) #only increase but adding an extra light, or the same
     nfloor = c(-1,0,1) #decrease by cleaning floor, increase by changin the wheels, or the same
     ngrip = c(-1,0,1)  #decresae by cleaning the gripper, increase by changing the gripper, or the same

     ## Select if adaptation resets trend (set 0 when reset)
     trends_after_adapt = c(trends[1],0,0)

     #nt = matrix(0, nrow = ((2*n+1)^3), ncol = 7)
     num_rows <- (length(nlight) * length(nfloor) * length(ngrip)) * n + 1
     nt = matrix(0, nrow = num_rows, ncol = 7)
     
     ind =1
     for (i in n:n) { # or 1:n to check 1 to n increments
          for (j in (i*nlight)) {
                    for (k in (i*nfloor)) {
                         for (l in (i*ngrip)) {
                         nt[ind, 1] = j
                         nt[ind, 2] = k
                         nt[ind, 3] = l
                         ind = ind + 1
                         np = c((cp[1] + j*msteps[1]), (cp[2] + k*msteps[2]), (cp[3] + l*msteps[3]))
                         nt[ind, 4] = violationtime(np, vmap, trends, tstep, maxtime, msteps, 2)[[1]]
                         nt[ind,5:7] <- np #real env. values
                         #print(c(j, k, l, nt[ind, 4]))
                    }}}}
n1=nt 

nt = nt[order(nt[,4],decreasing = TRUE), , drop = FALSE] #more rows than required as it can contain all neighbour incre. if "for (i in 1:n)" in the loop above #NOTE: use , drop = FALSE to keep as matrix, even when only one row satisfy the condition # nolint
nt_ = nt[which(nt[,4] > max(t_adapt,keept)), , drop = FALSE] #t_adapt=mintime
nt__ = nt_[which(nt_[,4] == max(nt_[,4])), , drop = FALSE] #max time

### TESTS
# print(trends)
# n1 = n1[which(n1[, 1] != 0),]
# print("all"); print(n1); print("ordered"); print(nt)
# print(">time to adapt or >current time to BE-zone(keept)"); print(nt_); print("prev. time to viol"); print(keept)
# print("greater time"); print(nt__)
# pausing()
####
return(nt__)
}




check_independent_possible_adaptations <- function(data, output,nay,t_adapt) {
     adaptations = nay
     if(nrow(adaptations) < 2) {
          print("no independent adaptation possible.")
          return()
     }
     ### >>from checknay - Select which adaptations possible, incr/decr[-1,1] in each env
     nlight = c(0,1) #only increase but adding an extra light, or the same
     nfloor = c(-1,0,1) #decrease by cleaning floor, increase by changin the wheels, or the same
     ngrip = c(-1,0,1)  #decresae by cleaning the gripper, increase by changing the gripper, or the same
     n=1
     #####
     # expected values in nt
     nlight=n*nlight; nfloor=n*nfloor; ngrip=n*ngrip

     int = 0

     # Initialize an empty list to store values
     adapt_ <- list()
     
     #==== iterate over Light
     for (j in nlight){ #light
          kl_depend <- 0
          for (k in nfloor){
               for (l in ngrip){
                    val = c(j,k,l)
                    exists <- any(apply(adaptations[, 1:3], 1, function(row) all(row == val)))
                    #print(val); print(exists)
                    # check if not exist adaptation (any combination of k,l with j)
                    if (!exists) { kl_depend <- 1; break }
               }
               if (kl_depend == 1) break
          }
          if (kl_depend==0){ # if not dependent on k and l
               adapt_ <- append(adapt_, list(list("light", j))); int = int + 1
          }
          adapt <- do.call(rbind, adapt_) # convert list to matrix
          # print(adapt)
          # pausing()
     }

     #==== iterate over Floor
     for (j in nfloor){ #floor
          kl_depend <- 0
          for (k in nlight){
               for (l in ngrip){
                    val = c(k,j,l) ##
                    exists <- any(apply(adaptations[, 1:3], 1, function(row) all(row == val)))
                    #print(val); print(exists)
                    # check if not exist adaptation (any combination of k,l with j)
                    if (!exists) { kl_depend <- 1; break }
               }
               if (kl_depend == 1) break
          }
          if (kl_depend==0){ # if not dependent on k and l
               adapt_ <- append(adapt_, list(list("floor", j))); int = int + 1
          }
          adapt <- do.call(rbind, adapt_) # convert list to matrix
          # print(adapt)
          # pausing()
     }
     #==== iterate over Gripper
     for (j in ngrip){ #gripper
          kl_depend <- 0
          for (k in nlight){
               for (l in nfloor){
                    val = c(k,l,j) ##
                    exists <- any(apply(adaptations[, 1:3], 1, function(row) all(row == val)))
                    #print(val); print(exists)
                    # check if not exist adaptation (any combination of k,l with j)
                    if (!exists) { kl_depend <- 1; break }
               }
               if (kl_depend == 1) break
          }
          if (kl_depend==0){ # if not dependent on k and l
               adapt_ <- append(adapt_, list(list("gripper", j))); int = int + 1
          }
          adapt <- do.call(rbind, adapt_) # convert list to matrix
          # print(adapt)
          # pausing()
     }
     if (!is.null(adapt)) {
          print("*independent adaptations:");print(adapt)
          pausing()
          return(adapt)
     }
     else { print("no independent adaptation possible.")}
     # pausing()
}








# function to predict violations 
checktrends <- function(data, vmap, tstep, hmeans, hlims, changelims, len, maxtime, mintime, msteps,adapt=0,t_adapt=1){
    
    # NOTE that adapt=0 (change to 1 if trigger adaptation)
    
    full_length = dim(data)[1]    
    # assume starting trends are all zero
    keeptrends = rep(0, 3)
    trends = rep(0, 3)
    trendchanges = rep(0, 3)
    keept = maxtime
    last_edge_or_boundary = ""
    time = data[,1]
    x = c(1:len)
    problem = 0
    output = rep(0,10)
    for (i in (len+1):full_length){
        # check whether absolute difference between actual and expected values exceed limits                  # nolint
        if ((abs(data[i, 2] - hmeans[1]) > hlims[1]) | (abs(data[i, 3] - hmeans[2]) > hlims[2])               # nolint
          |(abs(data[i, 4] - hmeans[3]) > hlims[3]))  {                                                       # nolint
               # calculate trends
               for (j in 1:3){
                    model = lm(data[(i-len):(i-1), (j+1)] ~ x)
                    trends[j] =  model$coefficients[2]
                    trendchanges[j] = abs(trends[j] - keeptrends[j])
               }

               ######## start trends=0     keep last time to violation and problem (keept,last_edge_or_boundary)                  # nolint
               if (((trends[1]) == 0.0) && ((trends[2]) == 0.0) && ((trends[3]) == 0.0))  {            # nolint
                    print(c(time[i], "new trend 0", "predicted violation in ", keept, "minutes." ))                               # nolint
                    change <- rep("none", 3)                                                                                      # nolint
                    output <- rbind(output, c(data[i,1:4], "new trend 0", keept, change, last_edge_or_boundary))                  # nolint
               }                                                                                                                  # nolint                        
               else{ ####### >start else, trends != 0
                    change = rep("none",3) # check outside limits
                    # which trend(s) change
                    if (abs(data[i, 2] - hmeans[1]) > hlims[1]) change[1] = "lightingOut " 
                    if (abs(data[i, 3] - hmeans[2]) > hlims[2])  change[2] = "floor frictionOut "
                    if (abs(data[i, 4] - hmeans[3]) > hlims[3])  change[3] = "gripperOut "
                    ###### save trend
                    changeTrend = rep("none",3)
                    if ((abs(trendchanges[1]) > changelims[1])){changeTrend[1] = "lighting "} else {changeTrend[1] = "none"}
                    if (abs(trendchanges[2]) > changelims[2]){changeTrend[2] = "floor friction "} else {changeTrend[2] = "none"}
                    if (abs(trendchanges[3]) > changelims[3]){changeTrend[3] = "gripper "} else {changeTrend[3] = "none"}

                    if ((abs(trendchanges[1]) < changelims[1]) & (abs(trendchanges[2]) < changelims[2]) &(abs(trendchanges[3]) < changelims[3]))  {

                              keept = keept-tstep
                              print(c(time[i], "same trends", "predicted violation in ", keept, "minutes." ))
                              output = rbind(output, c(data[i,1:4], "same trend", keept, change, " ", changeTrend))
                              # ========== Adaptation if necessary/possible
                                   if ((keept - tstep) < mintime){        #keept=current time to problem
                                        # need to respond now, so check neighbours
                                        nay = checknay(cp, trends, tstep, keept, vmap,  maxtime, msteps, t_adapt)
                                        if (nrow(nay) > 0) {
                                             # print all possible adaptations
                                             for (j in 1:nrow(nay)) { 
                                                  print(nay[j,])
                                             }
                                             # adapt to the first one
                                             if(adapt == 1) {
                                                  check_independent_possible_adaptations(data,output,nay,t_adapt)
                                                  #break() # stop when adaptation is done (assuming system recovered for the rest of the time)
                                             }
                                                  
                                        }
                                        else { print("no adaptation possible.") }
                                   }
                              # ==========
                    }
                    ###### new trend
                    else {
                              # calculate time to violation/edge of parameter space from current point
                              cp = c(data[i, 2], data[i, 3], data[i, 4])
                              # print(c(cp, trends))
                              t = violationtime(cp, vmap, trends, tstep, maxtime, msteps, 1)
                              if (t[[1]] < maxtime) {

                                   ## Check which one changed in trends
                                   # if (abs(trendchanges[1]) >=changelims[1]) {
                                   #      change[1] = "lighting "
                                   # }
                                   
                                   # if (abs(trendchanges[2]) <=changelims[2]) {
                                   #      change[1] = "floor friction "
                                   # }
                                        
                                   # if (abs(trendchanges[3]) <=changelims[3]) {
                                   #      change[1] = "gripper "
                                   # }

                                   keept = t[[1]]
                                   phrase = "predicted violation in "
                                   if (t[[2]] == "e") { phrase = "Could reach unknown parameter space in "}
                                   print(c(time[i], " new trend", phrase, keept, "minutes." ))
                                   last_edge_or_boundary = t[[2]]
                                   output = rbind(output, c(data[i,1:4], "new trend", keept, change, last_edge_or_boundary, changeTrend))
                                   #keeptrends = trends
                                   for (i in 1:3){
                                        if (change[i] != "none") { keeptrends[i] = trends[i] }
                                   }
                                   # ========== Adaptation if necessary/possible
                                   if ((keept - tstep) < mintime){ #keept=current time to problem
                                        # need to respond now, so check neighbours
                                        nay = checknay(cp, trends, tstep, keept, vmap,  maxtime, msteps, t_adapt)
                                        if (nrow(nay) > 0) {
                                             # print all possible adaptations
                                             for (j in 1:nrow(nay)) { 
                                                  print(nay[j,])
                                             }
                                             # adapt to the first one
                                             if(adapt == 1) {     
                                                  adaptations = nay
                                                  check_independent_possible_adaptations(data,output,adaptations,t_adapt)
                                                  #break() # stop when adaptation is done (assuming system recovered for the rest of the time)
                                             }
                                                  
                                        }
                                        else { print("no adaptation possible.") }
                                   }
                              # ==========
                              }
                              else { print(c(time[i], "new trend providing maximum time" , keept)) }
                                   
                              }
                         }
                    } ####### <end else, trends != 0
         else {
                       # otherwise all ok
                       print(c(time[i], "No problem" )) 
         }
    }
    return(output[-1,])
}



#############################
#>>>import data<<<
day = read.csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv",header = FALSE)
light = read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_filtered.csv", header = FALSE)
grip = read.csv("/Users/grisv/GitHub/Manifest/R code/data/degrading_grip_filtered.csv", header = FALSE)
lg = read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_degrading_grip_filtered.csv", header = FALSE)
# to add "len" normal datapoints to beginning of new data
day1 = read.csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv",header = FALSE)
# plot 
#plot_initial_data(day)
# SPIKY gripper
#if (spikyData == 1){ day = read.csv("/Users/grisv/GitHub/Manifest/R code/data/spiky_gripper_day.csv",header = FALSE)}








###########################

# ---- Section: Import Preparation ---- #
### SELECT params:
#spikyData = 0    #0=normal grip data, 1=spiker
#>>> change data file here, can be light, lightnew, grip or lg <<<
data_file <- "lg"#light  #lightnew #grip  #lg
#>>> Hyperparameters <<<
len = 10 #####<<<< CHANGE from original len = 10   time window to get trends
mintime = 20  # trigger time (ONLY used by python if adapt=1, use tv in python instead)
sigma1 = 4.0 #####<<<< CHANGE from original = 5.0
sigma2 = 4.0 #####<<<< CHANGE from original  = 3.0
# Define the global variable 'pause'
pause <- TRUE
# adaptation
adapt = 1   # to save adaptation in data, 0 = no adaptation, 1 = adaptation

# adaptation time (ONLY used by python if adapt=1)
t_adapt= mintime  #time to perform the adaptation (min)
#############################


#############################
# safe config. data to file (for python code)
write.csv(c(data_file,len,mintime,sigma1,sigma2), "Rconfig.csv", row.names = FALSE)
#############################

#############################
# pass to minutes from seconds 
originalDataIncr = 60
tstep <- originalDataIncr/60
day[, 1] <- day[, 1]/60
light[, 1] <- light[, 1]/60
grip[, 1] <- grip[, 1]/60
lg[, 1] <- lg[, 1]/60
day1[, 1] <- day1[, 1]/60
#############################



# get means and trends from historic data
historicmeans = colMeans(day)[2:4]
# get limits on values
mlim1 = sigma1*sqrt(var(abs(day[,2] - historicmeans[1])))
mlim2 = sigma1*sqrt(var(abs(day[,3] - historicmeans[2])))
mlim3 = sigma1*sqrt(var(abs(day[,4] - historicmeans[3])))
lims = c(mlim1, mlim2, mlim3)

#save means and lims -- python will read these files
write.csv(historicmeans, "1historicmeans.csv", row.names = F)
write.csv(lims, "1lims.csv", row.names = F)


# get trends
historictrends = gettrends (day, len)
l = dim(historictrends)[1]

# get change on trends and limits on change from historic data
change = historictrends[2:l,2:4]- historictrends[1:(l-1),2:4]
# get limits
clim1 = sigma2*sqrt(var(change[,1]))
clim2 = sigma2*sqrt(var(change[,2]))	
clim3 = sigma2*sqrt(var(change[,3]))
changelims = c(clim1, clim2, clim3)

#save changes/trends and lims -- python will read these files
write.csv(change, "2change.csv", row.names = F)
write.csv(changelims, "2lims.csv", row.names = F)


# get violation map
vmap = read.csv("/Users/grisv/GitHub/Manifest/violationMap.csv", header = T)


# ---- Make synthetic data ----
# add "len" normal datapoints to beginning of new data
start = dim(day1)[1] - len+1
end = dim(day1)[1]
extra = day1[start:end,]

# lose jump in faulty light data
lightnew = light
lightnew[72:240,2] = lightnew[72:240,2]+0.08
#############################
# save data from conf. (do here as it might be lightnew, just declared above)
data <- get(data_file) 
#############################

#############################
# ---- Section: Set env. data ---- #
# add "len" normal datapoints to beginning of new data
dataplus = rbind(extra, data)
dataplus$Time = tstep*c(0:(nrow(dataplus)-1)) #generates sequence of integers starting from 0 and multiplies by time_increment
dataplus$V1 = tstep*c(0:(nrow(dataplus)-1)) #to avoid confusion, replace previous time column (named V1)
# Round the second to fourth columns to 9 decimal places (this is to match Julie's answer)
dataplus[, 2:4] = round(dataplus[, 2:4], 9)
#############################

#############################
# ---- Check trends ---- 
#>>> Hyperparameters <<<
# maximum time possible 
maxtime = 6000000
# get msteps from violation map calculation
msteps = c(0.0125, 0.0062, 0.0125)


# Compute trends
out = checktrends(dataplus, vmap, tstep, historicmeans, lims, changelims, len, maxtime, mintime, msteps,adapt,t_adapt)
#############################

#############################
# ---- Section: Python Export Preparation ---- #
# change column names -- python will read these files
colnames(dataplus) = c("time", "m1", "m2", "m3", "-", "--")
colnames(out) = c("time", "m1", "m2", "m3", "trend", "time2problem","lightOutTypical","floorOutTypical","gripperOutTypical","edgeORboundary","light","floor","gripper")
# save data -- python will read these files
write.csv(out, "outputlg.csv", row.names = FALSE)
write.csv(dataplus, "dataplus.csv", row.names = FALSE)
# ---- End of Python Section ---- #
#############################



#############################
# ---- Section: Violation Check ---- #
#>>> Hyperparameters: requirement bounds <<<
R1 = 0.6
R2 = 100
R3 = 0.35

#############################