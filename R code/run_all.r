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



# function to check time from neighbouring violation map points 
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
                           nt[ind, 4] = violationtime(np, vmap, trends, tstep, maxtime, msteps, 2)[[1]]
                           ind = ind + 1
                    }  
            }  
     }  
     s = sort.int(nt[,4], index.return = T, decreasing = TRUE)
     ntsorted = nt[s$ix,]
     return(ntsorted[which(ntsorted[,4] >= (keept + timestep)),])
}


adapt_for_i_onwards <- function(data, output,adaptation,t_adapt,i) {
     #stop if current time + t_adapt exceeds the time in data file
     if (i+t_adapt>dim(data)[1]){
          stop("i+t_adapt exceeds the number of rows in data")
     }

     # get last saved time data row (by looking at last time saved in output)
     row <- which(data[1] == tail(output, n = 1)[1] )

     t_when_adapted = row[1]
     # adapt from row+t_adapt onwards
     keept = 99999 # time to problem
     change = rep("none",3) # no change
     for (j in (row+1):dim(data)[1]){
          if (data[j,1]>=t_when_adapted+t_adapt){
          # adapt data
           output = rbind(output, c(data[j,1:4], "same trend", keept, change, " "))
          }
     }
  return(output)
}


# function to predict violations 
checktrends <- function(data, vmap, tstep, hmeans, hlims, changelims, len, maxtime, mintime, msteps,adapt=0){
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
                    print(c(time[i], "new trend 0", "predicted violation in ", keept, "seconds." ))                               # nolint
                    change <- rep("none", 3)                                                                                      # nolint
                    output <- rbind(output, c(data[i,1:4], "new trend 0", keept, change, last_edge_or_boundary))                  # nolint
               }                                                                                                                  # nolint                        
               else{ ####### start else, trends != 0
                                             
                                             change = rep("none",3)
                                             # which trend(s) change
                                             if (abs(data[i, 2] - hmeans[1]) > hlims[1]) change[1] = "lighting " 
                                             if (abs(data[i, 3] - hmeans[2]) > hlims[2])  change[2] = "floor friction "
                                             if (abs(data[i, 4] - hmeans[3]) > hlims[3])  change[3] = "gripper "
                                             ###### same trend
                                             if ((abs(trendchanges[1]) < changelims[1]) & (abs(trendchanges[2]) < changelims[2]) &(abs(trendchanges[3]) < changelims[3]))  {

                                                       keept = keept-tstep
                                                       print(c(time[i], "same trends", "predicted violation in ", keept, "seconds." ))
                                                       output = rbind(output, c(data[i,1:4], "same trend", keept, change, " "))
                                                       # ========== Adaptation if necessary/possible
                                                            if ((keept - timestep) < mintime){
                                                                 # need to respond now, so check neighbours
                                                                 nay = checknay(cp, trends, tstep, keept, vmap,  maxtime, 1, msteps)
                                                                 if (nrow(nay) > 0) {
                                                                      # print all possible adaptations
                                                                      for (j in 1:nrow(nay)) { 
                                                                           print(nay[j,])
                                                                      }
                                                                      # adapt to the first one
                                                                      if(adapt == 1) {     
                                                                           adaptation = nay[1,]
                                                                           output <- adapt_for_i_onwards(data,output,adaptation,len,i)
                                                                           break() # stop when adaptation is done (assuming system recovered for the rest of the time)
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
                                                            print(c(time[i], " new trend", phrase, keept, "seconds." ))
                                                            last_edge_or_boundary = t[[2]]
                                                            output = rbind(output, c(data[i,1:4], "new trend", keept, change, last_edge_or_boundary))
                                                            #keeptrends = trends
                                                            for (i in 1:3){
                                                                 if (change[i] != "none") { keeptrends[i] = trends[i] }
                                                            }
                                                            # ========== Adaptation if necessary/possible
                                                            if ((keept - timestep) < mintime){
                                                                 # need to respond now, so check neighbours
                                                                 nay = checknay(cp, trends, tstep, keept, vmap,  maxtime, 1, msteps)
                                                                 if (nrow(nay) > 0) {
                                                                      # print all possible adaptations
                                                                      for (j in 1:nrow(nay)) { 
                                                                           print(nay[j,])
                                                                      }
                                                                      # adapt to the first one
                                                                      if (adapt == 1) {
                                                                           adaptation = nay[1,]
                                                                           output <- adapt_for_i_onwards(data,output,adaptation,len,i)
                                                                           break() # stop when adaptation is done (assuming system recovered for the rest of the time)
                                                                      }    
                                                                 }
                                                                 else { print("no adaptation possible.") }
                                                            }
                                                            # ==========
                                                       }
                                                       else { print(c(time[i], "new trend providing maximum time" , keept)) }
                                                       
                                                  }
                                             }
                    } ####### end else, trends != 0
         else {
                       # otherwise all ok
                       print(c(time[i], "No problem" )) 
         }
    }
    return(output[-1,])
}






# ---- Section: Import Preparation ---- #
### SELECT params:
spikyData = 0    #0=normal grip data, 1=spiker
#>>> change data file here, can be light, lightnew, grip or lg <<<
data <- grip #light  #lightnew #grip  #lg
#>>> Hyperparameters <<<
len = 20 #####<<<< CHANGE from original len = 10
sigma1 = 5.0 #####<<<< CHANGE from original = 5.0
sigma2 = 5.0 #####<<<< CHANGE from original  = 3.0
# adapt?
adapt = 0


#>>>import data<<<
day = read.csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv",header = FALSE)
# SPIKY gripper
if (spikyData == 1){ day = read.csv("/Users/grisv/GitHub/Manifest/R code/data/spiky_gripper_day.csv",header = FALSE)}

light = read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_filtered.csv", header = FALSE)
grip = read.csv("/Users/grisv/GitHub/Manifest/R code/data/degrading_grip_filtered.csv", header = FALSE)
lg = read.csv("/Users/grisv/GitHub/Manifest/R code/data/faulty_light_degrading_grip_filtered.csv", header = FALSE)
# plot 
#plot_initial_data(day)



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
day1 = read.csv("/Users/grisv/GitHub/Manifest/R code/data/sample_day_filtered.csv",header = FALSE)
start = dim(day1)[1] - len+1
end = dim(day1)[1]
extra = day1[start:end,]

# lose jump in faulty light data
lightnew = light
lightnew[72:240,2] = lightnew[72:240,2]+0.08


#############################
# ---- Section: Set env. data ---- #
# add "len" normal datapoints to beginning of new data
dataplus = rbind(extra, data)
dataplus$Time = 60*c(0:(nrow(dataplus)-1)) #generates sequence of integers starting from 0 and multiplies by 60
dataplus$V1 = 60*c(0:(nrow(dataplus)-1)) #to avoid confusion, replace previous time column (named V1)
# Round the second to fourth columns to 9 decimal places (this is to match Julie's answer)
dataplus[, 2:4] = round(dataplus[, 2:4], 9)
#############################

#############################
# ---- Check trends ---- 
#>>> Hyperparameters <<<
# monitoring timestep is currently the same for all measurements
timestep = 60
# maximum time possible 
maxtime = 6000000
# get msteps from violation map calculation
msteps = c(0.0125, 0.0062, 0.0125)
# minimum time allowed to predicted violation
mintime = 600

# Compute trends
out = checktrends(dataplus, vmap, timestep, historicmeans, lims, changelims, len, maxtime, mintime, msteps,adapt)
#############################

#############################
# ---- Section: Python Export Preparation ---- #
# change column names -- python will read these files
colnames(dataplus) = c("time", "m1", "m2", "m3", "-", "--")
colnames(out) = c("time", "m1", "m2", "m3", "trend", "time2problem","light","floor","gripper","edgeORboundary")
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

# check a data file for violations 
violationCheck<- function(data, R1, R2, R3){
     for (i in 1:dim(data)[1]){
                   M1 = data[i, 2]
                   M2 = data[i, 3]
                   M3 = data[i, 4]
       P1 =  0.2018 +  (0.8191 * M1)  
       P2 =  0.414 + (0.4612 * M1) 
       P3 = 0.1
       P4 =  -0.1618+ (1.2523 * M3)
       PR = 0.8
       T1 = 0  
       T2 = 19.765 + (15.627 * M1)
       T3 = 63.15
       T1F = 0 
       T2F = 7.343 + ( 72.234 * M1) + (161.485 * M2) - (231.337 * M1*M2)
       TR = 6.498 - (5.482 * M1) - (6.855 * M3) + (8.301 * M1*M3)
                   r1 = (P1*P2*P4) / (1 - PR*P3)
   	        r2 = (P1*(P3*T2 - P2*T3 - P3*T2F - T1 - T2 + T1F) + PR*P3*(T1F - P1*TR - P1*T1F) - T1F) / (PR*P3 -  1)
                   r3 = (P1*P2*(1 - P4)) / (1 - PR*P3)
                   if ((1 - PR*P3) == 0) {
                        print("fail")
                  }
                  else {
                        if (r1 < R1){
                             print(c(data[i,1], "R1 fail"))
                        }
                        if (r2 > R2) {
                             print(c(data[i,1], "R2 fail"))
                        }
                        if (r3 > R3) {
                             print(c(data[i,1], "R3 fail"))
                        }
                 }
     }
}
#############################