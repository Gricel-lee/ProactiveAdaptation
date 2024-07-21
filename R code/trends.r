# ---- Section: Data Preparation ----

#https://drive.google.com/drive/u/0/folders/1Rs0mhhz0iw9Jcmwr907wVTw2miBMzqLH
day = read.csv("/Users/grisv/GitHub/Manifest/R code/sample_day_filtered.csv")
light = read.csv("/Users/grisv/GitHub/Manifest/R code/faulty_light_filtered.csv")
grip = read.csv("/Users/grisv/GitHub/Manifest/R code/degrading_grip_filtered.csv")	
lg = read.csv("/Users/grisv/GitHub/Manifest/R code/faulty_light_degrading_grip_filtered.csv")

plot(light[,1], light[,2], xlab = "Time", ylab = "Lighting", type = "l")
plot(light[,1], light[,3], xlab = "Time", ylab = "Floor friction", type = "l")
plot(light[,1], light[,4], xlab = "Time", ylab = "Gripper", type = "l")

# Shift the time of the faulty data by 2 hours,
# then add it to the day data
light2 = light
light2[,1] = light[,1] + 7200
datalight = rbind(day, light2)

#Same for the grip
grip2 = grip
grip2[,1] = grip[,1] + 7200
datagrip = rbind(day, grip2)

#Same for the lg
lg2 = lg
lg2[,1] = lg[,1] + 7200
datalg = rbind(day, lg2)

# function to calculate trends from data:
# 1) for a range of values of the data in matrix "data", remove the first "size(len)" data.
# 2) for the rest of the data, obtain the trends 

# data=day is a matrix with time in first column, every row is an observation
# len is the TIME WINDOW, i.e., number of data points to use in trend calculation
gettrends <- function(data, len){
    full = dim(data)[1]     #dim(data)[1] is the number of rows in the data
    trends = matrix(nrow = (full-len), ncol = 4) #matrix with full-len rows (time window) and 4 columns (time, light, friction, gripper)
    x = c(1:len) #sequence of integers from 1 to len
    for (i in (len+1):full){ #for each row in the data
         trends[(i-len),1] = data[(i-len),1] #first column of trends: add the time stamp from the original data into the 'trends' matrix
         for (j in 1:3){ # second to fourth columns of trends:
            # get model   (lm() function fits a linear model)
            # argument 1: gets rows in data from current -1 (i-1, where the i row is from which getting the trend)  
            #             to current - len (i-len), which represents the time window
            #             from column (j+1), which represents, light, friction or gripper
            # argument 2: fits it as an independable variable x
            model = lm(data[(i-len):(i-1), (j+1)] ~ x) 
            # get slope
            trends[(i-len),(j+1)] =  model$coefficients[2]#extracts the 'slope' of the relationship between the variable and x using model$coefficients[2]
         }
    }
    return(trends)
}

# I AM HERE <--------

len = 10

historictrends = gettrends (day, len)
print(historictrends)

# lim1 = 2*sqrt(var(historictrends[,2]))
# lim2 = 2.5*sqrt(var(historictrends[,3]))
# lim3 = 2.5*sqrt(var(historictrends[,4]))
# trendlims = c(lim1, lim2, lim3)

# plot(historictrends[,1], historictrends[,2], xlab = "Time", ylab = "Lighting trends ", type = "l", ylim = c(-1.5*lim1, 1.5*lim1))
# abline(h = lim1, col = "purple")
# abline(h = -lim1, col = "purple")
# abline(h = 0, col = "green")

# plot(historictrends[,1], historictrends[,3], xlab = "Time", ylab = "Floor friction trends ", type = "l", ylim = c(-1.5*lim2, 1.5*lim2))
# abline(h = lim2, col = "purple")
# abline(h = -lim2, col = "purple")
# abline(h = 0, col = "green")


# plot(historictrends[,1], historictrends[,4], xlab = "Time", ylab = "Gripper trends ", type = "l", ylim = c(-1.5*lim3, 1.5*lim3))
# abline(h = lim3, col = "purple")
# abline(h = -lim3, col = "purple")
# abline(h = 0, col = "green")


# l = dim(historictrends)[1]
# change = historictrends[2:l,2:4]- historictrends[1:(l-1),2:4]

# clim1 = 2*sqrt(var(change[,1]))
# clim2 = 2*sqrt(var(change[,2]))
# clim3 = 2*sqrt(var(change[,3]))

# changelims = c(clim1, clim2, clim3)

# plot(historictrends[2:l,1], change[,1], xlab = "Time", ylab = "Lighting trend changes ", type = "l", ylim = c(-2*clim1, 2*clim1))
# abline(h = clim1, col = "purple")
# abline(h = -clim1, col = "purple")
# abline(h = 0, col = "green")

# plot(historictrends[2:l,1], change[,2], xlab = "Time", ylab = "Friction trend changes ", type = "l", ylim = c(-2*clim2, 1.5*clim2))
# abline(h = clim2, col = "purple")
# abline(h = -clim2, col = "purple")
# abline(h = 0, col = "green")

# plot(historictrends[2:l,1], change[,3], xlab = "Time", ylab = "Gripper trend changes ", type = "l", ylim = c(-2*clim3, 2*clim3))
# abline(h = clim2, col = "purple")
# abline(h = -clim2, col = "purple")
# abline(h = 0, col = "green")





# # function to calculate time to violation 
# violationtime <- function(s, vmap, trends, tstep, maxtime){
# 	 border = vmap[which(vmap[,4] == 0),1:3]
# 	 edge = vmap[which(vmap[,4] == 2),1:3]
#             borderedge = rbind(border, edge)
# 	R = sqrt(trends[1]^2 + trends[2]^2 + trends[3]^2)
#             x = borderedge[,1]-s[1]
#             y = borderedge[,2]-s[2]
#             z = borderedge[,3]-s[3]
#             betype = c(rep("b", dim(border)[1]), rep("e", dim(edge)[1]))
#             # don't consider border/edge points in wrong direction
#             indsx = which( (abs(x) <  0.00001) | (sign(x) == sign(trends[1])))
#             indsy = which( (abs(y) <  0.00001) |(sign(y) == sign(trends[2])))
#             indsz = which( (abs(z) <  0.00001) |(sign(z) == sign(trends[3])))
#             inds = intersect(indsx, intersect(indsy, indsz)) 
#             type = "0"     
#             timepred = 0  
#             if (length(inds) > 0) {
#                    x = x[inds]
#                    y = y[inds]
#                    z = z[inds]
#                    keep = borderedge[inds,]
#                    betype = betype[inds]
#                    if (length(inds) > 1) {
#                         B = sqrt(x^2 + y^2 + z^2)
# 	             C = (x* trends[1] + y*trends[2] + z*trends[3])/(R*B)
# 	             # deal with precision problems
#                         C[which(C > 1)] = 1
#                         C[which(C < -1)] = -1
#  	             angle = acos(C) 
#                         angle = (angle + 2*pi) %% (2*pi)          
#                         sorted =  sort.int(angle, index.return = T)
#                         # could be multiple points with the same angle 
#                         allsame = sorted$ix[which(sorted$x == sorted$x[1])]
#                          # find the point with the shortest distance in direction of change
#                          ix = allsame[which(B[allsame] == min(B[allsame]))]
#                          keepx = keep[ix,1]
#                          keepy = keep[ix,2]
#                          keepz = keep[ix,3]
#                          type = betype[ix]
#                    } else {
#                          keepx = keep[1]
#                          keepy = keep[2]
#                          keepz = keep[3]    
#                          type = betype[inds]       
#                    }
#                    displacement = sqrt((keepx-s[1])*(keepx-s[1])+ (keepy-s[2])*(keepy-s[2])+ (keepz-s[3])*(keepz-s[3]))
#                    velocity = sqrt(trends[1]*trends[1] + trends[2]*trends[2] +  trends[3]* trends[3])/tstep        
#                    timepred = maxtime                  
#                    if (velocity > 0) timepred = displacement/velocity              
#                    timepred = floor(timepred + 0.5)
#             }
# 	return(list(timepred, type))
# }

# vmap = read.csv("violationMap.csv", header = T)



















# # add "len" normal datapoints to beginning of new data
# start = dim(day)[1] - len+1
# end = dim(day)[1]
# extra = day[start:end,]

# #change data file here
# data <- lg
# dataplus = rbind(extra, data)
# dataplus$Time = 60*c(0:(nrow(dataplus)-1))

# plot(dataplus[,1], dataplus[,2], xlab = "Time", ylab = "Lighting", type = "l")
# plot(dataplus[,1], dataplus[,3], xlab = "Time", ylab = "Friction", type = "l")
# plot(dataplus[,1], dataplus[,4], xlab = "Time", ylab = "Gripper", type = "l")


# newtrends = gettrends (dataplus, len)

# plot(newtrends[,1], newtrends[,2], xlab = "Time", ylab = "Lighting trend", type = "l")
# abline(h = lim1, col = "purple")
# abline(h = -lim1, col = "purple")
# abline(h = 0, col = "green")

# plot(newtrends[,1], newtrends[,3], xlab = "Time", ylab = "Friction trend", type = "l")
# abline(h = lim2, col = "purple")
# abline(h = -lim2, col = "purple")
# abline(h = 0, col = "green")

# plot(newtrends[,1], newtrends[,4], xlab = "Time", ylab = "Gripper trend", type = "l")
# abline(h = lim3, col = "purple")
# abline(h = -lim3, col = "purple")
# abline(h = 0, col = "green")


# newlen = dim(newtrends)[1]
# newchange = newtrends[2:newlen,2:4]- newtrends[1:(newlen-1),2:4]
	
# plot(newtrends[2:newlen,1], newchange[,1], xlab = "Time", ylab = "Lighting change", type = "l")
# abline(h = clim1, col = "purple")
# abline(h = -clim1, col = "purple")
# abline(h = 0, col = "green")

# plot(newtrends[2:newlen,1], newchange[,2], xlab = "Time", ylab = "Friction change", type = "l")
# abline(h = clim2, col = "purple")
# abline(h = -clim2, col = "purple")
# abline(h = 0, col = "green")

# plot(newtrends[2:newlen,1], newchange[,3], xlab = "Time", ylab = "Gripper change", type = "l")
# abline(h = clim3, col = "purple")
# abline(h = -clim3, col = "purple")
# abline(h = 0, col = "green")





# # function to check time from neighbouring violation map points 
# checknay <- function(cp, trends, tstep, keept, vmap,  maxtime, n, msteps){
#      nt = matrix(0, nrow = ((2*n+1)^3), ncol = 4)
#      ind = 1
#      for (j in c(-n,0,n)) {
#             for (k in c(-1,0,n)) {
#                     for (l in c(-n,0,n)) {
#                            nt[ind, 1] = j
#                            nt[ind, 2] = k
#                            nt[ind, 3] = l
#                            np = c((cp[1] + j*msteps[1]), (cp[2] + k*msteps[2]), (cp[3] + l*msteps[3]))
#                            nt[ind, 4] = violationtime(np, vmap, trends, tstep, maxtime)[[1]]
#                            ind = ind + 1
#                     }  
#             }  
#      }  
#      s = sort.int(nt[,4], index.return = T, decreasing = TRUE)
#      ntsorted = nt[s$ix,]
#      return(ntsorted[which(ntsorted[,4] >= (keept + timestep)),])
# }

# # function to predict violations 
# checktrends <- function(data, vmap, tstep, trendlims, changelims, len, maxtime, mintime, msteps){
#     full_length = dim(data)[1]    
#     # assume starting trends are all zero
#     keeptrends = rep(0, 3)
#     trends = rep(0, 3)
#     trendchanges = rep(0, 3)
#     keept = maxtime
#     time = data[,1]
#     x = c(1:len)
#     problem = 0
#     output = rep(0,10)
#     for (i in (len+1):full_length){
#          for (j in 1:3){
#              model = lm(data[(i-len):(i-1), (j+1)] ~ x)
#              trends[j] =  model$coefficients[2]
#              trendchanges[j] = abs(trends[j] - keeptrends[j])
#          }
#          # check whether trends exceed limits
#          if ((abs(trends[1]) > trendlims[1]) | (abs(trends[2]) > trendlims[2]) |(abs(trends[3]) > trendlims[3]))  { 
#                   change = rep("none",3)
#                   # which trend(s) change
#                   if (abs(trends[1]) > trendlims[1]) change[1] = "lighting " 
#                   if (abs(trends[2]) > trendlims[2]) change[2] = "floor friction "
#                   if (abs(trends[3]) > trendlims[3]) change[3] = "gripper "
#                   # check whether this is a continuing trend
#                   if ((trendchanges[1] < changelims[1]) & (abs(trendchanges[2]) < changelims[2]) &(abs(trendchanges[3]) < changelims[3]))  {
#                            keept = keept-tstep
#                            print(c(time[i], "same trends", "predicted violation in ", keept, "seconds." ))
#                            output = rbind(output, c(data[i,1:4], "same trend", keept, change, " "))
#                   }
#                   else {
#                          # calculate time to violation/edge of parameter space from current point
#                          cp = c(data[i, 2], data[i, 3], data[i, 4])
#                          t = violationtime(cp, vmap, trends, tstep, maxtime)
#                          if (t[[1]] < maxtime) {
#                                keept = t[[1]]
#                                phrase = "predicted violation in "
#                                if (t[[2]] == "e") { phrase = "Could reach unknown parameter space in "}
#                                print(c(time[i], " new trend", phrase, keept, "seconds." ))
#                                output = rbind(output, c(data[i,1:4], "new trend", keept, change, t[[2]]))
#                                keeptrends = trends
#                                if ((keept - timestep) < mintime){
#                                      # need to respond now, so check neighbours
#                                      nay = checknay(cp, trends, tstep, keept, vmap,  maxtime, 1, msteps)
#                                      if (nrow(nay) > 0) {
#                                               for (j in 1:nrow(nay)) { print(nay[j,]) }
#                                      }
#                                      else { print("no adaptation possible.") }
#                                }
#                          }
#                          else { print(c(time[i], "new trend providing maximum time" , keept)) }
#                   }
#          }             
#          else {
#                        # otherwise all ok
#                        print(c(time[i], "No problem" )) 
#          }
#     }
#     return(output[-1,])
# }

# # monitoring timestep is currently the same for all measurements
# timestep = 60
# # maximum time possible 
# maxtime = 6000000
# # get msteps from violation map calculation
# msteps = c(0.0250, 0.0125, 0.0250)
# # minimum time allowed to predicted violation
# mintime = 600



# out = checktrends(dataplus, vmap, timestep, trendlims, changelims, len, maxtime, mintime, msteps)
# write.csv(out, "outputlg.csv", row.names = F)
 









# # function to determmine whether each data point is safe or not
# checkSBV <- function(data, vmap){
#     v = rep(100, dim(data)[1])
#     for (i in 1:dim(data)[1]){
#     dmin = 100
#     for (j in 1: dim(vmap)[1]){
# dis = sqrt((data[i,2]-vmap[j,1])*(data[i,2]-vmap[j,1])+ (data[i,3]-vmap[j,2])*(data[i,3]-vmap[j,2])+   (data[i,4]-    vmap[j,3])*(data[i,4]-vmap[j,3])) 
#          if (dis < dmin) 
#          {
#              dmin = dis
#              keep = j 
#         }
#     }
#     v[i] = vmap[keep,4]
#     }
#     return(v)
# }

# sbv = checkSBV(dataplus, safezone)




