#change the name of the csv files in lines 19, 20, and 21 to try different test cases
#the output is the plot found in the pics.png file
{
  require(TSP)
  require(png)
  
  max_travel_distance <- 250000
  max_drone_trip <- 1000
  max_drone_day <- 5000
  speed <- 60000
  
  options(stringsAsFactors=FALSE) #import data as values not factors
  
  png(filename="pics.png", width=500, height=500, type="cairo")  #output plot to an image
  plot(0,0, col = "white", main = "We're sorry, our algorithm encountered an error. Please try again later.", xaxt='n', yaxt='n', axes=F, ylab="", xlab="")  #output error message if code does not run
  dev.off()   #stop outputting plots to the image
  
  #import data from csv files
  data1<- read.table("tc3data1.csv", sep=",", as.is=T)      #data1 file   ex. tc3data1.csv
  data2 <- read.csv(file="tc3data2.csv", head=T, sep=",")   #data2 file   ex. tc3data2.csv
  data3 <- read.csv(file="tc3data3.csv", head=F, sep=",")   #data3 file   ex. tc3data3.csv
  #clean up data matrices
  data3_1 <-data3[1,]
  data1[1,1] <- gsub("﻿", "", x=data1[1,1])
  data3[1,1] <- gsub("﻿", "", x=data3[1,1])
  data1[1,1]<-as.numeric(data1[1,1])
  data1[2,1]<-as.numeric(data1[2,1])
  data1[3,1]<-as.numeric(data1[3,1])
  
  y <- 0  # used to check for valid input
  
  for(i in 1:nrow(data1)){  #remove any excess rows
    if(is.na(data1[i,2])){
      break
    }
  }
  data1<-data1[1:i-1,]
  
  i <- which(data2[,2] == "")[1]  #remove any excess rows
  if(!is.na(i)){
    data2 <- data2[1:i-1,]
  }
  for(i in 1:nrow(data3)){  #remove any excess rows
    if(is.na(data3[i,3])){
      break
    }
  }
  data3<-data3[1:i-1,]
  
  #####clean up data matrices
  if(any(duplicated(data2[,1]))){
    y <- 1   #if y is 1, input is not valid
  }
  if(any(is.na(data2[,1]))){
    y <- 1
  }
  rownames(data2) <- data2[,1]
  data2<- data2[,-1]
  
  #Check Data 1. Do not have location ID or Package ID as NA
  if(any((is.na(as.numeric(as.matrix(data1[1:3,1:2])))))){
    y <- 1
  }
  if(any((is.na(as.numeric(as.matrix(data1[2,3:4])))))){
    y <- 1
  }
  if(any((is.na(as.numeric(as.matrix(data1[4:nrow(data1), 2:4])))))){
    y <- 1
  }
  if(any((data1[4:nrow(data1),4] != 0) & (data1[4:nrow(data1),4] != 1))){
    y <- 1
  }
  if(any(duplicated(data1[4:nrow(data1),1]))){
    y <- 1
  }
  
  #check if the values are numeric and not na in data 2
  if(any(!is.na(as.matrix(data2))  & is.na(as.numeric(as.matrix(data2))))){
    y <- 1
  }
  
  
  #Data 3 Data Validation 
  #Do not have location ID or Package ID as NA.
  if(any(duplicated(data3[,1]))){
    y <- 1
  }
  if(any(is.na(data3[,1:2]))){
    y <- 1
  }
  if(any((data3[,3] != 1) & (data3[,3] != 2) & (data3[,3] != 3))){
    y <- 1
  }
  
  
  if(y == 1){
    png(filename="pics.png", width=500, height=500, type="cairo")  #output plot to an image
    plot(0,0, col = "white", main = "Error in input files. See example files.", xaxt='n', yaxt='n', axes=F, ylab="", xlab="")
    dev.off()   #stop outputting plots to the image
  }
  
  
  
  ##########
  ldata1 <- length(data1[,3])
  if(data1[2,3] < max_travel_distance){ #update max travel distance
    max_travel_distance <- data1[2,3]
  }
  if(data1[1,2] > max_drone_day){ #update max drone travel distance
    max_drone_day <- data1[1,2]
  }
  
  #store store location coordinates and information
  whlocation <- c(as.numeric(data1[3,1]),data1[3,2])
  locations <- matrix(c(data1[4:ldata1,2],data1[4:ldata1,3], data1[4:ldata1,4]),ncol=3)
  rownames(locations) <- c(data1[4:ldata1,1])
  colnames(locations) = c("x", "y", "truckable?")
  numlocations <- length(locations[,1])
  #create matrix to store distance between each location
  distances <- matrix(nrow = numlocations + 1,ncol = numlocations)
  colnames(distances) <- rownames(locations)
  rownames(distances) <- c(rownames(locations),"Warehouse")
  #populate distances matrix
  for(i in 1:numlocations){  
    xy <- c(locations[i,1],locations[i,2]) #coordinates of the current locations
    distances[numlocations+1,i] <- sqrt((whlocation[1]-xy[1])^2 + (whlocation[2]-xy[2])^2) #distance from wh
    
    for(j in 1:numlocations){  #iterate through each location
      xyj <- c(locations[j,1],locations[j,2]) #coordinates of location
      distances[j,i] <- sqrt((xyj[1]-xy[1])^2 + (xyj[2]-xy[2])^2) #distance from location i to j
      if(i==j){
        distances[i,j] <- NA  #NA for distance from location i to i
      }
    }
  }
  
  for(i in 1:numlocations){ #put NAs for any location that is too far away from wh
    if(distances[numlocations+1,i] > max_travel_distance / 2){
      distances[i,] <- NA
      distances[,i] <- NA
    }
  }
  
  #create a matrix to store info about where drones are need
  drone_update <- matrix(nrow = numlocations,ncol = 3)
  colnames(drone_update) <- c("closest location", "distance", "max travels")
  for(i in 1:numlocations){
    minDist = max_travel_distance
    if(locations[i,3] == 0){  #if location is unreachable by truck
      for(j in 1:numlocations){  #iterate through locations
        if(!is.na(distances[i,j]) & locations[j,3] == 1){  #if location j is reachable by truck and a drone can move from i to j
          if(distances[i,j] < minDist){  #if distance is minimum so far, update info
            minDist = distances[i,j]
            drone_update[i,"distance"] = distances[i,j]
            drone_update[i,"closest location"] = j  #index of closest location
            if(distances[i,j] < max_drone_trip/2){ #if distance is less than max drone trip
              drone_update[i,"max travels"] <- floor(max_drone_day / (distances[i,j] *2)) #store max amout of travels a drone can make between the 2 points
            } #end if distance is less than max drone trip
          } #end if distance is minimum so far, update info
        } #end if location j is reachable by truck and a drone can move from i to j 
      } #end for j
    }  #end if location is unreachable by truck
  } #end for i
  rownames(drone_update) <- rownames(locations)
  
  #create matrix to store info about cargo locations
  cargo_locations <- matrix(nrow = length(data3[,1]),ncol = 6)
  colnames(cargo_locations) = c("x", "y", "priority", "original location", "loc index", "d from wh")
  rownames(cargo_locations) <- c(data3[,1])
  numcargo <- length(data3[,1])
  
  for(i in 1:numcargo){  #for all cargo points
    cargo_locations[i,"priority"] <- data3[i,3] #stores priority of cargo
    j <- which(rownames(locations)  == data3[i,2])[1] #j = locations index of cargo location
    if(!is.na(j)){
      if(data3[i,2] == rownames(locations)[j]){  # if the location is unreachable by truck
        if(!is.na(drone_update[j,1])){ #if location is unreachable by truck
          cargo_locations[i,1:2] <- locations[drone_update[j,1],1:2]  #set location the truck will go to
          cargo_locations[i,"original location"] <- j  #stores original location index
          cargo_locations[i,"loc index"] <- drone_update[j,1]  #stores the index of the location that the truck will go to 
          cargo_locations[i,"d from wh"] <- distances[numlocations + 1, cargo_locations[i,5]] #stores distance from wh
        }
        else{  # will run if the location is reachable by truck
          cargo_locations[i,1:2] <- locations[j,1:2]  #storees coordinates of drop off location
          cargo_locations[i,"loc index"] <- j  #stores index of location
          cargo_locations[i,"d from wh"] <- distances[numlocations + 1, cargo_locations[i,5]]  #stores distance from wh
        }
      }
    }
  }
  
  # store important info about data
  truck_capacity <- data1[2,4]
  drones_per_vehicle <- data1[2,2]
  num_trucks <- as.numeric(data1[2,1])
  num_drones <- as.numeric(data1[1,1])
  max_cargo <- truck_capacity*num_trucks
  
  all_cargo_locations <- cargo_locations  # save cargo locations matrix so changes can be made
  cargo_locations <- cargo_locations[complete.cases(cargo_locations[ , "d from wh"]),] #remove locations that are unreachable
  sorted_cargo <- cargo_locations[order(cargo_locations[,3], -cargo_locations[,6], decreasing = T),] #sort cargo by priority than by distance from wh
  sorted_cargo <- as.data.frame(sorted_cargo)  #make it a data frame!
  
  # add location IDs to sorted cargo matrix
  locID <- rownames(locations)[sorted_cargo[,"loc index"]]
  sorted_cargo <- cbind(sorted_cargo, locID)
  colnames(sorted_cargo)[ncol(sorted_cargo)] <- "locID"
  
  # add location IDs of original location to sorted cargo matrix
  locID <- rownames(locations)[sorted_cargo[,"original location"]]
  sorted_cargo <- cbind(sorted_cargo, locID)
  colnames(sorted_cargo)[ncol(sorted_cargo)] <- "O_locID"
  
  #add distance traveled by drone for each cargo point
  sorted_cargo <- cbind(sorted_cargo, rep(0,nrow(sorted_cargo)))
  colnames(sorted_cargo)[ncol(sorted_cargo)] <- "drone_dist"
  sorted_cargo[which(!is.na(sorted_cargo[,"O_locID"])),"drone_dist"] <- drone_update[na.omit(sorted_cargo[,"O_locID"]), "distance"] * 2  #distance traveled by drone
  
  sorted_cargo <- sorted_cargo[sorted_cargo$drone_dist <= max_drone_trip,] #removes points unreachable by truck or drone
  long_sorted_cargo <- sorted_cargo  #save sorted cargo so changes can be made
  
  #find the index after which no more drone points will be delivered
  index_after_drones <- which(cumsum(long_sorted_cargo$drone_dist) > (max_drone_day*num_drones))[1]
  if(is.na(index_after_drones)){ #if all drone points can potentially be delivered
    index_after_drones <- max_cargo + 2 #set index_after_drones to be greater than max_cargo
  }
  rmDrone_sorted_cargo <- long_sorted_cargo
  
  drone_points <- long_sorted_cargo[which(!is.na(long_sorted_cargo[, "O_locID"]))[which(which(!is.na(long_sorted_cargo[, "O_locID"])) <= max_cargo)],] #stores drone points that are likely deliverable
  
  if(max_cargo > index_after_drones){  # if max_cargo is greater than index_after_drones
    rmDrone_sorted_cargo <- long_sorted_cargo[1:index_after_drones-1,] #add points up to the index
    rmDrone_sorted_cargo <- rbind(rmDrone_sorted_cargo, long_sorted_cargo[which(!is.na(long_sorted_cargo[index_after_drones:nrow(long_sorted_cargo), "O_locID"]))+index_after_drones-1,]) #add points after index that do not need to be delivered by drone
    
    drone_points <- long_sorted_cargo[which(!is.na(long_sorted_cargo[, "O_locID"])),]  #store cargo that must be delivered by drone
    derone_index <- which(cumsum(drone_points$drone_dist) > (max_drone_day*num_drones))[1]  #find index after which no more drone points will be delivered
    extra_drone_points <- drone_points[derone_index:nrow(drone_points),] #drone points that probably won't be delivered
    drone_points <- drone_points[1:derone_index-1,] #trims matrix
  }
  
  if(length(sorted_cargo[,1]) > max_cargo){
    sorted_cargo <- sorted_cargo[1:max_cargo,]  #trim sorted cargo if it was longer than the max cargo that can be delivered
  }
  if(length(rmDrone_sorted_cargo[,1]) > max_cargo){
    rmDrone_sorted_cargo <- rmDrone_sorted_cargo[1:max_cargo,] #trim rmdrone sorted cargo if it was longer than the max cargo
  }
  #create matrix of cargo location coordinates
  scargolocations <- matrix(c(rmDrone_sorted_cargo[,1],rmDrone_sorted_cargo[,2]), ncol = 2) #coordinates of locations in rmdrone_sorted_cargo matrix
  rownames(scargolocations) <- rownames(rmDrone_sorted_cargo)
  colnames(scargolocations) = c("x", "y")
  
  
  #create matrix to store cargo with normalized locations
  normal_cargo <- as.matrix(long_sorted_cargo[which(is.na(long_sorted_cargo[, "O_locID"])),1:2])
  
  #create matrix to store drone cargo with normalized locations
  drone_normal_cargo <- matrix(nrow = nrow(drone_points), ncol=2)
  colnames(drone_normal_cargo) <- c("x","y")
  
  #normalize cargo locations
  for(i in 1:nrow(normal_cargo)){
    normal_cargo[i,] <- normal_cargo[i,] - whlocation  #get location relative to warehouse location
    distance <- sqrt(normal_cargo[i,1]^2 + normal_cargo[i,2]^2)  #distance from warehouse to location
    if(distance != 0){  #ensure you do not divide by zero
      normal_cargo[i,] <- normal_cargo[i,]/distance  #divide by distance to normalize
    }
  }
  
  #normalize drone cargo locations
  if(nrow(drone_points) != 0){
    for(i in 1:nrow(drone_normal_cargo)){
      drone_normal_cargo[i,] <- as.matrix(drone_points[i,1:2]) - whlocation
      distance <- sqrt(drone_normal_cargo[i,1]^2 + drone_normal_cargo[i,2]^2)
      drone_normal_cargo[i,] <- drone_normal_cargo[i,]/distance
    }
  }
  
  rownames(drone_normal_cargo) <- rownames(drone_points) #add row names
  
  #add cluster column
  info_normal_cargo <- cbind(normal_cargo, rep(0,nrow(normal_cargo)))
  colnames(info_normal_cargo)[3] <- "cluster"
  #add angle column
  info_normal_cargo <- cbind(info_normal_cargo, atan2(info_normal_cargo[,2], info_normal_cargo[,1]))
  colnames(info_normal_cargo)[4] <- "angle"
  #add cluster column
  if(nrow(drone_normal_cargo != 0)){
    info_d_normal_cargo <- cbind(drone_normal_cargo, rep(0,nrow(drone_normal_cargo)))  #add cluster column
  }else{ #if nrow(drone_normal_cargo) == 0
    info_d_normal_cargo <- matrix(nrow = 0, ncol = 3)  #create matrix
    colnames(info_d_normal_cargo) <- c("x", "y", "pickle")
  }
  colnames(info_d_normal_cargo)[3] <- "cluster"
  #add angle column
  info_d_normal_cargo <- cbind(info_d_normal_cargo, atan2(info_d_normal_cargo[,2], info_d_normal_cargo[,1]))
  colnames(info_d_normal_cargo)[4] <- "angle"
  #add column to represent the distance the drone travels
  info_d_normal_cargo <- cbind(info_d_normal_cargo, as.matrix(drone_points$drone_dist))
  colnames(info_d_normal_cargo)[5] <- "drone_dist"
  
  info_d_normal_cargo <- info_d_normal_cargo[order(info_d_normal_cargo[,"angle"]),] #sort by angle
  under_max_dist <- FALSE  #set false. this variable is used in the while loop and when it is true, the while loop will stop
  while (!under_max_dist){
    if(y == 1){
      break
    }
    #create matrices to represent the database tables so data can be inserted
    table_vehicles <- matrix(nrow= num_trucks + num_drones, ncol=4)
    colnames(table_vehicles) <- c("vehicleID","truck","breakdownCost","disasterID")
    table_packages <-matrix(nrow=numcargo, ncol=5)
    colnames(table_packages) <-c("pacID", "vehicleID","location", "priority", "disasterID")
    table_truck_stops <-matrix(nrow= 0, ncol= 8)
    colnames(table_truck_stops)<-c("truckID","truckstopNum","location","dist2prev", "compl", "cost", "totalTime", "disasterID")
    table_drone_stops <-matrix(nrow=0, ncol=10)
    colnames(table_drone_stops)<-c("droneID","truckstopNum","location","dist2prev", "compl", "cost", "time", "disasterID", "dronestop", "truckID")
    
    #insert data into packages matrix
    rownames(table_packages) <- data3[,1]
    table_packages[,"pacID"] <- shQuote(data3[,1])
    table_packages[,"location"] <- shQuote(data3[,2])
    table_packages[,"priority"] <- data3[,3]
    table_packages[,"vehicleID"] <- "NULL"
    
    tryCatch({
      
      # below is clustering of drone points
      if(nrow(info_d_normal_cargo) != 0){  #if there are points that must be delivered by drone
        maxdif <- max(diff(c(tail(info_d_normal_cargo[,"angle"],1)-2*pi,info_d_normal_cargo[,"angle"])))  #find the greatest angle between any two points
        start_group <- which(diff(c(tail(info_d_normal_cargo[,"angle"],1)-2*pi,info_d_normal_cargo[,"angle"]))==maxdif)  #which location has the greatest angle to an ajacent point
        grouped_d_normal_cargo<-rbind(info_d_normal_cargo[start_group:nrow(info_d_normal_cargo),], info_d_normal_cargo[1:(start_group-1),])  #order points starting at the above point
        if(nrow(grouped_d_normal_cargo) > nrow(info_d_normal_cargo)){
          grouped_d_normal_cargo <- grouped_d_normal_cargo[1:nrow(info_d_normal_cargo),]  #trim grouped_d_normal_cargo if its longer than info
        }
        # add total distance column to represent the total distance a drone has traveled
        grouped_d_normal_cargo <- cbind(grouped_d_normal_cargo, rep(0, nrow(grouped_d_normal_cargo)))
        colnames(grouped_d_normal_cargo)[ncol(grouped_d_normal_cargo)] <- "total_dist"
        
        currentSum <- 0  #represent the current distance traveled by the drone
        clusterNum <- 1  #used to number clusters
        numincluster <- 0 #represents the number of points in a cluster
        for(i in 1:nrow(grouped_d_normal_cargo)){
          currentSum <- currentSum + grouped_d_normal_cargo[i, "drone_dist"]  #add drone travel distance to currentSum
          if(currentSum > max_drone_day || numincluster >= truck_capacity){  #if total drone travel distance is too high or number of points in clusster is at/above max
            currentSum <- grouped_d_normal_cargo[i, "drone_dist"]  #reset currentsum this single distance
            clusterNum <- clusterNum + 1  #move to the next cluster
            numincluster <- 0 #set number in cluster as zero
            if(clusterNum > num_drones){
              break     #break out of loop when the number of clusters is greater than the number of drones
            }
          }
          grouped_d_normal_cargo[i, "total_dist"] <- currentSum  #set the current distance traveld for point i
          grouped_d_normal_cargo[i, "cluster"] <- clusterNum  #set the cluster for point i
          numincluster <- numincluster + 1  #increment number in cluster
        }
        grouped_d_normal_cargo <- grouped_d_normal_cargo[which(grouped_d_normal_cargo[,"cluster"] != 0),] #remove points that werent clustered
        #create drone# column and store the cluster in it
        grouped_d_normal_cargo <- cbind(grouped_d_normal_cargo, grouped_d_normal_cargo[,"cluster"])
        colnames(grouped_d_normal_cargo)[ncol(grouped_d_normal_cargo)] <- "drone#"
        grouped_drone_points <- drone_points[rownames(grouped_d_normal_cargo),]  #save the grouped drone points
        ########  end cluster drone points
        tryCatch({
          if(nrow(info_normal_cargo) > (max_cargo - sum(grouped_d_normal_cargo[,"cluster"] != 0))){
            info_normal_cargo <- info_normal_cargo[1:(max_cargo - sum(grouped_d_normal_cargo[,"cluster"] != 0)),] #trims cargo to be = to max cargo
          }
        }, error=function(e){})
        info_normal_cargo <- info_normal_cargo[order(info_normal_cargo[,"angle"]),] #sort by angle
        drones_used <- rep(1, tail(grouped_d_normal_cargo[,"cluster"], 1)) #create vector to store number of drones used in each cluster
        
        drone_cluster_counts <- table(grouped_d_normal_cargo[,"cluster"]) #stores how many points are in each cluster
        extra_drones <- num_drones - num_trucks #how many more drones than trucks
        ex_drones_used <- 0 #counts number of these extra drones that are used
        cur_num_drones <- num_drones #current number of drones being used
        if(length(drone_cluster_counts) < num_drones){
          cur_num_drones <- length(drone_cluster_counts)  #update number of drones being used
        }
        for(k in 1:3){
          if(num_drones > num_trucks && drones_per_vehicle > k){  #if number of drones is greater than number of trucks and drones per vehicle is greater than k
            for(i in 1:cur_num_drones){
              if(i == cur_num_drones){ #if loop is about to end
                if(drone_cluster_counts[i] + drone_cluster_counts[1] <= truck_capacity){ #if this cluster and the first cluster can be combined
                  #combine the clusters
                  grouped_d_normal_cargo[which(grouped_d_normal_cargo[,"cluster"] == i),"cluster"] <- 1
                  drone_cluster_counts <- table(grouped_d_normal_cargo[,"cluster"]) #update counts
                  ex_drones_used <- ex_drones_used + 1
                  drones_used[1] <- drones_used[1] + 1
                  drones_used[i] <- 0
                  cur_num_drones <- cur_num_drones - 1
                }
              }
              else{
                tryCatch({
                  if(drone_cluster_counts[i] + drone_cluster_counts[i+1] <= truck_capacity){  #if cluster i and the next cluster can be combined
                    #combine the clusters
                    grouped_d_normal_cargo[which(grouped_d_normal_cargo[,"cluster"] > i),"cluster"] <- grouped_d_normal_cargo[which(grouped_d_normal_cargo[,"cluster"] > i),"cluster"] -1
                    drone_cluster_counts <- table(grouped_d_normal_cargo[,"cluster"])
                    ex_drones_used <- ex_drones_used + 1
                    drones_used[i] <- drones_used[i] + 1
                    drones_used <- c(drones_used[-(i+1)],0)
                    cur_num_drones <- cur_num_drones - 1
                  }
                }, error=function(e){})
              }
              if(ex_drones_used >= extra_drones){
                break   #break out of the loop once all drones are used
              }
              
            }
          }
        }
        drone_cluster_counts <- table(grouped_d_normal_cargo[,"cluster"]) #update cluster counts
        add_to_drone <- matrix(nrow = 0, ncol = 7)  #create matrix to store cargo points to be added to the drone clusters
        colnames(add_to_drone) <- colnames(grouped_d_normal_cargo)
        for(i in 1:tail(grouped_d_normal_cargo[,"cluster"], 1)){  #for each drone cluster (each cluster represents one truck)
          if(drone_cluster_counts[i] < truck_capacity){  #if there space still left on truck
            add_cargo <- truck_capacity - drone_cluster_counts[i]  #how many cargo points can be added
            this_drone_cluster <- grouped_d_normal_cargo[which(grouped_d_normal_cargo[,"cluster"] == i),]  #stores currnet cluster
            this_angle <- (head(this_drone_cluster[,"angle"],1) + tail(this_drone_cluster[,"angle"],1)) / 2  #bisects the angle of the cluster
            for(j in 1:add_cargo){
              index <- which.min(abs(this_angle - info_normal_cargo[,"angle"]))  #which point is closest the to bisection
              add_this <- info_normal_cargo[index,]  #stores the closest point
              namelocID <- rownames(info_normal_cargo)[index]  #stored locID of closest point
              add_this["cluster"] <- i  #puts point in the cluster
              add_to_drone <- rbind(add_to_drone, c(add_this,0,0,0))  #adds point to the list of points to be added to the drone clusteres
              rownames(add_to_drone)[nrow(add_to_drone)] <- namelocID  #adds locID
              info_normal_cargo <- info_normal_cargo[-index,]  #removes point from the list of non drone points
            }
          }
        }
        
        grouped_d_normal_cargo <- rbind(grouped_d_normal_cargo, add_to_drone)  #combines drone clusters with the points that were added
        grouped_d_normal_cargo <- grouped_d_normal_cargo[order(grouped_d_normal_cargo[,"cluster"]),] #sort by cluster
        
        num_trucks_left <- num_trucks-tail(grouped_d_normal_cargo[,"cluster"], 1)  #number of trucks that still don't have packages
        cargo_per_cluster <- floor(nrow(info_normal_cargo)/(num_trucks-tail(grouped_d_normal_cargo[,"cluster"], 1))) #how many cargo items each remaining truck should hold
      }  # end if there are points that need to be delivered by drone
      if(nrow(info_d_normal_cargo) == 0){ # if there are points that need to be delivered by drone
        grouped_drone_points <- matrix(nrow = 0, ncol = 10)  #create empty matrix
        grouped_d_normal_cargo <- matrix(nrow = 0, ncol = 4)  #create empty matrix
        colnames(grouped_drone_points) <- c("x","y","priority","original location","loc index", "d from wh","locID","O_locID","drone_dist","cluster")
        colnames(grouped_d_normal_cargo) <- c("x","y","cluster","angle")
        num_trucks_left <- num_trucks #number of trucks that still don't have packages
        cargo_per_cluster <- floor(nrow(info_normal_cargo) / num_trucks)  #how many cargo items each remaining truck should hold
      }
      #cluster reamining cargo locations 
      #belove is grouping based on the minimum of maximum angle of cluster
      cluster_angles <- diff(c(info_normal_cargo[,"angle"],info_normal_cargo[,"angle"]+2*pi), lag= cargo_per_cluster-1) #find angles of potential clusters
      cluster_angles <- matrix(cluster_angles[1:(cargo_per_cluster*num_trucks_left)], ncol=num_trucks_left)  #groups angles that will be together
      maxes <- apply(cluster_angles, 1, max) #find the max angle in each group
      start_group <- which(maxes==min(maxes))[1]  #finds the group with the minimum of the max angle in the group
      grouped_normal_cargo<-rbind(info_normal_cargo[start_group:nrow(info_normal_cargo),], info_normal_cargo[1:(start_group-1),]) #groups cargo starting at start_group
      if(nrow(grouped_normal_cargo) > nrow(info_normal_cargo)){
        grouped_normal_cargo <- grouped_normal_cargo[1:nrow(info_normal_cargo),] #ensure grouped matrix is the same length or smaller than info matrix
      }
      rownames(grouped_normal_cargo)[1] <- rownames(info_normal_cargo)[start_group] #ensure rowname of first row is correct
      for(i in 1:num_trucks_left){  #add cluster numbers to each point
        grouped_normal_cargo[((i-1)*cargo_per_cluster+1):(i*cargo_per_cluster),"cluster"] <- i  #cluster points with the correct number of points in each cluster
      }
      #matrix to store grouped locations
      grouped_locations <- long_sorted_cargo[rownames(grouped_normal_cargo),]
      grouped_locations <- cbind(grouped_locations, grouped_normal_cargo[,"cluster"])
      colnames(grouped_locations)[ncol(grouped_locations)] <- "cluster"
      grouped_locations <- grouped_locations[which(grouped_locations[,"cluster"] != 0),] #removes any locations that were not clustered
      plot(grouped_locations[,1], grouped_locations[,2], col=grouped_locations[,"cluster"] + 1)
      ################## end clustering
      
      #new matrix to store grouped drone points
      grouped_drone_points <- long_sorted_cargo[rownames(grouped_d_normal_cargo),]
      grouped_drone_points <- cbind(grouped_drone_points, grouped_d_normal_cargo[,"cluster"])
      colnames(grouped_drone_points)[ncol(grouped_drone_points)] <- "cluster"
      
      
      png(filename="pics.png", width=500, height=500, type="cairo")  #output plot to an image
      ima <- readPNG("map.png")  #read in image to be plotted over
      plot(rbind(grouped_locations[,1:2],grouped_drone_points[,1:2], whlocation))  #plot locations that being delivered
      mylims <- par("usr")  #gets the limits of the plot
      rasterImage(ima, mylims[1], mylims[3], mylims[2], mylims[4])  #put the image to be plotted over on the plot
      
      truckID <- 0  #set truckID variable to be used in loops
      under_max_dist <- TRUE  #set true to check if changed to false later
      
      grouped_drone_points <- grouped_drone_points[which(!is.na(grouped_drone_points[,"x"])),]  #makes sure there are no NAs
      over_drone_travel <- 0 #set to zero. this variable will be the number of routes that contain drones that are too long
      if(nrow(grouped_drone_points) != 0){
        for(i in 1:tail(grouped_drone_points[,"cluster"],1)){  #for each cluster involving drone points
          truckID <- truckID + 1
          cluster <- cbind(grouped_drone_points[which(grouped_drone_points[,"cluster"] == i),], grouped_d_normal_cargo[which(grouped_d_normal_cargo[,"cluster"]==i), "drone#"])  #set current cluster based on cluster number
          colnames(cluster)[ncol(cluster)] <- "drone#"
          names <- rownames(cluster)  #store ronames
          cluster <- rbind(c(whlocation,rep(0,ncol(grouped_drone_points)-1)), cluster) #add warehouse to cluster
          rownames(cluster) <- c("warehouse",names) #sets correct rownames
          
          #run traveling sales person algorithm to find shortest route
          etsp <- ETSP(cluster[,1:2])
          tour <- solve_TSP(etsp)
          tour_d <- tour_length(tour) #get route length
          if(tour_d > max_travel_distance){  #if route length is greater than max travel distance
            under_max_dist <- FALSE  #set false
            over_drone_travel <- over_drone_travel + 1
          }
          if(under_max_dist){  #if no routes have been too long
            tour_route <- as.integer(tour)  #gets indexes of cluster in order of the route
            tour_route <- c(tour_route[which(tour_route==1):length(tour_route)],tour_route[1:which(tour_route==1)-1]) #route starting at wh
            for(k in 1:length(tour_route)){  #add stop numbers to each location
              cluster[tour_route[k],"stop"] <- k-1
            }
            cluster <- cluster[order(cluster[,"stop"]),]  #order based on stop number
            #put warehouse at end of route
            cluster <- rbind(cluster, cluster[1,])
            cluster <- cluster[-1,]
            cluster[length(cluster[,1]),"stop"] <- length(cluster[,1])
            
            trucks_cluster <- cluster  #store cluster to represent truck stops
            m <- 2
            while(m <= length(trucks_cluster[,1])-1){  #removes repeated locations to list each location only once
              if(trucks_cluster[m,"locID"] == trucks_cluster[m-1,"locID"]){
                trucks_cluster <- trucks_cluster[-m,]
                trucks_cluster[m:nrow(trucks_cluster),"stop"] <- trucks_cluster[m:nrow(trucks_cluster),"stop"]-1  #changes stop numbers
                m <- m-1
              }
              m <- m+1
            }
            drones_in_cluster <- drones_used[i]  #how many drones are being used in this cluster
            #create matrix to be added to table_truck_stops
            truck_route <- matrix(nrow = nrow(trucks_cluster), ncol = 7)
            colnames(truck_route) <- c("truckID", "stop#", "locID", "dist", "complet", "cost", "time")
            # add info to truck_route
            truck_route[,"locID"] <- trucks_cluster[,"locID"]
            truck_route[,"truckID"] <- truckID
            truck_route[,"complet"] <- 1
            truck_route[,"stop#"] <- trucks_cluster[,"stop"]
            for(k in 1:nrow(trucks_cluster)){  #add info about each stop on the truck route
              if(k==1){  #if first stop on route
                truck_route[k,"dist"]<-trucks_cluster[k,"d from wh"]
                truck_route[k,"cost"]<-data2[1,trucks_cluster[k,"locID"]]
              }
              else if(k==length(trucks_cluster[,1])){  #if last stop on route
                truck_route[k,"locID"] <-"Warehouse"
                truck_route[k,"dist"]<-trucks_cluster[k-1,"d from wh"]
                truck_route[k,"cost"]<-data2[trucks_cluster[k-1,"locID"],1]
              }
              else{
                truck_route[k,"dist"] <- distances[trucks_cluster[k,"locID"],trucks_cluster[k-1,"locID"]]
                truck_route[k,"cost"]<-data2[trucks_cluster[k-1,"locID"],trucks_cluster[k,"locID"]]
              }
            }
            table_truck_stops <- rbind(table_truck_stops, cbind(truck_route,0)) # add this truck's stops to the truck stops table
            
            drone_boys <- cluster[which(cluster[,"drone#"] != 0),]  #gets packages that are delivered by drones
            #create matrix to be added to table_drone_routes
            drone_stops <-matrix(nrow=nrow(drone_boys), ncol=10)
            colnames(drone_stops)<-c("droneID","truckstopNum","location","dist2prev", "compl", "cost", "time", "disasterID", "dronestop", "truckID")
            #adds info to drone_stops
            drone_stops[,"truckID"] <- truckID
            drone_stops[,"location"] <- drone_boys[,"O_locID"]
            drone_stops[,"droneID"] <- drone_boys[,"drone#"] + num_trucks
            drone_stops[,"truckstopNum"] <- match(drone_boys[,"locID"], trucks_cluster[,"locID"])
            drone_stops[,"dist2prev"] <- drone_boys[,"drone_dist"]
            drone_stops[,"compl"] <- 1
            for(f in 1:nrow(drone_stops)){
              drone_stops[f,"dronestop"] <- length(which(drone_stops[1:f,"droneID"] == drone_stops[f,"droneID"]))  #counts the number of stops that the drone has made
            }
            table_drone_stops <- rbind(table_drone_stops, drone_stops)  #adds info the table_drone_stops
            
            table_packages[rownames(cluster)[1:nrow(cluster)-1],"vehicleID"] <- truckID  #set vehicleID for each point in cluster
            table_packages[rownames(drone_boys), "vehicleID"] <- drone_boys[,"drone#"] + num_trucks #set vehicleID to be droneID for cargo delivered by drones
            
            #set start and end locations of arrows representing the truck route
            x0 <- c(whlocation[1], locations[truck_route[1:nrow(truck_route)-1,"locID"],"x"])
            y0 <- c(whlocation[2], locations[truck_route[1:nrow(truck_route)-1,"locID"],"y"])
            x1 <- c(locations[truck_route[1:nrow(truck_route)-1,"locID"],"x"], whlocation[1])
            y1 <- c(locations[truck_route[1:nrow(truck_route)-1,"locID"],"y"], whlocation[2])
            arrows(x0, y0, x1, y1, col = i + 1, lwd = 3, angle=30, length=.17)  #add the arrows to the plot
            
            #set start and end locations for line segments between drone start point and drop off point
            x0 <- locations[drone_boys[,"O_locID"], "x"]
            y0 <- locations[drone_boys[,"O_locID"], "y"]
            x1 <- locations[drone_boys[,"locID"], "x"]
            y1 <- locations[drone_boys[,"locID"], "y"]
            segments(x0, y0, x1, y1, col = i+1, lwd=3)  #add line segment between drone start point and drop off point
            points(x0, y0, col = i+1, pch=13, cex=2, lwd=2)  #add symbol at each location where a drone drops of cargo
          }
        }
      }
      
      grouped_locations <- grouped_locations[which(!is.na(grouped_locations[,"x"])),]  #makes sure there are no NAs 
      over_truck_travel <- 0 #set to zero. this variable is the number of routes tht don't contain drones that are longer than the max travel dist
      for(i in 1:tail(grouped_locations[,"cluster"],1)){  #for each cluster not involving drone points
        truckID <- truckID + 1
        cluster <- grouped_locations[which(grouped_locations[,"cluster"] == i),] #set current cluster based on cluster number
        #add warehouse to the cluster 
        names <- rownames(cluster)
        cluster <- rbind(c(whlocation,rep(0,ncol(grouped_locations)-2)), cluster)
        rownames(cluster) <- c("warehouse",names)
        
        #run traveling sales person algorithm to find shortest route
        etsp <- ETSP(cluster[,1:2])
        tour <- solve_TSP(etsp)
        tour_d <- tour_length(tour) #get route length
        if(tour_d > max_travel_distance){  #if route length is great than max travel distances
          under_max_dist <- FALSE #set false
          over_truck_travel <- over_truck_travel + 1
        }
        if(under_max_dist){  #if no routes have been too long
          tour_route <- as.integer(tour)  #gets indexes of cluster in order of the route
          tour_route <- c(tour_route[which(tour_route==1):length(tour_route)],tour_route[1:which(tour_route==1)-1]) #route starting at wh
          for(k in 1:length(tour_route)){  #add stop numbers to each locations
            cluster[tour_route[k],"stop"] <- k-1
          }
          cluster <- cluster[order(cluster[,"stop"]),]  #order based on stop number
          cluster <- rbind(cluster, cluster[1,])
          #put warehouse at end of route
          cluster <- cluster[-1,]
          cluster[length(cluster[,1]),"stop"] <- length(cluster[,1])
          
          trucks_cluster <- cluster #store cluster to represent truck stops
          m <- 2
          while(m <= length(trucks_cluster[,1])-1){ #removes repreated locations to list each location only once
            if(trucks_cluster[m,"locID"] == trucks_cluster[m-1,"locID"]){
              trucks_cluster <- trucks_cluster[-m,]
              trucks_cluster[m:nrow(trucks_cluster),"stop"] <- trucks_cluster[m:nrow(trucks_cluster),"stop"]-1 #changes stop numbers
              m <- m-1
            }
            m <- m+1
          }
          #create matrix to be added to table_truck_stops
          truck_route <- matrix(nrow = nrow(trucks_cluster), ncol = 7)
          colnames(truck_route) <- c("truckID", "stop#", "locID", "dist", "complet", "cost", "time")
          # add info to truck_route
          truck_route[,"locID"] <- trucks_cluster[,"locID"]
          truck_route[,"truckID"] <- truckID
          truck_route[,"complet"] <- 1
          truck_route[,"stop#"] <- trucks_cluster[,"stop"]
          for(k in 1:nrow(trucks_cluster)){  #add info about each stop on the truck route
            if(k==1){  #if first stop on route
              truck_route[k,"dist"]<-trucks_cluster[k,"d from wh"]
              truck_route[k,"cost"]<-data2[1,trucks_cluster[k,"locID"]]
            }
            else if(k==length(trucks_cluster[,1])){  #if last stop on route
              truck_route[k,"locID"] <-"Warehouse"
              truck_route[k,"dist"]<-trucks_cluster[k-1,"d from wh"]
              truck_route[k,"cost"]<-data2[trucks_cluster[k-1,"locID"],1]
            }
            else{
              truck_route[k,"dist"] <- distances[trucks_cluster[k,"locID"],trucks_cluster[k-1,"locID"]]
              truck_route[k,"cost"]<-data2[trucks_cluster[k-1,"locID"],trucks_cluster[k,"locID"]]
            }
          }
          
          table_truck_stops <- rbind(table_truck_stops, cbind(truck_route,0)) # add this truck's stops to the truck stops table
          table_packages[rownames(cluster)[1:nrow(cluster)-1],"vehicleID"] <- truckID  #set vehicleID for each point in cluster
          
          #set start and end locations of arrows representing the truck route
          x0 <- c(whlocation[1], locations[truck_route[1:nrow(truck_route)-1,"locID"],"x"])
          y0 <- c(whlocation[2], locations[truck_route[1:nrow(truck_route)-1,"locID"],"y"])
          x1 <- c(locations[truck_route[1:nrow(truck_route)-1,"locID"],"x"], whlocation[1])
          y1 <- c(locations[truck_route[1:nrow(truck_route)-1,"locID"],"y"], whlocation[2])
          arrows(x0, y0, x1, y1, col = i+1+tail(grouped_locations[,"cluster"],1), lwd = 3, angle=30, length=.17)  #add the arrows to the plot
        }
      }
      deliveredLocations <- rbind(grouped_drone_points, grouped_locations)  #create matrix of every cargo that was delivered
      if(!under_max_dist){ #if one or more routes are too long
        grouped_locations <- cbind(grouped_locations, info_normal_cargo[rownames(grouped_locations),"angle"])  #add angle column
        colnames(grouped_locations)[ncol(grouped_locations)] <- "angle"
        grouped_locations <- grouped_locations[order(round(grouped_locations[,"d from wh"], digits = -3), grouped_locations[,"angle"], decreasing = T),]  #order by d from wh then by angle
        info_normal_cargo <- info_normal_cargo[!rownames(info_normal_cargo) %in% (rownames(grouped_locations)[1:(over_drone_travel + over_truck_travel)]),]
        
        grouped_drone_points <- cbind(grouped_drone_points, info_d_normal_cargo[rownames(grouped_drone_points),"angle"])  #add angle column
        colnames(grouped_drone_points)[ncol(grouped_drone_points)] <- "angle"
        grouped_drone_points <- grouped_drone_points[order(round(grouped_drone_points[,"d from wh"], digits = -3), grouped_drone_points[,"angle"], decreasing = T),]  # order by d from wh
        #remove points that are far from warehouse
        #clustering / routing will be reran until all routes are of viable length
        info_d_normal_cargo <- info_d_normal_cargo[!rownames(info_d_normal_cargo) %in% (rownames(grouped_drone_points)[1:over_drone_travel]),]
      }
    }, error=function(e){})
  }
  
  table_truck_stops[,"totalTime"] <- as.numeric(table_truck_stops[,"dist2prev"]) / speed  #calculate time taken in hours 
  table_vehicles[,"breakdownCost"] <- 0  #save breakdownCost as 0 for now
  table_drone_stops[,"time"] <- as.numeric(table_drone_stops[,"dist2prev"]) / speed  #calculate time taken in hours
  
  breakdown <- function(){  #simulates reported breakdowns, to be run for every route stop
    broken <- rnorm(1)  #select from normal distribution
    if (broken >= 2.75){  #if z value is greater than 2.75, approx. 0.3% chance
      broken <- 1}  #vehicle is broken
    else{
      broken <- 0}  #vehicle is not broken
    return(broken)
  }
  repairImpact <- function(dist1, dist2){  #simulates repair time/cost for breakdowns, given distances from warehouse of both stops
    dist <- max(c(dist1, dist2))  #choose farthest distance
    travelTime <- (dist)/1000  #travel time in minutes based on 60 km/h average speed, 60 min per hour.  This is time TO the breakdown, not round-trip
    fixTime <- abs(rnorm(1, 15, 30))  #time to repair breakdown, mean 15 min, std dev 30 min
    time <- travelTime + fixTime #total time until truck is fixed (minutes)
    laborCost <- 15 * ((travelTime * 2  + fixTime)/ 60) #Labor/operating cost based on $15/hr, round-trip plus time to fix
    parts <- seq(0, 500, 50)  #Parts cost from $0-$500, in $50 increments
    partsCost <- sample(parts, 1) #Parts cost for repairing this breakdown
    cost <- partsCost + laborCost
    cost <- round(cost, 2) #round cost to cents
    time <- time/60 # change time to hours
    impact <- c(time, cost) #return both time and cost
    return(impact)
  }
  breakdowns <- replicate(nrow(table_truck_stops), breakdown())  #vector of if breakdowns occur at each stop
  indeces_of_breakdown <- which(breakdowns == 1)  # at what stops do break downs occur 
  if(length(indeces_of_breakdown) != 0){  #if there is at least one break down
    for(i in 1:length(indeces_of_breakdown)){  #for each break down
      if(indeces_of_breakdown[i] == 1){  #if breakdown happens at first stop
        dist1 <- distances["Warehouse", table_truck_stops[indeces_of_breakdown[i],"location"]]  #distance from wh to location
        dist2 <- 0
      }
      else if(table_truck_stops[indeces_of_breakdown[i] - 1,"location"] == "Warehouse"){
        dist1 <- distances["Warehouse", table_truck_stops[indeces_of_breakdown[i],"location"]] #distance from wh to location
        dist2 <- 0
      }
      else if(table_truck_stops[indeces_of_breakdown[i],"location"] == "Warehouse"){  #if breakdown happens on the way to or from the warehouse)
        dist1 <- distances["Warehouse", table_truck_stops[indeces_of_breakdown[i]-1,"location"]] #distance from wh to location
        dist2 <- 0
      }
      else{
        dist1 <- distances["Warehouse", table_truck_stops[indeces_of_breakdown[i],"location"]]  #distance from wh to location
        dist2 <- distances["Warehouse", table_truck_stops[indeces_of_breakdown[i]-1,"location"]] #distance from wh to last location
      }
      impact <- repairImpact(dist1, dist2)  #calculate time/cost impact
      table_vehicles[as.numeric(table_truck_stops[indeces_of_breakdown[i], "truckID"]), "breakdownCost"] <- as.numeric(table_vehicles[as.numeric(table_truck_stops[indeces_of_breakdown[i], "truckID"]), "breakdownCost"]) + impact[2]  #add cost impact
      table_truck_stops[indeces_of_breakdown[i], "totalTime"] <- as.numeric(table_truck_stops[indeces_of_breakdown[i], "totalTime"]) + impact[1]  #add time impact
    }
  }
  breakdowns <- replicate(nrow(table_drone_stops), breakdown())  #vector of if breakdowns occur at each stop
  indeces_of_breakdown <- which(breakdowns == 1)  # at what stops do break downs occur 
  if(length(indeces_of_breakdown) != 0){#if there is at least one break down
    for(i in 1:length(indeces_of_breakdown)){  #for each break down
      dist1 <- distances["Warehouse", table_drone_stops[indeces_of_breakdown[i],"location"]]  #distance from wh to location
      dist1 <- dist1 + (max_drone_trip / 2)
      dist2 <- 0
      impact <- repairImpact(dist1, dist2)  #calculate time/cost impact
      table_vehicles[as.numeric(table_drone_stops[indeces_of_breakdown[i], "droneID"]), "breakdownCost"] <- as.numeric(table_vehicles[as.numeric(table_drone_stops[indeces_of_breakdown[i], "droneID"]), "breakdownCost"]) + impact[2]  #add cost impact
      table_drone_stops[indeces_of_breakdown[i], "time"] <- as.numeric(table_drone_stops[indeces_of_breakdown[i], "time"]) + impact[1]  #add time impact
    }
  }
  for(i in 1:num_trucks){
    #sum time taken 
    trucktime <- sum(as.numeric(table_truck_stops[which(as.numeric(table_truck_stops[,"truckID"]) == i), "totalTime"]))
    dronetime <- sum(as.numeric(table_drone_stops[which(as.numeric(table_drone_stops[,"truckID"]) == i), "time"]))
    totaltime <- trucktime + dronetime
    if(totaltime > 24){  #if time is greater than one day
      numstops <- nrow(table_truck_stops[which(as.numeric(table_truck_stops[,"truckID"]) == i),]) - 2  #number of truck stops minus 2
      for(j in numstops:1){
        this_table <- table_truck_stops[which(as.numeric(table_truck_stops[,"truckID"]) == i), ]  #get the stops
        #add up time taken up to this point 
        time <- sum(as.numeric(this_table[1:j, "totalTime"])) + (1000/60000*25)  #(1000/60000*25) is upper limit from drone time
        time <- time + distances["Warehouse", this_table[j,"location"]] / speed
        if(time < 24){  #if less than one day, this will be the last stop
          break
        }
      }
      table_truck_stops[which(as.numeric(table_truck_stops[,"truckID"]) == i)[(j+1):length(which(as.numeric(table_truck_stops[,"truckID"]) == i))], "compl"] <- 0  #indicate that stops that arent completed are not completed
      #find which drone stops will not be completed
      droneones <- which(as.numeric(table_drone_stops[,"truckID"]) == i)
      whichdrone <- which(table_drone_stops[droneones,"truckstopNum"] > j)
      table_drone_stops[droneones[whichdrone], "compl"] <- 0  #indicate the stops are not completed
    }
  }
  dev.off()   #stop outputting plots to the image
}
