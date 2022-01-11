
#THE ORDER IN WHICH ORDERS ARE EXECUTED IS IMPORTANT FOR THEIR CORRECT IMPLEMENTATION!!
#1) retreat
#2) disband
#3) move & unstack
#4) recruit
#5) captured ruler

#0) EXECUTE ORDERS ---------------------------
execute_orders<-function(orders,territories,nations){
  
  orders <- execute_retreat(orders,territories)
  territories <- execute_disband(orders,territories)
  territories <- execute_move(orders,territories)
  territories <- execute_recruit(orders,territories)
  temp <- capture_ruler(orders,territories, nations)
  territories <- temp$territories
  nations <- temp$nations
  
  return(list(territories=territories, nations=nations))
}



#1) RETREAT ---------------------------
execute_retreat <- function(orders,territories){
  
  if(nrow(orders)>0){
    for (i in 1:nrow(orders)){
      if((orders$success[i])&(orders$type[i]=="retreat")){ #SUCCESSFUL RETREAT -> MOVE
        orders$type[i]<-"move"
      } 
      if ((!orders$success[i]) & (orders$type[i]=="retreat")){ #FAILED RETREAT -> DISBAND
        orders$success[i]<-TRUE
        orders$type[i]<-"disband"
      }
    }
  }
  
  return(orders)
}



#2) DISBAND ---------------------------
execute_disband <- function(orders,territories){
  
  #keep only successful move orders
  orders <- orders[(orders$success==TRUE)&(orders$type=="disband"),]
  
  if(nrow(orders)>0){
    for (i in 1:nrow(orders)){
      tile <- as.numeric(orders$tile_number[i])
      
      #remove troops from territory
      territories$data$n_troops[tile] <- territories$data$n_troops[tile] -1 
      if(territories$data$n_troops[tile]==0){
        territories$data$troops[tile] <- NA
      }

      #update displaying size for troops on map
      territories$data$size_display_troops[tile] <- ifelse(territories$data$n_troops[tile] %in% c(0,1),1,2)
      
    }
  }
  return(territories)
}


#3) MOVE & UNSTACK---------------------------
execute_move <- function(orders,territories){
  
  #keep only successful move orders
  orders <- orders[(orders$success==TRUE)&(orders$type %in% c("move","unstack")),]
  
  if(nrow(orders)>0){
    for (i in 1:nrow(orders)){
      
      from <- as.numeric(orders$tile_number[i])
      to <- as.numeric(orders$to[i])
      
      #check whether troops at destination are from the same nation as origin (or if there is no troop at destination)
      if ((orders$troops_source[i] !=  territories$data$troops[to])|is.na(territories$data$troops[to])){ #NOT SAME NATION
        #replicate troops in destination territory by removing what was present there
        territories$data$troops[to] <- orders$troops_source[i]
        territories$data$n_troops[to] <- orders$moving_troops[i]
      } else {#SAME NATION
        #add troops to destination territory
        territories$data$n_troops[to] <- territories$data$n_troops[to] + orders$moving_troops[i]
      }
      
      #remove troops from origin territory (if not already occupied by other nation)
      if(territories$data$troops[from] == orders$troops_source[i]){
        territories$data$n_troops[from] <- territories$data$n_troops[from] - orders$moving_troops[i]
        territories$data$troops[from] <- ifelse(territories$data$n_troops[from]==0,NA,orders$troops_source[i])
      }
      
      #update displaying size for troops on map
      territories$data$size_display_troops[c(from,to)] <- ifelse(territories$data$n_troops[c(from,to)] %in% c(0,1),1,2)
    }
  }
  return(territories)
}


#4) RECRUIT ---------------------------
execute_recruit <- function(orders,territories){
  
  #keep only successful move orders
  orders <- orders[(orders$success==TRUE)&(orders$type=="recruit"),]
  
  if(nrow(orders)>0){
    for (i in 1:nrow(orders)){
      tile <- as.numeric(orders$tile_number[i])
      
      #add troop to territory
      territories$data$n_troops[tile] <- territories$data$n_troops[tile] + 1
      territories$data$troops[tile] <- territories$data$owner[tile]
      
      #update displaying size for troops on map
      territories$data$size_display_troops[tile] <- ifelse(territories$data$n_troops[tile] %in% c(0,1),1,2)
      
    }
  }
  return(territories)
}


#5) CAPTURED RULER ---------------------------
# !!!! This function does not update
capture_ruler <- function(orders,territories, nations){
  captured_rulers<- orders$tile_number[orders$type=="ruler captured"]
  if (length(captured_rulers)>0){
    for (i in as.numeric(captured_rulers)){
      territories$data$ruler[i] <- FALSE
      nations$ruler_alive[nations$names==territories$data$owner[i]] <- FALSE
      if (sum((territories$data$owner==territories$data$owner[i])&(!territories$data$city)&is.na(territories$data$troops),na.rm=TRUE)>0){
        territories$data$owner[(territories$data$owner==territories$data$owner[i])&(!territories$data$city)&is.na(territories$data$troops)] <- "no owner"  
      }
      
    }
  }
  return(list(territories=territories, nations=nations))
}
      
      
      
  


