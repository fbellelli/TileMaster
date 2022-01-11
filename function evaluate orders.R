evaluate_orders <- function(orders,territories,nations,turn,seasons=seasons, def_n_tiles_troops=3){

  #initiate vector for storing troops that need to retreat and captured ruler
  retreat <- NULL
  captured_ruler <- NULL

  if (nrow(orders)>0){

    #initiate success column in order table
    orders$success <- TRUE
  
    if ((turn %/% 2) %% length(seasons) + 1 ==length(seasons)){ 
      #ADJUSTMENT TURNS --------------------------------------
    
      #get information about tiles and troops
      orders$owner_source <- territories$data$owner[as.integer(orders$tile_number)]
      orders$troops_source <- territories$data$troops[as.integer(orders$tile_number)]
    
      #fail all orders which are not recruit or disband
      orders$success[!(orders$type %in% c("recruit","disband"))] <- FALSE
    
      #update number of troops after disbanding
      for (i in 1:nrow(orders)){
        if(orders$type[i]=="disband"){
          nations$tot_troops[nations$names==orders$owner_source[i]] <- nations$tot_troops[nations$names==orders$owner_source[i]] - 1
        }
      }
      
      #fail recruit orders for territories occupied by foreign troops or in territories owned by no nations
      orders$success[(orders$type=="recruit")&(orders$owner_source=="no owner")]<- FALSE
      orders$success[(orders$type=="recruit")&(ifelse(is.na(orders$troops_source), FALSE, orders$owner_source!=orders$troops_source ))]<-FALSE
      

      #import info about tot troops and cities and fail recruit orders above number of cities
      orders$tot_cities <- NA
      orders$tot_troops <- NA
      orders$tot_tiles <- NA
      for (i in 1:nrow(orders)){
        if((orders$type[i]=="recruit")&(orders$success[i]==TRUE)){
          #get number of cities and troops owned by nation
          orders$tot_cities[i] <- nations$tot_cities[nations$names==orders$owner_source[i]]
          orders$tot_troops[i] <- nations$tot_troops[nations$names==orders$owner_source[i]]
          orders$tot_tiles[i] <- nations$tot_tiles[nations$names==orders$owner_source[i]]
          
          #fail orders in excess
          if(orders$tot_cities[i] + ((orders$tot_tiles[i] - orders$tot_cities[i]) %/% def_n_tiles_troops) > orders$tot_troops[i]){
            nations$tot_troops[nations$names==orders$owner_source[i]] <- nations$tot_troops[nations$names==orders$owner_source[i]] + 1
          } else {
            orders$success[i]<- FALSE
          }
        }
      }
    
    
    
   }else{
      #MOVEMENT TURNS ----------------------------------------
    
      #fail all recruit orders
      orders$success[orders$type == "recruit"] <- FALSE
    
      #remove any move order to cells that are not adjacent
      orders <- orders[(orders$type %in% c("disband","recruit"))|territories$adjacency[cbind(as.numeric(orders$tile_number),as.numeric(orders$to))],]
    
      #import information about tile and troops for source and destination
      orders$troops_source <- territories$data$troops[as.integer(orders$tile_number)]
      orders$n_troops_source <- territories$data$n_troops[as.integer(orders$tile_number)]
      orders$troops_dest <- territories$data$troops[as.integer(orders$to)]
      orders$n_troops_dest <- territories$data$n_troops[as.integer(orders$to)]
      orders$owner_source <- territories$data$owner[as.integer(orders$tile_number)]
      orders$owner_dest <- territories$data$owner[as.integer(orders$to)]
      orders$moving_troops <- ifelse(orders$type=="unstack",1,
                                     ifelse(orders$type=="move",orders$n_troops_source,0))
    
      #remove any move order given to territories with no troops
      orders <- orders[(orders$type %in% c("recruit","disband"))|((orders$type %in% c("move","unstack"))&(orders$n_troops_source>=1)),]
    
      #convert unstack into move orders when only one troop is present
      orders$type[(orders$type=="unstack")&(orders$n_troops_source==1)]<-"move"
    
      #CLASSIFY MOVEMENT ORDERS into clash, invasion and reinforcement orders. disband and recruit are categorised as "other" 
      orders$order_class <- "invasion"
      orders$order_class[orders$type %in% c("disband","recruit")] <- "other"
    
      #find clash orders (situation in which troops on both side move on each other)
      temp <- orders[orders$type %in% c("unstack","move"),] #orders directed to tiles with other orders
      temp$seq1 <- paste(temp$tile_number,temp$to)
      temp$seq2 <- paste(temp$to,temp$tile_number)
      temp <- temp[temp$seq1 %in% temp$seq2,1:(ncol(temp)-2)] #isolate clashing orders
      temp <- temp[temp$troops_source!=temp$troops_dest] #check that two troops are of different nations
      orders$order_class[do.call(paste,orders) %in% do.call(paste,temp)] <- "clash"   #label in order table
    
      #find reinforcement movements
      orders$order_class[(orders$type %in% c("unstack","move"))&(orders$order_class=="invasion")&!(orders$to %in% orders$tile_number[orders$type %in% c("move")])&(orders$n_troops_dest>0)&(orders$troops_source==orders$troops_dest)] <- "reinforce"  #filter out territories with troops that are moving away, destination territories with no troops and confirm that troops at destination are of the same nation as origin of order

      #--- BIDIRECTIONAL CLASH ---   
      #rule out clashes first (strongest side wins, need to take into account rulers)
      temp <- orders[orders$order_class == "clash",]
      temp$strength <- temp$moving_troops
      #add ruler's bonus on movement strength
      rulers_table <- territories$data[territories$data$ruler,c("tile_number","owner")]
      if(nrow(temp)>0){
        for (i in 1:nrow(temp)){
          #check whether destination of movement is an owned territory (otherwise the ruler's influence doesn't cover it anyways)
          if (temp$troops_source[i]==temp$owner_dest[i]){
            #check whether the ruler of the nation is alive
            if (nations$ruler_alive[nations$names==temp$troops_source[i]]){
              #check whether the ruler is adjacent to destination, if so, add 1 to strength
              if (territories$distance[as.numeric(temp$to[i]), rulers_table$tile_number[rulers_table$owner == temp$owner_dest[i]]]<=1){temp$strength[i]<-temp$strength[i]+1}
            }
          }
        }
        #rule out clash outcome
        for (i in 1:nrow(temp)){
          if(temp$strength[i]<=temp$strength[(temp$tile_number==temp$to[i])&(temp$to==temp$tile_number[i])]){orders$success[(orders$tile_number==temp$tile_number[i])&(orders$to==temp$to[i])&(orders$type==temp$type[i])]<-FALSE}
        }
      }
     

      #--- BOUNCING ATTACKS ---    
      #make list attacker's strength for all moving troops
      temp <- orders[(orders$order_class == "invasion")|((orders$order_class == "clash")&(orders$success==TRUE)),]
      #make the table
      attacker_strength <- temp %>% group_by(to,troops_source) %>% summarise(strength = sum(moving_troops, na.rm=TRUE),.groups = "drop") %>% as.data.frame()
      attacker_strength$owner_dest <- territories$data$owner[as.integer(attacker_strength$to)]
      if (nrow(attacker_strength)>0){ 
        #add ruler's bonus
        for (i in 1:nrow(attacker_strength)){
          #check whether destination of movement is an owned territory (otherwise the ruler's influence doesn't cover it anyways)
          if (attacker_strength$troops_source[i]==attacker_strength$owner_dest[i]){
            #check whether the ruler of the nation is alive
            if (nations$ruler_alive[nations$names==attacker_strength$troops_source[i]]){ 

              #check whether the ruler is adjacent to destination, if so, add 1 to strength
              if (territories$distance[as.numeric(attacker_strength$to[i]), rulers_table$tile_number[rulers_table$owner == attacker_strength$owner_dest[i]]]<=1){
                attacker_strength$strength[i]<-attacker_strength$strength[i]+1}
            }
          }
        }

        #deal with bouncing attacks (taking into account 2+ nations)
        #find number of sides for each attacked territory
        attacker_strength <- attacker_strength %>% group_by(to) %>% mutate(n_sides = n()) %>% as.data.frame()
        #check for bouncing attacks only among territories with 2 or more sides invading
        attacker_strength$success <- TRUE
        temp <- attacker_strength[attacker_strength$n_sides>1,]
        if (nrow(temp)>0){
          #get the biggest attack for every territory
          temp <- temp %>% group_by(to) %>% mutate(max_att=max(strength,na.rm=TRUE)) %>% as.data.frame() 
          #fail attacks smaller than the biggest
          temp$success[temp$strength<temp$max_att] <- FALSE
          #check for bouncing between armies of equal side
          temp$temp<-temp$success
          for (i in 1:nrow(temp)){
            if(temp$success[i]==TRUE){
              if (sum((temp$success==TRUE)&(temp$to==temp$to[i])&(temp$strength==temp$strength[i]), na.rm=TRUE)>1){
                temp$temp[i] <- FALSE
              }
            }   
          }
          temp$success <- temp$temp
          temp$temp<-NULL
          #update success of bounced orders in orders and attacker strength tables
          temp <- temp[temp$success==FALSE,]
          orders$success[do.call(paste,orders[,c("to","troops_source")]) %in% do.call(paste,temp[,c("to","troops_source")])] <- FALSE
        
          #remove bounced attacks from list of attacks to be evaluated
          attacker_strength$success[do.call(paste,attacker_strength[,c("to","troops_source")]) %in% do.call(paste,temp[,c("to","troops_source")])] <- FALSE
          attacker_strength <- attacker_strength[(attacker_strength$success==TRUE),]
        }
      }
     
      #--- FINAL ATTACKS VS DEFENCE --- 
      #establish defence strength
      if (nrow(attacker_strength)>0){
      attacker_strength$defence <- 0
      attacker_strength$reinforcement <- FALSE

      
        #find number of defending troops in each territory
        for (i in 1:nrow(attacker_strength)){
          #get number of troops in territory at beginning of turn
          attacker_strength$defence[i] <- territories$data$n_troops[as.numeric(attacker_strength$to[i])]
          if (attacker_strength$defence[i]>0){
          
            #set defence strength to 0 if the troop was disbanded or moved
            if(any(((orders$tile_number==attacker_strength$to[i])&(orders$type %in% c("move","disband"))))){
              attacker_strength$defence[i] <- 0
            } else {
              #reduce by 1 if it was unstacked
              if(any(((orders$tile_number==attacker_strength$to[i])&(orders$type %in% c("unstack"))))){
                attacker_strength$defence[i] <- attacker_strength$defence[i] - 1
              }  
              #add 1 if ruler is nearby
              if(attacker_strength$owner_dest[i]==territories$data$troops[as.numeric(attacker_strength$to[i])]){  ## check if the defender's troop are in a territory it owns, if so, check if the ruler is alive, if it is, check whether the ruler position is adjacent or the same as the tile under attack
                if(nations$ruler_alive[nations$names==attacker_strength$owner_dest[i]]){
                  if(territories$distance[as.numeric(attacker_strength$to[i]), rulers_table$tile_number[rulers_table$owner == attacker_strength$owner_dest[i]]]<=1){
                    attacker_strength$defence[i] <- attacker_strength$defence[i] + 1
                  }
                }
              }  
              #add reinforcements
              if(any((orders$order_class=="reinforce")&(orders$to==attacker_strength$to[i]))){
                attacker_strength$defence[i] <- attacker_strength$defence[i] + orders$moving_troops[(orders$order_class=="reinforce")&(orders$to==attacker_strength$to[i])]
                attacker_strength$reinforcement[i] <- TRUE
              }  
            }
          }
        }
      
        #ignore territories with no defence (the attack is successful), focus only on defended ones to see if they are repelled or not
        attacker_strength <- attacker_strength[attacker_strength$defence>0,]
        if (nrow(attacker_strength)>0){
        
          #define which attacks failed
          attacker_strength$success <- ifelse(attacker_strength$strength>attacker_strength$defence, TRUE, FALSE)
        
          #update attack & reinforcement order success if they failed
          for (i in 1:nrow(attacker_strength)){
            if (attacker_strength$success[i]==FALSE){
              orders$success[(orders$type %in% c("move","unstack"))&(orders$to == attacker_strength$to[i])&(orders$troops_source == attacker_strength$troops_source[i])] <- FALSE
            }
            if ((attacker_strength$success[i]==TRUE)&(attacker_strength$reinforcement[i]==TRUE)){
              orders$success[(orders$order_class %in% c("reinforce"))&(orders$to == attacker_strength$to[i])] <- FALSE
            }
          }
        }
      }
    

      #--- IDENTIFY RETREATING TROOPS & CAPTURED RULER--- 
      #based on all successful move and unstack order, determine which troops should retreat
      #loop over successfully attacked territories
      for (i in unique(as.numeric(orders$to[(orders$owner_dest != orders$troops_source)&(orders$type %in% c("unstack","move"))&(orders$success)&(orders$order_class!="reinforce")]))){
        
        #RULER check whether any ruler was present in captured territories
        if (i %in% rulers_table$tile_number){
          captured_ruler<-c(captured_ruler,i)
        }
        
        #RETREAT check whether there was any troop in the territory at the beginning of turn and if it has not successfully moved or disbanded. Check also that the successful invasion is not from the same player.
        if ((territories$data$n_troops[i]>0)&!(i %in% as.numeric(orders$tile_number[(orders$type == "disband")|((orders$type == "move")&(orders$success == TRUE))]))&(ifelse(length(orders$troops_source[(as.numeric(orders$to)==i)&(orders$success)&(orders$order_class=="invasion")])>0,territories$data$troops[i] != unique(orders$troops_source[(as.numeric(orders$to)==i)&(orders$success)&(orders$order_class=="invasion")]),TRUE)) ){
          retreat <- c(retreat,i)
        }
      }

      #--- INPUT RETREATS AND RULER CAPTURES AS ORDERS ---
      
      #DEAL WITH RETREATING TROOPS:
      if(length(retreat)>0){
        for (tile in as.numeric(retreat)){

          #make list of adjacent territories
          adjacent <- c(1:nrow(territories$adjacency))[territories$adjacency[,tile]]

          #check all adjacent territories that are involved in enemy orders as origin or destination. Or friendly destination tiles that had ennemy troops at beginning of turn.
          blocked <- as.numeric(unique(c(territories$data$tile_number[territories$data$troops!=territories$data$troops[tile]],orders$to[orders$troops_source!=territories$data$troops[tile]], orders$to[(orders$troops_dest != territories$data$troops[tile])|is.na(orders$troops_dest)] )))
          blocked<-blocked[!is.na(blocked)]

          #if there is no free/friendly territory then eliminate troop
          friendly <- adjacent[!(adjacent %in% blocked)]
          if(length(friendly)==0){

            orders[(nrow(orders)+1),] <- c(c("retreat",tile,NA,FALSE),rep(NA,ncol(orders)-4))
            orders$troops_source[nrow(orders)] <- territories$data$troops[tile]
            orders$moving_troops[nrow(orders)] <- territories$data$n_troops[tile]
            
          } else {
            #if there is free/friendly territory calculate probability of retreating:
            #logit score: . +1 if ruler is nearby
            #             . +1 for every non-retreating nearby friendly troop
            #             . +1 if the territory is a city
            #             . +1 if the territory is nation core
            #             . +/- 0.2 for every free territory above/below 3
            #             . -1 if the territory is owned by enemy nation

            score <- 0
            # +1 if the territory is a city
            score <- score + if (territories$data$city[tile]) 1 else 0
            # +1 if the territory is a nation's core
            score <- score + if (territories$data$core[tile]==territories$data$troops[tile]) 1 else 0
            # +1 for every non-retreating nearby friendly troop
            score <- score + sum(territories$data$n_troops[friendly], na.rm=TRUE)
            # +1 if ruler is nearby
            ruler <- c(1:nrow(territories$data))[territories$data$ruler & (territories$data$owner==territories$data$troops[tile])]
            score <- score + if(length(ruler)>0){as.numeric(ruler %in% adjacent)} else 0
            # -1 if territory is owned by enemy
            score <- score - if(!(territories$data$owner[tile] %in% c("no owner", territories$data$troops[tile]))) 1 else 0
            # +/- 0.7 for every free territory above/below 4
            score <- score + 0.7*(length(friendly)-4)

            prob_retreat <- exp(score+0.5)/(exp(score+0.5)+1)

            #sample with given probability whether to retreat or disband
            if(sample(c(TRUE,FALSE),1,prob = c(prob_retreat, 1-prob_retreat))){
              #RETREAT
              
              #probability of each tile for retreat based on ownership of tile
              tiles_prob <- ifelse(territories$data$owner[as.numeric(friendly)] == "no owner",1.5,  #prob empty tile compared to enemy's
                                   ifelse(territories$data$owner[as.numeric(friendly)]==territories$data$owner[tile],2,  #prob own nations' tile compared to enemy's
                                          1)) # enemy's tile 
              tot <- sum(tiles_prob)
              tiles_prob <- tiles_prob/tot
              
              
              #sample randomly among friendly territories where to retreat
              tile_retreat <- as.numeric(sample(as.character(friendly),1, prob = tiles_prob))
              
              #if the retreat tiles contain a ruler and the ruler does not belong to the retreating nation:
              if ((tile_retreat %in% rulers_table$tile_number)&!(territories$data$owner[tile_retreat]==territories$data$owner[tile])){
                captured_ruler <- unique(c(captured_ruler,tile_retreat))
              } 
                        
              orders[(nrow(orders)+1),] <- c(c("retreat",tile,tile_retreat,TRUE),rep(NA,ncol(orders)-4))
              orders$troops_source[nrow(orders)] <- territories$data$troops[tile]
              orders$moving_troops[nrow(orders)] <- if (nrow(orders[(orders$type=="unstack") & (as.numeric(orders$tile_number) == tile),])>0){territories$data$n_troops[tile]-1} else {territories$data$n_troops[tile]}

            } else {

              orders[(nrow(orders)+1),] <- c(c("retreat",tile,NA,FALSE),rep(NA,ncol(orders)-4))

            }

          }
        }
      }

      #DEAL WITH CAPTURED RULERS
      if(length(captured_ruler)>0){
        for (tile in as.numeric(captured_ruler)){
          orders[nrow(orders)+1,] <- c(c("ruler captured",tile,NA,TRUE),rep(NA,ncol(orders)-4))
          orders$troops_source[nrow(orders)] <- territories$data$owner[tile]
          orders$moving_troops[nrow(orders)] <- 0
        }
      }

      #CORRECT ORDERS COLUMN TABLE FORMATTING
      #orders$tile_number <- as.numeric(orders$tile_number)
      #orders$to <- as.numeric(orders$to)
      orders$success <- as.logical(orders$success)
      orders$n_troops_source <- as.integer(orders$n_troops_source)
      orders$n_troops_dest <- as.integer(orders$n_troops_dest)
      orders$moving_troops <- as.integer(orders$moving_troops)
      
    } # end: movement turn
  } # end: nrow(orders)>0 
  
  #function output:--------------------------------------------
  return(orders)
}



