


#0) AI ----------------------

generate_ai_moves <- function(territories,nations,turn,difficulty_bias=1,seasons=c("spring","summer","autumn","winter (adjustment)"), def_n_tiles_troops = 3, type_ai="base" , dev_data = FALSE){
  
  #identify tiles of interest
  POI_table <- find_POI(territories,nations)
  
  #evaluate tiles of interst
  POI_table <- evaluate_POI(POI_table,nations,as.numeric(difficulty_bias))
  
  #RECRUIT TURNS:
  if ((turn %/% 2) %% length(seasons) + 1 == length(seasons)){
    
    final_moves <- recruit_orders(POI_table,territories, nations, def_n_tiles_troops)
    
  } else {
    #MOVEMENT TURNS:
    
    #evaluate moves to tiles of interest
    if (type_ai == "base"){
      #use programmatic AI for ranking
      moves_table <- rank_moves(POI_table,territories,distance_discount = 0.6)
    } else if (type_ai == "ML") {
      #use ML models for ranking moves
      moves_table <- rank_moves_ml(POI_table,territories,distance_discount = 0.6)
    } else { #mixed AI
      
      #use mixed methods for ranking: base as defaults, ML for nations about to be overpowered
      #extract base move value
      moves_table <- rank_moves(POI_table,territories,distance_discount = 0.6)
      
      #decide whether to apply ML AI to any nation
      temp <- moves_table[moves_table$type == "defence" & moves_table$target_distance<2,] %>% 
        group_by(troops_owner,troops_tile) %>%
        summarise(diff_troops1 = min(diff_troops1),
                  tot_enemies1 = max(tot_enemies1),
                  diff_troops_combined = mean(diff_troops_combined), .groups="drop") %>% suppressWarnings()
      temp$residual_cores <- 0
      temp$overpowered <- FALSE
      for (i in 1:nrow(temp)){
        temp$residual_cores[i] <- sum((territories$data$owner == temp$troops_owner[i]) & (territories$data$core == temp$troops_owner[i]))
        temp$overpowered[i] <- all(temp$diff_troops_combined[temp$troops_owner==temp$troops_owner[i]]<0)
      }
      temp <- left_join(temp,nations[,c("names","tot_troops","tot_cities")], by=c("troops_owner"="names")) %>% as.data.frame() %>% suppressWarnings()
      temp$n_nations <- nrow(nations)
      temp$tot_cities_in_map <- sum(territories$data$city)
      temp$tot_troops_in_map <- sum(nations$tot_troops)
      temp$ML_ai <- ifelse(
                      (temp$overpowered)| #are all troops about to be overpowered?
                      (temp$residual_cores == 0 & temp$tot_troops < temp$tot_troops_in_map/temp$n_nations)| #no core left and less troops than it should have
                      (temp$tot_troops < ceiling(0.5*temp$tot_troops_in_map/temp$n_nations))| #very low number of troops
                      (temp$tot_cities < ceiling(0.5*temp$tot_cities_in_map/temp$n_nations)),
                      TRUE,
                      FALSE)
      reference_AI_type <- temp[!duplicated(temp$troops_owner),c("troops_owner","ML_ai")] #table containing final result
      
      #replace ML AI moves value for identified nations
      if (any(reference_AI_type$ML_ai)){
        
        temp2 <- rank_moves_ml(POI_table,territories,distance_discount = 0.6)
        temp2 <- 
        moves_table <- rbind(moves_table[!(moves_table$troops_owner %in% reference_AI_type$troops_owner[reference_AI_type$ML_ai]),],
                             temp2[temp2$troops_owner %in% reference_AI_type$troops_owner[reference_AI_type$ML_ai],])
      }
      
    }

    
    #select moves
    final_moves <- select_moves(moves_table,territories,nations,dev_data=dev_data)
    
    #ensure consistency of table for merger in app
    final_moves$orders$to <- as.character(final_moves$orders$to)
    final_moves$orders$tile_number <- as.character(final_moves$orders$tile_number)
    
  }
  
  
  
  #return a list with ruler position and AI orders
  return(final_moves)
} 


#____________________________



#1) IDENTIFY TILES OF INTEREST ----------------------

#the function below returns a list of interesting tiles for each nations with important statistics for each:

# "nations" | "tile"  --- ID of the table. tile of interest and nation for which they are interesting
# "type"  --- POI are classified in "attack","mixed","defence": defence are owned territories at the border with enemy/empty tiles. Attack territories are tiles at 2 of distance which are interesting targets. Mixed are tiles at 1 distance from border
# "tile_owner" | "is_city"  --- info about tile
# "is_core" | "core_of" | "own_core" | "residual_cores"   --- answers to the following questions: is the POI a core? Of which nation is it a core? is it a core og the nations in column "nations"? how many cores are left to the nation for which the POI is a core? 
# "troops0" | "troops1" | "troops2"   --- friendly (i.e. belonging to "nations") troops within radius of 0,1,2 from POI 
# "tot_enemies0" | "tot_enemies1" | "tot_enemies2"  --- total number of enemy troops within radius of 0,1,2
# "owner_troops0" | "owner_troops1" | "owner_troops2"   --- total number of troops within radius 0,1,2 belonging to the owner of the POI
# "max_enemies1" | "max_enemies2"   --- maximum number of enemy troops belonging to a single nation that are within 1 and 2 tile. (i.e. maximum enemy threat)
# "enemy_nations1" | "enemy_nations2"   --- number of enemy nations with troops within 1 and 2 of radius from POI 
# "tile_n_enemy_POI"  --- number of enemy nations for which the tile is a POI 
# "diff_troops1" | "diff_troops2"   --- difference between "troops" and "max_enemies"
# "def_position1" | "def_position2" --- score for how good the POI is for defending territories (see precise definition in function)   
# "att_position1" |   "att_position2" --- scire for how goods the POI is for attacking other mixed/attack tiles (see precise definition in function) 
# "tile_value"  --- scoring of the recruitment value of the tile: cities and cores are worth more (see precise definition in function)

find_POI <- function(territories,nations){
  
  POI_table <- as.data.frame(matrix(nrow=0,ncol=3))
  colnames(POI_table)<-c("nations","tile","type")
  
  for (i in nations$names){#loop over nation names
    
    #initialise an empty list of possible foreign/empty city targets for the nation
    city_targets <- NULL
    neighbouring_targets <- NULL
    
    for (j in c(1:nrow(territories$data))[territories$data$owner==i]){#loop over tiles of nations
      
      #find all neighbouring territories to j
      neighbours <- c(1:nrow(territories$data))[territories$adjacency[,j]]
      
      #select all border tiles as defensive POI and check all its neighbouring tiles
      if (any(territories$data$owner[neighbours]!=i)){
        POI_table[nrow(POI_table)+1,] <- c(i,j,"defence")
        
        #look for all empty/foreign neighbouring tiles and select them as mixed POI
        for (k in neighbours[territories$data$owner[neighbours]!=i]){
          neighbouring_targets <- c(neighbouring_targets,k)
        }
      }
      
      #look for cities 2-tiles away from border to add to city targets
      city_targets <- c(city_targets,c(1:nrow(territories$data))[territories$distance[,j] == 2 & territories$data$city & territories$data$owner != i])
    } 
    
    #add neighbouring targets to POI table
    if (length(neighbouring_targets)>0){
      for (j in unique(neighbouring_targets)){
        POI_table[nrow(POI_table)+1,] <- c(i,j,"mixed")
      }
    }
    
    #add city targets as an expansion POI if it is not already included as mixed POI
    if (length(city_targets)>0){
      for (j in unique(city_targets[!(city_targets %in% neighbouring_targets)])){
        POI_table[nrow(POI_table)+1,] <- c(i,j,"attack")
      }
    }
  }
  
  #COMPUTE ADDITIONAL STATISTICS ABOUT THE TILE:
  
  #get tile info
  POI_table$tile_owner <- territories$data$owner[as.numeric(POI_table$tile)]
  POI_table$is_city <- territories$data$city[as.numeric(POI_table$tile)]
  POI_table$is_core <- !("" == territories$data$core[as.numeric(POI_table$tile)])
  POI_table$core_of <- territories$data$core[as.numeric(POI_table$tile)]
  POI_table$own_core <- ifelse(POI_table$is_core,POI_table$core_of==POI_table$nations,FALSE)
  
  #initialise troops variables and loop over rows of table 
  POI_table$residual_cores <- 0
  POI_table$troops0 <- 0
  POI_table$troops1 <- 0
  POI_table$troops2 <- 0
  POI_table$tot_enemies0 <- 0
  POI_table$tot_enemies1 <- 0
  POI_table$tot_enemies2 <- 0
  POI_table$owner_troops0 <- 0
  POI_table$owner_troops1 <- 0
  POI_table$owner_troops2 <- 0
  POI_table$max_enemies1 <- 0
  POI_table$max_enemies2 <- 0
  POI_table$enemy_nations1 <- 0
  POI_table$enemy_nations2 <- 0
  POI_table$tile_n_enemy_POI <- 0
  for (i in 1:nrow(POI_table)){
    
    
    #home cores still controled by the TILES'S CURRENT OWNER
    POI_table$residual_cores[i] <- sum(territories$data$owner[territories$data$core==POI_table$core_of[i]] == territories$data$core[territories$data$core==POI_table$core_of[i]],na.rm=TRUE)
    
    #number of friendly troops that are WITHIN 0,1 and 2 tiles distance radius (i.e. friendly projection capacity)
    POI_table$troops0[i] <- sum(territories$data$n_troops[(territories$data$troops == POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==0)], na.rm = TRUE)
    POI_table$troops1[i] <- POI_table$troops0[i] + sum(territories$data$n_troops[(territories$data$troops == POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==1)], na.rm = TRUE)
    POI_table$troops2[i] <- POI_table$troops1[i] + sum(territories$data$n_troops[(territories$data$troops == POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==2)], na.rm = TRUE)
    
    #total number of enemy troops (of all nations) within a radius of 0, 1 and 2
    POI_table$tot_enemies0[i] <- sum(territories$data$n_troops[(territories$data$troops != POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==0)], na.rm = TRUE)
    POI_table$tot_enemies1[i] <- POI_table$tot_enemies0[i] + sum(territories$data$n_troops[(territories$data$troops != POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==1)], na.rm = TRUE)
    POI_table$tot_enemies2[i] <- POI_table$tot_enemies1[i] + sum(territories$data$n_troops[(territories$data$troops != POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==2)], na.rm = TRUE)
    
    #total number of tile owner troop within a radius of 0, 1 and 2
    POI_table$owner_troops0[i] <- sum(territories$data$n_troops[(territories$data$troops == POI_table$tile_owner[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==0)], na.rm = TRUE)
    POI_table$owner_troops1[i] <- POI_table$owner_troops0[i] + sum(territories$data$n_troops[(territories$data$troops == POI_table$tile_owner[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==1)], na.rm = TRUE)
    POI_table$owner_troops2[i] <- POI_table$owner_troops1[i] + sum(territories$data$n_troops[(territories$data$troops == POI_table$tile_owner[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]==2)], na.rm = TRUE)
    
    #max enemy troops of single nation that are WITHIN 0,1 and 2 tiles distance radius (i.e. enemy projection capacity)
    POI_table$max_enemies1[i] <- territories$data[!is.na(territories$data$troops) & (territories$data$troops != POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]<=1),] %>%
      group_by(troops) %>% summarise(threat=sum(n_troops,na.rm=TRUE), .groups="drop") %>% summarise(max_threat = ifelse(nrow(.)==0,0,max(threat))) %>% unlist(.,use.names=FALSE)
    POI_table$max_enemies2[i] <- territories$data[!is.na(territories$data$troops) & (territories$data$troops != POI_table$nations[i]) & (territories$distance[as.numeric(POI_table$tile[i]),]<=2),] %>%
      group_by(troops) %>% summarise(threat=sum(n_troops,na.rm=TRUE), .groups="drop") %>% summarise(max_threat = ifelse(nrow(.)==0,0,max(threat))) %>% unlist(.,use.names=FALSE)
    
    #number of enemy nations with troops within range of 1 and 2 of tile
    neighbours<-c(1:nrow(territories$data))[territories$distance[as.numeric(POI_table$tile[i]),]<=1]
    POI_table$enemy_nations1[i] <- sum(unique(territories$data$troops[neighbours]) != POI_table$nations[i], na.rm=TRUE)
    neighbours<-c(1:nrow(territories$data))[territories$distance[as.numeric(POI_table$tile[i]),]<=2]
    POI_table$enemy_nations2[i] <- sum(unique(territories$data$troops[neighbours]) != POI_table$nations[i], na.rm=TRUE)
    
    #number of enemy nations for which the tile is a POI
    POI_table$tile_n_enemy_POI[i] <- length(POI_table$tile[POI_table$tile==POI_table$tile[i]]) -1 
    
  }
  
  #difference between the country's troops and maximum number of troops of single enemy within a range of 1 and 2
  POI_table$diff_troops1 <- POI_table$troops1 - POI_table$max_enemies1
  POI_table$diff_troops2 <- POI_table$troops2 - POI_table$max_enemies2
  
  #tile value taking into account presence of city/core: empty=1,city=3,OTHERS' CORES: 5, OWN CORES: 3+cores=5|2cores=7|1core=10
  POI_table$tile_value <- ifelse(POI_table$is_city,
                                 ifelse(POI_table$is_core,
                                        ifelse(POI_table$own_core,
                                               5+ifelse(POI_table$residual_cores<=1,5,ifelse(POI_table$residual_cores==2,2,0)), #own cores: 5,7,10 (respectively with 3+,2,1- cores)
                                               4+ifelse((POI_table$core_of == POI_table$tile_owner)&(POI_table$residual_cores<=1), 2, ifelse((POI_table$core_of == POI_table$tile_owner)&(POI_table$residual_cores==2),1,0))), #other's nations cores 4,5,6 (respectively with 3+,2,1- cores)
                                        3),1) #simple city, simple empty tile
  
  #initialise positional variables and loop over variables
  POI_table$def_position1 <- 0
  POI_table$def_position2 <- 0
  POI_table$att_position1 <- 0
  POI_table$att_position2 <- 0
  for (i in 1:nrow(POI_table)){
    
    #sum of value of tiles at 1 or 0 distance which are threatened by at least 1 enemy troop at distance within 1 and 2
    if (POI_table$type[i] != "attack"){
      neighbours<-c(1:nrow(territories$data))[territories$distance[as.numeric(POI_table$tile[i]),]<=1]
      temp <- neighbours[neighbours %in% as.numeric(POI_table$tile[(POI_table$nations == POI_table$nations[i])&(POI_table$type != "attack")&(POI_table$max_enemies1>0)])] #neighbouring tiles under threat at distance 1
      POI_table$def_position1[i] <- if (length(temp)>0){ #take into account presence of city and number of residual cores
        sum(POI_table$tile_value[(POI_table$nations==POI_table$nations[i]) & (as.numeric(POI_table$tile) %in% temp)])
      } else {0}
      temp <- neighbours[neighbours %in% as.numeric(POI_table$tile[(POI_table$nations == POI_table$nations[i])&(POI_table$type != "attack")&(POI_table$max_enemies2>0)])] #neighbouring tiles under threat at distance 1
      POI_table$def_position2[i] <- if (length(temp)>0){ #take into account presence of city and number of residual cores
        sum(POI_table$tile_value[(POI_table$nations==POI_table$nations[i]) & (as.numeric(POI_table$tile) %in% temp)])
      } else {0}
    }
    
    #value (1 for empty tile, 3 for city, 3/4/5/6 for core) of mixed/attack POI at 1 or 0 distance
    neighbours<-c(1:nrow(territories$data))[territories$distance[as.numeric(POI_table$tile[i]),]<=1] #find neighbouring cells to POI
    temp <- neighbours[neighbours %in% as.numeric(POI_table$tile[(POI_table$nations == POI_table$nations[i])&(POI_table$type != "defence")])] #retain only attack/mixed POI
    POI_table$att_position1[i] <- if (length(temp)>0){ #take into account presence of city and number of residual cores
      sum(POI_table$tile_value[(POI_table$nations==POI_table$nations[i]) & (as.numeric(POI_table$tile) %in% temp)])  #give more weights when less cores
    } else {0}
    neighbours<-c(1:nrow(territories$data))[territories$distance[as.numeric(POI_table$tile[i]),]<=2]
    temp <- neighbours[neighbours %in% as.numeric(POI_table$tile[(POI_table$nations == POI_table$nations[i])&(POI_table$type != "defence")])] #retain only attack/mixed POI
    POI_table$att_position2[i] <- if (length(temp)>0){ #take into account presence of city and number of residual cores
      sum(POI_table$tile_value[(POI_table$nations==POI_table$nations[i]) & (as.numeric(POI_table$tile) %in% temp)])  #give more weights when less cores
    } else {0}
    
  }
  
  
  return(POI_table)
}
#____________________________________________________



#2) SUMMARISE TILE VALUE IN A SINGLE INDEX ----------

#The function below encapsulates the utility of each point of interest in a unique value. This utility score will then be used to decide which tile movements should target.
evaluate_POI <- function(POI_table, nations, difficulty_bias=1){
  
  #import nation's expansionism status
  POI_table$ai_expansionism <- nations$ai_expansionism[match(POI_table$nations,nations$names)]
  
  #calculate POI value summary
  POI_table$POI_value <- POI_table$ai_expansionism*((POI_table$tile_value*as.numeric(POI_table$type != "defence"))+log(1+POI_table$att_position1)) +  #Expansion value of tile
    (1/(POI_table$ai_expansionism+0.01))*(POI_table$tile_value*as.numeric(POI_table$type != "attack") + POI_table$def_position1)  #Defensive value of tile
  
  #add bias against human player based on difficulty level
  if (any(nations$is_player)){
    POI_table$POI_value <- ifelse(POI_table$tile_owner %in% nations$names[nations$is_player],
                                  POI_table$POI_value*difficulty_bias,
                                  POI_table$POI_value)
  }
  
  #add bias against players that are getting too strong in the match
  strong_players <- nations$names[nations$tot_cities>=quantile(nations$tot_cities,0.9)] #players too strong are those in the top 10% quantile distribution of tiles
  if (length(strong_players)>0){
    strong_players <- data.frame(name=strong_players, 
                                 strong_player_bias=min(max(0.75*nations$tot_cities[nations$names %in% strong_players]/mean(nations$tot_cities),1),2)) #bias increases as the player's number of cities is above average
    POI_table <- left_join(POI_table, strong_players, by=c("tile_owner"="name")) %>% as.data.frame()
    
    #apply bias
    POI_table$strong_player_bias <- ifelse(is.na(POI_table$strong_player_bias)|(POI_table$nations==POI_table$tile_owner),1,POI_table$strong_player_bias)
    POI_table$POI_value <- POI_table$POI_value*POI_table$strong_player_bias
  }
  
  
  #round it
  POI_table$POI_value <- round(POI_table$POI_value,0)  
  
  return(POI_table)                       
}
#_________________________________________________



#3) RANK TARGETS FOR EACH UNIT -----------

#The function below assign score for each unit-POI combination
rank_moves <- function(POI_table, territories, distance_discount=0.6){
  
  #make a table of all troops position and owner
  troops_table <- data.frame(troops_tile = as.numeric(territories$data$tile_number[territories$data$n_troops>0]),
                             troops_owner = territories$data$troops[territories$data$n_troops>0],
                             troops_n = territories$data$n_troops[territories$data$n_troops>0])
  
  #make a table with all unit-POI combinations
  moves_table <- left_join(troops_table,POI_table,by=c("troops_owner"="nations"))  
  
  #calculate the distance between units and POI
  moves_table$target_distance <- territories$distance[as.matrix(cbind(as.numeric(moves_table$troops_tile), as.numeric(moves_table$tile)))]
  
  #calculate a distance-corrected expected value for each unit-tile combination
  moves_table$diff_troops_combined <- moves_table$diff_troops1+distance_discount*(moves_table$diff_troops2-moves_table$diff_troops1)
  moves_table$move_value <- moves_table$POI_value * 
    ifelse(moves_table$type!="attack",ifelse(moves_table$diff_troops_combined < -2, 1.1,ifelse(moves_table$diff_troops_combined < -1, 1.2,ifelse(moves_table$diff_troops_combined <=0, 1.3,1))),1) *   # increase value of defensive moves which are more likely to get attacked
    ifelse(moves_table$type!="defence",ifelse(moves_table$diff_troops_combined > 2, 1.3,ifelse(moves_table$diff_troops_combined > 0 , 1.1,ifelse(moves_table$diff_troops_combined < -1, 0.8,1))),1) *   # increase value of offensive moves which are less likely to be contended by an enemy
    ifelse((moves_table$type=="defence")&(moves_table$max_enemies1==0),0.5,1)* #decrease value of defence tiles which are not at risk of getting attacked
    distance_discount^ifelse(moves_table$target_distance>=2,moves_table$target_distance-1,0) # distance discounting
  
  return(moves_table)
}
#_________________________________________________


#3BIS) ALTERNATIVE TARGETS RANKING -----------

#The function below assign score for each unit-POI combination
rank_moves_ml <- function(POI_table, territories, distance_discount=0.6){
  
  #make a table of all troops position and owner
  troops_table <- data.frame(troops_tile = as.numeric(territories$data$tile_number[territories$data$n_troops>0]),
                             troops_owner = territories$data$troops[territories$data$n_troops>0],
                             troops_n = territories$data$n_troops[territories$data$n_troops>0])
  
  #make a table with all unit-POI combinations
  moves_table <- left_join(troops_table,POI_table,by=c("troops_owner"="nations"))  
  
  #calculate the distance between units and POI
  moves_table$target_distance <- territories$distance[as.matrix(cbind(as.numeric(moves_table$troops_tile), as.numeric(moves_table$tile)))]
  
  #calculate a distance-corrected troops balance 
  moves_table$diff_troops_combined <- moves_table$diff_troops1+distance_discount*(moves_table$diff_troops2-moves_table$diff_troops1)
  
  #preparate data for calculate success prob of move
  temp <- moves_table
  temp$is_city <- as.numeric(temp$is_city)
  temp$is_core <- as.numeric(temp$is_core)
  temp$own_core <- as.numeric(temp$own_core)
  temp$type_defence <- ifelse(temp$type == "defence",1,0)
  temp$type_mixed <- ifelse(temp$type %in% c("mixed","attack"),1,0)
  
  #load success model and predict success/failure probs
  success_model <- readRDS("success_model.rds")
  success_prediction <- predict.train(success_model, newdata = temp, type = "prob")
  temp$prob_success <- success_prediction[,"TRUE"]
  
  # save results into move table
  #moves_table$prob_success <- success_prediction[,"TRUE"]
  
  #load scoring model and calculate move value
  value_model <- readRDS("value_model.rds")
  value_prediction <- predict.train(value_model, newdata = temp)
  
  #apply biases
  moves_table$move_value <- 10*(value_prediction + abs(min(value_prediction, na.rm=TRUE))) * 
    #ifelse(moves_table$type!="attack",ifelse(moves_table$diff_troops_combined < -2, 1.1,ifelse(moves_table$diff_troops_combined < -1, 1.2,ifelse(moves_table$diff_troops_combined <=0, 1.3,1))),1) *   # increase value of defensive moves which are more likely to get attacked
    #ifelse(moves_table$type!="defence",ifelse(moves_table$diff_troops_combined > 2, 1.3,ifelse(moves_table$diff_troops_combined > 0 , 1.1,ifelse(moves_table$diff_troops_combined < -1, 0.8,1))),1) *   # increase value of offensive moves which are less likely to be contended by an enemy
    ifelse((moves_table$type=="defence")&(moves_table$max_enemies1==0),0.5,1)* #decrease value of defence tiles which are not at risk of getting attacked
    distance_discount^ifelse(moves_table$target_distance>=2,moves_table$target_distance-1,0) # distance discounting
  
  
  return(moves_table)
}
#_________________________________________________



#4) SELECT MOVES AND COMPILE ORDERS --------------


select_moves<- function(moves_table,territories,nations, dev_data=FALSE){
  
  orders <- data.frame(type=character(0), tile_number=integer(0), to=integer(0))
  ruler_placement <- data.frame(nation=character(0),tile = numeric(0))
  dev_data_record <- NULL
  
  #loop over every AI nation
  for (nation in nations$names[!nations$is_player]){
    
    #subset moves table for nation and prepare an empty table for selected moves
    potential_moves <- moves_table[moves_table$troops_owner == nation,] 
    selected_moves <- potential_moves[rep(FALSE,nrow(potential_moves)),]
    
    #proceed with troop orders and ruler placement only if the nation has troops:
    if (nrow(potential_moves)>0){
      
      #SELECT ONE OF THE BEST MOVES FOR EACH UNIT 
      #loop over all units of the nation
      for (i in unique(potential_moves$troops_tile)){
        
        #for every unit randomise move choice among moves within 90% of the score of the highest move
        candidate_moves <- potential_moves[(potential_moves$troops_tile==i) & (potential_moves$move_value > 0.9*max(potential_moves$move_value[potential_moves$troops_tile==i])),]
        if (nrow(candidate_moves)>1){
          prob_candidate_moves <- candidate_moves$move_value/sum(candidate_moves$move_value)
          selected_moves <- rbind(selected_moves, candidate_moves[sample(1:nrow(candidate_moves),size=1,prob = prob_candidate_moves),]) 
        } else {
          selected_moves <- rbind(selected_moves,candidate_moves) 
        }
      }
      
      #IMPROVE MOVES COORDINATION AND IDENTIFY TROOPS TO UNSTACK
      selected_moves$unstack <- FALSE
      for (i in unique(selected_moves$tile)){#loop over all targeted tiles
        
        #for every selected target check whether more troops than necessary were sent
        if (selected_moves$type[match(i,selected_moves$tile)] == "defence"){
          
          #in defence, troops should not exceed enemy maximum troops
          search <- TRUE
          redundant <- i
          while (search == TRUE){
            
            #if more troops than necessary are assigned to a target pick another target tile for redundant troops. start reassigning troops that are the furthest away from the target
            if (any(selected_moves$tile == i)&(sum(selected_moves$troops_n[selected_moves$tile==i])>selected_moves$max_enemies1[match(i,selected_moves$tile)])){
              
              #check whether there is only one troop receiving the order
              if (sum(selected_moves$tile==i,na.rm=TRUE)==1){
                #if so then use unstack (admitted the troop is piled and current troop tile is a POI)
                if ((selected_moves$troops_n[selected_moves$tile==i]>1)&(selected_moves$troops_tile[selected_moves$tile==i] %in% as.numeric(potential_moves$tile))){selected_moves$unstack[selected_moves$tile==i] <- TRUE}
                search <- FALSE
              } else {
                #otherwise proceed with reassigning of target:
                reassignment_succesful <- FALSE
                while (reassignment_succesful==FALSE){
                  
                  #make a list of indices of the furthest troops from the objective tile i and randomly sample which one to reassign
                  furthest_troops <- as.character(c(1:nrow(selected_moves))[(selected_moves$tile==i)&(selected_moves$target_distance==max(selected_moves$target_distance[selected_moves$tile==i]))])
                  reassign_troop <- sample(furthest_troops,1)
                  reassign_troop_tile <- selected_moves$troops_tile[as.numeric(reassign_troop)]
                  
                  #select a new target from potential moves of the unit using the 0.9 criteria. Make sure to exclude from potential moves of the unit the tiles marked in "redundant"
                  candidate_moves <- potential_moves[(potential_moves$troops_tile==reassign_troop_tile) & !(potential_moves$tile %in% redundant),] 
                  candidate_moves <- candidate_moves[candidate_moves$move_value>0.9*max(candidate_moves$move_value),]
                  if (nrow(candidate_moves)>1){
                    prob_candidate_moves <- candidate_moves$move_value/sum(candidate_moves$move_value)
                    new_move <- candidate_moves[sample(1:nrow(candidate_moves),size=1,prob = prob_candidate_moves),] 
                    new_move$unstack<-FALSE  
                  } else {
                    new_move <- candidate_moves
                    new_move$unstack<-FALSE  
                  }
                  
                  #check whether targeting the new tile would create troop redundancy in new destination (redundancy limit for defence 0, mixed/attack +1 over max_enemies1)
                  #if not, then save the new target in selected moves
                  #if yes, then add it to "redundant" vector and repeat the reassignment of the troop until successful
                  if (new_move$type[1] != "defence"){ #for mixed and attack tiles:
                    if (sum(selected_moves$troops_n[selected_moves$tile==new_move$tile[1]])>1+new_move$max_enemies1[1]){
                      redundant <- c(redundant,new_move$tile[i])
                      reassignment_succesful <- FALSE #i.e. keep searching
                    } else {
                      #remove the old unit's move from selected moves table
                      selected_moves <-selected_moves[-as.numeric(reassign_troop),]
                      
                      #add new move
                      selected_moves <- rbind(selected_moves,new_move)
                      reassignment_succesful <- TRUE #i.e. stop searching
                    }
                  } else { #for defence tiles:
                    if (sum(selected_moves$troops_n[selected_moves$tile==new_move$tile[1]])>new_move$max_enemies1[1]){
                      redundant <- c(redundant,new_move$tile[1])
                      reassignment_succesful <- FALSE #i.e. keep searching
                    } else {
                      #remove the old unit's move from selected moves table
                      selected_moves <-selected_moves[-as.numeric(reassign_troop),]
                      
                      selected_moves <- rbind(selected_moves,new_move)
                      reassignment_succesful <- TRUE #i.e. stop searching
                    }
                  }
                }
              } 
            } else {search <- FALSE} #i.e. terminate reassignment
          }#bracket ends defence tile case
          
        } else {
          
          #same as for defence, but the redundancy limit is 1 above max_enemies1
          search <- TRUE
          redundant <- i
          while (search == TRUE){
            
            #if more troops than necessary are assigned to a target pick another target tile for redundant troops. start reassigning troops that are the furthest away from the target
            if (sum(selected_moves$troops_n[selected_moves$tile==i])> 1 + selected_moves$max_enemies1[match(i,selected_moves$tile)]){
              
              #check whether there is only one troop receiving the order
              if (sum(selected_moves$tile==i,na.rm=TRUE)==1){
                #if so then use unstack (admitted the troop is piled and current troop tile is a POI)
                if ((selected_moves$troops_n[selected_moves$tile==i]>1)&(selected_moves$troops_tile[selected_moves$tile==i] %in% as.numeric(potential_moves$tile))){ selected_moves$unstack[selected_moves$tile==i] <- TRUE}
                search <- FALSE
              } else {
                #otherwise proceed with reassigning of target:
                reassignment_succesful <- FALSE
                while (reassignment_succesful==FALSE){
                  
                  #make a list of indices of the furthest troops from the objective tile i and randomly sample which one to reassign
                  furthest_troops <- as.character(c(1:nrow(selected_moves))[(selected_moves$tile==i)&(selected_moves$target_distance==max(selected_moves$target_distance[selected_moves$tile==i]))])
                  reassign_troop <- sample(furthest_troops,1)
                  reassign_troop_tile <- selected_moves$troops_tile[as.numeric(reassign_troop)]
                  
                  #remove the sampled unit from selected moves table
                  selected_moves <-selected_moves[-as.numeric(reassign_troop),]
                  
                  #select a new target from potential moves of the unit using the 0.9 criteria. Make sure to exclude from potential moves of the unit the tiles marked in "redundant"
                  candidate_moves <- potential_moves[(potential_moves$troops_tile==reassign_troop_tile) & !(potential_moves$tile %in% redundant),] 
                  candidate_moves <- candidate_moves[candidate_moves$move_value>0.9*max(candidate_moves$move_value),]
                  if (nrow(candidate_moves)>1){
                    prob_candidate_moves <- candidate_moves$move_value/sum(candidate_moves$move_value)
                    new_move <- candidate_moves[sample(1:nrow(candidate_moves),size=1,prob = prob_candidate_moves),] 
                    new_move$unstack<-FALSE  
                  } else {
                    new_move <- candidate_moves
                    new_move$unstack<-FALSE
                  }
                  
                  #check whether targeting the new tile would create troop redundancy in new destination (redundancy limit for defence 0, mixed/attack +1 ove max_enemies1)
                  #if not, then save the new target in selected moves
                  #if yes, then add it to "redundant" vector and repeat the reassignment of the troop until successful
                  if (new_move$type[1] != "defence"){ #for mixed and attack tiles:
                    if (sum(selected_moves$troops_n[selected_moves$tile==new_move$tile[1]])>1+new_move$max_enemies1[1]){
                      redundant <- c(redundant,new_move$tile[i])
                      reassignment_succesful <- FALSE #i.e. keep searching
                    } else {
                      selected_moves <- rbind(selected_moves,new_move)
                      reassignment_succesful <- TRUE #i.e. stop searching
                    }
                  } else { #for defence tiles:
                    if (sum(selected_moves$troops_n[selected_moves$tile==new_move$tile[1]])>new_move$max_enemies1[1]){
                      redundant <- c(redundant,new_move$tile[1])
                      reassignment_succesful <- FALSE #i.e. keep searching
                    } else {
                      selected_moves <- rbind(selected_moves,new_move)
                      reassignment_succesful <- TRUE #i.e. stop searching
                    }
                  }
                  
                  if (!(i %in% selected_moves$tile)){reassignment_succesful<-TRUE}
                }
              }
            } else {search <- FALSE} #i.e. terminate reassignment
            
            if (!(i %in% selected_moves$tile)){search<-FALSE}
          }
        }#bracket ends attack/mixed tile case
        
      }#finish looping over all target tiles
      
      
      #DECIDE PATH FOR TILES 2+ DISTANCE AWAY
      for (i in 1:nrow(selected_moves)){
        
        #check if distance of unit from target
        if (selected_moves$target_distance[i]>1){
          
          # for every target distant 2 or more away, extract all neighbouring tiles
          neighbours <- c(1:nrow(territories$data))[territories$adjacency[,as.numeric(selected_moves$troops_tile[i])]]
          neighbours <- data.frame(troops_tile = selected_moves$troops_tile[i], neighbouring_tile=neighbours, distance_to_target=10, value = 0) 
          
          # check the distance of the neigbours to the target
          neighbours$distance_to_target <- territories$distance[neighbours$neighbouring_tile,as.numeric(selected_moves$tile[i])]
          
          #among extracted neighbours keep only the ones that are at the minimum distance to target
          neighbours <- neighbours[neighbours$distance_to_target == min(neighbours$distance_to_target),]
          
          #get the move value if the tile is a POI
          for (j in 1:nrow(neighbours)){
            neighbours$value[j] <- if (neighbours$neighbouring_tile[j] %in% as.numeric(potential_moves$tile[potential_moves$troops_tile == neighbours$troops_tile[j]])){
              potential_moves$move_value[(potential_moves$troops_tile == neighbours$troops_tile[j])&(as.numeric(potential_moves$tile)==neighbours$neighbouring_tile[j])]
            } else {0}
          }
          
          #sample among the neighbours with the largest value
          neighbours <- neighbours[as.numeric(sample(as.character(1:nrow(neighbours))[neighbours$value==max(neighbours$value)], 1)),]
          
          #replace result to selected moves (using info from potential moves if available)
          if (neighbours$neighbouring_tile %in% as.numeric(potential_moves$tile[potential_moves$troops_tile == neighbours$troops_tile])){
            selected_moves[i,] <- cbind(potential_moves[(as.numeric(potential_moves$tile) == neighbours$neighbouring_tile) & (potential_moves$troops_tile == neighbours$troops_tile),],"unstack"=selected_moves$unstack[i])
          } else { #if not on POI llist than the tile is necessarily intern to nation's land:
            selected_moves$tile[i] <- as.character(neighbours$neighbouring_tile)
            selected_moves$type[i] <- "intern"
            selected_moves$tile_owner[i] <- nation
            selected_moves[i,7:ncol(selected_moves)]<-NA
            selected_moves$unstack <- FALSE
          }
        }
      }
      
      
      #PLACE RULER AND REVALUATE MOVES - in safest spot conditional to maximising score of freed-up troop or defence of territory 
      
      #check whether ruler of the nation is alive, proceed only if it is
      if (nations$ruler_alive[nations$names==nation]){
        
        ruler_to_place <- TRUE
        
        #check whether ruler can be used in defence, otherwise place it in a safe spot:
        if (any(selected_moves$type == "defence")){
          #summarise defensive troop movement
          def_summary <- selected_moves[selected_moves$type == "defence",] %>% group_by(tile) %>% summarise(troops_n = sum(troops_n),
                                                                                                            threat = max(max_enemies1),
                                                                                                            units_moving = n(),
                                                                                                            POI_value = max(POI_value),.groups="drop") %>% as.data.frame()
          
          def_summary$diff <- def_summary$troops_n - def_summary$threat
          
          #check whether additional help is needed in any defence tile (i.e. if threat > n_troops but not too desperate otherwise might die) 
          if (any((def_summary$diff < 0))){
            
            #check also neighbouring tiles for better defensive position          
            #make list of owned tiles in which the ruler might be placed
            defend_tiles <- as.numeric(def_summary$tile[def_summary$diff<0])
            temp <- defend_tiles
            for (k in defend_tiles){
              temp <- unique(c(temp, c(1:nrow(territories$data))[territories$distance[,k]<=1]))
            }
            temp <- data.frame(tile=temp[territories$data$owner[temp]==nation])
            
            #check the threat level in each candidate position
            temp$threat <- NA
            temp$n_troops <- NA
            temp$neighbouring_troops <- NA
            for (k in 1:nrow(temp)){
              temp$threat[k] <- sum(territories$data$n_troops[(territories$data$troops != nation) & (territories$distance[as.numeric(temp$tile[k]),]==1)], na.rm = TRUE)
              temp$neighbouring_troops[k] <- sum(territories$data$n_troops[(territories$data$troops == nation) & (territories$distance[as.numeric(temp$tile[k]),]<=1)], na.rm = TRUE)
              temp$n_troops[k] <- sum(selected_moves$troops_n[as.numeric(selected_moves$tile) == temp$tile[k]])
            }
            temp$diff <- temp$threat - temp$n_troops
            temp$safe <- (temp$threat - temp$n_troops) <= 0
            
            #if a safe tile is available select the one neighbouring with most troops, othewise pick the one with troops and lowest threat
            if (any(temp$safe)){
              ruler_placement[nrow(ruler_placement)+1,] <- c(nation,sample(as.character(temp$tile[(temp$safe)&(temp$neighbouring_troops==max(temp$neighbouring_troops[temp$safe]))]),1))
              ruler_to_place <- FALSE
            } else {
              ruler_placement[nrow(ruler_placement)+1,] <- c(nation,sample(as.character(temp$tile[(temp$n_troops>0)&(temp$diff==min(temp$diff[temp$n_troops>0]))]),1))
              ruler_to_place <- FALSE
            }
            
            
          }
          
          #otherwise just place it to support the defence spot neighbouring most troops
          if (ruler_to_place){
            #find number of neighbouring troops for each target
            def_summary$n_neighbours <- 0
            for (k in 1:nrow(def_summary)){
              def_summary$n_neighbours[k] <- sum(territories$data$troops[c(1:nrow(territories$data))[territories$adjacency[,as.numeric(def_summary$tile[k])]]] == nation, na.rm = TRUE)
            }
            #place ruler in one of the tiles neighbouring with most troops
            ruler_placement[nrow(ruler_placement)+1,] <- c(nation,sample(def_summary$tile[def_summary$n_neighbours == max(def_summary$n_neighbours)],1))
          }
          
        } else {
          #if no defensive use is found to ruler, place in a randomly selected tile among the least exposed to enemy threat
          
          #make a summary table for all owned tiles
          tiles_stats <- data.frame(tiles=territories$data$tile_number[territories$data$owner == nation], threat = 0) 
          for (k in 1:nrow(tiles_stats)){
            tiles_stats$threat[k] <- sum(territories$data$n_troops[(territories$data$troops != nation) & (territories$distance[as.numeric(tiles_stats$tile[k]),]==1)], na.rm = TRUE) 
          }
          
          #pick one of the territories with lowest threat
          ruler_placement[nrow(ruler_placement)+1,] <- c(nation,sample(as.character(tiles_stats$tile[tiles_stats$threat == min(tiles_stats$threat)]),1))
        }
      }#END of RULER placement
      
      
      
      
      
      #REVIEW ORDERS AFTER POSITIONING OF THE RULER:
      
      #if ruler is alive (hence placed somewhere) then proceed
      if (nation %in% ruler_placement$nation){
        
        #get info on ruler tile's and neighbouring cells owned by player
        ruler_tile <- as.numeric(ruler_placement$tile[ruler_placement$nation==nation])
        tiles_area_ruler <- c(1:nrow(territories$data))[(territories$distance[,ruler_tile]<=1)&(territories$data$owner==nation)]
        
        #make a table of all "defence" moves directed to cells within the ruler's area
        temp <- selected_moves[(selected_moves$type == "defence")&(as.numeric(selected_moves$tile) %in% tiles_area_ruler),] 
        
        #if there is any defensive move directed to these cells, proceed with checking:
        if (nrow(temp)>0){
          
          #compile stats on each target tile
          def_summary <- temp %>%
            group_by(tile) %>% 
            summarise(troops_n = sum(troops_n)+1,
                      threat = max(max_enemies1),
                      units_moving = n(),
                      POI_value = max(POI_value),.groups="drop") %>% as.data.frame()
          def_summary$diff <- def_summary$troops_n - def_summary$threat
          
          #loop over every target tile and check for redundancies
          for (target_row in 1:nrow(def_summary)){
            
            #check for redundancy
            if (def_summary$diff[target_row]>0){
              
              target_tile <- as.numeric(def_summary$tile[target_row])
              
              #check whether multiple troops are involved. If only one is present consider unstack moves.
              if ((def_summary$units_moving[target_row]==1)){
                
                #check whether the only order is for a troop with multiple units and directed to a tile for which the enemy's threat is 1
                if ((def_summary$troops_n[target_row]>1)&(def_summary$threat[target_row]==1)){
                  troop_tile <- as.numeric(selected_moves$troops_tile[as.numeric(selected_moves$tile) == target_tile]) 
                  selected_moves$unstack[(as.numeric(selected_moves$tile)==target_tile)&(as.numeric(selected_moves$troops_tile)==troop_tile)] <- TRUE
                }
                
                #check whether the only order is for a troop with multiple units and placed on the same tile to which the attack is directed  
                if ((def_summary$troops_n[target_row]>1)&(as.numeric(selected_moves$troops_tile[as.numeric(selected_moves$tile) == target_tile])==target_tile)){
                  
                  #Prepare stats to give the unstack order
                  troop_tile <- as.numeric(selected_moves$troops_tile[as.numeric(selected_moves$tile) == target_tile])  
                  troop_neighbours <- c(1:nrow(territories$data))[territories$distance[,troop_tile]<=1]
                  troop_neighbours <- troop_neighbours[troop_neighbours != target_tile]
                  
                  #make a stats table for potential moves
                  troop_moves <- data.frame(tile=troop_neighbours)
                  troop_moves <- potential_moves[as.numeric(potential_moves$troops_tile)==troop_tile,] %>%
                    mutate(tile=as.numeric(tile))%>%
                    left_join(troop_moves,.,by=c("tile"="tile")) %>% as.data.frame()
                  troop_moves$move_value <- ifelse(is.na(troop_moves$move_value),0,troop_moves$move_value)
                  troop_moves$type <- ifelse(is.na(troop_moves$type),"internal",troop_moves$type)
                  troop_moves$in_ruler_area <- troop_moves$tile %in% tiles_area_ruler
                  
                  #import info on ordered troop movements to check redundancy of any of these potential moves
                  troop_moves$friendly_troops <- 0
                  for (k in troop_moves$tile){
                    
                    #import info on max_enemies1 if not present
                    if (is.na(troop_moves$max_enemies1[troop_moves$tile==k])){
                      troop_moves$max_enemies1[troop_moves$tile==k] <- territories$data[!is.na(territories$data$troops) & (territories$data$troops != nation) & (territories$distance[as.numeric(k),]<=1),] %>%
                        group_by(troops) %>% summarise(threat=sum(n_troops,na.rm=TRUE), .groups="drop") %>% summarise(max_threat = ifelse(nrow(.)==0,0,max(threat))) %>% unlist(.,use.names=FALSE)
                    }
                    
                    #find all troops ordered to tile k
                    if (k %in% as.numeric(selected_moves$tile)){
                      troop_moves$friendly_troops[troop_moves$tile==k] <- sum(ifelse(selected_moves$unstack[as.numeric(selected_moves$tile)==k],
                                                                                     ifelse(as.numeric(selected_moves$troops_tile[as.numeric(selected_moves$tile)==k]) != as.numeric(selected_moves$tile[as.numeric(selected_moves$tile)==k]),1,selected_moves$troops_n[as.numeric(selected_moves$tile)==k] - 1),
                                                                                     selected_moves$troops_n[as.numeric(selected_moves$tile)==k]))
                    }
                  }
                  troop_moves$def_force <- ifelse(troop_moves$friendly_troops>0,troop_moves$friendly_troops + as.numeric(troop_moves$in_ruler_area), troop_moves$friendly_troops)  
                  troop_moves$diff <- troop_moves$def_force - troop_moves$max_enemies1
                  
                  #remove internal movements and attack or defence movements for which the troop would be redundant
                  troop_moves <- troop_moves[((troop_moves$type=="defence")&(troop_moves$diff<0))|((troop_moves$type %in% c("mixed","attack"))&(troop_moves$diff<=0)),]
                  
                  #if any potential move is left, select one among the ones with the highest score
                  if (nrow(troop_moves)>0){
                    #keep only moves at 90% of highest score
                    troop_moves <- troop_moves[troop_moves$move_value >= 0.9*max(troop_moves$move_value,na.rm=TRUE),]
                    
                    #randomly pick one
                    troop_moves <- troop_moves[sample(c(1:nrow(troop_moves)),1),]
                    
                    #update selected move table
                    selected_moves <- selected_moves[(as.numeric(selected_moves$troops_tile)!=troop_tile),] #remove old order
                    selected_moves[nrow(selected_moves)+1,] <- NA
                    selected_moves[nrow(selected_moves),c("troops_owner","tile","troops_tile")] <- c(nation,troop_moves$tile[1],troop_tile)
                    selected_moves$unstack[nrow(selected_moves)] <- TRUE
                    selected_moves$troops_n[nrow(selected_moves)] <- territories$data$n_troops[as.numeric(troop_moves$tile[1])]
                  }
                  
                }
                
                #If multiple troops are directed to the target tile, start re-evaluating from the smallest troop  
              } else if ((def_summary$units_moving[target_row]>1)){
                
                #get list of tiles in which troops are located
                troop_tiles <- as.numeric(selected_moves$troops_tile[as.numeric(selected_moves$tile) == target_tile])  
                troop_info <- data.frame(troop_tiles = troop_tiles, n_troops = territories$data$n_troops[troop_tiles])
                
                stop_reassignment <- FALSE
                while ((def_summary$diff[target_row]>0)&(stop_reassignment==FALSE)){
                  
                  #select one of the troops with the lowest number of units
                  troop_tile <- as.numeric(sample(as.character(troop_info$troop_tiles[troop_info$n_troops==min(troop_info$n_troops)]),1))
                  
                  #check whether redirecting the troop will create a troop deficit, if so unstacking will be needed
                  if (def_summary$diff[target_row] - troop_info$n_troops[troop_info$troop_tiles==troop_tile] < 0){
                    #if unstacking the troop would still leave the target in deficit, then stop reassignment, otherwise allow unstack
                    if (def_summary$diff[target_row] - troop_info$n_troops[troop_info$troop_tiles==troop_tile] + 1 < 0){stop_reassignment <- TRUE}
                    temp_unstack <- TRUE
                  } else {
                    temp_unstack <- FALSE
                  }
                  
                  #check whether there is need to look for a new target (if unstacking is necessary and the troop is in a different territory from old target, there is no need to look for new target) 
                  if ((temp_unstack == TRUE)&(troop_tile != target_tile)){
                    
                    #update selected_moves table by introducing unstack order
                    selected_moves$unstack[as.numeric(selected_moves$troops_tile)==troop_tile] <- temp_unstack
                    
                    #update table def_summary 
                    def_summary$troops_n[target_row] <- def_summary$troops_n[target_row] - (troop_info$n_troops[troop_info$troop_tiles==troop_tile] -1)
                    def_summary$diff[target_row] <- def_summary$diff[target_row] - (troop_info$n_troops[troop_info$troop_tiles==troop_tile] -1)
                    
                    #remove from troop_info table
                    troop_info<-troop_info[troop_info$troop_tiles != troop_tile,]
                    
                  } else {
                    
                    #make a list of all potential tiles where selected troop could move
                    troop_neighbours <- c(1:nrow(territories$data))[territories$distance[,troop_tile]<=1]
                    troop_neighbours <- troop_neighbours[troop_neighbours != target_tile]
                    
                    #make a stats table for potential moves
                    troop_moves <- data.frame(tile=troop_neighbours)
                    troop_moves <- potential_moves[as.numeric(potential_moves$troops_tile)==troop_tile,] %>%
                      mutate(tile=as.numeric(tile))%>%
                      left_join(troop_moves,.,by=c("tile"="tile")) %>% as.data.frame()
                    troop_moves$move_value <- ifelse(is.na(troop_moves$move_value),0,troop_moves$move_value)
                    troop_moves$type <- ifelse(is.na(troop_moves$type),"internal",troop_moves$type)
                    troop_moves$in_ruler_area <- troop_moves$tile %in% tiles_area_ruler
                    
                    #import info on ordered troop movements to check redundancy of any of these potential moves
                    troop_moves$friendly_troops <- 0
                    for (k in troop_moves$tile){
                      
                      #import info on max_enemies1 if not present
                      if (is.na(troop_moves$max_enemies1[troop_moves$tile==k])){
                        troop_moves$max_enemies1[troop_moves$tile==k] <- territories$data[!is.na(territories$data$troops) & (territories$data$troops != nation) & (territories$distance[as.numeric(k),]<=1),] %>%
                          group_by(troops) %>% summarise(threat=sum(n_troops,na.rm=TRUE), .groups="drop") %>% summarise(max_threat = ifelse(nrow(.)==0,0,max(threat))) %>% unlist(.,use.names=FALSE)
                      }
                      
                      #find all troops ordered to tile k
                      if (k %in% as.numeric(selected_moves$tile)){
                        troop_moves$friendly_troops[troop_moves$tile==k] <- sum(ifelse(selected_moves$unstack[as.numeric(selected_moves$tile)==k],
                                                                                       ifelse(as.numeric(selected_moves$troops_tile[as.numeric(selected_moves$tile)==k]) != as.numeric(selected_moves$tile[as.numeric(selected_moves$tile)==k]),1,selected_moves$troops_n[as.numeric(selected_moves$tile)==k] - 1),
                                                                                       selected_moves$troops_n[as.numeric(selected_moves$tile)==k]))
                      }
                    }
                    troop_moves$def_force <- ifelse(troop_moves$friendly_troops>0,troop_moves$friendly_troops + as.numeric(troop_moves$in_ruler_area), troop_moves$friendly_troops)  
                    troop_moves$diff <- troop_moves$def_force - troop_moves$max_enemies1
                    
                    #remove internal movements and attack or defence movements for which the troop would be redundant
                    troop_moves <- troop_moves[((troop_moves$type=="defence")&(troop_moves$diff<0))|((troop_moves$type %in% c("mixed","attack"))&(troop_moves$diff<=0)),]
                    
                    
                    #if any potential move is left, select one among the ones with the highest score
                    if (nrow(troop_moves)>0){
                      #keep only moves at 90% of highest score
                      troop_moves <- troop_moves[troop_moves$move_value >= 0.9*max(troop_moves$move_value,na.rm=TRUE),]
                      
                      #randomly pick one
                      troop_moves <- troop_moves[sample(c(1:nrow(troop_moves)),1),]
                      
                      #update selected move table
                      temp_ntroops <- territories$data$n_troops[as.numeric(troop_moves$tile[1])]
                      selected_moves <- selected_moves[(as.numeric(selected_moves$troops_tile)!=troop_tile),] #remove old order
                      selected_moves[nrow(selected_moves)+1,] <- NA
                      selected_moves[nrow(selected_moves),c("troops_owner","tile","troops_tile")] <- c(nation,troop_moves$tile[1],troop_tile)
                      selected_moves$troops_n[nrow(selected_moves)] <- temp_ntroops
                      selected_moves$unstack[nrow(selected_moves)] <- temp_unstack
                      
                      #update table def_summary (notice that unstacking troops treated here are only the ones for which target_tile==troop_tile)
                      def_summary$units_moving[target_row] <- def_summary$units_moving[target_row] -1 
                      def_summary$troops_n[target_row] <- def_summary$troops_n[target_row] - if (temp_unstack){1} else {temp_ntroops}
                      def_summary$diff[target_row] <- def_summary$diff[target_row] - if (temp_unstack){1} else {temp_ntroops}
                      
                      #remove from troop_info table
                      troop_info<-troop_info[troop_info$troop_tiles != troop_tile,]
                      
                    } else {
                      #if no move is left for the troop then remove it from table troop_info to avoid infinite loop
                      troop_info<-troop_info[troop_info$troop_tiles != troop_tile,]
                    }
                  }
                  #exist reallocation loop if there is no more troop to reallocate
                  if (nrow(troop_info)==0){stop_reassignment <- TRUE}
                }
              }
            }
          }
        }
      } 
      
      #save data for AI development
      if (dev_data == TRUE){
        if (is.null(dev_data_record)) {dev_data_record <- selected_moves} else {dev_data_record <- rbind(dev_data_record,selected_moves)}
      }
      
      
      #EXPORT MOVES TO ORDERS TABLE
      for (k in 1:nrow(selected_moves)){
        if (as.numeric(selected_moves$troops_tile[k]) != as.numeric(selected_moves$tile[k])){ #exclude hold moves
          type <- if (selected_moves$unstack[k]==TRUE) "unstack" else "move"
          tile_number <- selected_moves$troops_tile[k]
          to <- selected_moves$tile[k] 
          
          orders[nrow(orders)+1,] <- c(type,tile_number,to)
        } 
      }
    }#end of if nrow(potential_moves)>0
  }
  
  if (dev_data == TRUE){
    return(list(orders=orders, ruler_placement=ruler_placement, dev_data=dev_data_record))
  } else {
    return(list(orders=orders, ruler_placement=ruler_placement))
  }
}
#_________________________________________________





# 5) RECRUIT ORDERS --------------------------------

recruit_orders<- function(POI_table,territories,nations, def_n_tiles_troops){
  
  orders <- data.frame(type=character(0), tile_number=integer(0), to=integer(0))
  ruler_placement <- data.frame(nation=character(0),tile = numeric(0))
  
  #add temporary column to nation table on info about excess/deficit n troops
  nations$recruit <- (nations$tot_cities + (nations$tot_tiles - nations$tot_cities) %/% def_n_tiles_troops) - nations$tot_troops
  
  #loop over every AI nation
  for (nation in nations$names[!nations$is_player]){
    
    #Excess troops: DISBAND
    if (nations$recruit[nations$names==nation]<0){
      
      troops_to_disband <- -1 * nations$recruit[nations$names==nation]
      
      #make a table of potential troops to disband
      troops <- data.frame(tile=territories$data$tile_number[(territories$data$troops==nation)&!is.na(territories$data$troops)])
      troops$n_troops <- territories$data$n_troops[territories$data$tile_number %in% troops$tile]
      
      #import POI value of troop tile
      troops$POI <- 0
      for (k in 1:nrow(troops)){
        #check whether is on POI_table, if so import POI value
        if (any((POI_table$nations==nation) & (POI_table$tile == troops$tile[k]))){
          troops$POI[k] <- POI_table$tile_value[(POI_table$nations==nation) & (POI_table$tile == troops$tile[k])]
        }
      }
      
      
      #disband one unit at the time
      while (troops_to_disband>0){
        
        #disband troop on tile with lowest value
        type <- "disband"
        tile_number <- as.numeric(sample(as.character(troops$tile[troops$POI==min(troops$POI)]),1))
        to <- NA
        
        orders[nrow(orders)+1,] <- c(type,tile_number,to)
        
        
        #update table of available troops for disbanding
        troops <- troops[troops$tile != tile_number,]
        
        #update value of disband counter.        
        troops_to_disband <- if (nrow(troops)==0) 0 else troops_to_disband - 1  
        
      }
      
    }
    
    #Deficit troops: RECRUIT
    if (nations$recruit[nations$names==nation]>0){
      
      #number of troops to recruit
      troops_to_recruit <- nations$recruit[nations$names==nation]
      
      #make a list of cores which are still owned
      cores <- territories$data$tile_number[(territories$data$core==nation)&(territories$data$owner==nation)]
      
      #proceed with recruitment if any core is still owned
      if (length(cores)>0){
        
        #recruit one unit at the time
        while (troops_to_recruit>0){
          
          #check whether any cores is a POI. if so, pick the one with highest value
          if (any(cores %in% as.numeric(POI_table$tile[POI_table$nations==nation]))){
            
            temp <- POI_table[(POI_table$nations==nation)&(as.numeric(POI_table$tile) %in% cores),]
            
            type <- "recruit"
            tile_number <- as.numeric(sample(as.character(temp$tile[temp$tile_value==max(temp$tile_value)]),1))
            to <- NA
            
            orders[nrow(orders)+1,] <- c(type,tile_number,to)
            
          } else {
            #if no core is among POI, pick randomly
            type <- "recruit"
            tile_number <- as.numeric(sample(as.character(cores),1))
            to <- NA
            
            orders[nrow(orders)+1,] <- c(type,tile_number,to)
          }
          
          #update available cores for recruiting
          cores <- cores[cores != tile_number]
          
          #update value of recruit counter. If no core is available for recruitment, set it to zero
          if (length(cores)==0){
            troops_to_recruit <- 0
          } else {
            troops_to_recruit <- troops_to_recruit-1  
          }
        }
        
      }
      
    }
    
  }
  
  return(list(orders=orders, ruler_placement=ruler_placement))
}

