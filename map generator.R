

#DEFINE ITERATED DIRICHLET FUNCTION ---------------------------------------------------

iter_dirichlet <- function(x,y,max_range,relaxation_iterations, correct_adjacency=TRUE){
  #create a ppp data for spatstat
  points<- ppp(x,y,xrange = c(0,max_range),yrange = c(0,max_range))
  
  #Create a smooth map by iterating between Voronoi tesselation and shifting points to the centroid of each tile
  iteration <- 0   
  while (iteration < relaxation_iterations) {
    
    #extract Voronoi tiles
    tessere <- tiles(dirichlet(points))
    
    #calculate centroid and extract from list
    temp <- lapply(tessere,centroid.owin)
    new_points <-data.frame(x=rep(NA,length(temp)),y=rep(NA,length(temp)))
    for (i in 1:length(temp)){
      new_points$x[i] <- temp[[i]]$x
      new_points$y[i] <- temp[[i]]$y
    }
    rm(i,temp)
    
    #update
    points<- ppp(new_points$x,new_points$y,xrange = c(0,max_range),yrange = c(0,max_range))
    iteration <- iteration + 1
  }
  
  #extract adjacency matrix, distance matrix and tile area
  distance<-delaunayDistance(points)
  adjacency<-distance==1
  area <- tile.areas(dirichlet(points))
  
  #correct adjacency and distance matrix
  if (correct_adjacency==TRUE){
    
    #compute distance from edge for each tile and keep a list of all tiles that touch the edge
    edge_tiles <- c(1:length(x))[bdist.tiles(dirichlet(points))==0]
    
    #get a list of all tiles with their boundaries
    tessere <- tiles(dirichlet(points))
    
    #loop over every combination of edge tiles (once)
    for (i_index in 1:length(edge_tiles)){
      for (j_index in i_index:length(edge_tiles)){
        
        #run only when i is different from j
        if (i_index != j_index){
          i <- edge_tiles[i_index]
          j <- edge_tiles[j_index]
          
          #check whether the combination of tiles is marked as adjacent
          if (adjacency[i,j]==TRUE){
            
            #get all boundary points of tile i and j (rounded to increase robustness)
            i_x <- round(tessere[[i]]$bdry[[1]]$x, round(length(x)/max_range,0))
            i_y <- round(tessere[[i]]$bdry[[1]]$y, round(length(x)/max_range,0))
            j_x <- round(tessere[[j]]$bdry[[1]]$x, round(length(x)/max_range,0))
            j_y <- round(tessere[[j]]$bdry[[1]]$y, round(length(x)/max_range,0))
            
            #If any two points are shared among i and j, then do not change matrix
            if (sum(i_y[i_x %in% j_x] %in% j_y)<2){
              adjacency[i,j] <- FALSE
              adjacency[j,i] <- FALSE
            }
          }
        }
      }
    }
    
    #compute distance matrix from the corrected adjacency matrix with Floyd-Warshall algorithm
    temp <- matrix(as.integer(adjacency), nrow= length(points$x), ncol= length(points$x)) #convert adjacency matrix into 1/0 instead of logical
    temp[temp==0]<-NA   #substitute zeros with NA to run Floyd-Warshall algorithm
    distance <- Rfast::floyd(temp)
  }
  
  #extract final coordinates as data frame
  final <- list(data = coords.ppp(points), distance = distance, adjacency = adjacency, area = area)
  
  
  return(final)
}
#________________________________________________________________________________________




# FUNCTIONS FOR CORE ASSIGNMENT -------------------------------------------

# A) This function returns a table with all best candidate cells for assignment. n is the number of countries for which the cell is a candidate.
find_assign_candidates <- function(territories_data, nations_data){
  
  #1) FOR EACH NATION MAKE A LIST OF ADJACENT FREE CELLS
  candidate_cells <- data.frame(tile_index=numeric(0),nation_index=numeric(0))
  for (i in nations_data$names){ #loop over nation
    
    cells_to_check <- territories_data$data$tile_number[territories_data$data$owner==i] #list of cells owned by nation
    
    check_distance <- 1
    #check cells at increasing distance
    while (check_distance <= max(territories_data$distance)){
      if (any(territories_data$distance[,cells_to_check]==check_distance)){
        
        #make a list of all neighbouring cells
        temp <- NULL
        for(j in cells_to_check){ #loop over cells owned by nation
          temp <- c(temp, territories_data$data$tile_number[territories_data$distance[,j]==check_distance])
        }
        
        #only keep cells that are not already owned
        temp <- unique(temp)
        temp <- temp[temp %fin% territories_data$data$tile_number[territories_data$data$owner == "no owner"]]
        
        if (length(temp)>0){
          #put results in final candidate table
          candidate_cells <- rbind(candidate_cells, data.frame(tile_index=temp,
                                                               nation_index=i))
          #terminate while loop (go to next nation)
          check_distance <- max(territories_data$distance)
        }
      }
      #increase distance to check 
      check_distance <- check_distance + 1
    }
  }
  
  # 2) COUNT FREQUENCY OF EACH CANDIDATE CELL
  candidate_cells <- left_join(candidate_cells, count(candidate_cells, tile_index), by= "tile_index")
  
  #output updated territories data
  return(candidate_cells)
}



#B) This function performs the assignment of one core per nation listed. it uses the function defined above to find the candidates
assign_cells <- function(territories_data, nations_data,nations_to_assign){
  
  for (i in nations_to_assign){#loop over nations
    #find best candidates for all nations
    candidate_cells <- find_assign_candidates(territories_data, nations_data)
    
    #pick one of the least contentious cells for nation i then go to next nation
    nation_cells <- candidate_cells[(candidate_cells$nation_index==i),]
    best_cells <- nation_cells$tile_index[nation_cells$n==min(nation_cells$n)]
    if (length(best_cells)==1){
      cell_to_assign <- best_cells
    } else{
      cell_to_assign <- sample(best_cells, 1)
    }
    
    #assign territory ownership
    territories_data$data$owner[territories_data$data$tile_number==cell_to_assign] <- i
  }
  
  #return updated territories data
  return(territories_data)
}

#c) This function performs a RANDOM ASSIGNMENT of territories
assign_cells_random <- function(territories_data, nations_data,nations_to_assign){
  
  for (i in nations_to_assign){#loop over nations
    #find empty cells
    candidate_cells <- territories_data$data$tile_number[territories_data$data$owner == "no owner"]
    
    #pick one of the least contentious cells for nation i then go to next nation
    if (length(candidate_cells)>0){
      cell_to_assign <- sample(candidate_cells, 1)
      
      #assign territory ownership
      territories_data$data$owner[territories_data$data$tile_number==cell_to_assign] <- i
    }
  }
  
  #return updated territories data
  return(territories_data)
}
#_______________________________________________________________________









# INITIATE GAME FUNCTION -----------------------------------------------

initiate_game <- function(number_of_nations = 5, number_of_empty_cells = 20, number_of_free_cities = 10, base_number_of_cores = 3, tiles_iterations=20, central_handicap= TRUE, random_assignment = FALSE, names_territories_list = unlist(unname(read.csv("data/nomi comuni italiani.csv"))), names_nations_list = unlist(unname(read.csv("data/nomi inventati.csv"))), n_control_nations = 1, colour_palette = c("#e4201b", "#377eb8", "#4caf4a", "#984ea3", "#ff7f00", "#fffc33", "#a65628", "#f781bf", "#999999", "#a5cee3", "#fb9a99", "#b2df89"), player_mode="test"){
  
  number_of_cells  <- base_number_of_cores*number_of_nations + number_of_empty_cells + number_of_free_cities
  number_of_cities <- base_number_of_cores*number_of_nations + number_of_free_cities
  
  # 0) EVALUATE PARAMETERS -----------------------------------
  
  
  # limit number of players to 12 and no less than 1
  if (number_of_nations>12){number_of_nations<-12}
  if (number_of_nations<1){number_of_nations<-12}
  
  # # adjust relationship between number of nations/cells/cores by increasing cells
  # if (number_of_cells < number_of_nations*(base_number_of_cores+1)){
  #   number_of_cells <- number_of_nations*(base_number_of_cores+1)
  # }
  # 
  # #adjust number of cities when in excess or deficit
  # if (number_of_cities > number_of_cells){
  #   number_of_cities <- number_of_cells
  # }
  # if (number_of_cities < number_of_nations*(base_number_of_cores+1)){
  #   number_of_cities <- number_of_nations*(base_number_of_cores)+round(number_of_nations/3)
  # }
  
  #Check if there are enough names for territories/nations
  if ((length(names_territories_list) < number_of_cells)|(length(names_nations_list) < number_of_nations)){
    stop("Not enough names for nations or territories! Add more names or reduce nations/territories.")
  }
  
  #set maximum values for certain parameters
  if (tiles_iterations > 100){tiles_iterations <- 100}
  
  #Define range for plot
  max_range <- max(3*number_of_cells,1000)
  
  #sample random points
  tiles_x <- sample(0:max_range,number_of_cells)
  tiles_y <- sample(0:max_range,number_of_cells)
  #________________________________________________________________________________________
  
  
  
  # 1) GENERATE TESSELATION POINTS -----------------------------------
  
  #generate territories
  territories <- iter_dirichlet(x= tiles_x,
                                y= tiles_y,
                                max_range = max_range,
                                relaxation_iterations = tiles_iterations,
                                correct_adjacency = TRUE)
  
  #assign index to territories
  territories$data$tile_number <- 1:number_of_cells 
  #__________________________________________________________________
  
  
  
  # 2) GENERATE SCATTERED STARTING POINTS -----------------------------
  
  #Generate points
  start_points<- iter_dirichlet(x = sample(0:max_range,number_of_nations),
                                y = sample(0:max_range,number_of_nations),
                                max_range = max_range,
                                relaxation_iterations = 3)
  
  #Find in which tiles the points belong
  start_points$data$tile_number <- tileindex(x= start_points$data$x,
                                             y= start_points$data$y,
                                             Z= dirichlet(ppp(territories$data$x,territories$data$y,xrange = c(0,max_range),yrange = c(0,max_range)))) %>% as.numeric() #tiles from voronoi diagram
  #_______________________________________________________________________
  
  
  
  
  # 3) INITIATE NATIONS AND FIRST CORE ----------------------------------
  
  #Name nations
  nations <- data.frame(names=sample(names_nations_list,number_of_nations), index = 1:number_of_nations)

  #set which nations are controlled by player
  nations$is_player <- c(rep(TRUE,n_control_nations),rep(FALSE,number_of_nations-n_control_nations))
  nations$names <- ifelse(nations$is_player,paste(nations$names,"(player)"),nations$names)
  
  #Define ownership of start points
  start_points$data$owner <- sample(nations$names,number_of_nations)
  
  #assign first core territory ownership
  territories$data <- left_join(territories$data, start_points$data[,!(names(start_points$data) %in% c("x", "y"))], by="tile_number")
  territories$data$owner[is.na(territories$data$owner)] <- "no owner"
  
  #place ruler
  territories$data$ruler <- territories$data$owner != "no owner"
  
  #assign colours
  nations$colours <- colour_palette[1:number_of_nations]
  #_______________________________________________________________________
  
  
  
  
  
  # 4) ASSIGN ADDITIONAL CORES ----------------------------------
  
  #Assign cores
  assigned_cores <- 1
  while (assigned_cores < base_number_of_cores) {
    
    #assign a territory to every nation
    if(random_assignment){
      territories<-assign_cells_random(territories,nations,nations$names)
    } else {
      territories<-assign_cells(territories,nations,nations$names)
    }
    
    #update counter
    assigned_cores <- assigned_cores + 1
  }
  
  #assign one additional core to nations with handicap
  if ((number_of_nations>2)&(central_handicap==TRUE)&(random_assignment==FALSE)){
    handicap <- numeric()
    for (i in nations$names) {
      temp_x <- mean(territories$data$x[territories$data$owner==i])
      temp_y <- mean(territories$data$y[territories$data$owner==i])
      
      if((temp_x >0.3*max_range) &(temp_y >0.3*max_range)&(temp_x <0.7*max_range) &(temp_y <0.7*max_range)){
        handicap <- c(handicap,i)
      }
    }
    
    #start_points$data$handicap <- (number_of_nations>2)&(central_handicap==TRUE)&((start_points$data$x<0.7*max_range)&(start_points$data$x>0.3*max_range))&((start_points$data$y<0.7*max_range)&(start_points$data$y>0.3*max_range))
    #handicap <- start_points$data$owner[start_points$data$handicap]
    if (length(handicap)>number_of_free_cities){
      handicap <- NULL
    }
    if (length(handicap)>0){
      if(random_assignment){
        territories<-assign_cells_random(territories,nations,handicap)
      } else {
        territories<-assign_cells(territories,nations,handicap)
      }
    }
    
    #remove non necessary data
    #territories$data$handicap <- NULL
    rm(temp_x,temp_y)
  }
  
  #mark territories ditributed until now as "core" of that specific nation in territories$data
  territories$data$core <- ifelse(territories$data$owner=="no owner", "", territories$data$owner)
  #_______________________________________________________________________
  
  
  
  # 5)  OTHER MAP FEATURES --------------------------------------------
  
  #assign name to territories 
  territories$data$name<-sample(names_territories_list,number_of_cells)
  
  #distibute cities to cores
  territories$data$city <- ifelse(territories$data$core != "", TRUE, FALSE)
  
  #distribute remaining cities randomly
  if ((number_of_cities-sum(territories$data$city))>0){
    tiles_number_city <- sample(territories$data$tile_number[territories$data$city==FALSE], number_of_cities-sum(territories$data$city))
    territories$data$city[territories$data$tile_number %fin% tiles_number_city] <- TRUE
    rm(tiles_number_city)
  } 
  
  #import tile area into territories data and scale to %
  territories$data$area <- unname(territories$area)
  territories$area <- NULL
  territories$data$area <- round(100*territories$data$area/(max_range^2),0)
  
  #distribute starting troops
  territories$data$troops <- ifelse(territories$data$owner=="no owner",NA,territories$data$owner)
  
  #initiate troop counter for territories and cue for the size to display on map
  territories$data$n_troops <- ifelse(is.na(territories$data$troops),0,1)
  territories$data$size_display_troops<-ifelse(territories$data$n_troops %in% c(0,1),1,2)
  
  #initiate counter for the number of turns nations have been holding territories
  territories$data$turns_of_possession <- 0
  #_______________________________________________________________________
  
  
  #6) INITIATE OTHER GAME FEATURES ---------------------------------------
  
  #initiate order table
  orders <- data.frame(type=character(0),tile_number=numeric(0),to=numeric(0))
  
  #initiate time
  seasons <- c("spring","summer","autumn","winter (adjustment)")
  turn <- 0

  # make a list of territories with cities
  territories$city_tiles <- territories$data$tile_number[territories$data$city]
  
  #SUPPORT FOR SHARED SCREEN MODE FOR TURN 1
  if((player_mode=="shared screen")&(sum(nations$is_player,na.rm=TRUE)>0)){
    #this takes into account only ownership of the first player that is to input orders
    territories$player_tiles <- territories$data$tile_number[(territories$data$owner %in%  nations$names[nations$is_player][1])]
    territories$punits_tiles <- territories$data$tile_number[territories$data$troops %in% nations$names[nations$is_player][1]]
  } else {
    #make a list of territories that belong to a player (will be update every turn)
    territories$player_tiles <- territories$data$tile_number[(territories$data$owner %in%  nations$names[nations$is_player])]
    
    #make list of territories with player-controlled units (will be updated every turn)
    territories$punits_tiles <- territories$data$tile_number[territories$data$troops %in% nations$names[nations$is_player]]
  } 
  
  
  #create column with number of cities, tiles and troops controlled by each nation in nation table
  nations$tot_tiles<-0
  nations$tot_cities<-0
  nations$tot_troops<-0
  for (i in 1:nrow(nations)){
    nations$tot_tiles[i]<-sum((territories$data$owner==nations$names[i]), na.rm=TRUE)
    nations$tot_cities[i]<-sum((territories$data$owner==nations$names[i])&(territories$data$city==TRUE), na.rm=TRUE)
    nations$tot_troops[i]<-sum(territories$data$troops==nations$names[i], na.rm=TRUE)
  }
  
  #create a column in nation table saying whether ruler is alive
  nations$ruler_alive <- TRUE
  
  #create columns for AI evaluation
  nations$ai_expansionism <- 1.5   #Higher numbers give priority to expansion over defence. Numbers below 1 give priority to defence
  #_______________________________________________________________________
  
  
  
  
  # PACKAGE ALL OBJECTS IN A LIST TO RETURN -------------------------------
  
  final <- list(territories = territories,
                nations = nations,
                orders = orders,
                turn = turn,
                seasons = seasons,
                init_parameters = list(number_of_nations = number_of_nations, number_of_cells = number_of_cells, number_of_cities = number_of_cities, n_control_nations = n_control_nations)
  )
  
  return(final)
  #_______________________________________________________________________
  
  
} #FINE FUNZIONE INITIATE GAME
#_______________________________________________________________________





