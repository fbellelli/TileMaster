plot_map <- function(territories_data, nations_data=NULL, orders_data = NULL, player_to_play=1, core_highlight=FALSE){
  

  #plot base ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  plot <- ggplot(territories_data$data,aes(x,y,key=tile_number)) +
    geom_voronoi(aes(x,y, 
                     fill=ifelse(owner=="no owner",NA,owner)),
                 alpha=0.5,
                 colour="black", size = 0.1) +
    labs(fill = "Nations:") +
    guides(alpha=FALSE) +
    theme_minimal() +                 
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  #colour assignment
  if (is.null(nations_data)){
    #choose set of colors based on number of nations
    number_of_nations <- sum(unique(territories_data)!= "no owner")
    if (number_of_nations>9){
      plot <- plot + scale_fill_brewer(palette="Paired")}
    if (number_of_nations<=9){
      plot <- plot + scale_fill_brewer(palette="Set1")}
  } else {
    #apply colours from nations table
    colori <- nations_data$colours
    names(colori) <- nations_data$names
    plot <- plot + scale_fill_manual(values=colori, na.value="white")
  }

  #add cities (and highlight cores)
  if (core_highlight==TRUE){
    plot <- plot +  geom_point(aes(x,y, colour=ifelse((core!="")&(core==owner),"Core","Not core")), size=10,shape=12,
                               data = territories_data$data[territories_data$data$city==TRUE,])+scale_colour_manual(values=c("black","gray80"), guide=FALSE)
  } else {
    plot <- plot +  geom_point(aes(x,y), size=10,shape=12,
                               data = territories_data$data[territories_data$data$city==TRUE,])
    
  }

  #plot additional layer with orders~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(orders_data)){
    if (nrow(orders_data)>0){
      
      #get coordinates of tiles involved
      for (i in 1:nrow(orders_data)){
        orders_data$x[i] <- territories_data$data$x[territories_data$data$tile_number==orders_data$tile_number[i]]
        orders_data$y[i] <- territories_data$data$y[territories_data$data$tile_number==orders_data$tile_number[i]]
        orders_data$x_to[i] <- ifelse(is.na(orders_data$to[i]),NA,
                                      territories_data$data$x[territories_data$data$tile_number==orders_data$to[i]])
        orders_data$y_to[i] <- ifelse(is.na(orders_data$to[i]),NA,
                                      territories_data$data$y[territories_data$data$tile_number==orders_data$to[i]])
        # orders_data$x_mid[i] <- ifelse(is.na(orders_data$from[i]),NA,
        #                                (orders_data$x_from[i] + orders_data$x_to[i])/2)
        # orders_data$y_mid[i] <- ifelse(is.na(orders_data$from[i]),NA,
        #                                (orders_data$y_from[i] + orders_data$y_to[i])/2)
      }
      
      
      #PLOT ORDERS (for results & input turns)
      if ("success" %in% colnames(orders_data)){ # RESULTS TURNS
        
        #SUCCESFUL ORDERS ---
        #add move segment (arrow of move and unstack added as annotation)
        if (nrow(orders_data[(orders_data$type=="move")&(orders_data$success==TRUE),])>0){
          plot <- plot + geom_segment(aes(x=x, y=y, xend=x_to, yend=y_to),
                                      data = orders_data[(orders_data$type=="move")&(orders_data$success==TRUE),],
                                      size = 2)
        }
        
        #add retreat segment
        if (nrow(orders_data[(orders_data$type=="retreat")&(orders_data$success==TRUE),])>0){
          plot <- plot + geom_segment(aes(x=x, y=y, xend=x_to, yend=y_to),
                                      data = orders_data[(orders_data$type=="retreat")&(orders_data$success==TRUE),],
                                      size = 1.5, colour= "green", linetype="dotted")
        }
        
        #add recruit orders
        if (nrow(orders_data[(orders_data$type=="recruit")&(orders_data$success==TRUE),])>0){
          plot <- plot + geom_point(aes(x,y), size=12, shape = 8,
                                    data = orders_data[(orders_data$type=="recruit")&(orders_data$success==TRUE),])
        }
        
        
        #add disband orders
        if (nrow(orders_data[(orders_data$type=="disband")&(orders_data$success==TRUE),])>0){
          plot <- plot + geom_point(aes(x,y), size=11, shape = 4, stroke=2,
                                    data = orders_data[(orders_data$type=="disband")&(orders_data$success==TRUE),])
        }
        
        #UNSUCCESSFUL ORDERS ---
        #add move segment (arrow of move and unstack added as annotation)
        if (nrow(orders_data[(orders_data$type=="move")&(orders_data$success==FALSE),])>0){
          plot <- plot + geom_segment(aes(x=x, y=y, xend=x_to, yend=y_to), colour="red",
                                      data = orders_data[(orders_data$type=="move")&(orders_data$success==FALSE),],
                                      size = 2)
        }
        
        #add recruit orders
        if (nrow(orders_data[(orders_data$type=="recruit")&(orders_data$success==FALSE),])>0){
          plot <- plot + geom_point(aes(x,y), size=12, shape = 8, colour="red",
                                    data = orders_data[(orders_data$type=="recruit")&(orders_data$success==FALSE),])
        }
        
        
        #add disband orders
        if (nrow(orders_data[(orders_data$type=="disband")&(orders_data$success==FALSE),])>0){
          plot <- plot + geom_point(aes(x,y), size=11, shape = 4, stroke=2,colour="red",
                                    data = orders_data[(orders_data$type=="disband")&(orders_data$success==FALSE),])
        }
        
        
        
      } else { # INPUT ORDER TURNS
        #add move segment (arrow of move and unstack added as annotation)
        if (nrow(orders_data[(orders_data$type=="move"),])>0){
          plot <- plot + geom_segment(aes(x=x, y=y, xend=x_to, yend=y_to),
                                      data = orders_data[(orders_data$type=="move"),],
                                      size = 2)
        }
      
        #add recruit orders
        if (nrow(orders_data[orders_data$type=="recruit",])>0){
          plot <- plot + geom_point(aes(x,y), size=12, shape = 8,
                                    data = orders_data[orders_data$type=="recruit",])
        }
      
      
        #add disband orders
        if (nrow(orders_data[orders_data$type=="disband",])>0){
          plot <- plot + geom_point(aes(x,y), size=11, shape = 4, stroke=2,
                                    data = orders_data[orders_data$type=="disband",])
        }
        
        
      }
    }
  }  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #plot additional layer with troops 
  plot <- plot+geom_point(aes(x,y, fill = troops, stroke=size_display_troops), size=7, shape = 16,
                          data = territories_data$data[!is.na(territories_data$data$troops),]) + scale_size_continuous(range=c(5.5,7))    
  #Mark retreating troops
  if (("success" %in% colnames(orders_data))&(sum(orders_data$type=="retreat",na.rm=TRUE)>0)){
    plot <- plot + geom_point(aes(x,y), size=7, shape = 4, stroke=1.5, colour="red",
                              data = orders_data[orders_data$type=="retreat",])
  }
  
  #plot additional layer with rulers
  if (player_to_play==1){
  plot <- plot+geom_point(aes(x,y), size=3, shape = 17, colour="white",
                          data = territories_data$data[territories_data$data$ruler,])
  } else {
  plot <- plot+geom_point(aes(x,y), size=3, shape = 17, colour="white",
                            data = territories_data$data[(territories_data$data$ruler)&(territories_data$data$owner==nations_data$names[nations_data$is_player][player_to_play]),])
  }
  #Mark captured rulers
  if (("success" %in% colnames(orders_data))&(sum(orders_data$type=="ruler captured", na.rm=TRUE)>0)){
    plot <- plot + geom_point(aes(x,y), size=3, shape = 24, colour = "black",fill="red",
                              data = orders_data[orders_data$type=="ruler captured",])
  }
  
  
  #add a masking layer to give indications on territories
  plot <- plot + geom_voronoi(aes(x,y,group = 1,
                                  text = paste("<b>Nation:</b>",owner,
                                               "<br><b>Territory name:</b>", name,"(",tile_number,")",
                                               "<br><b>Core of:</b>", core,
                                               "<br><b>City:</b>", city,
                                               "<br><b>Surface:</b>", round(area),
                                               "<br><b>Troops:</b>",n_troops,
                                               "<br><b>Troop owner:</b>",troops)),
                              alpha = 0)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #display plot interactively and add arrows for move orders (ggplotly does not support arrows of geom_segment)
  if (!is.null(orders_data)){
    if (nrow(orders_data)>0){
      #ARROW ORDERS (for results & input turns)
      if ("success" %in% colnames(orders_data)){ # RESULTS TURNS
        #1||||||||||||||||||||||||||||||||||||||||||
        
        final_plot <- ggplotly(plot, tooltip = "text", source = "M")
        
        #successful move
        if(nrow(orders_data[(orders_data$type=="move")&(orders_data$success==TRUE),])>0){
          final_plot <- final_plot %>% add_annotations(text="",xref = "x", axref = "x",
                                                       yref = "y", ayref = "y",
                                                       showarrow = TRUE,
                                                       arrowsize = 2.2,
                                                       startstandoff = 15,
                                                       standoff= 15,
                                                       arrowcolor="black",
                                                       x=orders_data[(orders_data$type=="move")&(orders_data$success==TRUE),"x_to"], y=orders_data[(orders_data$type=="move")&(orders_data$success==TRUE),"y_to"], 
                                                       ax=orders_data[(orders_data$type=="move")&(orders_data$success==TRUE),"x"], ay=orders_data[(orders_data$type=="move")&(orders_data$success==TRUE),"y"])
        }
        #unsuccesful move
        if(nrow(orders_data[(orders_data$type=="move")&(orders_data$success==FALSE),])>0){
          final_plot <- final_plot %>% add_annotations(text="",xref = "x", axref = "x",
                                                       yref = "y", ayref = "y",
                                                       showarrow = TRUE,
                                                       arrowsize = 2.2,
                                                       startstandoff = 15,
                                                       standoff= 15,
                                                       arrowcolor="red",
                                                       x=orders_data[(orders_data$type=="move")&(orders_data$success==FALSE),"x_to"], y=orders_data[(orders_data$type=="move")&(orders_data$success==FALSE),"y_to"], 
                                                       ax=orders_data[(orders_data$type=="move")&(orders_data$success==FALSE),"x"], ay=orders_data[(orders_data$type=="move")&(orders_data$success==FALSE),"y"])
        }
        #successful unstack
        if(nrow(orders_data[(orders_data$type=="unstack")&(orders_data$success==TRUE),])>0){
          final_plot <- final_plot %>% add_annotations(text="",xref = "x", axref = "x",
                                                       yref = "y", ayref = "y",
                                                       showarrow = TRUE,
                                                       arrowsize = 1.6,
                                                       startstandoff = 15,
                                                       standoff= 15,
                                                       arrowcolor="gray27",
                                                       x=orders_data[(orders_data$type=="unstack")&(orders_data$success==TRUE),"x_to"], y=orders_data[(orders_data$type=="unstack")&(orders_data$success==TRUE),"y_to"], 
                                                       ax=orders_data[(orders_data$type=="unstack")&(orders_data$success==TRUE),"x"], ay=orders_data[(orders_data$type=="unstack")&(orders_data$success==TRUE),"y"])
        }
        #unsuccessful unstack
        if(nrow(orders_data[(orders_data$type=="unstack")&(orders_data$success==FALSE),])>0){
          final_plot <- final_plot %>% add_annotations(text="",xref = "x", axref = "x",
                                                       yref = "y", ayref = "y",
                                                       showarrow = TRUE,
                                                       arrowsize = 1.6,
                                                       startstandoff = 15,
                                                       standoff= 15,
                                                       arrowcolor="red",
                                                       x=orders_data[(orders_data$type=="unstack")&(orders_data$success==FALSE),"x_to"], y=orders_data[(orders_data$type=="unstack")&(orders_data$success==FALSE),"y_to"], 
                                                       ax=orders_data[(orders_data$type=="unstack")&(orders_data$success==FALSE),"x"], ay=orders_data[(orders_data$type=="unstack")&(orders_data$success==FALSE),"y"])
        }
        
        #2||||||||||||||||||||||||||||||||||||||||||
        
        
        
        
        

      } else { #INPUT TURNS ---
          
        #3||||||||||||||||||||||||||||||||||||||||||
        final_plot <- ggplotly(plot, tooltip = "text", source = "M")
        
        if(nrow(orders_data[(orders_data$type=="move"),])>0){
        final_plot <- final_plot %>% add_annotations(text="",xref = "x", axref = "x",
                                                     yref = "y", ayref = "y",
                                                     showarrow = TRUE,
                                                     arrowsize = 2.2,
                                                     startstandoff = 15,
                                                     standoff= 15,
                                                     arrowcolor="black",
                                                     x=orders_data[(orders_data$type=="move"),"x_to"], y=orders_data[(orders_data$type=="move"),"y_to"], 
                                                     ax=orders_data[(orders_data$type=="move"),"x"], ay=orders_data[(orders_data$type=="move"),"y"])
        }
        if(nrow(orders_data[(orders_data$type=="unstack"),])>0){
          final_plot <- final_plot %>% add_annotations(text="",xref = "x", axref = "x",
                                                       yref = "y", ayref = "y",
                                                       showarrow = TRUE,
                                                       arrowsize = 1.6,
                                                       startstandoff = 15,
                                                       standoff= 15,
                                                       arrowcolor="gray27",
                                                       x=orders_data[(orders_data$type=="unstack"),"x_to"], y=orders_data[(orders_data$type=="unstack"),"y_to"], 
                                                       ax=orders_data[(orders_data$type=="unstack"),"x"], ay=orders_data[(orders_data$type=="unstack"),"y"])
        }
        #4||||||||||||||||||||||||||||||||||||||||||
      
      }
    }else {
      final_plot <- ggplotly(plot, tooltip = "text", source = "M")
    }
  } else {
    final_plot <- ggplotly(plot, tooltip = "text", source = "M")
  }
  
  
  #correct legend 
  if (core_highlight==TRUE){
    for (i in 1:length(final_plot$x$data)){
      if (!is.null(final_plot$x$data[[i]]$name)){
        final_plot$x$data[[i]]$name =  gsub("^\\(","",str_split(final_plot$x$data[[i]]$name,",")[[1]][1])
      }
    }
  } 
  
  #RETURN FINAL PLOT  
  return(final_plot)
}  





