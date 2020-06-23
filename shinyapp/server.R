#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
######################################################################### 
   # Partie faite par Raphaël
#########################################################################
  
    # Filtrage en fonction des années/mois/jours sélectionnés
    filteredCountDataByHour <- reactive({
        count(full_taxi %>%
            filter(day == input$dayInput) %>%
              filter(month == input$monthInput) %>%
                filter(year == input$yearInput) %>%
                  group_by(pickupHour))

    })
    
    #ajout des infos journées
    filtereddata <- reactive({
      full_taxi %>%
        filter(day == input$dayInput) %>%
        filter(month == input$monthInput) %>%
        filter(year == input$yearInput) %>%
        select(distance,trip_duration,weekDay,maxPrecip,maxTemp,maxWind)%>% 
        dplyr::summarise(duree_moy = round(mean(trip_duration,na.rm=T),0),
                         distance = round(mean(distance,na.rm=T),2),
                         Jour_semaine = unique(weekDay),
                         pluie = round(mean(maxPrecip,na.rm=T),2),
                         temperature = round(mean(maxTemp,na.rm=T),2),
                         vent = round(mean(maxWind,na.rm=T),2))%>%
        dplyr::rename("Durée moyenne du trajet en secondes" = 1,
                      "Distance en km" = 2,
                      "Jour de la semaine" =3,
                      "Pluie" = 4,
                      "Température" = 5,
                      "Vent" = 6)
    })
    
    output$filtereddataDT <- shiny::renderDataTable({filtereddata()})
    

    # Histo nb courses / heure pour la journée sélectionnée par l'utilisateur 
    output$myPlot <- renderPlotly({
      plot <- plot_ly()
      axe_x <- list(title = "Hour")
      axe_y <- list(title = "Nb traj")
      plot <- layout(plot, title="Nombre de trajets en fonction du jour et de l'heure", xaxis = axe_x, yaxis = axe_y)
      plot <- add_bars(plot, x=filteredCountDataByHour()$pickupHour, y=filteredCountDataByHour()$n)

    })

  # # TODO : à voir par la suite quand on aura intégré les zipcode départ/arrivée dans le jeu de données
  #   output$myMap <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles(providers$Stamen.TonerLite,
  #                      options = providerTileOptions(noWrap = TRUE)
  #     ) %>%
  #     addPolygons(data=nycShapeFile,
  #                 fill=TRUE,
  #                 stroke=TRUE,
  #                 opacity=1,
  #                 color="black",
  #                 weight=1)
  # })
    
    ###############################################################
    #Partie pour les stats desc vraies
    ###############################################################
    
    #Description durée - histogramme
    output$hist_duration <- renderPlotly({
      p <- tds %>%
        ggplot()+
        geom_histogram(aes(x=trip_duration))+
        xlim(c(1,4000))+
        ggtitle("Nombre de trajets en fonction de leur durée") +
        xlab("Durée du trajet (en secondes)") + 
        ylab("Nombre de trajets")
      
      p <- ggplotly(p)
    })
    
    #Description durée - boxplot
    output$bp_duration <- renderPlotly({
      p <- tds %>%
        ggplot(aes(y=trip_duration))+
        geom_boxplot()+
        ylim(c(0,1750)) +
        scale_x_discrete(labels=NULL,breaks=NULL)+
        ggtitle("Boîte à moustaches - durée des trajets")+
        ylab("Durée du trajet (en secondes)")
      
      p <- ggplotly(p)
    })
    
    #Description Distance - histogramme
    output$hist_dist <- renderPlotly({
      p <- tds %>%
        ggplot()+
        geom_histogram(aes(x=distance))+
        ggtitle("Nombre de trajets en fonction de la distance parcourue") +
        xlab("Distance du trajet (en km)") + 
        ylab("Nombre de trajets")
      
      p <- ggplotly(p)
    })
    
    #Description distance - boxplot
    output$bp_dist <- renderPlotly({
      p <- tds %>%
        ggplot(aes(y=distance))+
        geom_boxplot()+
        scale_x_discrete(labels=NULL,breaks=NULL)+
        scale_y_continuous(limits = c(0,10))+
        ggtitle("Boîte à moustaches - distance des trajets")+
        ylab("Distance du trajet (en km)")
      
      p <- ggplotly(p)
    })
    
    # Lien distance / durée
    output$link_dist_dur <- renderPlotly({
      ##Warning too much data points to be plotted => Need to take a subset
      set.seed(1234)
      subset <- tds[sample(nrow(tds),5000),]
      
      p <- ggplot(subset)+
        aes(x=distance,y=trip_duration)+
        geom_point(color="yellow")+
        geom_smooth(method=lm, se=FALSE,color="black")+
        ggtitle("Lien entre la distance et la durée des trajets")+
        ylab("Durée du trajet (en secondes)")+
        xlab("Distance du trajet (en km)")
      p <- ggplotly(p)
    })
    
    # Description heures de départ
    output$hours_dur <- renderPlotly({
      p <- tds %>%
        dplyr::mutate(pickupHour=as.factor(pickupHour))%>%
        ggplot()+
        aes(x=pickupHour,y=trip_duration)+
        geom_boxplot(outlier.shape = NA)+
        ylim(0,2000)+
        ggtitle("Temps de trajet selon l'heure de départ")+
        ylab("Durée du trajet (en secondes)")+
        xlab("Heure de départ du trajet")
      
      p <- ggplotly(p)
      # p <- plotly_build(p)
      # for(i in 1:length(p$x$data)) {
      #   p$x$data[[i]]$marker$opacity = 0
      # }
    })

    # Description heures de départ
    output$hours_dist <- renderPlotly({
      p <- tds %>%
        dplyr::mutate(pickupHour=as.factor(pickupHour))%>%
        ggplot()+
        aes(x=pickupHour,y=distance)+
        geom_boxplot(outlier.size = NULL,outlier.shape = NA)+
        ylim(0,15)+
        ggtitle("Distance du trajet selon l'heure de départ")+
        ylab("Distance du trajet (en km)")+
        xlab("Heure de départ du trajet")
      
      p <- ggplotly(p)
    })
    
    # Influence heures coeff reg
    output$hours_reg <- renderPlotly({
      #On prend l'échantillon à 5k indiv
      set.seed(1234)
      subset <- tds[sample(nrow(tds),5000),]
      #ggplot + ggplotly
      p <- ggplot(subset) +
        aes(y=trip_duration, x=distance, col=pickupHour) +
        geom_point() +
        geom_smooth(method=lm, se=FALSE) +
        ggtitle("Impact de l'heure sur le coefficient de la régression linéaire") +
        ylab("Distance du trajet (en km)")+
        xlab("Heure de départ du trajet")+
        labs(col = "Heure de départ")+
        theme(legend.position="bottom")
      
      
      p <- ggplotly(p)
    })
    
    # description weekdays durée
    output$wd_dur <- renderPlot({
      boxplot(trip_duration~weekDay,
              data=tds,
              main="Temps de trajet selon le jour de la semaine",
              outline=FALSE)
    })
    
    # description weekdays distance
    output$wd_dist <- renderPlotly({
      # boxplot(distance~weekDay,
      #         data=tds,
      #         main="Durée du trajet selon le jour de la semaine",
      #         outline=FALSE)
      plot_ly(tds, y = ~distance, color = ~weekDay, type = "box", boxpoints = F)%>%
        layout(title = "Durée du trajet selon le jour de la semaine",
               xaxis = list(title = "Distance du trajet (en km)"),
               yaxis = list(title = "Jour de la semaine"))
    })
    
    # Influence weekdays coeff reg
    output$wd_reg <- renderPlotly({
      #On prend l'échantillon à 5k indiv
      set.seed(1234)
      subset <- tds[sample(nrow(tds),5000),]
      #ggplot + ggplotly
      p <- ggplot(subset)+
        aes(y=trip_duration, x=distance, col=weekDay) +
        geom_point() + 
        geom_smooth(method=lm, se=FALSE)+
        ggtitle("Impact du jour de la semaine sur coefficient de la régression linéaire")
      
      p <- ggplotly(p)
    })
    
    # On crée la carte avec les polygones
    output$map_dens <- renderLeaflet({
      #on définit les coupures (quantiles) et la palette de couleurs 
      #(impossible de faire marcher leaflet sans palette prédéfinie)
      bins <- c(0, 13, 75, 1411, 7097, 50000, Inf)
      pal <- colorBin("YlOrRd", domain = data_map1$nb_trips, bins = bins)
      
      #On définit ce que s'affiche sur la carte
      labels <- sprintf(
        "<strong>%s</strong><br/>%g trajets effectués",
        data_map1_bis$zcta, data_map1_bis$nb_trips
      ) %>% lapply(htmltools::HTML)
      
      m <- leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addPolygons(data=data_map1_bis,
                    fillColor = ~pal(nb_trips),
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        addLegend(pal = pal, values = data_map1$nb_trips, opacity = 0.7, title = NULL,
                   position = "bottomright")
    })
    
    # On fait une prévision du modèle
    data_prediction <- reactive({
      data.frame("distance"= input$distance_input,
                 "month" = as.character(input$month_input),
                 "weekDay" = as.character(input$wday_input),
                 "pickupHour" = as.character(input$hour_input),
                 "pickupZcta" = as.character(input$zcta_input))
    })
    
    model_pred <- reactive({
      predict(rf_taxi, data_prediction())
    })

    output$prediction <- renderPrint(model_pred())
})
