options(shiny.maxRequestSize = 9*1024^2)
library(ggplot2)

shinyServer(function(input, output) {
  d=reactive({inFile <- input$file1
  read.csv(inFile$datapath)}) 
  output$contents <- renderTable({
    d()
  })
  
  x=reactive({ 
    as.integer(input$region)
  
  })
 
  output$plot <-renderPlot({
    if (input$select==3){

      df <- d()
      nam.all <- names(df)[-c(1,10:12)]
      inp <- x()
      nam.checkbox <- nam.all[inp[1]]
      df.sub <- df[,c(nam.checkbox,"y")]

     df <- d()
     p <- ggplot(data = df.sub, aes(x = df.sub[,1], y = y)) +
       geom_smooth(method = "lm", se=FALSE, color="blue", size=0.5) +
       geom_point(color="black",size=3) +
       ggtitle("Regression")

     print(p)
    }
    

    if (input$select==2){

      df <- d()

      gwr.bw <- gwr.sel(y~ kerja+pertanian+hotel+ipm+pmdn+pma+wisatawan+penduduk,
                        data=df, coords=cbind(df$latitude,df$longitude))


      model <-  gwr(y ~ kerja+pertanian+hotel+ipm+pmdn+pma+wisatawan+penduduk,data = df,
                    coords=cbind(df$latitude, df$longitude),
                    bandwidth = gwr.bw, hatmatrix=TRUE)

      results<-as.data.frame(model$SDF)
      df$coefkerja <- results$kerja
      df$coefpertanian <- results$pertanian
      df$coefhotel <- results$hotel
      df$coefipm <- results$ipm
      df$coefpmdn  <- results$pmdn
      df$coefpma <- results$pma
      df$coefwisatawan <- results$wisatawan
      df$coefpenduduk <- results$penduduk

      if (1 %in% input$region){
       reg = "kerja"
      }else if (2 %in% input$region){
        reg = "pertanian"
      }else if (3 %in% input$region){
        reg = "hotel"
      }else if (4 %in% input$region){
        reg = "ipm"
      }else if (5 %in% input$region){
        reg = "pmdn"
      }else if (6 %in% input$region){
        reg = "pma"
      }else if (7 %in% input$region){
        reg = "wisatawan"
      }else{
        reg = "penduduk"}
      


      df$coefvar <- results[[reg]]

      gwr.plot <- ggplot(df, aes(x=latitude,y=longitude))+
      geom_point(aes(colour=coefvar))+
      scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                               na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
      print(gwr.plot)
    }
  })
  
  output$text1 <- renderPrint({ 
    
    if (input$select==3){

      df <- d()
      nam.all <- names(df)[-c(1,10:12)]
      inp <- x()
      nam.checkbox <- nam.all[inp]
      nam.checkbox <- c(nam.checkbox,"y")
      df.sub <- df[,nam.checkbox] 
      
     mod <-  summary(lm( y ~ . , data=df.sub  ) )
     print(mod)
    
    print(" SIGNIFICANT VARIABLES FOR ALPHA = 0.05")
    n <- ncol(df.sub)
    
   
      
     (wh <- which( mod$coeff[-1,4] < 0.05))
     (var.nam <- row.names(mod$coeff)[-1] )
     print(var.nam[wh])
    }
    
    if (input$select==1){
    }
    
    if (input$select==2){
      
      
      df <- d()
      nam.all <- names(df)[-c(1,10:12)]
      inp <- x()
      nam.checkbox <- nam.all[inp]
      nam.checkbox <- c(nam.checkbox,"y")
      df.sub <- df[,nam.checkbox] 
    
      gwr.bw <- gwr.sel(y~.,
                        data=df.sub, coords=cbind(d()$latitude,d()$longitude))
      
      model <- gwr(y ~ . ,data = df.sub,
                   coords=cbind(d()$latitude, d()$longitude),
                   bandwidth = gwr.bw, hatmatrix=TRUE)
    print(model)
    
    print("")
    print("BFC02.gwr.test")
    
    
    print(BFC02.gwr.test(model))
    
    print("")
    print("b1/tb1")
    
    print("kerja")
    
    
    b1 <- model$SDF$kerja
    seb1 <- model$SDF$kerja_se
    tb1 <- b1/seb1
    print(tb1)
    
    print("pertanian")
    
    b1 <- model$SDF$pertanian
    seb1 <- model$SDF$pertanian_se
    tb1 <- b1/seb1
    print(tb1)
    
    print("wisatawan")
    
    b1 <- model$SDF$wisatawan
    seb1 <- model$SDF$wisatawan_se
    tb1 <- b1/seb1
    print(tb1)
    
    print("penduduk")
    
    b1 <- model$SDF$penduduk
    seb1 <- model$SDF$penduduk_se
    tb1 <- b1/seb1
    print(tb1)
    
    }
    
    })
})
