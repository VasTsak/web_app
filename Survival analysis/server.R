library(XLConnect)
library(survival)
library(digest)
library(shinyjs)

shinyServer(function(input, output,session) {
  blah=reactive({inFile <- input$file
  readWorksheetFromFile(inFile$datapath, sheet=1)}) 
  output$contents <- renderTable({
    blah()
  })
  x=reactive({as.integer(input$region)})
  y=reactive({as.integer(input$select)})
  output$text1<-renderPrint({head(blah())})
  output$text2<-renderPrint({
    
    for (i in 1:(length(x())-2)){
      print(survdiff(Surv(time,status)~ blah()[,i+2],data=blah()))
    }
    print(survdiff(Surv(time,status)~x2,data=blah()))
    print(survdiff(Surv(time,status)~x2,data=blah()))
    print(survdiff(Surv(time,status)~x3,data=blah()))
    print(survdiff(Surv(time,status)~x4,data=blah()))
    print(survdiff(Surv(time,status)~x5,data=blah()))
    print(survdiff(Surv(time,status)~x6,data=blah()))
    print(survdiff(Surv(time,status)~x7,data=blah()))
  })
  output$text3 <- renderPrint({ 
    df <- blah()
    nam.all <- names(df)[-c(1,2)]
    inp <- x()
    nam.checkbox <- c(nam.all[inp],"time","status")
    df.sub <- df[,nam.checkbox] 
    mod <-  summary(coxph(Surv(time,status)~ . , data=df.sub ) )
    print(mod)
    print(" SIGNIFICANT VARIABLES FOR ALPHA = 0.05")
    
    coeffs <- coef(mod)
    coeffs=as.matrix(coeffs[,5])
    
    (wh <- which( coeffs[1:7] < 0.05))
    (var.nam <- row.names(coeffs))
    print(var.nam[wh])
  })
  
  output$text4 <- renderPrint({ 
    df <- blah()
    nam.all <- names(df)[-c(1,2)]
    inp <- x()
    nam.checkbox <- c(nam.all[inp],"time","status")
    df.sub <- df[,nam.checkbox] 
    mod <-  summary(coxph(Surv(time,status)~ . , data=df.sub ) )
    
    coeffs <- coef(mod)
    coeffs=as.matrix(coeffs[,5])
    
    (wh <- which( coeffs[1:7] < 0.05))
    (var.nam <- row.names(coeffs))
    
    nam.checkbox  <- c(var.nam[wh],"time","status")
    df.sub <- df[,nam.checkbox] 
    model=summary(coxph(Surv(time,status)~.,data=df.sub ))
    print(model)

  })
  output$text5 <- renderPrint({ 
    df <- blah()
    nam.all <- names(df)[-c(1,2)]
    inp <- x()
    nam.checkbox <- c(nam.all[inp],"time","status")
    df.sub <- df[,nam.checkbox] 
    mod <-  summary(coxph(Surv(time,status)~ . , data=df.sub ) )
    
    coeffs <- coef(mod)
    coeffs=as.matrix(coeffs[,5])
    
    (wh <- which( coeffs[1:7] < 0.05))
    (var.nam <- row.names(coeffs))
    
    nam.checkbox  <- c(var.nam[wh],"time","status")
    df.sub <- df[,nam.checkbox] 
    model=coxph(Surv(time,status)~.,data=df.sub )
    assumption=cox.zph(model)
    print(assumption)
    
  })
  output$text6 <- renderPrint({ 
    df <- blah()
    nam.all <- names(df)[-c(1,2)]
    inp <- x()
    nam.checkbox <- c(nam.all[inp],"time","status")
    df.sub <- df[,nam.checkbox] 
    mod <-  summary(coxph(Surv(time,status)~ . , data=df.sub ) )
    
    coeffs <- coef(mod)
    coeffs=as.matrix(coeffs[,5])
    
    (wh <- which( coeffs[1:7] < 0.05))
    (var.nam <- row.names(coeffs))
    
    nam.checkbox  <- c(var.nam[wh],"time","status")
    df.sub <- df[,nam.checkbox] 
    model=coxph(Surv(time,status)~.,data=df.sub )
    print(summary(model))
    
  })
  
  output$text7<-renderPrint({
    df=blah()
    nam.all <- names(df)[-c(1,2)]
    inp <- df[nam.all[x()]]
    ind<- df[nam.all[y()]]
    wh <- which(names(inp) %in% names(ind))
    inp=inp[-wh]
    nam.checkbox <- c(names(inp),"time","status")
    df.sub <- df[,nam.checkbox] 
    modelx <- coxph(Surv(time, status) ~.+ frailty(df[,names(ind)]), data = df.sub)
    summary(modelx)
  })
  
  output$text9<-renderTable({
    
    df <- blah()
    nam.all <- names(df)[-c(1,2)]
    inp <- x()
    nam.checkbox <- c(nam.all[x()],"time","status")
    df.sub <- df[,nam.checkbox] 
    mod <-  summary(coxph(Surv(time,status)~ . , data=df.sub ) )
    
    coeffs <- coef(mod)
    coeffs=as.matrix(coeffs[,5])
    
    (wh <- which( coeffs[1:7] < 0.05))
    (var.nam <- row.names(coeffs))
    ind<-df[,var.nam[wh]]
   
    whin<-1:length(names(ind))
    modelcox<-coxph(Surv(time,status)~ . , data=df)
    rescox = residuals(modelcox)
    kuadratcox = rescox^2
    n = length(rescox)
    k = 5
    sigma =sum(kuadratcox)
    MSEcox = sigma/n
    a = n*log(MSEcox)
    b = 2*k
    AICcox = a+b
    d = n*log(MSEcox)
    e = k*log(n)
    BICcox = d+e
    Model=1:(length(names(ind))+1)
    MSE=1:(length(names(ind))+1)
    AIC=1:(length(names(ind))+1)
    BIC=1:(length(names(ind))+1)
    Model[1]="Model Cox PH"
    MSE[1]=MSEcox
    AIC[1]=AICcox
    BIC[1]=BICcox
    
    for (i in 1:length(names(ind))){
    whin[i]=which(names(df.sub) %in% names(ind)[i])
        df.sub=df.sub[,-whin[i]]
        modelx <- coxph(Surv(time, status) ~.-df[,names(ind)[i]]+ frailty(df[,names(ind)[i]]), data = df.sub)
   
    modelx4<-coxph(Surv(time, status) ~.-x4+ frailty(x4), data = df)
    
    res = residuals(modelx)
    quad = res^2
    n = length(res)
    k = 5
    sigma =sum(quad)
    MSEx = sigma/n
    a = n*log(MSEx)
    b = 2*k
    AICx = a+b
    d = n*log(MSEx)
    e = k*log(n)
    BICx = d+e
    Model[i+1]=paste("Model",names(ind)[i], "sebagai variabel frailty")
    MSE[i+1]=MSEx
    AIC[i+1]=AICx
    BIC[i+1]=BICx
    
    }
    x=data.frame(Model,MSE,AIC,BIC)
    x
    
  })
  })
