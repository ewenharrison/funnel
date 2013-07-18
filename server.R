library(shiny)
library(ggplot2)
library(binom)
 
shinyServer(function(input, output) {
# Define confidence limits for given population mortality and put in dataframe
  limits <- reactive(function() {
    binom_n<-seq(5, 110, length.out=40)
    ci.90<-binom.confint(input$pop.mort*binom_n, binom_n, conf.level = 0.90, methods = "wilson")
    ci.95<-binom.confint(input$pop.mort*binom_n, binom_n, conf.level = 0.95, methods = "wilson")
    ci.99<-binom.confint(input$pop.mort*binom_n, binom_n, conf.level = 0.99, methods = "wilson")
    df<-data.frame(n=ci.90$n, pop.mort=ci.90$mean, ci.90l=ci.90$lower, ci.90u=ci.90$upper, ci.95l=ci.95$lower, ci.95u=ci.95$upper, ci.99l=ci.99$lower, ci.99u=ci.99$upper) 
    return(df)
  })
  
  # points on graph for simulations. Done like this so can make reactive if necessary. 
  n0<-10
  n1<-25
  n2<-50
  n3<-75
  n4<-100
  
  # Simulations
  sim <- reactive(function() {
    y0<-rbinom(input$n_sim, n0, input$surg.mort)/n0
    y1<-rbinom(input$n_sim, n1, input$surg.mort)/n1
    y2<-rbinom(input$n_sim, n2, input$surg.mort)/n2
    y3<-rbinom(input$n_sim, n3, input$surg.mort)/n3
    y4<-rbinom(input$n_sim, n4, input$surg.mort)/n4	
    df<-data.frame(n0, y0, n1, y1, n2, y2, n3, y3, n4, y4, surg.mort=input$surg.mort)
    return(df)
  })
  
  # Proportions based on standard distributional theory
  # How many surgeons above 95% CI?
  table <- reactive(function() {
    b0<-binom.confint(input$pop.mort*n0, n0, conf.level = 0.95, methods = "wilson"); p0<-(1-pbinom(b0[[6]]*n0, n0, input$surg.mort))
    b1<-binom.confint(input$pop.mort*n1, n1, conf.level = 0.95, methods = "wilson"); p1<-(1-pbinom(b1[[6]]*n1, n1, input$surg.mort))
    b2<-binom.confint(input$pop.mort*n2, n2, conf.level = 0.95, methods = "wilson"); p2<-(1-pbinom(b2[[6]]*n2, n2, input$surg.mort))
    b3<-binom.confint(input$pop.mort*n3, n3, conf.level = 0.95, methods = "wilson"); p3<-(1-pbinom(b3[[6]]*n3, n3, input$surg.mort))
    b4<-binom.confint(input$pop.mort*n4, n4, conf.level = 0.95, methods = "wilson"); p4<-(1-pbinom(b4[[6]]*n4, n4, input$surg.mort))
    b5<-binom.confint(input$pop.mort*n0, n0, conf.level = 0.99, methods = "wilson"); p5<-(1-pbinom(b5[[6]]*n0, n0, input$surg.mort))
    b6<-binom.confint(input$pop.mort*n1, n1, conf.level = 0.99, methods = "wilson"); p6<-(1-pbinom(b6[[6]]*n1, n1, input$surg.mort))
    b7<-binom.confint(input$pop.mort*n2, n2, conf.level = 0.99, methods = "wilson"); p7<-(1-pbinom(b7[[6]]*n2, n2, input$surg.mort))
    b8<-binom.confint(input$pop.mort*n3, n3, conf.level = 0.99, methods = "wilson"); p8<-(1-pbinom(b8[[6]]*n3, n3, input$surg.mort))
    b9<-binom.confint(input$pop.mort*n4, n4, conf.level = 0.99, methods = "wilson"); p9<-(1-pbinom(b9[[6]]*n4, n4, input$surg.mort))
    m<-matrix(c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9), byrow=TRUE, nrow=2, ncol=5)
	rownames(m) <- c("Proportion of surgeons above 95% CI", ("Proportion of surgeons above 99% CI"))
	colnames(m) <- c(n0, n1, n2, n3, n4)
    return(m)
  })
  
  # Plot
  output$plot <- renderPlot(function(){
    g1<-ggplot()+
      geom_line(data=limits(), aes(n, ci.95l), colour = "blue")+ 
      geom_line(data=limits(), aes(n, ci.95u), colour = "blue")+
      geom_line(data=limits(), aes(n, ci.99l), colour = "red")+ 
      geom_line(data=limits(), aes(n, ci.99u), colour = "red")+
	  geom_line(data=limits(), aes(n, pop.mort), colour="black")+
	  
	  geom_jitter(data=sim(), aes(x=n0, y=y0), alpha=0.3, colour="black", position = position_jitter(width = 2, height=0.003))+
      geom_point(data=sim(), aes(x=n0, y=surg.mort), colour="black", size=6, alpha=0.7)+	  
	  geom_jitter(data=sim(), aes(x=n1, y=y1), alpha=0.3, colour="darkred", position = position_jitter(width = 2, height=0.003))+
      geom_point(data=sim(), aes(x=n1, y=surg.mort), colour="darkred", size=6, alpha=0.7)+
	  geom_jitter(data=sim(), aes(x=n2, y=y2), alpha=0.3, colour="darkblue", position = position_jitter(width = 2, height=0.003))+
      geom_point(data=sim(), aes(x=n2, y=surg.mort), colour="darkblue", size=6, alpha=0.7)+
	  geom_jitter(data=sim(), aes(x=n3, y=y3), alpha=0.3, colour="darkgreen", position = position_jitter(width = 2, height=0.003))+
      geom_point(data=sim(), aes(x=n3, y=surg.mort), colour="darkgreen", size=6, alpha=0.7)+
	  geom_jitter(data=sim(), aes(x=n4, y=y4), alpha=0.3, colour="darkorange", position = position_jitter(width = 2, height=0.003))+
      geom_point(data=sim(), aes(x=n4, y=surg.mort), colour="darkorange", size=6, alpha=0.7)+	  
	  scale_x_continuous(name="Procedure number", limits=c(0, 110), breaks=c(10, 25, 50, 75, 100))+
      scale_y_continuous(name="Mortality rate")+
	  theme_bw()+
      theme(text=element_text(size=20))	
	print(g1)
  })
  
  # Table
  output$view <- renderTable(table())
})