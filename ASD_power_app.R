# ASD Power Visualization Shiny App
# Description: This Shiny application visualizes the change in statistical power under different parameter settings for adaptive signature designs.
# Author: Yuan Li
# Date: 2024-05-10
# Usage: Run this script in an R environment with the required packages installed.

library(shiny)
library(shinythemes)
library(ggplot2)
library(gt)

####################################
# User Interface                   #
####################################
ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage("Power for ASD Design",
             ############################# page 1 :overall test #############################3
             tabPanel("Power for Overall Subjects",
                      # Input values  
                      sidebarPanel(
                        HTML("<h3>Input parameters</h3>"),
                        sliderInput("nsz1", 
                                    label = "Total Sample Size", 
                                    value = 300, 
                                    min = 60, 
                                    max = 1000),
                        sliderInput("nsz_p1", 
                                    label = "Allocation Proportion of Treatment E in Overall Subjects", 
                                    value = 0.5, 
                                    min = 0.01, 
                                    max = 0.99),
                        sliderInput("alpha1", 
                                    label = "Significance Level in Overall Test", 
                                    value = 0.04, 
                                    min = 0.00, 
                                    max = 0.10),
                        sliderInput("Pe1", 
                                    label = "Response Probability of Treatment E in Overall Subjects", 
                                    value = 0.6, 
                                    min = 0.01, 
                                    max = 1),
                        sliderInput("Pc1", 
                                    label = "Response Probability of Treatment C in Overall Subjects", 
                                    value = 0.4, 
                                    min = 0.01, 
                                    max = 0.99),
                        
                        actionButton("calculatebutton1", 
                                     "Calculate Power", 
                                     class = "btn btn-primary")
                      ),
                      
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot", plotOutput("plot1")),
                                    tabPanel("Power Calculation", 
                                             verbatimTextOutput('contents1'),
                                             tableOutput("Power1"))
                        ) #tabsetpanel
                      ) #mainPanel                
             ), #tabPanel(), Home
             
             
             ########################## page 2 :sensitive test #########################################
             tabPanel("Power for Sensitive Subjects",
                      # Input values  
                      sidebarPanel(
                        HTML("<h3>Input parameters</h3>"),
                        sliderInput("nsz2", 
                                    label = "Sample Size of Sensitive Subjects", 
                                    value = 20, 
                                    min = 0, 
                                    max = 500),
                        sliderInput("nsz_p2", 
                                    label = "Allocation Proportion of Treatment E in Sensitive Subjects", 
                                    value = 0.5, 
                                    min = 0.01, 
                                    max = 0.99),
                        sliderInput("alpha2", 
                                    label = "Significance Level in Overall Test", 
                                    value = 0.04, 
                                    min = 0.00, 
                                    max = 0.10),
                        sliderInput("Pe2", 
                                    label = "Response Probability of Treatment E in Sensitive Subjects", 
                                    value = 0.6, 
                                    min = 0.01, 
                                    max = 1),
                        sliderInput("Pc2", 
                                    label = "Response Probability of Treatment C in Sensitive Subjects", 
                                    value = 0.4, 
                                    min = 0.01, 
                                    max = 0.99),
                        
                        actionButton("calculatebutton2", 
                                     "Calculate Power", 
                                     class = "btn btn-primary")
                      ),
                      
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot", 
                                             verbatimTextOutput('note2'),
                                             plotOutput("plot2")),
                                    tabPanel("Power Calculation", 
                                             verbatimTextOutput('contents2'),
                                             tableOutput("Power2"))           
                        ) #tabsetpanel
                      ) #mainPanel                
             ), #tabPanel(), Home
             
             
             ########################## page 3 :entire power #########################################
             tabPanel("Weighted Power for Entire Test",
                      # Input values  
                      sidebarPanel(
                        #HTML("<h4>Input parameters</h4>"),
                        sliderInput("nsz3", 
                                    label = "Total Sample Size", 
                                    value = 300, 
                                    min = 0, 
                                    max = 1000),
                        
                        sliderInput("nsz_sens3", 
                                    label = "Sample Size for Sensitive Subjects", 
                                    value = 60, 
                                    min = 0, 
                                    max = 500),
                        
                        sliderInput("nsz_p3", 
                                    label = "Allocation Proportion of Treatment E", 
                                    value = 0.5, 
                                    min = 0.01, 
                                    max = 0.99),
                        
                        sliderInput("alpha3", 
                                    label = "Significance Level in Overall Test", 
                                    value = 0.04, 
                                    min = 0.00, 
                                    max = 0.10),
                        
                        sliderInput("alpha_sens3", 
                                    label = "Significance Level in Sensitive Test", 
                                    value = 0.01, 
                                    min = 0.00, 
                                    max = 0.10),
                        
                        sliderInput("Pe_sens3", 
                                    label = "Response Probability of Treatment E (Sensitive)", 
                                    value = 0.6, 
                                    min = 0.01, 
                                    max = 1),
                        sliderInput("Pc_sens3", 
                                    label = "Response Probability of Treatment C (Sensitive)", 
                                    value = 0.4, 
                                    min = 0.01, 
                                    max = 0.99),
                        
                        sliderInput("Pe_nonsens3", 
                                    label = "Response Probability of Treatment E (Non-Sensitive)", 
                                    value = 0.6, 
                                    min = 0.01, 
                                    max = 1),
                        sliderInput("Pc_nonsens3", 
                                    label = "Response Probability of Treatment C (Non-Sensitive)", 
                                    value = 0.4, 
                                    min = 0.01, 
                                    max = 0.99),
                        
                        actionButton("calculatebutton3", 
                                     "Calculate Power", 
                                     class = "btn btn-primary")),
                      
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot", 
                                             verbatimTextOutput('note3'),
                                             plotOutput("plot3_sens"),
                                             plotOutput("plot3_wgt")),
                                    tabPanel("Power Calculation", 
                                             verbatimTextOutput('contents3'),
                                             tableOutput("Power3"))           
                        ) #tabsetpanel
                      ) #mainPanel                
             ) #tabPanel(), Home
             
  ) # navbarPage()
) # fluidPage()


#####################################################################
############################  Server  ###############################
#####################################################################
server <- function(input, output) {
  
  # function
  power_cal <- function(nsz, nsz_p, alpha, Pe, Pc){
    nsz_E <- nsz* nsz_p
    nsz_C <-  nsz*(1- nsz_p)
    Z.crt <- qnorm(p=alpha/2, lower.tail=FALSE)
    theta <- Pe - Pc
    Pbar <- (nsz_E*Pe + nsz_C*Pc)/(nsz_E + nsz_C)
    num <- Z.crt * sqrt(Pbar*(1-Pbar)*(1/nsz_E+1/nsz_C)) - abs(theta)
    denom <- sqrt((Pe * (1-Pe)/nsz_E) + (Pc * (1-Pc)/nsz_C))
    power <- 1-pnorm(num/denom)
    return(power)
  }
  
  datasetInput1 <- reactive({  
    power <- power_cal(nsz =input$nsz1, nsz_p = input$nsz_p1, alpha = input$alpha1, Pe = input$Pe1 , Pc = input$Pc1)
    power <- round(power,3)
    
    print_nsz <- paste('Total sample size: ',input$nsz1)
    print_nsz_p <- paste('Treatment E allocation probability: ', input$nsz_p1)
    print_alpha <- paste('Significance level in the overall test: ', input$alpha1)
    print_rrE <- paste('Response rate of Treatment E: ', input$Pe1)
    print_rrC <- paste('Response rate of Treatment C: ', input$Pc1 )
    print_power <- c(power,'','','','')
    df <-  as.data.frame(cbind(rbind(print_nsz,print_nsz_p,print_alpha, print_rrE, print_rrC),print_power))
    colnames(df) <- c('Parameters Setting','Theoretical Power')
    print(df)
  })
  
  output$contents1 <- renderPrint({
    if (input$calculatebutton1>0) { 
      isolate("The power is calculated") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$Power1 <- renderTable({
    if (input$calculatebutton1>0) { 
      isolate(datasetInput1()) 
    }
  })
  
  output$plot1 <- renderPlot({
    power_theory <- c()
    for (i in seq(60, 1000, by=20)){
      power0 <- power_cal(nsz =i, nsz_p = input$nsz_p1, alpha = input$alpha1, Pe = input$Pe1 , Pc = input$Pc1)
      power_theory <- c(power_theory,power0)
    }
    
    df_powerN<- data.frame(N = seq(60, 1000, by=20), Power =power_theory)
    df_powerN$N <- as.factor(df_powerN$N)
    powerN <- ggplot(df_powerN, aes(x=N, y=Power,group=1)) +
      geom_line() +
      geom_point()+
      ggtitle("Power vs. Total sample size")+
      ylab("Power") +
      xlab("Total sample size") +
      ylim(0,1.0)+
      scale_x_discrete(breaks=seq(60, 1000, by=100))+
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill='transparent'),
            panel.border=element_blank(), 
            axis.line = element_line(), 
            axis.ticks = element_line(colour='black'),
            plot.title = element_text(hjust = 0.5,size=16, color="red", face="bold"),
            legend.key.size = unit(1.5, 'cm')) 
    
    
    powerP_theory <- c()
    for (i in seq(0.1,0.9, by=0.01)){
      power0 <- power_cal(nsz =input$nsz1, nsz_p = i, alpha = input$alpha1, Pe = input$Pe1 , Pc = input$Pc1)
      powerP_theory <- c(powerP_theory,power0)
    }
    df_powerP<- data.frame(Prob = seq(0.1,0.9, by=0.01), Power =powerP_theory)
    df_powerP$Prob <- as.factor(df_powerP$Prob)
    powerP <- ggplot(df_powerP, aes(x=Prob, y=Power, group=1)) +
      geom_line() +
      geom_point()+
      ggtitle("Power vs. Treatment allocation probability")+
      ylab("Power") +
      xlab("Treatment E allocation probability") +
      ylim(0,1.0)+
      scale_x_discrete(breaks=seq(0.1,0.9, by=0.1))+
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill='transparent'),
            panel.border=element_blank(), 
            axis.line = element_line(), 
            axis.ticks = element_line(colour='black'),
            plot.title = element_text(hjust = 0.5,size=16, color="red", face="bold"),
            legend.key.size = unit(1.5, 'cm')) 
    
    RR_theory <- c()
    powerRR_theory <- c()
    for (i in seq(0.1,0.9, by=0.01)){
      power0 <- power_cal(nsz =input$nsz1, nsz_p = input$nsz_p1, alpha = input$alpha1, Pe = i , Pc = input$Pc1)
      powerRR_theory <- c(powerRR_theory,power0)
      rr <- i-input$Pc1
      RR_theory <- c(RR_theory, rr)
    }
    
    df_powerRR<- data.frame(RR = RR_theory, Power =powerRR_theory)
    df_powerRR$RR <- as.factor(df_powerRR$RR)
    powerRR <- ggplot(df_powerRR, aes(x=RR, y=Power, group=1)) +
      geom_line() +
      geom_point() +
      ggtitle("Power vs. Difference in response rate")+
      ylab("Power") +
      xlab("Difference in response rate (Treatment E-Treatment C)") +
      ylim(0,1.0)+
      scale_x_discrete(breaks=seq(-1, 1, by=0.1))+
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill='transparent'),
            panel.border=element_blank(), 
            axis.line = element_line(), 
            axis.ticks = element_line(colour='black'),
            plot.title = element_text(hjust = 0.5,size=16, color="red", face="bold"),
            legend.key.size = unit(1.5, 'cm')) 
    ggpubr::ggarrange(powerN,powerP,powerRR, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
  }, height = 650, width = 800)
  
  
  ####################### page 2 ###################################
  datasetInput2 <- reactive({  
    
    power <- power_cal(nsz =input$nsz2, nsz_p = input$nsz_p2, alpha = input$alpha2, Pe = input$Pe2 , Pc = input$Pc2)
    power <- round(power,3)
    
    print_nsz <- paste('Total sample size: ',input$nsz2)
    print_nsz_p <- paste('Treatment E allocation probability: ', input$nsz_p2)
    print_alpha <- paste('Significance level in the overall test: ', input$alpha2)
    print_rrE <- paste('Response rate of Treatment E: ', input$Pe2)
    print_rrC <- paste('Response rate of Treatment C: ', input$Pc2 )
    print_power <- c(power,'','','','')
    df <-  as.data.frame(cbind(rbind(print_nsz,print_nsz_p,print_alpha, print_rrE, print_rrC),print_power))
    colnames(df) <- c('Parameters Setting','Theoretical Power')
    print(df)
  })
  
  output$contents2 <- renderPrint({
    if (input$calculatebutton2>0) { 
      isolate("The power is calculated") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$Power2 <- renderTable({
    if (input$calculatebutton2>0) { 
      isolate(datasetInput2()) 
    }
  })
  
  output$note2 <- renderPrint({
    if (input$nsz2 <= input$nsz1) { 
      isolate(paste("Note: The sensitive proportion based on current parameter settings is ", round(input$nsz2/input$nsz1,3))) 
    } else {
      return("WARNING: The sample size of sensiitve subjects should be less than the total sample size.")
    }
  })
  
  output$plot2 <- renderPlot({
    power_theory <- c()
    for (i in seq(20, 500, by=20)){
      power0 <- power_cal(nsz =i, nsz_p = input$nsz_p2, alpha = input$alpha2, Pe = input$Pe2 , Pc = input$Pc2)
      power_theory <- c(power_theory,power0)
    }
    
    df_powerN<- data.frame(N = seq(20, 500, by=20), Power =power_theory)
    df_powerN$N <- as.factor(df_powerN$N)
    powerN <- ggplot(df_powerN, aes(x=N, y=Power,group=1)) +
      geom_line() +
      geom_point()+
      ggtitle("Power vs. Total sample size")+
      ylab("Power") +
      xlab("Total sample size") +
      ylim(0,1.0)+
      scale_x_discrete(breaks=seq(20, 500, by=60))+
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill='transparent'),
            panel.border=element_blank(), 
            axis.line = element_line(), 
            axis.ticks = element_line(colour='black'),
            plot.title = element_text(hjust = 0.5,size=16, color="red", face="bold"),
            legend.key.size = unit(1.5, 'cm')) 
    
    
    powerP_theory <- c()
    for (i in seq(0.1,0.9, by=0.01)){
      power0 <- power_cal(nsz =input$nsz2, nsz_p = i, alpha = input$alpha2, Pe = input$Pe2 , Pc = input$Pc2)
      powerP_theory <- c(powerP_theory,power0)
    }
    df_powerP<- data.frame(Prob = seq(0.1,0.9, by=0.01), Power =powerP_theory)
    df_powerP$Prob <- as.factor(df_powerP$Prob)
    powerP <- ggplot(df_powerP, aes(x=Prob, y=Power, group=1)) +
      geom_line() +
      geom_point()+
      ggtitle("Power vs. Treatment allocation probability")+
      ylab("Power") +
      xlab("Treatment E allocation probability") +
      ylim(0,1.0)+
      scale_x_discrete(breaks=seq(0.1,0.9, by=0.1))+
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill='transparent'),
            panel.border=element_blank(), 
            axis.line = element_line(), 
            axis.ticks = element_line(colour='black'),
            plot.title = element_text(hjust = 0.5,size=16, color="red", face="bold"),
            legend.key.size = unit(1.5, 'cm')) 
    
    RR_theory <- c()
    powerRR_theory <- c()
    for (i in seq(0.1,0.9, by=0.01)){
      power0 <- power_cal(nsz =input$nsz2, nsz_p = input$nsz_p2, alpha = input$alpha2, Pe = i , Pc = input$Pc2)
      powerRR_theory <- c(powerRR_theory,power0)
      rr <- i-input$Pc2
      RR_theory <- c(RR_theory, rr)
    }
    
    df_powerRR<- data.frame(RR = RR_theory, Power =powerRR_theory)
    df_powerRR$RR <- as.factor(df_powerRR$RR)
    powerRR <- ggplot(df_powerRR, aes(x=RR, y=Power, group=1)) +
      geom_line() +
      geom_point() +
      ggtitle("Power vs. Difference in response rate")+
      ylab("Power") +
      xlab("Difference in response rate (Treatment E-Treatment C)") +
      ylim(0,1.0)+
      scale_x_discrete(breaks=seq(-1, 1, by=0.1))+
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill='transparent'),
            panel.border=element_blank(), 
            axis.line = element_line(), 
            axis.ticks = element_line(colour='black'),
            plot.title = element_text(hjust = 0.5,size=16, color="red", face="bold"),
            legend.key.size = unit(1.5, 'cm')) 
    
    ggpubr::ggarrange(powerN,powerP,powerRR, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
  }, height = 600, width = 800)
    
    ####################### page 3 ###################################
    datasetInput3 <- reactive({  
      sens_prob <- input$nsz_sens3/input$nsz3
      rrE_wgt <- input$Pe_sens3 *sens_prob + input$Pe_nonsens3 *(1 - sens_prob)
      rrC_wgt <- input$Pc_sens3 *sens_prob + input$Pc_nonsens3 *(1 - sens_prob)
      power_wgt <- power_cal(nsz =input$nsz3, nsz_p = input$nsz_p3, alpha = input$alpha3, Pe = rrE_wgt, Pc = rrC_wgt)
      power_wgt <- round(power_wgt,3)
      power_sens <- power_cal(nsz =input$nsz_sens3, nsz_p = input$nsz_p3, alpha = input$alpha_sens3, Pe = input$Pe_sens3, Pc = input$Pc_sens3)
      power_sens <- round(power_sens,3)
      
      print_nsz <- paste('Total sample size: ',input$nsz3)
      print_nsz_sens <- paste('Sample size of sensitive subjects: ',input$nsz_sens3)
      print_nsz_p <- paste('Treatment E allocation probability: ', input$nsz_p3)
      print_alpha <- paste('Significance level in the overall test: ', input$alpha3)
      print_alpha_sens <- paste('Significance level in the sensitive test: ', input$alpha_sens3)
      print_rrE_sens <- paste('Response rate of Treatment E in sensitive subjects: ', input$Pe_sens3)
      print_rrE_nonsens<- paste('Response rate of Treatment E in non-sensitive subjects: ', input$Pe_nonsens3)
      print_rrC_sens <- paste('Response rate of Treatment C in sensitive subjects: ', input$Pc_sens3)
      print_rrC_nonsens <- paste('Response rate of Treatment C in non-sensitive subjects: ', input$Pc_nonsens3)
      print_power_sens <- paste('Theoretical power for sensitive subjects: ', power_sens)
      print_power_wgt <- paste('Theoretical power for overall subjects: ', power_wgt)
      print_result <- c(power,'','','','')
      df <-  as.data.frame(cbind(rbind(print_nsz, print_nsz_sens, print_nsz_p,print_alpha, print_alpha_sens, print_rrE_sens, 
                                       print_rrE_nonsens, print_rrC_sens, print_rrC_nonsens),rbind(print_power_sens,print_power_wgt,'','','','',
                                                                                                   '','','')))
      colnames(df) <- c('Parameters Setting','Results')
      print(df)
    })
    
    output$contents3 <- renderPrint({
      if (input$calculatebutton3>0) { 
        isolate("The power is calculated") 
      } else {
        return("Server is ready for calculation.")
      }
    })
    
    # Prediction results table
    output$Power3 <- renderTable({
      if (input$calculatebutton3>0) { 
        if (input$nsz_sens3 <= input$nsz3) { 
          isolate(datasetInput3()) 
        }
        else{
          return("ERROR: The sample size of sensiitve subjects should be less than the total sample size.")
        }
      }
    })
    
    output$note3 <- renderPrint({
      if (input$nsz_sens3 <= input$nsz3) { 
        isolate(paste("Note: The sensitive proportion based on current parameter settings is ", round(input$nsz_sens3/input$nsz3,3))) 
      } else {
        return("ERROR: The sample size of sensiitve subjects should be less than the total sample size.")
      }
    })
    
    output$plot3_sens <- renderPlot({
      power_sens <- c()
      for (i in seq(20, 500, by=20)){
        power0 <- power_cal(nsz =i, nsz_p = input$nsz_p3, alpha = input$alpha_sens3, Pe = input$Pe_sens3, Pc = input$Pc_sens3)
        power_sens <- c(power_sens,power0)
      }
      
      df_powersensN<- data.frame(N = seq(20, 500, by=20), Power =power_sens)
      df_powersensN$N <- as.factor(df_powersensN$N)
      powersensN <- ggplot(df_powersensN, aes(x=N, y=Power,group=1)) +
        geom_line() +
        geom_point()+
        ggtitle("Power vs. sample size")+
        ylab("Power") +
        xlab("Sample size") +
        ylim(0,1.0)+
        scale_x_discrete(breaks=seq(20, 500, by=60))+
        theme(text = element_text(size = 10),
              panel.background = element_rect(fill='transparent'),
              panel.border=element_blank(), 
              axis.line = element_line(), 
              axis.ticks = element_line(colour='black'),
              plot.title = element_text(hjust = 0.5,size=12, color="red", face="bold"),
              legend.key.size = unit(1.5, 'cm')) 
      
      
      powerP_sens <- c()
      for (i in seq(0.1,0.9, by=0.01)){
        power0 <- power_cal(nsz =input$nsz_sens3, nsz_p = i, alpha = input$alpha_sens3, Pe = input$Pe_sens3, Pc = input$Pc_sens3)
        powerP_sens <- c(powerP_sens,power0)
      }
      df_powersensP<- data.frame(Prob = seq(0.1,0.9, by=0.01), Power =powerP_sens)
      df_powersensP$Prob <- as.factor(df_powersensP$Prob)
      powersensP <- ggplot(df_powersensP, aes(x=Prob, y=Power, group=1)) +
        geom_line() +
        geom_point()+
        ggtitle("Power vs. Treatment allocation probability")+
        ylab("Power") +
        xlab("Treatment E allocation probability") +
        ylim(0,1.0)+
        scale_x_discrete(breaks=seq(0.1,0.9, by=0.1))+
        theme(text = element_text(size = 10),
              panel.background = element_rect(fill='transparent'),
              panel.border=element_blank(), 
              axis.line = element_line(), 
              axis.ticks = element_line(colour='black'),
              plot.title = element_text(hjust = 0.5,size=12, color="red", face="bold"),
              legend.key.size = unit(1.5, 'cm')) 
      
      RR_sens <- c()
      powerRR_sens <- c()
      for (i in seq(0.1,0.9, by=0.01)){
        power0 <- power_cal(nsz =input$nsz_sens3, nsz_p = input$nsz_p3, alpha = input$alpha_sens3, Pe = i, Pc = input$Pc_sens3)
        powerRR_sens <- c(powerRR_sens,power0)
        rr <- i-input$Pc_sens3
        RR_sens <- c(RR_sens, rr)
      }
      
      df_powersensRR<- data.frame(RR = RR_sens, Power =powerRR_sens)
      df_powersensRR$RR <- as.factor(df_powersensRR$RR)
      powersensRR <- ggplot(df_powersensRR, aes(x=RR, y=Power, group=1)) +
        geom_line() +
        geom_point() +
        ggtitle("Power vs. Difference in response rate")+
        ylab("Power") +
        xlab("Difference in response rate (Treatment E-Treatment C)") +
        ylim(0,1.0)+
        scale_x_discrete(breaks=seq(-1, 1, by=0.1))+
        theme(text = element_text(size = 10),
              panel.background = element_rect(fill='transparent'),
              panel.border=element_blank(), 
              axis.line = element_line(), 
              axis.ticks = element_line(colour='black'),
              plot.title = element_text(hjust = 0.5,size=12, color="red", face="bold"),
              legend.key.size = unit(1.5, 'cm')) 
      
      library(ggpubr)
      if (input$nsz_sens3 <= input$nsz3){
        #ggpubr::ggarrange(powersensN,powersensP,powersensRR,powerwgtN,powerwgtP,powerwgtRR, ncol=3, nrow=2, 
         #                 common.legend = TRUE, legend="bottom", font.label = list(size = 14, color = "black", face = "bold", family = NULL))
        plot <- ggpubr::ggarrange(powersensN,powersensP,powersensRR , ncol=3, nrow=1, 
                         common.legend = TRUE, legend="bottom")
        ggpubr::annotate_figure(plot, top = text_grob("(I) Theoretical Power for Sensitive Subjects", 
                                           color = "blue", face = "bold", size = 20))
      }
    }, height = 400, width = 1100)
      
      
      #### weighted power plots #########3
    output$plot3_wgt <- renderPlot({ 
      sens_prob <- input$nsz_sens3/input$nsz3
      rrE_wgt <- input$Pe_sens3 *sens_prob + input$Pe_nonsens3 *(1 - sens_prob)
      rrC_wgt <- input$Pc_sens3 *sens_prob + input$Pc_nonsens3 *(1 - sens_prob)
      power_wgt <- c()
      for (i in seq(60, 1000, by=20)){
        power0 <- power_cal(nsz =i, nsz_p = input$nsz_p3, alpha = input$alpha3, Pe = rrE_wgt, Pc = rrC_wgt)
        power_wgt <- c(power_wgt,power0)
      }
      
      df_powerwgtN<- data.frame(N = seq(60, 1000, by=20), Power =power_wgt)
      df_powerwgtN$N <- as.factor(df_powerwgtN$N)
      powerwgtN <- ggplot(df_powerwgtN, aes(x=N, y=Power,group=1)) +
        geom_line() +
        geom_point()+
        ggtitle("Power vs. sample size")+
        ylab("Power") +
        xlab("Sample size") +
        ylim(0,1.0)+
        scale_x_discrete(breaks=seq(60, 1000, by=100))+
        theme(text = element_text(size = 10),
              panel.background = element_rect(fill='transparent'),
              panel.border=element_blank(), 
              axis.line = element_line(), 
              axis.ticks = element_line(colour='black'),
              plot.title = element_text(hjust = 0.5,size=12, color="red", face="bold"),
              legend.key.size = unit(1.5, 'cm')) 
      
      
      powerP_wgt <- c()
      for (i in seq(0.1,0.9, by=0.01)){
        power0 <- power_cal(nsz =input$nsz3, nsz_p = i, alpha = input$alpha3, Pe = rrE_wgt, Pc = rrC_wgt)
        powerP_wgt <- c(powerP_wgt,power0)
      }
      df_powerwgtP<- data.frame(Prob = seq(0.1,0.9, by=0.01), Power =powerP_wgt)
      df_powerwgtP$Prob <- as.factor(df_powerwgtP$Prob)
      powerwgtP <- ggplot(df_powerwgtP, aes(x=Prob, y=Power, group=1)) +
        geom_line() +
        geom_point()+
        ggtitle("Power vs. Treatment allocation probability")+
        ylab("Power") +
        xlab("Treatment E allocation probability") +
        ylim(0,1.0)+
        scale_x_discrete(breaks=seq(0.1,0.9, by=0.1))+
        theme(text = element_text(size = 10),
              panel.background = element_rect(fill='transparent'),
              panel.border=element_blank(), 
              axis.line = element_line(), 
              axis.ticks = element_line(colour='black'),
              plot.title = element_text(hjust = 0.5, size=12, color="red", face="bold"),
              legend.key.size = unit(1.5, 'cm')) 
      
      RR_wgt <- c()
      powerRR_wgt <- c()
      for (i in seq(0.1,0.9, by=0.01)){
        power0 <- power_cal(nsz =input$nsz3, nsz_p = input$nsz_p3, alpha = input$alpha3, Pe = i, Pc = rrC_wgt)
        powerRR_wgt <- c(powerRR_wgt,power0)
        rr <- i-rrC_wgt
        RR_wgt <- c(RR_wgt, rr)
      }
      
      df_powerwgtRR<- data.frame(RR = RR_wgt, Power =powerRR_wgt)
      #df_powerwgtRR$RR <- as.factor(df_powerwgtRR$RR)
      powerwgtRR <- ggplot(df_powerwgtRR, aes(x=RR, y=Power, group=1)) +
        geom_line() +
        geom_point() +
        ggtitle("Power vs. Difference in response rate")+
        ylab("Power") +
        xlab("Difference in response rate (Treatment E-Treatment C)") +
        ylim(0,1.0)+
        #scale_x_discrete(breaks=seq(-1, 1, by=0.2))+
        theme(text = element_text(size = 10),
              panel.background = element_rect(fill='transparent'),
              panel.border=element_blank(), 
              axis.line = element_line(), 
              axis.ticks = element_line(colour='black'),
              plot.title = element_text(hjust = 0.5, size=12, color="red", face="bold"),
              legend.key.size = unit(1.5, 'cm')) 
      library(ggpubr)
      if (input$nsz_sens3 <= input$nsz3){
      plot <- ggpubr::ggarrange(powerwgtN,powerwgtP,powerwgtRR, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
       ggpubr::annotate_figure(plot, top = text_grob("(II) Theoretical Power for Overall Subjects", 
                                            color = "Blue", face = "bold", size = 20))
      }
    }, height = 400, width = 1100)
}

####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)





