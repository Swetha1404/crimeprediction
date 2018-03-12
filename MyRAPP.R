library(shiny)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Crime Prediction"),
  
  # Sidebar layout with input and output definitions ----
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Predict", div(style="display:inline-block",h4(textOutput("caption2", container = span))),
                         div(style="display:inline-block",textInput("caption1", "Working Directory(path must contain only backslash(/))", "Enter working directory")),
                         div(style="display:inline-block",textInput("caption", "CSV File(with extension)", "Enter csv file path - must only contain backslash(/)")),
                         div(style="display:inline-block",actionButton("do", "Predict"))),
                tabPanel("Mapping", 
                         div(style="display:inline-block",textOutput("caption3", container = span)),
                         div(style="display:inline-block",h4(textOutput("caption4", container = span))),
                         div(style="display:inline-block",h4(textOutput("caption5", container = span))),
                         div(style="display:inline-block",textOutput("caption6", container = span)),
                         div(style="display:inline-block",textOutput("caption7", container = span)),
                         div(style="display:inline-block",textOutput("caption8", container = span)),
                         div(style="display:inline-block",textOutput("caption9", container = span)),
                         div(style="display:inline-block",textOutput("caption10", container = span)),
                         div(style="display:inline-block",actionButton("do1", "Map"))
                ),
                tabPanel("Enter your information", 
                         div(style="display:inline-block",h4(textOutput("caption11", container = span))),
                         div(style="display:inline-block",h4(textOutput("caption12", container = span))),
                         div(style="display:inline-block",textInput("caption13", "Working Directory(path must contain only backslash(/))", "Enter working directory")),
                         div(style="display:inline-block",textInput("caption14", "Name", "Enter name")),
                         div(style="display:inline-block",textInput("caption15", "Gmail", "Enter your email(only Gmail)")),
                         div(style="display:inline-block",textInput("caption16", "State-please enter Uttar Pradesh to demonstrate warning message", "Enter State")),
                         div(style="display:inline-block",actionButton("do2", "Save(Click Twice)"))
                ),
                tabPanel("Send Message",
                         div(style="display:inline-block",h4(textOutput("caption17", container = span))),
                         div(style="display:inline-block",textOutput("caption18", container = span)),
                         div(style="display:inline-block",textInput("caption19", "Working Directory(path must contain only backslash(/))", "Enter working directory")),
                         div(style="display:inline-block",textInput("caption22", "Predicted CSV File(with extension)", "Enter the file that has been created previously in your working diretory")),
                         div(style="display:inline-block",textInput("caption20", "User Detail CSV File(with extension)", "Enter the file that has been created previously in your working diretory")),
                         div(style="display:inline-block",actionButton("do3", "Send"))
                         
                )
    )
    
  )
)

server = function(input, output,session) {observeEvent(input$do, {
  setwd(input$caption1)
  indiacrime = read.csv(input$caption, header = TRUE, check.names = FALSE)
  
  # Create an output directory if it does not exist
  if(!file.exists("output")) {
    dir.create("output")
  }
  
  setwd("./output")
  
  
  # Write the output in the ./output directory
  
  line <- c("State,Crime,2013,2014,2015,2016,2017,2018,","\n")
  filename <- paste("Allcrimes",".txt")
  cat(line, file=filename,append=TRUE)
  
  #fileConn<-file("output/AllCrimes.txt")
  #writeLines(c("State,Crime,2013,2014,2015,2016,2017,2018"), fileConn)
  
  setwd("..")
  
  statecrime <- function(indiacrime, row, state,crime) {
    
    year <- c(2001:2012)
    atitle <- paste(state," - ", crime)
    thecrime <- as.numeric(indiacrime[row,3:14])
    
    ymin <- min(thecrime) - 300
    ymax <- max(thecrime) + 500
    print(ymin)
    print(ymax)
    
    if(ymin > 8000) {
      interval <- 2000
      ymin <- min(thecrime) - 1500
      ymax <- max(thecrime) + 10000
    }
    else {
      interval <- 100
      
    }
    
    # Make seperate folders for each state
    if(!file.exists(state)) {
      dir.create(state)
    }
    setwd(state)
    crimeplot <- paste(crime,".jpg")
    jpeg(crimeplot)
    
    # Plot the details of the crime
    plot(year,thecrime ,pch= 15, col="orange", xlab = "Year", ylab= crime, main = atitle,
         xlim=c(2001,2018),ylim=c(ymin,ymax), axes=FALSE)
    
    # Set the axes
    axis(side=1, at=c(2000:2018))
    axis(side=2, at=seq(ymin, ymax, by=interval))
    box()
    
    # Fit a linear regression model
    lmfit <-lm(thecrime~year)
    
    # Draw the lmfit line
    abline(lmfit)
    
    # Calculate the projected incidents of the crime
    nyears <-c(2013:2018)
    nthecrime <- rep(0,length(nyears))
    
    # Projected crime incidents from 2013 to 2018 using a linear regression model
    for (i in seq_along(nyears)) {
      nthecrime[i] <- lmfit$coefficients[2] * nyears[i] + lmfit$coefficients[1]
      
    }
    
    # Add the legend
    alegend <- paste(
      "Projected ",crime, " in ", state)
    points(nyears,nthecrime,pch= 17, col="violet")
    legend( x="topleft", 
            legend=c(alegend),
            col=c("orange"), bty="n" , lwd=1, lty=c(2), 
            pch=c(15) )
    
    dev.off()
    setwd("..")
    
    # Write the projected crime rate in a file
    nthecrime <- round(nthecrime,2)
    nthecrime <- c(state,crime,nthecrime, "\n")
    print(nthecrime)
    #write(nthecrime,file=fileconn, ncolumns=9, append=TRUE,sep="\t")
    filename <- paste("Allcrimes",".txt")
    
    # Write the output in the ./output directory
    setwd("./output")
    
    cat(nthecrime, file=filename, sep=",",append=TRUE)
    
    # Come back up one level
    setwd("..")
    
  }
  
  
  
  
  # 1. Andhra Pradesh 
  i <- 1
  statecrime(indiacrime, i, "Andhra Pradesh","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Andhra Pradesh","Total crimes against women")
  
  # 2. ARUNACHAL PRADESH 
  i <- 2
  statecrime(indiacrime, i, "Arunachal Pradesh","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Arunachal Pradesh","Total crimes against women")
  
  # 3. ASSAM 
  i <- 3
  statecrime(indiacrime, i, "Assam","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Assam","Total crimes against women")
  
  # 4. BIHAR 
  i <- 4
  statecrime(indiacrime, i, "Bihar","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Bihar","Total crimes against women")
  
  # 5. CHHATTISGARH 
  i <- 5
  statecrime(indiacrime, i, "Chhattisgarh","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Chhattisgarh","Total crimes against women")
  
  # 6. GOA 
  i <- 6
  statecrime(indiacrime, i, "Goa","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Goa","Total crimes against women")
  
  # 7. GUJARAT 
  i <- 7
  statecrime(indiacrime, i, "Gujarat","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Gujarat","Total crimes against women")
  
  # 8. HARYANA 
  i <- 8
  statecrime(indiacrime, i, "Haryana","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Haryana","Total crimes against women")
  
  # 9. HIMACHAL PRADESH 
  i <- 9
  statecrime(indiacrime, i, "Himachal Pradesh","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Himachal Pradesh","Total crimes against women")
  
  # 10. JAMMU & KASHMIR 
  i <- 10
  statecrime(indiacrime, i, "Jammu & Kashmir","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Jammu & Kashmir","Total crimes against women")
  
  # 11. JHARKHAND 
  i <- 11
  statecrime(indiacrime, i, "Jharkhand","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Jharkhand","Total crimes against women")
  
  # 12. KARNATAKA 
  i <- 12
  statecrime(indiacrime, i, "Karnataka","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Karnataka","Total crimes against women")
  
  
  # 13. KERALA 
  i <- 13
  statecrime(indiacrime, i, "Kerala","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Kerala","Total crimes against women")
  
  
  # 14. MADHYA PRADESH 
  i <- 14
  statecrime(indiacrime, i, "Madhya Pradesh","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Madhya Pradesh","Total crimes against women")
  
  # 15. MAHARASHTRA 
  i <- 15
  statecrime(indiacrime, i, "Maharashtra","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Maharashtra","Total crimes against women")
  
  # 16. MANIPUR 
  i <- 16
  statecrime(indiacrime, i, "Manipur","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Manipur","Total crimes against women")
  
  # 17. MEGHALAYA 
  i <- 17
  statecrime(indiacrime, i, "Meghalaya","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Meghalaya","Total crimes against women")
  
  # 18. MIZORAM 
  i <- 18
  statecrime(indiacrime, i, "Mizoram","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Mizoram","Total crimes against women")
  
  # 19. NAGALAND 
  i <- 19
  statecrime(indiacrime, i, "Nagaland","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Nagaland","Total crimes against women")
  
  # 20. Odisha 
  i <- 20
  statecrime(indiacrime, i, "Odisha","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Odisha","Total crimes against women")
  
  # 21. PUNJAB 
  i <- 21
  statecrime(indiacrime, i, "Punjab","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Punjab","Total crimes against women")
  
  # 22. RAJASTHAN 
  i <- 22
  statecrime(indiacrime, i, "Rajasthan","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Rajasthan","Total crimes against women")
  
  # 23. SIKKIM 
  i <- 23
  statecrime(indiacrime, i, "Sikkim","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Sikkim","Total crimes against women")
  
  # 24. TAMIL NADU 
  i <- 24
  statecrime(indiacrime, i, "Tamil Nadu","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Tamil Nadu","Total crimes against women")
  
  # 25. TRIPURA 
  i <- 25
  statecrime(indiacrime, i, "Tripura","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Tripura","Total crimes against women")
  
  # 26. UTTAR PRADESH 
  i <- 26
  statecrime(indiacrime, i, "Uttar Pradesh","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Uttar Pradesh","Total crimes against women")
  
  # 27. UTTARAKHAND 
  i <- 27
  statecrime(indiacrime, i, "Uttarakhand","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Uttarakhand","Total crimes against women")
  
  # 28. WEST BENGAL 
  i <- 28
  statecrime(indiacrime, i, "West Bengal","Rape")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "West Bengal","Total crimes against women")
  
  
  # 30. A & N ISLANDS 
  i <- 30
  statecrime(indiacrime, i, "A & N Islands","Rape")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "A & N Islands","Total crimes against women")
  
  # 31. CHANDIGARH 
  i <- 31
  statecrime(indiacrime, i, "Chandigarh","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Chandigarh","Total crimes against women")
  
  # 32. D & N HAVELI 
  i <- 32
  statecrime(indiacrime, i, "D & N Haveli","Rape")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "D & N Haveli","Total crimes against women")
  
  # 33. DAMAN & DIU 
  i <- 33
  statecrime(indiacrime, i, "Daman & Diu","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Daman & Diu","Total crimes against women")
  
  # 34. DELHI 
  i <- 34
  statecrime(indiacrime, i, "Delhi","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Delhi","Total crimes against women")
  
  # 35. LAKSHADWEEP 
  i <- 35
  statecrime(indiacrime, i, "Lakshwadeep","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Lakshwadeep","Total crimes against women")
  
  # 36. PUDUCHERRY 
  i <- 36
  statecrime(indiacrime, i, "Puducherry","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Puducherry","Total crimes against women")
  
  # 29. TOTAL (STATES) 
  i <- 29
  statecrime(indiacrime, i, "Total (States)","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Total (States)","Total crimes against women")
  
  
  # 37. TOTAL (UTs) 
  i <- 37
  statecrime(indiacrime, i, "Total (UTs)","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Total (UTs)","Total crimes against women")
  
  # 38. TOTAL (ALL-INDIA) 
  i <- 38
  statecrime(indiacrime, i, "Total (All-India)","Rape")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Kidnapping& Abduction")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Dowry Deaths")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Assault on Women")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Insult to modesty")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Cruelty by husband_relatives")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Imporation of girls from foreign country")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Immoral traffic act")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Dowry prohibition act")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Indecent representation of Women Act")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Commission of Sati Act")
  i <- i+38
  statecrime(indiacrime, i, "Total (All-India)","Total crimes against women")
  
  
  setwd("C:/Users/DELL/Desktop/output")
  superbowl <- read.table(
    "Allcrimes .txt", 
    sep=",", header=TRUE)
  
  write.csv(superbowl, file = "Allcrimes.csv",row.names=FALSE)
  output$caption2 <- renderText({paste("The output files will be created within the output folder in your working directory(processing time=45s)\n")})
  output$caption3 <- renderText({paste("\n")})
  
  
  
}
)
  observeEvent(input$do1, {
    browseURL("https://sjce1.maps.arcgis.com/home/webmap/viewer.html?useExisting=1")
    
  })
  
  observeEvent(input$do2, {
    setwd(input$caption13)
    string <- paste(c(input$caption14,",",input$caption15,",",input$caption16,",","\n"))
    
    
    # Write the output in the ./output directory
    
    cat(string, file="info.txt", sep=",",append=TRUE)
    setwd(input$caption13)
    superbowl <- read.table(
      "info.txt", 
      sep=",", header=TRUE)
    
    
    write.csv(superbowl, file = "info.csv",row.names=FALSE)
  })
  
  observeEvent(input$do3, {
    setwd(input$caption19)
    setwd("./output")
    pf <- read.csv(input$caption22)
    v1 <- pf[1:421,7]
    v2 <- which(v1 >= 50000, arr.ind=T) 
    print(v2)
    v3 <- pf[[1]]
    v4<- v3[v2]
    v7 <- toString(v4)
    setwd("..")
    string1 <- paste("Girls be careful there is a high crime rate in", paste(v4))
    af <- read.csv(input$caption20)
    v5 <- af[[7]]
    for(i in v5 == v7)
    {
      v6 <- which(v5 == v7, arr.ind =T)
      print(v6)
      v8 <- af[[4]]
      v9 <- v8[v6]
      
    }
    v10 <- toString(v9)
    print(v10)
    test_email <- mime() %>%
      to(c("daattali+gmailr@gmail.com", v10)) %>%
      html_body(string1) %>%
      subject(input$subj)
    # attach_file("file.txt")
    
    test_email <- sub("Content-Disposition: inline\r\n--","Content-Disposition: inline\r\n\r\n--", as.character(test_email))
    
    ret_val <- send_message(test_email)
    
  })
  
  
  output$caption2 <- renderText({paste("The output files will be created within the output folder in your working directory(processing time=45s)\n")})
  output$caption5 <- renderText({paste("Steps:\n")})
  output$caption6 <- renderText({paste("1.ArcGIS Website will be open\n")})
  output$caption7 <- renderText({paste("2.Enter Username:sweme081,password:happy1sad2\n")})
  output$caption8 <- renderText({paste("3.In toolbar select Add -> Add layer from file\n")})
  output$caption9 <- renderText({paste("4.Enter the csv file containing the predicted values\n")})
  output$caption10 <- renderText({paste("5.Click Import Layer -> Add Layer\n")})
  output$caption12 <- renderText({paste("User information file will be created in your working directory - Please delete the first row created within the file\n")})  
  output$caption18 <- renderText({paste("Warning message is sent to the people living in a state with crime rate over 50,000\n")})
}

shinyApp(ui,server)