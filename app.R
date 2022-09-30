library(shiny)

info <- read.csv('USNews_Univ_Ranking.csv')
info$type <- as.factor(info$type)
univ.private <- subset(info, type == 'private')
univ.state <- subset(info, type == 'state')
info.tidy1 <- univ.private[,c('state','type','cost','rank')]

info.tidy2 <- univ.state[,c('state','type','cost.instate','rank')]
info.tidy2$type <- 'in-state'
names(info.tidy2)[3] <- 'cost'

info.tidy3 <- univ.state[,c('state','type','cost.ostate','rank')]
info.tidy3$type <- 'out-of-state'
names(info.tidy3)[3] <- 'cost'

info.tidy <- rbind(info.tidy1, info.tidy2, info.tidy3)
head(info.tidy)
nrow(info.tidy)

# average tuition
avg.tuition <- aggregate(info.tidy$cost, by = (info.tidy[,c('state','type')]), mean)
names(avg.tuition)[3] <- 'tuition'
head(avg.tuition)
info$cost.diff <- info$cost.ostate - info$cost.instate

library(ggplot2)
library(forcats)

ui <- fluidPage(
  titlePanel('US News University Ranking Info 2018'),
  mainPanel(
    tabsetPanel(type = 'tabs',
      tabPanel("Samples",
               plotOutput("plot01",height=600,width=600),
               textOutput("text01")),
      tabPanel("Tuition Range",
               plotOutput("plot02",height=600,width=600),
               textOutput("text02")),
      tabPanel("Tuition Range(2)",
               plotOutput("plot03",height=600,width=600),
               textOutput("text03")),
      tabPanel("Tution Range(Ordered)",
               plotOutput("plot04",height=600,width=600),
               textOutput("text04")),
      tabPanel("Avg Tuition",
               plotOutput("plot05",height=600,width=600),
               textOutput("text05")),
      tabPanel("Cost Histogram",
               plotOutput("plot06",height=600,width=600),
               textOutput("text06")),
      tabPanel("Enrollment(Size)",
               plotOutput("plot07",height=600,width=600),
               textOutput("text07")),
      tabPanel("Rank vs Cost",
               plotOutput("plot08",height=600,width=600),
               textOutput("text08")),
      tabPanel("Number",
               plotOutput("plot10",height=600,width=600),
               textOutput("text10"))
    )
  )
)

server <- function(input, output){
  output$plot01 <- renderPlot({
    ggplot(info) + geom_bar(aes(x=state, fill=type)) +
      coord_flip() + ggtitle('Number of universities in States')
  })
  output$text01 <- renderText({
    'Histogram showing Number Of Universities Sampled (230 total, 98 private and 132 state universities)'
  })
  
  output$plot02 <- renderPlot({
    ggplot() + 
      geom_boxplot(data = univ.private,aes(type, cost, color='private')) +
      geom_boxplot(data = univ.state,aes(type, cost.instate, color='in-state')) +
      geom_boxplot(data = univ.state,aes(type, cost.ostate, color='out-of-state')) +
      ggtitle('univeristy tuition') + xlab('type') + ylab('tuition')
  })
  output$text02 <- renderText({
    'Boxplot of Tuition Range vs University Types'
  })
  
  output$plot03 <- renderPlot({
    ggplot() + 
      geom_boxplot(data = univ.private,aes(state,cost, color='private', group=state)) +
      geom_boxplot(data = univ.state,aes(state,cost.instate, color='in-state', group=state)) +
      geom_boxplot(data = univ.state,aes(state,cost.ostate, color='out-of-state', group=state)) +
      ggtitle('univeristy tuition') + xlab('type') + ylab('tuition')
  })
  output$text03 <- renderText({
    'Tuition Range vs state'
  })
  
  output$plot04 <- renderPlot({
    ggplot(univ.state) + 
      geom_boxplot(aes(fct_reorder(state,cost.instate), cost.instate, color='in-state')) + 
      geom_boxplot(aes(fct_reorder(state,cost.instate), cost.ostate, color='out-of-state')) + 
      coord_flip()
  })
  output$text04 <- renderText({
    'state univs in-state cost'
  })
  
  output$plot05 <- renderPlot({
    ggplot(avg.tuition) +
      geom_point(aes(x=state,y=tuition, colour = type)) +
      ggtitle('tuition summary') + coord_flip()
  })
  output$text05 <- renderText({
    'Average Tuition'
  })
  
  output$plot06 <- renderPlot({
    ggplot(info.tidy) + 
      geom_histogram(aes(cost, fill=type), position = 'dodge', bins=15) +
      ggtitle('tuition (out-of-state) histogram')
  })
  output$text06 <- renderText({
    'Tuition histogram'
  })
  
  output$plot07 <- renderPlot({
    ggplot(info) + 
      geom_histogram(aes(enroll, fill=type), position = 'dodge', bins=15) +
      ggtitle('enrollment histogram')
  })
  output$text07 <- renderText({
    'enrollment histogram'
  })
  
  output$plot08 <- renderPlot({
    ggplot(info.tidy,aes(x=rank, y=cost, color=type)) + 
      geom_point() + geom_smooth(aes(x=rank, y=cost, group=type), method = lm) +
      xlab('rank') + ylab('tutition') +
      ggtitle('rank versus tuition')
  })
  output$text08 <- renderText({
    'Rank vs Out-of-State Tuition'
  })

  output$plot10 <- renderPlot({
    ggplot(subset(info,type=='state')) + geom_point(aes(y=cost.diff, x = rank)) +
      geom_smooth(aes(y=cost.diff, x = rank), method=lm)
  })
  output$text10 <- renderText({
    'in-state vs out-of-state cost different vs ranking'
  })
  
  
}

shinyApp(ui, server)