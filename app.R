# Load libraries and local files
library(shiny)
library(dplyr)
library(shinythemes)
source('demdis.R', encoding = 'utf-8')
source('demdis_graph.R', encoding = 'utf-8')

# Define UI
ui <- fluidPage(
  titlePanel("Demdis Curator App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("partic","Upload the participants-votes.csv file"), 
      fileInput('comms', 'Upload the comments.csv file'),
      actionButton('submit', 'Run PolisV2'),
      uiOutput("media"),
    ),
    mainPanel(
      uiOutput("tb")
      
    )
  )
)

server <- function(input,output) {
  # Runs PolisV2 backend upon hitting the submit button
  main_data <- eventReactive(input$submit, {
    polis.clust(clean(read.csv(input$partic$datapath)), 
                boosted = TRUE, 
                force = input$force_clusters,
                comment_id = input$comm_id_adjust,
                coeff = input$adjust_coeff)
  })
  num_clusters <- eventReactive(input$submit, {
    input$force_clusters
  })
  adjusted_comm <- eventReactive(input$submit, {
    input$comm_id_adjust
  })
  adjusting_coeff <- eventReactive(input$submit, {
    input$adjust_coeff
  })
  partic <- eventReactive(input$submit, {
    recreate(read.csv(input$partic$datapath), main_data())
  })
  subpartic <- eventReactive(input$submit, {
    partic()[apply(!is.na(partic()[, 8:ncol(partic())]), 1, sum) >= 7, ]
  })
  all <- eventReactive(input$submit, {
    top_comms(partic(), read.csv(input$comms$datapath, encoding = 'UTF-8'))
  })
  medium.ind <- eventReactive(input$txtokno, {
    grep(input$txtokno, subpartic()$xid)
  })
  medium.df <- eventReactive(input$txtokno, {
    comms.custom(partic(), read.csv(input$comms$datapath, encoding = 'UTF-8'),
                 medium.ind())
  })
  gud <- eventReactive(input$submit, {all()$clust %>% group_by(comment.id) %>% 
    summarize(n = sum(n.votes), u = sum(unseen)) %>% 
    mutate(total = n + u) %>% filter(n >= 0.5 * total) %>% 
    select(comment.id) %>% unlist %>% unname
  })
  
  # Lists the most polarizing comments
  output$polarized <- renderTable({
    if(is.null(input$partic) | is.null(input$comms)) {return ()}
    else {
      return(all()$points %>% filter(comment.id %in% gud()) %>% 
               arrange(desc(mk)) %>% head(n = 8))
    }
  })
  
  output$polartext <- renderText({
    txt <- all()$final %>% filter(comment.id == input$polar) %>% 
      select(comment.body) %>% unlist
    paste("<h3>", txt, ' (', input$polar, ')', "</h3>", sep = '')
  })
  
  output$polarplot <- renderPlot({
    if(is.null(input$partic) | is.null(input$comms)) {return ()}
    else {
      draw.comments(all(), id = input$polar)
    }
  })
  
  # Lists the most consensual comments
  output$consensus <- renderTable({
    if(is.null(input$partic) | is.null(input$comms)) {return ()}
    else {
      return(all()$points %>% arrange(desc(body)) %>% head(n = 8))
    }
  })
  
  output$consenstext <- renderText({
    txt <- all()$final %>% filter(comment.id == input$consens) %>% 
      select(comment.body) %>% unlist
    paste("<h3>", txt, ' (', input$consens, ')', "</h3>", sep = '')
  })
  
  output$consensplot <- renderPlot({
    if(is.null(input$partic) | is.null(input$comms)) {return ()}
    else {
      draw.comments(all(), id = input$consens)
    }
  })
  
  # Renders text above individual comment plots
  output$allcommtext <- renderText ({
    txt <- all()$final %>% filter(comment.id == input$commik) %>% 
      select(comment.body) %>% unlist
    paste("<h3>", txt, ' (', input$commik, ')', "</h3>", sep = '')
  })
  
  # Renders the individual comment plots
  output$comm <- renderPlot({
    if(is.null(input$partic) | is.null(input$comms)) {return()}
    else {
      draw.comments(all(), input$commik)
    }
    })
  
  # Lists the cluster-defining comments 
  output$clusters <- renderTable({
    if (is.null(input$partic) | is.null(input$comms))  {return()}
    else {
      df <- all()$clust %>% 
        filter(group.id == input$clusticek, n.agr > 0.4 * (n.votes + unseen)) %>% 
        arrange(desc(agr_pct), desc(n.agr)) %>% 
        select(comment.id, n.votes, n.agr, n.skip, n.dis, agr_pct) %>% 
        head(n = 8)
      return(df)
    }
  })
  
  # Renders the cluster-defining comment
  output$onecom <- renderPlot({
    if(is.null(input$partic) | is.null(input$comms)){return()}
    else {
      df <- all()$clust %>% 
        filter(group.id == input$clusticek, n.agr > 0.4 * (n.votes + unseen)) %>% 
        arrange(desc(agr_pct), desc(n.agr)) %>% 
        select(comment.id, n.votes, n.agr, n.skip, n.dis, agr_pct) %>% 
        head(n = 8)
      draw.onecomm(all(), df$comment.id[input$commnum],
                   group = as.numeric(input$clusticek))
    }
  })
  
  # Renders HTML text above cluster-defining comment
  output$clusttext <- renderText({
    df <- all()$clust %>% 
      filter(group.id == input$clusticek, n.agr > 0.4 * (n.votes + unseen)) %>% 
      arrange(desc(agr_pct), desc(n.agr)) %>% 
      select(comment.id, n.votes, n.agr, n.skip, n.dis, agr_pct) %>% 
      head(n = 8)
    txt <- all()$final %>% filter(comment.id == df$comment.id[input$commnum]) %>% 
      select(comment.body) %>% unlist
    return(paste("<h3>", txt, ' (', df$comment.id[input$commnum], ')', "</h3>",
                 sep = ''))
  })
  
  # Renders the cluster plot
  output$plotik <- renderPlot({ 
    if(is.null(input$partic)){return()}
    else {
    print(input$adjust_coeff)
    draw.clusters(main_data())
    if (input$txtokno != '') {
      channel.clusts <- table(subpartic()[medium.ind(), 3])
      points(main_data()[medium.ind(), tail(1:dim(main_data())[2], 2)], col = 'red', 
             cex = 1.5, pch = 19)
      legend('topleft', 
             legend = c(paste(str_to_title(input$txtokno), 'users'),
                        paste(LETTERS[1:length(channel.clusts)], channel.clusts)),
             cex = 1.6, 
             bty = 'n', 
             pch = c(19, rep(NA, length(channel.clusts))), 
             col = c('red', rep(NA, length(channel.clusts))))
      }
    }
  })
  
  output$settings <- renderText({
    paste("<h3>", "Current settings:", "</h3>",
          "<h5>", "Number of clusters: ", num_clusters(), "</h5>",
          "<h5>", "Adjusted comment: ", adjusted_comm(), "</h5>",
          "<h5>", "Adjusting coefficient: ", adjusting_coeff(), "</h5>",
          sep = '')
  })
  
  # Renders the cluster plot with adjusted PCA
  
  output$media <- renderUI({
    if(is.null(input$partic) | is.null(input$comms) | is.null(all())) {return()}
    list(hr(), 
         textInput("txtokno", "Show users", value = '')
    )
  })
  
  output$channeltext <- renderText({
    if (input$txtokno == '') {return()}
    txt <- all()$final %>% filter(comment.id == input$medcommik) %>% 
      select(comment.body) %>% unlist
    paste("<h3>", txt, ' (', input$medcommik, ')', "</h3>", sep = '')
  })
  
  output$medcomm <- renderPlot({
    if(input$txtokno == '') {return()}
    else {
      draw.graph(medium.df(), input$medcommik, group = 0, 
                 custom = input$txtokno)
    }
  })
  
  ## MainPanel tabset renderUI code ##
  # Renders the tabs upon file load
  output$tb <- renderUI({
    if(is.null(input$partic) | is.null(input$comms)) {return()}
    else
      tabsetPanel(
        tabPanel("Cluster graph",
                 selectInput("force_clusters", "Manually force number of clusters:",
                             choices = c("Auto", "1", "2", "3", "4"), selected = "Auto"),
                 selectInput('comm_id_adjust', 'Select comment did to adjust:', 
                             choices = read.csv(input$comms$datapath, encoding = 'UTF-8')$comment.id),
                 numericInput('adjust_coeff', 'Select adjustment multiplier', 
                              value = 1, min = 1),
                 htmlOutput("settings"),
                 plotOutput("plotik")),
        
        tabPanel("Consensual comments", 
                 selectInput('consens', 'Select comment to draw:',
                             choices = all()$points %>% 
                               arrange(desc(body)) %>% head(n = 8) %>% 
                               select(comment.id)),
                 tableOutput("consensus"),
                 htmlOutput('consenstext'),
                 plotOutput('consensplot')),
        
        tabPanel('Polarizing comments', 
                 selectInput('polar', 'Select comment to draw:', 
                             choices = all()$points %>% arrange(desc(mk)) %>% 
                               filter(comment.id %in% gud()) %>% 
                               head(n = 8) %>% select(comment.id)), 
                 tableOutput('polarized'),
                 htmlOutput('polartext'),
                 plotOutput('polarplot')),
        
        tabPanel('Cluster-defining comments', 
                 selectInput('clusticek', 'Select cluster:', 
                             choices = all()$clust$group.id %>% unique),
                 numericInput('commnum', 'Select order of comment to draw graph for:',
                             min = 1, value = 1, max = 8),
                 tableOutput('clusters'),
                 htmlOutput('clusttext'),
                 plotOutput('onecom')),
        
        tabPanel("Comment graph", 
                 numericInput("commik", "Select comment id", value = 0, 
                              min = 0),
                 htmlOutput('allcommtext'),
                 plotOutput("comm")),
        
        tabPanel('Channel votes',
                 numericInput('medcommik', 'Select comment id', value = 0, 
                              min = 0),
                 htmlOutput('channeltext'),
                 plotOutput('medcomm')))
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)