#################################################
#      Network App    #
#################################################

shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- read.csv(input$file$datapath ,header=TRUE, sep = ",")
    row.names(Dataset) = Dataset[,1]
    Dataset = as.matrix(Dataset[,2:ncol(Dataset)])
    return(Dataset)
  }
})

Dataset2 <- reactive({
  if (is.null(input$file1)) { return(NULL) }
  else{
    Dataset <- read.csv(input$file1$datapath ,header=TRUE, sep = ",")
    row.names(Dataset) = Dataset[,1]
    Dataset = Dataset[,2:ncol(Dataset)]
    return(Dataset)
  }
})

# Select variables:
output$yvarselect <- renderUI({
  if (is.null(input$file1)) { return(NULL) }
  else{
  
  selectInput("colattr", "Select Color variable",
              colnames(Dataset2()), colnames(Dataset2())[1])
  }
})

graph = reactive({
  graph <- graph.adjacency(Dataset(), mode = input$mode, weighted=NULL)
  graph = simplify(graph)  
  # col.names <- make.names(V(graph)$name, unique = TRUE)
  
  return(graph)
})

wc = reactive({
  wc = walktrap.community(graph())
})

centralities = reactive({
  graph = graph()
  metrics <- data.frame(Resp.Name = make.names(V(graph)$name, unique = TRUE),Community = wc()$membership,Degree=degree(graph), Out.Degree =degree(graph, v=V(graph), mode=c("out")),In.Degree =degree(graph, v=V(graph), mode=c("in")),
                        Betweenness=betweenness(graph), Closeness = closeness(graph), Eigenvector.Centrality.Scores = evcent(graph)$vector, Graph.Coreness = graph.coreness(graph))
  # row.names(metrics) = V(graph)$name
  
  metrics = metrics[(order(metrics[,1],metrics[,2],metrics[,3],metrics[,4],metrics[,5],metrics[,6],metrics[,7], decreasing= T)),]
  
  return(metrics)
})

output$centdata = renderDataTable({
  centralities()
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
output$graph1 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  else{
    
    colattr = Dataset2()[,input$colattr]
    
    if (is.null(input$file1)) {
      colattr   = 'lightskyblue'
    }
  graph = graph()
  
  E(graph)$weight <- count.multiple(graph)
  
  egam = (log(E(graph)$weight)+.3)/max(log(E(graph)$weight)+.3)
  E(graph)$color = rgb(.5,.5,0,egam)
  
  par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
  plot( graph,			#the graph to be plotted
        layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
        vertex.frame.color='lightskyblue', 		#the color of the border of the dots 
        vertex.color= colattr,
        vertex.label.color='black',		#the color of the name labels
        vertex.label.font=1,    			#the font of the name labels
        vertex.size = input$cex2,     # size of the vertex
        vertex.label= make.names(V(graph)$name, unique = TRUE),	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
        vertex.label.cex=(degree(graph)+1)/mean(degree(graph)) * input$cex		#specifies the size of the font of the labels. can also be made to vary

  ) 
  }
})

output$graph2 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
 
  else {
    par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
    plot(wc(),
          graph(),			#the graph to be plotted
          layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
          # vertex.frame.color='lightskyblue', 		#the color of the border of the dots
          # vertex.color= colattr,
          vertex.label.color='black',		#the color of the name labels
          vertex.label.font=1,    			#the font of the name labels
          vertex.size = input$cex2,     # size of the vertex
          vertex.label= make.names(V(graph())$name, unique = TRUE),	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
          vertex.label.cex= input$cex		#specifies the size of the font of the labels. can also be made to vary
          
    ) 
  }
})

output$graph3 <- renderUI({
  plot_output_list <- lapply(1:max(wc()$membership), function(i) {
    plotname <- paste("plot", i, sep="")
    plotOutput(plotname, height = 800, width = 800)
  })

  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})


for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot", my_i, sep="")
    
    output[[plotname]] <- renderPlot({
      
      adj.mat = Dataset()
      wc = wc()
      
      test = adj.mat[wc$membership == my_i,wc$membership == my_i]
      g = graph.adjacency(test, mode=input$mode)
      g = simplify(g)
      pct = round(length(wc$names[wc$membership == my_i])/length(wc$names)*100,2)
      
      graph = g
      
      # plot(adj.mat)
      # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
      plot( graph,			#the graph to be plotted
            layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
            vertex.frame.color='lightskyblue', 		#the color of the border of the dots
            #vertex.color= colattr,
            vertex.label.color='black',		#the color of the name labels
            vertex.label.font=1,    			#the font of the name labels
            vertex.size = input$cex2,     # size of the vertex
            vertex.label= make.names(V(graph)$name, unique = TRUE),	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
            vertex.label.cex=(degree(graph)+1)/mean(degree(graph)) * input$cex		#specifies the size of the font of the labels. can also be made to vary

      )

      
      
      title(paste("Community : ",my_i), sub = paste("Population share - ",pct,"%"))
      
      })
  })
}

output$downloadData1 <- downloadHandler(
  filename = function() { "Centralities.csv" },
  content = function(file) {
    write.csv(centralities(), file, row.names=F)
  }
)

})

#input = list(file = list(datapath = 'C:\\Users\\30773\\Desktop\\MKTR 2017\\friendship network adj mat mktr 2017.csv'),
#mode = 'directed')
