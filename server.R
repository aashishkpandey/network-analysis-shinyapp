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

centralities = reactive({
  graph = graph()
  metrics <- data.frame(Resp.Name = make.names(V(graph)$name, unique = TRUE),Degree=degree(graph), Out.Degree =degree(graph, v=V(graph), mode=c("out")),In.Degree =degree(graph, v=V(graph), mode=c("in")),
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
  graph = graph()
  par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
  plot( graph,			#the graph to be plotted
        layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
        vertex.frame.color='blue', 		#the color of the border of the dots 
        vertex.color= Dataset2()[,input$colattr],
        vertex.label.color='black',		#the color of the name labels
        vertex.label.font=1,    			#the font of the name labels
        vertex.size = input$cex2,     # size of the vertex
        vertex.label= make.names(V(graph)$name, unique = TRUE),	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
        vertex.label.cex=input$cex		#specifies the size of the font of the labels. can also be made to vary

  ) 
  }
})

})

