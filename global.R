max_plots = 30
# wc1 = function(n,mat1,mat2){
#   for (i in 1:ncol(mat2)) {
#     freq = as.matrix(mat1[(match(rownames(mat2[(mat2[,i] > 1),]),rownames(mat1))),][,i])
#     freq = as.matrix(freq[(order(freq[,1], decreasing=T)),])
#     top_word = as.matrix(freq[1:n,])
#     #mypath <- (paste("WC_mat2_theta_topic",i,"_",n, ".jpeg", sep = ""))
#     #jpeg(file = mypath, pointsize = 12,  width = 800, height = 800, quality=200)
#     wordcloud(rownames(top_word), top_word, scale = c(8, 1), 1, , random.order=FALSE, random.color=FALSE, colors=c(1:4));
#     dev.off()
#   }
# }




