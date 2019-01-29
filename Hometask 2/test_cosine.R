rm(list = ls())

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/my_distfunc.R")

text1 <- "word1 word1 word2"
text2 <- "word2 word2"
text3 <- "word1 word2"

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/my_text_processing.R")

texts <- c(text1, text2, text3)

# Tokenization.
tokens <- tokenization(texts)
# Convert to document-term frequency matrix.
tokens_matrix <- dfm_matrix(tokens)
plot(c(0,max(tokens_matrix[,1])+1),c(0,max(tokens_matrix[,2])+1),main="Vector space representation of texts", xlab = "term1=word1", ylab = "term2=word2")
for(i in 1:length(texts)){
  a<-switch(i,"red","green","blue")
  arrows(0,0, tokens_matrix[i,1],tokens_matrix[i,2],lwd=4,col=a)
  text(x=tokens_matrix[i,1], y=tokens_matrix[i,2]+0.5, label=paste("text", i))
}

cos_text1_text2 <- my_dist_cosine(tokens_matrix[1,], tokens_matrix[2,])
cos_text1_text3 <- my_dist_cosine(tokens_matrix[1,], tokens_matrix[3,])
cos_text2_text3 <- my_dist_cosine(tokens_matrix[2,], tokens_matrix[3,])

angle_text1_text2 <- acos(cos_text1_text2) * 180 / pi
angle_text1_text3 <- acos(cos_text1_text3) * 180 / pi
angle_text2_text3 <- acos(cos_text2_text3) * 180 / pi

print(paste("Cosine distance between text1 and text2: ", cos_text1_text2, ", angle (degrees): ", angle_text1_text2))
print(paste("Cosine distance between text1 and text3: ", cos_text1_text3, ", angle (degrees): ", angle_text1_text3))
print(paste("Cosine distance between text2 and text3: ", cos_text2_text3, ", angle (degrees): ", angle_text2_text3))
