########################################
#Calculating Linguistic Style Matching #
#            Bei Yan                   #
########################################

# Linguistic style matching (LSM,Gonzales, Hancock and Pennebaker,2009) 
# uses 9 categories of function words calculated using LIWC:
# ppron	ipron	article	prep	auxverb	adverb	conj negate quant
# to calculate the metric we also need WC (wordcount)

#First,create an empty dataframe to write the calculated LSM
LSM <- data.frame(matrix(ncol = 2, nrow = 0)) 
colnames(LSM) <- c("groupId","LSM")

#get the variables names of the function words
words <- colnames(yourdata)["position of the 9 function words"]
#get the group IDs in your data
groupId <- unique(yourdata$groupId)

#calculating LSM
for (i in groupId){
  #extract member data in each group
  group_data <- yourdata[yourdata$groupId==i,] 
  total_WC <- sum(group_data$WC)
  p_m_g <- 0 
  for (word in words){
    function_word <- group_data[word]
    total_function_word <- sum(function_word)
    p_m_word <- 0
    # n below is the total number of members in your group
    for (p in 1:n) {
      p_function_word <- function_word[p,]
      p_WC <- group_data$WC[p]
      p_prop <-  p_function_word/p_WC
      non_p_prop <- (total_function_word - p_function_word)/(total_WC-p_WC)
      p_prop_m <- 1-abs(p_prop-non_p_prop)/(p_prop+non_p_prop)
      p_m_word <- p_m_word+p_prop_m
    }
    p_m_word_g <- p_m_word/n
    p_m_g <- p_m_g + p_m_word_g
  }
  LSM_G <- p_m_g/9 
  group_LSM <- data.frame(i,LSM_G)
  colnames(group_LSM)<- c("groupId","LSM")
  LSM <- rbind(LSM,group_LSM)
}
View(LSM)
write.csv(LSM,"LSM,csv")