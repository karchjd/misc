library(perm)
typeIerrorsPerm <- 0
typeIerrorsWelch <- 0
reps <- 1000
n1 <- 10
sd1 <- 20

n2 <- 100
sd2 <- 1

Welch <- function(group1,group2){
  return((mean(group1)-mean(group2))/sqrt(var(group1)/length(group1)+var(group2)/length(group2)))
}

permTest <- function(group1,group2,permutations=1000){
  testStatistic <- Welch
  observed <- testStatistic(group1,group2)
  
  allValues <- c(group1,group2)
  groupMemberShips <- c(rep(TRUE,length(group1)),rep(FALSE,length(group2)))
  hypothetical <- rep(NA,permutations)
  for (i in 1:permutations){
    curGroupMembersShips <- sample(groupMemberShips)
    curGroup1 <- allValues[curGroupMembersShips]
    curGroup2 <- allValues[!curGroupMembersShips]
    hypothetical[i] <- testStatistic(curGroup1,curGroup2)
  }
  return(sum(observed<=hypothetical)/permutations)
}

for (i in 1:reps){ #repeat 1000 times
  group1 <- rnorm(n=n1,sd=sd1)
  group2 <- rnorm(n=n2,sd=sd2)
  
  permRes <- permTest(group1,group2)
  if (permRes<.05){
    typeIerrorsPerm <- typeIerrorsPerm+1
  }
  
  tTest <- t.test(group1,group2,var.equal = FALSE)$p.value
  if (tTest<.05){
    typeIerrorsWelch <- typeIerrorsWelch+1
  }
} #end of for
cat(sprintf('Type I error rate Permutation Welch: %.2f\n',typeIerrorsPerm/reps))
cat(sprintf('Type I error rate Welch: %.2f\n',typeIerrorsWelch/reps))





