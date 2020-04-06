#This script allows users to select paramaters for adaptive play and observe a possible path
#to a convention under no pertubations.

#Paramater selection
m = 19
s = 18
a = 1/4
b = 2/3
#scenario = "match"
#scenario = "switch different"
scenario = "switch same"

#randomly generating histories and taking most recent samples and calculating next actions
hA = sample(c(0,1), size = m, replace = TRUE)
hB = sample(c(0,1), size = m, replace = TRUE)

sA = tail(hB,s)
sB = tail(hA,s)

aA = ifelse(sum(sA/s) >= a, 1, 0)
aB = ifelse(sum(sB/s) >= b, 1, 0)

#randomly regenerating histories until one that is consistent with desired scenario is generated.
while (scenario == "match" & aA != aB){
  hA = sample(c(0,1), size = m, replace = TRUE)
  hB = sample(c(0,1), size = m, replace = TRUE)
  
  sA = tail(hB,s)
  sB = tail(hA,s)
  
  aA = ifelse(sum(sA/s) >= a, 1, 0)
  aB = ifelse(sum(sB/s) >= b, 1, 0)
  
  #added j,k = 0 for appropriate evolution length
  j = 0
  k = 0
} 

while (scenario == "switch different"){
  hA = sample(c(0,1), size = m, replace = TRUE)
  hB = sample(c(0,1), size = m, replace = TRUE)
  
  sA = tail(hB,s)
  sB = tail(hA,s)
  
  aA = ifelse(sum(sA/s) >= a, 1, 0)
  aB = ifelse(sum(sB/s) >= b, 1, 0)
  
  #if aA == aB then the randomized history does not give us the desired scenario.
  if (aA == aB) next
  
  #define rA and rB as the number of records in sample matching aA and aB respectively
  #that need to be removed for the BR to change.
  rA = ifelse(aA == 1, ceiling((1-a)*s-sum(sA==0)), ceiling(a*s-sum(sA)))
  rB = ifelse(aB == 1, ceiling((1-b)*s-sum(sB==0)), ceiling(b*s-sum(sB)))
  
  #incase rA or rB = 0 (they are indifferent between actions) set required to 1
  #to avoid code complications and remove indifference.
  rA = max(rA,1)
  rB = max(rB,1)
  
  #initialize k, j: the number of actions until player A, B's BR changes.
  k = rA
  j = rB
  while (sum(sA[1:k]==aA)!=rA){
    k = k+1
  }
  while (sum(sB[1:j]==aB)!=rB){
    j = j+1
  }
  #if j != k then the radomized history gives us the desired scenario.
  if (j != k) break
}

while (scenario == "switch same"){
  hA = sample(c(0,1), size = m, replace = TRUE)
  hB = sample(c(0,1), size = m, replace = TRUE)
  
  sA = tail(hB,s)
  sB = tail(hA,s)
  
  aA = ifelse(sum(sA/s) >= a, 1, 0)
  aB = ifelse(sum(sB/s) >= b, 1, 0)
  
  #if aA == aB then the randomized history does not give us the desired scenario.
  if (aA == aB) next
  
  #define rA and rB as the number of records in sample matching aA and aB respectively
  #that need to be removed for the BR to change.
  rA = ifelse(aA == 1, ceiling((1-a)*s-sum(sA==0)), ceiling(a*s-sum(sA)))
  rB = ifelse(aB == 1, ceiling((1-b)*s-sum(sB==0)), ceiling(b*s-sum(sB)))
  
  #incase rA or rB = 0 (they are indifferent between actions) set required to 1
  #to avoid code complications and remove indifference.
  rA = max(rA,1)
  rB = max(rB,1)
  
  #initialize k, j: the number of actions until player A, B's BR changes.
  k = rA
  j = rB
  while (sum(sA[1:k]==aA)!=rA){
    k = k+1
  }
  while (sum(sB[1:j]==aB)!=rB){
    j = j+1
  }
  #if j == k then the radomized history gives us the desired scenario.
  if (j == k) break
}

#record the evolution of memory in data frames.
jk = min(j,k)
memory_slots = character(m)

for (i in 1:(m)){
  memory_slots[m+1-i] = (paste0("m",i))
}

hA_evo = data.frame(matrix(nrow = jk+m, ncol = m))
hB_evo = data.frame(matrix(nrow = jk+m, ncol = m))

colnames(hA_evo) = memory_slots
colnames(hB_evo) = memory_slots

#selecting possible samples until a convention is achieved.
if (scenario != "switch same"){
  #If scenario is not "switch same" we can ensure a convention is met by sampling
  #the most recent s records jk+m times (jk periods until coordination, m to fill memory).
  
  i = 0
  while (i < (jk+m)){
    i = i+1
    
    sA = tail(hB,s)
    sB = tail(hA,s)
    
    aA = ifelse(sum(sA/s) >= a, 1, 0)
    aB = ifelse(sum(sB/s) >= b, 1, 0)
    
    hA = c(hA,aA)
    hB = c(hB,aB)
    
    hA_evo[i,] = (tail(hA,m))
    hB_evo[i,] = (tail(hB,m))
  }
} else {
  #Scenario is "same switch". Since both best responses change after the same period
  #we simply need to change the way one player (A) samples to get them to coordinate.
  #We can reach a convention if the one player (B) samples the most recent s records
  #for jk+m periods and the other player (A) samples in following way:
  #1. For the first jk-1 periods sample the most recent s records
  #2. For periods jk through jk+s sample the most recent s+1 records except for the record
  #   created in period jk-1
  #3. For periods jk+s+1 through jk+m sample the most recent s records
  #This works because 2. stops player A from flipping their BR until their whole sample is
  #the same action.
  
  i = 0
  while (i < (jk+m)){
    i = i+1
    
    if (i > (jk-1) & i < (jk+s+1)){
      sA = tail(hB,s+1)[-(jk+s+1-i)]
    } else {
      sA = tail(hB,s)
    }
    sB = tail(hA,s)
    
    aA = ifelse(sum(sA/s) >= a, 1, 0)
    aB = ifelse(sum(sB/s) >= b, 1, 0)
    
    hA = c(hA,aA)
    hB = c(hB,aB)
    
    hA_evo[i,] = (tail(hA,m))
    hB_evo[i,] = (tail(hB,m))
  }
}

library(tidyverse)
library(ggplot2)
library(gganimate)


hA_evo_tib = as_tibble(hA_evo)
hA_evo_tib = hA_evo_tib %>%
  mutate(period = 1:(jk+m))
hA_evo_tib = gather(hA_evo_tib, position, record, memory_slots)
hA_evo_tib$position = as.numeric(sub(pattern = "m", replacement = "", x = hA_evo_tib$position))

hB_evo_tib = as_tibble(hB_evo)
hB_evo_tib = hB_evo_tib %>%
  mutate(period = 1:(jk+m))
hB_evo_tib = gather(hB_evo_tib, position, record, memory_slots)
hB_evo_tib$position = as.numeric(sub(pattern = "m", replacement = "", x = hB_evo_tib$position))


anim = ggplot() +
  geom_point(data = hA_evo_tib, aes(color = record, x = position, y = 1)) +
  geom_point(data = hB_evo_tib, aes(color = record, x = position, y = 0)) +
  scale_color_viridis_c(begin = .1, end = .9) +
  transition_time(period) +
  ylim(-10,11) +
  theme_void()

animate(anim, nframes = m+jk, fps = 3)



sA = tail(hB,s)
sB = tail(hA,s)

aA = ifelse(sum(sA/s) >= a, 1, 0)
aB = ifelse(sum(sB/s) >= b, 1, 0)

sA
sB
aA
aB

hA = c(hA,aA)
hB = c(hB,aB)

hA
hB

