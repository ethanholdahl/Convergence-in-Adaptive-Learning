#This script allows users to select parameters for adaptive play and observe a possible path
#to a convention under no perturbations.

#load required libraries for the visualization
library("tidyverse")

#Parameter selection
m = 13
s = 11
#alpha and beta
a = .6
b = .4
#scenario = "Match"
#scenario = "Switch Different"
#scenario = "Switch Same"
scenario = "Random"

makeAnimation = function(m, s, a, b, scenario) {
  #randomly generating histories
  hA = sample(c(0, 1), size = m, replace = TRUE)
  hB = sample(c(0, 1), size = m, replace = TRUE)
  
  #sampling most recent s records
  sA = tail(hB, s)
  sB = tail(hA, s)
  
  #calculating best response (choosing action 1 if not unique)
  aA = ifelse(sum(sA / s) >= a, 1, 0)
  aB = ifelse(sum(sB / s) >= b, 1, 0)
  
  
  #randomly regenerating histories until one that is consistent with desired scenario is generated.
  while (scenario == "Match" & aA != aB) {
    hA = sample(c(0, 1), size = m, replace = TRUE)
    hB = sample(c(0, 1), size = m, replace = TRUE)
    
    sA = tail(hB, s)
    sB = tail(hA, s)
    
    aA = ifelse(sum(sA / s) >= a, 1, 0)
    aB = ifelse(sum(sB / s) >= b, 1, 0)
  }
  
  while (scenario == "Switch Different") {
    hA = sample(c(0, 1), size = m, replace = TRUE)
    hB = sample(c(0, 1), size = m, replace = TRUE)
    
    sA = tail(hB, s)
    sB = tail(hA, s)
    
    aA = ifelse(sum(sA / s) >= a, 1, 0)
    aB = ifelse(sum(sB / s) >= b, 1, 0)
    
    #if aA == aB then the randomized history does not give us the desired scenario.
    if (aA == aB)
      next
    
    #define rA and rB as the number of records in sample matching aA and aB respectively
    #that need to be removed for the BR to change.
    rA = ifelse(aA == 1, ceiling((1 - a) * s - sum(sA == 0)), ceiling(a *
                                                                        s - sum(sA)))
    rB = ifelse(aB == 1, ceiling((1 - b) * s - sum(sB == 0)), ceiling(b *
                                                                        s - sum(sB)))
    
    #in case rA or rB = 0 (they are indifferent between actions) set required to 1
    #to avoid code complications and remove indifference.
    rA = max(rA, 1)
    rB = max(rB, 1)
    
    #initialize k, j: the number of actions until player A, B's BR changes.
    k = rA
    j = rB
    while (sum(sA[1:k] == aA) != rA) {
      k = k + 1
    }
    while (sum(sB[1:j] == aB) != rB) {
      j = j + 1
    }
    #if j != k then the randomized history gives us the desired scenario. Exit generation
    if (j != k)
      break
  }
  
  while (scenario == "Switch Same") {
    hA = sample(c(0, 1), size = m, replace = TRUE)
    hB = sample(c(0, 1), size = m, replace = TRUE)
    
    sA = tail(hB, s)
    sB = tail(hA, s)
    
    aA = ifelse(sum(sA / s) >= a, 1, 0)
    aB = ifelse(sum(sB / s) >= b, 1, 0)
    
    #if aA == aB then the randomized history does not give us the desired scenario.
    if (aA == aB){
      next
    }
    
    #define rA and rB as the number of records in sample matching aA and aB respectively
    #that need to be removed for the BR to change.
    rA = ifelse(aA == 1, ceiling((1 - a) * s - sum(sA == 0)), ceiling(a *
                                                                        s - sum(sA)))
    rB = ifelse(aB == 1, ceiling((1 - b) * s - sum(sB == 0)), ceiling(b *
                                                                        s - sum(sB)))
    
    #incase rA or rB = 0 (they are indifferent between actions) set required to 1
    #to avoid code complications and remove indifference.
    rA = max(rA, 1)
    rB = max(rB, 1)
    
    #initialize k, j: the number of actions until player A, B's BR changes.
    k = rA
    j = rB
    while (sum(sA[1:k] == aA) != rA) {
      k = k + 1
    }
    while (sum(sB[1:j] == aB) != rB) {
      j = j + 1
    }
    #if j == k then the randomized history gives us the desired scenario. Exit generation
    if (j == k){
      break
    }
  }
  
  while (scenario == "Random") {
    #Identify the scenario randomly generated and reassign the scenario variable.
    #if aA == aB then the randomized history is that of a "Match" scenario.
    if (aA == aB) {
      scenario = "Match"
      next
    }
    
    #define rA and rB as the number of records in sample matching aA and aB respectively
    #that need to be removed for the BR to change.
    rA = ifelse(aA == 1, ceiling((1 - a) * s - sum(sA == 0)), ceiling(a *
                                                                        s - sum(sA)))
    rB = ifelse(aB == 1, ceiling((1 - b) * s - sum(sB == 0)), ceiling(b *
                                                                        s - sum(sB)))
    
    #in case rA or rB = 0 (they are indifferent between actions) set required to 1
    #to avoid code complications and remove indifference.
    rA = max(rA, 1)
    rB = max(rB, 1)
    
    #initialize k, j: the number of actions until player A, B's BR changes.
    k = rA
    j = rB
    while (sum(sA[1:k] == aA) != rA) {
      k = k + 1
    }
    while (sum(sB[1:j] == aB) != rB) {
      j = j + 1
    }
    #if j != k then the randomized history is that of a "Switch Different" scenario.
    #Otherwise, j == k and it is that of a "Switch Same" scenario.
    if (j != k){
      scenario = "Switch Different"
    } else {
      scenario = "Switch Same"
    }
  }
  
  if (scenario == "Match") {
    #added j,k = 0 for appropriate evolution length
    j = 0
    k = 0
  }
  
  
  
  #record the evolution of memory in data frames.
  #jk = the period at which the first best response changes
  jk = min(j, k)
  memory_slots = character(m)
  
  #create memory slot names for tibble
  for (i in 1:(m)) {
    memory_slots[m + 1 - i] = (paste0("m", i))
  }
  
  #initialize data frame for history
  hA_evo = data.frame(matrix(nrow = jk + m + 1, ncol = m))
  hB_evo = data.frame(matrix(nrow = jk + m + 1, ncol = m))
  
  #assign column names
  colnames(hA_evo) = memory_slots
  colnames(hB_evo) = memory_slots
  
  #selecting possible samples until a convention is achieved.
  if (scenario != "Switch Same") {
    #If scenario is not "Switch Same" we can ensure a convention is met by sampling
    #the most recent s records jk+m times (jk periods until coordination, m to fill memory).
    
    
    i = -1
    while (i < (jk + m)) {
      #record starting history as period 0
      i = i + 1
      hA_evo[(i+1),] = (tail(hA, m))
      hB_evo[(i+1),] = (tail(hB, m))
      
      #take new sample of most recent s records
      sA = tail(hB, s)
      sB = tail(hA, s)
      
      #calculate BR
      aA = ifelse(sum(sA / s) >= a, 1, 0)
      aB = ifelse(sum(sB / s) >= b, 1, 0)
      
      #insert BR into history
      hA = c(hA, aA)
      hB = c(hB, aB)
    }
  } else {
    #Scenario is "Switch Same". Since both best responses change after the same period
    #we simply need to change the way one player (A) samples to get them to coordinate.
    #We can reach a convention if the one player (B) samples the most recent s records
    #for jk+m periods and the other player (A) samples in following way:
    #1. For the first jk-1 periods sample the most recent s records
    #2. For periods jk through jk+s sample the most recent s+1 records except for the record
    #   created in period jk-1
    #3. For periods jk+s+1 through jk+m sample the most recent s records
    #This works because 2. stops player A from flipping their BR until their whole sample is
    #the same action.
    
    i = -1
    while (i < (jk + m)) {
      #record starting history as period 0
      i = i + 1
      
      hA_evo[(i+1),] = (tail(hA, m))
      hB_evo[(i+1),] = (tail(hB, m))
      
      if (i > (jk - 1) & i < (jk + s + 1)) {
        sA = tail(hB, s + 1)[-(jk + s + 1 - i)]
      } else {
        sA = tail(hB, s)
      }
      sB = tail(hA, s)
      
      aA = ifelse(sum(sA / s) >= a, 1, 0)
      aB = ifelse(sum(sB / s) >= b, 1, 0)
      
      hA = c(hA, aA)
      hB = c(hB, aB)
    }
  }
  
  #Transform evolution of histories to long form data for plotting
  hA_evo_tib = as_tibble(hA_evo)
  #assign periods based on row position
  hA_evo_tib = hA_evo_tib %>%
    mutate(period = 0:(jk + m))
  #transform to long form data
  hA_evo_tib = gather(hA_evo_tib, position, record, all_of(memory_slots))
  #transform memory slot name to numeric position
  hA_evo_tib$position = as.numeric(sub(
    pattern = "m",
    replacement = "",
    x = hA_evo_tib$position
  ))
  #assign sample dummy to 1 is in most recent s records in a given period 
  hA_evo_tib = hA_evo_tib %>%
    mutate(sample = ifelse(position <= s, 1, 0))
  hA_evo_tib$sample = factor(hA_evo_tib$sample)
  hA_evo_tib$record = factor(hA_evo_tib$record)
  
  
  #Transform evolution of histories to long form data for plotting
  hB_evo_tib = as_tibble(hB_evo)
  #assign periods based on row position
  hB_evo_tib = hB_evo_tib %>%
    mutate(period = 0:(jk + m))
  #transform to long form data
  hB_evo_tib = gather(hB_evo_tib, position, record, all_of(memory_slots))
  #transform memory slot name to numeric position
  hB_evo_tib$position = as.numeric(sub(
    pattern = "m",
    replacement = "",
    x = hB_evo_tib$position
  ))
  #assign sample dummy to 1 is in most recent s records in a given period 
  hB_evo_tib = hB_evo_tib %>%
    mutate(sample = ifelse(position <= s, 1, 0))
  
  
  if (scenario == "Switch Same") {
    #apply special sampling rules as outlined above
    #add position s+1 to sample for periods jk through jk+s
    hB_evo_tib$sample[((jk + m + 1) * (m - s - 1) + jk + 1):((jk + m + 1) * (m - s - 1) + jk + s + 1)] = 1
    #remove the record created in period jk-1 in periods jk through jk+s
    hB_evo_tib$sample[((jk + m + 1) * (m - s - 1) + jk + s + 1) + (jk + m) * (0:s)] = 0
  }
  hB_evo_tib$sample = factor(hB_evo_tib$sample)
  hB_evo_tib$record = factor(hB_evo_tib$record)
  
  
  animation = list()
  for(i in 0:(jk + m)) {
    #filter data by current period, extract number of each action in each player sample.
    hA_evo_tib_i = hA_evo_tib %>%
      filter(period == i)
    PBcounts = hA_evo_tib_i %>%
      filter(sample == 1)
    PBcount1 = as.numeric(sum(PBcounts$record ==1))
    PBcount0 = as.numeric(sum(PBcounts$record ==0))
    
    
    hB_evo_tib_i = hB_evo_tib %>%
      filter(period == i)
    PAcounts = hB_evo_tib_i %>%
      filter(sample == 1)
    PAcount1 = as.numeric(sum(PAcounts$record ==1))
    PAcount0 = as.numeric(sum(PAcounts$record ==0))
    
    anim = ggplot() +
      geom_point(
        data = hA_evo_tib_i,
        shape = 22,
        stroke = 2,
        size = 5,
        aes(
          fill = record,
          x = -position,
          y = 0,
          color = sample
        )
      ) +
      geom_point(
        data = hB_evo_tib_i,
        shape = 22,
        stroke = 2,
        size = 5,
        aes(
          fill = record,
          x = -position,
          y = 1,
          color = sample
        )
      ) +
      scale_fill_manual(values = c("1" = "yellow",
                                   "0" = "blue")) +
      scale_color_manual(values = c("1" = "red",
                                    "0" = "black"),
                         labels = c("1" = "Yes", "0" = "No")) +
      ylim(-8, 9) +
      xlim(-1.35 * m - 3, 1) +
      theme_void() +
      annotate(
        "text",
        x = -1.2 * m - 2,
        y = 1,
        size = 5,
        label = "Player A Memory (Player B History)",
        color = "black"
      ) +
      annotate(
        "text",
        x = -1.2 * m - 2,
        y = 0,
        size = 5,
        label = "Player B Memory (Player A History)",
        color = "black"
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = 8,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "yellow"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = 8,
        size = 5,
        label = paste0("Needed to make 1 a BR for Player A: ", ceiling(a*s)),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = 7,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "blue"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = 7,
        size = 5,
        label = paste0("Needed to make 0 a BR for Player A: ", ceiling((1-a)*s)),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = 6,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "yellow"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = 6,
        size = 5,
        label = paste0("Count: ", PAcount1),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = 5,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "blue"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = 5,
        size = 5,
        label = paste0("Count: ", PAcount0),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "text",
        x = -.55 * m,
        y = 4,
        size = 5,
        label = "Best Response for Player A:",
        color = "black",
        hjust = "right"
      ) +
      annotate(
        "point",
        x = -.5 * m,
        y = 4,
        shape = 22,
        stroke = 1,
        size = 5,
        color = "black",
        fill = ifelse(PAcount1 >= a*s, "yellow", "blue")
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = -3,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "yellow"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = -3,
        size = 5,
        label = paste0("Needed to make 1 a BR for Player B: ", ceiling(b*s)),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = -4,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "blue"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = -4,
        size = 5,
        label = paste0("Needed to make 0 a BR for Player B: ", ceiling((1-b)*s)),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = -5,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "yellow"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = -5,
        size = 5,
        label = paste0("Count: ", PBcount1),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "point",
        x = -1.18 * m - 1.92,
        y = -6,
        shape = 22,
        stroke = 2,
        size = 5,
        color = "red",
        fill = "blue"
      ) +
      annotate(
        "text",
        x = -1.13 * m - 1.82,
        y = -6,
        size = 5,
        label = paste0("Count: ", PBcount0),
        color = "black",
        hjust = "left"
      ) +
      annotate(
        "text",
        x = -.55 * m,
        y = -7,
        size = 5,
        label = "Best Response for Player B:",
        color = "black",
        hjust = "right"
      ) +
      annotate(
        "point",
        x = -.5 * m,
        y = -7,
        shape = 22,
        stroke = 1,
        size = 5,
        color = "black",
        fill = ifelse(PBcount1 >= b*s, "yellow", "blue")
      )
    animation[[(i+1)]] = anim 
  }
  
  return(animation)
}


test = makeAnimation(11, 9, a,b,"Switch Same")
test[[1]]
test[[2]]
test[[3]]
test[[4]]
test[[5]]
test[[6]]
test[[7]]
