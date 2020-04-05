#This script allows users to select paramaters for adaptive play and observe a possible path
#to a convention under no pertubations.

#Paramater selection
m = 17
s = 13
a = 1/2
b = 1/2
#scenario = "match"
scenario = "switch different"
#scenario = "switch same"

#randomly generating histories and taking most recent samples and calculating next actions
hA = sample(c(0,1), size = m, replace = TRUE)
hB = sample(c(0,1), size = m, replace = TRUE)

sA = tail(hB,s)
sB = tail(hA,s)

aA = ifelse(sum(sA/s) > a, 1, 0)
aB = ifelse(sum(sB/s) > b, 1, 0)

#randomly regenerating histories until one that is consistent with desired scenario is generated.
while (scenario == "match" & aA != aB){
  hA = sample(c(0,1), size = m, replace = TRUE)
  hB = sample(c(0,1), size = m, replace = TRUE)
  
  sA = tail(hB,s)
  sB = tail(hA,s)
  
  aA = ifelse(sum(sA/s) > a, 1, 0)
  aB = ifelse(sum(sB/s) > b, 1, 0)
} 

while (scenario == "switch different"){
  hA = sample(c(0,1), size = m, replace = TRUE)
  hB = sample(c(0,1), size = m, replace = TRUE)
  
  sA = tail(hB,s)
  sB = tail(hA,s)
  
  aA = ifelse(sum(sA/s) > a, 1, 0)
  aB = ifelse(sum(sB/s) > b, 1, 0)
  if (aA == aB) next
  #define rA and rB as the number of records in sample matching aA and aB respectively that need to be removed for the BR to change.
  rA = ifelse(aA == 1, ceiling((1-a)*s-sum(sA==0)), ceiling(a*s-sum(sA)))
  rB = ifelse(aB == 1, ceiling((1-a)*s-sum(sB==0)), ceiling(a*s-sum(sB)))
  #initialize k, j: the number of actions until player A, B's BR changes.
  k = rA
  j = rB
  while (sum(sA[1:k]==aA)!=rA){
    k = k+1
  }
  while (sum(sB[1:j]==aB)!=rB){
    j = j+1
  }
  if (j == k) break
}

while (scenario == "switch same"){
  hA = sample(c(0,1), size = m, replace = TRUE)
  hB = sample(c(0,1), size = m, replace = TRUE)
  
  sA = tail(hB,s)
  sB = tail(hA,s)
  
  aA = ifelse(sum(sA/s) > a, 1, 0)
  aB = ifelse(sum(sB/s) > b, 1, 0)
  if (aA == aB) next
  #define rA and rB as the number of records in sample matching aA and aB respectively that need to be removed for the BR to change.
  rA = ifelse(aA == 1, ceiling((1-a)*s-sum(sA==0)), ceiling(a*s-sum(sA)))
  rB = ifelse(aB == 1, ceiling((1-a)*s-sum(sB==0)), ceiling(a*s-sum(sB)))
  #initialize k, j: the number of actions until player A, B's BR changes.
  k = rA
  j = rB
  while (sum(sA[1:k]==aA)!=rA){
    k = k+1
  }
  while (sum(sB[1:j]==aB)!=rB){
    j = j+1
  }
  if (j == k) break
}

#selecting possible samples until a convention is achieved.
if (scenario != "switch same"){
  #If scenario does is not "switch same" we can ensure a convention is met by sampling
  #the most recent s records s+m times (s to ensure coordination, m to fill memory)
  i = 0
  while (i < (s+m)){
    sA = tail(hB,s)
    sB = tail(hA,s)
    
    aA = ifelse(sum(sA/s) > a, 1, 0)
    aB = ifelse(sum(sB/s) > b, 1, 0)
    
    hA = c(hA,aA)
    hB = c(hB,aB)
    
    print(tail(hA,m))
    print(tail(hB,m))
    i = i+1
  }
}


sA = tail(hB,s)
sB = tail(hA,s)

aA = ifelse(sum(sA/s) > a, 1, 0)
aB = ifelse(sum(sB/s) > b, 1, 0)

sA
sB
aA
aB

hA = c(hA,aA)
hB = c(hB,aB)

hA
hB

