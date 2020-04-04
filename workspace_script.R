m = 17
s = 13
a = 1/2
b = 1/2
scenario = "match"
scenario = "switch different"
scenario = "switch same"

hA = sample(c(0,1), size = m, replace = TRUE)
hA = sample(c(0,1), size = m, replace = TRUE)

sA = tail(hB,s)
sB = tail(hA,s)

aA = ifelse(sum(sA/s) > a, 1, 0)
aB = ifelse(sum(sB/s) > b, 1, 0)

while (scenario == "match" & a1 != a2){
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
  if (aA == 1){ #player A will play 1 and player B will play 0
    #define k, j number of actions until player A, B's BR changes
    k = 
    j = 
  }
}

h1 = c(h1,a1)
h2 = c(h2,a2)

