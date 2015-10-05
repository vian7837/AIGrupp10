emission_prob = function(){
  #   readings kommer fås av huvudfunktionen senare, här används testReadings,
  #   som är uppbyggd på samma sätt som readings, en lista med tre vektorer 
  testReadings = list(c(171),c(154),c(187))
  # probs kommer fås av huvudfunktionen senare, här anropas getProbs() 
  probs = getProbs()
  nitrogen = probs$nitrogen
  phosphate = probs$phosphate
  salinity = probs$salinity
  viableNodes = c()
  for (i in 1:40){
    if (as.integer(testReadings[1])-salinity[i,2] <= salinity[i,1] && 
        as.integer(testReadings[1])+salinity[i,2] >= salinity[i,1]) { 
      if (as.integer(testReadings[2])-phosphate[i,2] <= phosphate[i,1] &&
          as.integer(testReadings[2])+phosphate[i,2] >= phosphate[i,1]){
        if(as.integer(testReadings[3])-nitrogen[i,2] <= nitrogen[i,1] &&
           as.integer(testReadings[3])+nitrogen[i,2] >= nitrogen[i,1]) {
          viableNodes <- append(viableNodes, 1)
          #             print(paste("Node:",i))
          #             print(salinity[i,1])
          #             print(phosphate[i,1])
          #             print(nitrogen[i,1])
          #             print("----------")
          next()
          
        } 
      }
    }
    
    viableNodes <- append(viableNodes, 0)
    
  }
  
  return (viableNodes)
}