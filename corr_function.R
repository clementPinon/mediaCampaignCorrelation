
corr <- function(file, channels,threshold = 0) {
  # file correspond à notre data.frame, csv macroreport par exemple
  # CSV file can be an export from GA API data query explorer
  # it should include ga:sourceMedium, ga:visits, ga:transactions, ga:date
  # channels correspond à un tableau répertoriant toutes les sources de trafic
  # Initialisation du vecteur dans lequel nous allons stocker les corrélations entre visites et transactions

  #NOTE: would work best if you compute the correlation between investments on a daily basis and transactions
  Correlation <- vector()
  typologie <- vector()
  
  for (i in 1:length(channels)) {
    # initialisation des totaux des visites et des transactions pour chaque canaux 
    nVisits <- NULL
    nTransactions <- NULL
    # création d'un subset de notre dataset
    Data <- subset(file, file$ga.sourceMedium == channels[i])
    # on indique ensutie le nombre total de visites dues à ce médium et le nombre total de transactions
    nVisits <- sum(Data$ga.visits)   
    nTransactions <- sum(Data$ga.transactions) 
    # on définit un threshold qui va conditionner le calcul de la corrélation sur la condition que le nombre de transaction générées par ce médium soit suffisant
    if(nTransactions > threshold ){
      Correlation <- c(Correlation, cor(Data$ga.visits,Data$ga.transactions))
    }
    else {Correlation <- c(Correlation, 0)}
    typologie <- rbind(typologie, c(Visits = nVisits, Transactions = nTransactions))
  }
  
  data.frame(channels, typologie, Correlation)
  
  ## Return a numeric vector of correlations
}