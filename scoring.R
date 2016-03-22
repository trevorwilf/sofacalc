

########################################
## these functions are used to score 
########################################

sofascore <- function (FiO2 = NULL,
                       PaO2 = NULL,
                       Mechanicalventilation = NULL,
                       Platelets = NULL,
                       Bilirubin = NULL,
                       Glasgowcomascore = NULL,
                       MAP = NULL,
                       Vasopressors = NULL,
                       Dopamine = NULL,
                       Dobutamine = NULL,
                       Epinephrine = NULL,
                       Norepinephrine = NULL,
                       Creatinine = NULL, 
                       Urineoutputscore = NULL
                       ) {
  
  #FiO2PaO2ratio
  FiO2PaO2ratio <- 100*PaO2/FiO2
  #FiO2PaO2ratio <- 100*(as.numeric(PaO2)/as.numeric(FiO2))
  #FiO2PaO2ratio <- FiO2PaO2ratio*100
  #print(FiO2PaO2ratio)
  if (FiO2PaO2ratio > 400) {
    FiO2PaO2score <- 0
  }else{
    if (FiO2PaO2ratio > 300) {
      FiO2PaO2score <- 1
    }else{
      if (FiO2PaO2ratio > 200) {
        FiO2PaO2score <- 2
      }else{
        if(Mechanicalventilation){
          if (FiO2PaO2ratio > 100) {
            FiO2PaO2score <- 3
          }else{
            FiO2PaO2score <- 4
          }
        }else{
          FiO2PaO2score <- 2
        }
      }
    }
  }
  
  #Platelets
  if (Platelets > 150) {
    Plateletscore <- 0
  }else{
    if (Platelets > 100) {
      Plateletscore <- 1
    }else{
      if (Platelets > 50) {
        Plateletscore <- 2
      }else{
        if (Platelets > 20) {
          Plateletscore <- 3
        }else{
          Plateletscore <- 4
        }
      }
    }
  }
  
  #Bilirubin
  if (Bilirubin > 12) {
    Bilirubinscore <- 4
  }else{
    if (Bilirubin > 6) {
      Bilirubinscore <- 3
    }else{
      if (Bilirubin > 2) {
        Bilirubinscore <- 2
      }else{
        if (Bilirubin > 1.2) {
          Bilirubinscore <- 1
        }else{
          Bilirubinscore <- 0
        }
      }
    }
  }
  
  #Glasgowcomascore
  if (Glasgowcomascore > 14) {
    Glasgowcomascore1 <- 0
  }else{
    if (Glasgowcomascore > 13) {
      Glasgowcomascore1 <- 1
    }else{
      if (Glasgowcomascore > 10) {
        Glasgowcomascore1 <- 2
      }else{
        if (Glasgowcomascore > 6) {
          Glasgowcomascore1 <- 3
        }else{
          Glasgowcomascore1 <- 4
        }
      }
    }
  }
  
  #Creatinine/Urineoutput
    if ((Creatinine > 5) | (Urineoutputscore == 4)) {
      Creatininescore <- 4
    }else{
      if ((Creatinine > 3.5) | (Urineoutputscore == 3)) {
        Creatininescore <- 3
      }else{
        if (Creatinine > 2) {
          Creatininescore <- 2
        }else{
          if (Creatinine > 1.2) {
            Creatininescore <- 1
          }else{
            Creatininescore <- 0
          }
        }
      }
    }
  
  # MAP
  if (MAP < 70) {
    MAPScore <- 1
  }else{
    MAPScore <- 0
  }
  
  if (Vasopressors) {
    if (Dopamine <= 5 | Dobutamine > 0 ){
      MAPScore <- 2
    }
    
    if (Dopamine > 5 | Epinephrine <= 0.1 | Norepinephrine <= 0.1 ){
      MAPScore <- 3
    } 
    
    if (Dopamine > 15 | Epinephrine > 0.1 | Norepinephrine > 0.1 ){
      MAPScore <- 4
    } 
  }
  
  total <- FiO2PaO2score + Plateletscore + Bilirubinscore + Glasgowcomascore1 + Creatininescore + MAPScore
  score <- c(FiO2PaO2score, Plateletscore, Bilirubinscore, Glasgowcomascore1, Creatininescore, MAPScore, total)
  scorenames <- c("PaO2:FiO2 Ratio Score", "Platelets Score", "Bilirubin Score", 
                  "Glasgow Coma Score", "Creatinine/Urine Output Score", "MAP Score",
                  "Total")
  
  result <- data.frame(scorenames, score)
  
  tmp <- mortalityrate(total)
  #print(result)
  #print(tmp)
  result <- rbind(result, tmp)

  
  return(result)
}

mortalityrate <- function (sofascore = 0) {
  if (sofascore > 16) {
    mortality <- "> 90%"
    obsmortality <- 90
  }else{
    if (sofascore == 16) {
      mortality <- "> 90%"
      obsmortality <- 87.3
    }else{
      if (sofascore ==15) {
        mortality <- "> 80%"
        obsmortality <- 82
      }else{
        if (sofascore ==14) {
          mortality <- "50 - 60%"
          obsmortality <- 51.5
        }else{
          if (sofascore ==13) {
            mortality <- "50 - 60%"
            obsmortality <- 60
          }else{
            if (sofascore == 12) {
              mortality <- "40 - 50%"
              obsmortality <- 45.8
            }else{
              if (sofascore == 11) {
                mortality <- "40 - 50%"
                obsmortality <- 40
              }else{
                if (sofascore == 10) {
                  mortality <- "40 - 50%"
                  obsmortality <- 45.8
                }else{
                  if (sofascore == 9) {
                    mortality <- "15 - 20%"
                    obsmortality <- 22.5
                  }else{
                    if (sofascore == 8) {
                      mortality <- "15 - 20%"
                      obsmortality <- 22.5
                    }else{
                      if (sofascore == 7) {
                        mortality <- "15 - 20%"
                        obsmortality <- 15.3
                      }else{
                        if (sofascore == 6) {
                          mortality <- "< 10%"
                          obsmortality <- 4.5
                        }else{
                          if (sofascore == 5) {
                            mortality <- "< 10%"
                            obsmortality <- 10
                          }else{
                            if (sofascore == 4) {
                              mortality <- "< 10%"
                              obsmortality <- 7
                            }else{
                              if (sofascore == 3) {
                                mortality <- "< 10%"
                                obsmortality <- 3.3
                              }else{
                                if (sofascore == 2) {
                                  mortality <- "< 10%"
                                  obsmortality <- 3.8
                                }else{
                                  if (sofascore == 1) {
                                    mortality <- "< 10%"
                                    obsmortality <- 5.8
                                  }else{
                                    if (sofascore == 0) {
                                      mortality <- "< 10%"
                                      obsmortality <- 3.3
                                    }
                                  }
                                }
                              }
                            }
                          }                          
                        }
                      } 
                    } 
                  }  
                }  
              }  
            }  
          }  
        }
      }
    }
  }
  result <- c(mortality, obsmortality)
  result <- data.frame(c("Estimated mortality rate:", "Observed mortality rate:"), result)
  colnames(result) <- c("scorenames", "score")
  return(result)
  
}






