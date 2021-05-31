library(rjson)
library(varhandle)
library(jsonlite)

setwd("/Users/emilbreustedt/Desktop/R/exp/")
# inputs:
# "Ziel.csv" with column names and one empty row
# directory "input" with all csv of subjects

# outputs:
# "Ziel_neu.csv" with inserted data

#set counter to zero and clear dataset variable
rm(list = ls())

# loop through all files in input and create counter variable,
# increasing with each iteration

counter <- 0
for (data in list.files("./input/")) {
  print(data)
  counter <- counter + 1
  
  # get subject csv
  temp <-
    read.csv(paste("./input/", data, sep = ""), header = TRUE)
  
  # if dataframe not yet exists create it
  if (!exists("dataset")) {
    # create dataset
    dataset <-
      read.csv("Ziel.csv",
               sep = ";",
               header = T)
  }
  
  # add new row at counter position
  dataset[counter, 1] <- counter + 100 # ID
  dataset[counter, 2] <- substring(data, 33, 42) # Date
  
  # row numbers  in the responses column
  resrow <- c(5,
              rep(6, 4),
              rep(7, 4),
              rep(8, 3),
              rep(10, 7),
              rep(12, 5),
              rep(14, 9))
  
  # keys
  tag <- c(
    "Alter",
    "handedneß",
    "sex",
    "firstlanguage",
    "ethnic",
    "legasthenie",
    "dyskalkulie",
    "student",
    "beschäftigung",
    "bildung",
    "educationmath",
    "workmath",
    "GADa",
    "GADb",
    "GADc",
    "GADd",
    "GADe",
    "GADf",
    "GADg",
    "STAI1",
    "STAI2",
    "STAI3",
    "STAI4",
    "STAI5",
    "AMAS1",
    "AMAS2",
    "AMAS3",
    "AMAS4",
    "AMAS5",
    "AMAS6",
    "AMAS7",
    "AMAS8",
    "AMAS9"
  )
  
  # vectors for kontroll parameter
  kontroll <- c("ernsthaft", "Umgebung", "Gerät")
  kontrollnum <- c(195, 196, 197)
  
  # unfactorizing for indexing
  temp$rt <- unfactor(temp$rt)
  temp$key_press <- unfactor(temp$key_press)
  temp$type <- unfactor(temp$type)
  temp$operation <- unfactor(temp$operation)
  temp$consistency <- unfactor(temp$consistency)
  if(is.factor(temp$correct_response)){
    temp$correct_response <- unfactor(temp$correct_response)
  }

  # 01, 02, ... 28
  RTnum <- sprintf("%02d", c(1:28))
  
  count_col <- 0 # initalize counter for id columns
  
  # function for translating the key-presses to the numbers
  translate <- function(k1, k2) {
    if(!is.na(k1) && !is.na(k2))  {
    
      if (k1 == 48 || k1 == 96) {
      r1 <- 0
    } else if (k1 == 49 || k1 == 97) {
      r1 <- 1
    } else if (k1 == 50 || k1 == 98) {
      r1 <- 2
    } else if (k1 == 51 || k1 == 99) {
      r1 <- 3
    } else if (k1 == 52 || k1 == 100) {
      r1 <- 4
    } else if (k1 == 53 || k1 == 101) {
      r1 <- 5
    } else if (k1 == 54 || k1 == 102) {
      r1 <- 6
    } else if (k1 == 55 || k1 == 103) {
      r1 <- 7
    } else if (k1 == 56 || k1 == 104) {
      r1 <- 8
    } else {
      r1 <- 9
    }
    
    if (k2 == 48 || k2 == 96) {
      r2 <- 0
    } else if (k2 == 49 || k2 == 97) {
      r2 <- 1
    } else if (k2 == 50 || k2 == 98) {
      r2 <- 2
    } else if (k2 == 51 || k2 == 99) {
      r2 <- 3
    } else if (k2 == 52 || k2 == 100) {
      r2 <- 4
    } else if (k2 == 53 || k2 == 101) {
      r2 <- 5
    } else if (k2 == 54 || k2 == 102) {
      r2 <- 6
    } else if (k2 == 55 || k2 == 103) {
      r2 <- 7
    } else if (k2 == 56 || k2 == 104) {
      r2 <- 8
    } else {
      r2 <- 9
    }
    return(paste0(r1, r2)) 
    }
    else{
      return(NA)
    }
  }
  
  
  # iterating through number of rows to avoid exceptions for different lengths
  for (i in 3:nrow(temp)) {
    # getting the values of the keys after converting to JSON
    # columns 3 - 34
    if (i <= 34) {
      dataset[counter, i] <-
        fromJSON(toString(temp$responses[resrow[i - 2]]))[tag[i - 2]]
    }
    
    else if (i <= 82) {
      # assigning task_id's
      # columns 35 - 82
      # 6 columns for every task id
      
      # task id 15/25 seperation
      if (i == 59) {
        
        # write id 15(consistent)
        dataset[counter, 119] <-
          round(as.numeric(temp$rt[temp$task_id == (i - 34) &
        temp$consistency == "C"][1])) #time

dataset[counter, 120] <- translate( # key_press
  temp$key_press[temp$task_id == (i - 34) &
                   temp$consistency == "C"][1], 
  temp$key_press[temp$task_id == (i - 34) &
                  temp$consistency == "C"][2])

dataset[counter, 121] <-
  temp$type[temp$task_id == (i - 34) &
              temp$consistency == "C"][1]# type

dataset[counter, 122] <-
  temp$operation[temp$task_id == (i - 34) &
                   temp$consistency == "C"][1] # operation

dataset[counter, 123] <-
  temp$consistency[temp$task_id == (i - 34) &
                     temp$consistency == "C"][1] # consistency

dataset[counter, 124] <-
  temp$correct_response[temp$task_id == (i - 34) &
                          temp$consistency == "C"][1] # correct_response

# write id 25(inconsistent)
dataset[counter, i + count_col] <-
  round(as.numeric(temp$rt[temp$task_id == (i - 34) &
temp$consistency == "I"][1])) #time

dataset[counter, i + count_col + 1] <- translate( # key_press
  temp$key_press[temp$task_id == (i - 34) &
                   temp$consistency == "I"][1], 
  temp$key_press[temp$task_id == (i - 34) &
                   temp$consistency == "I"][2])

dataset[counter, i + count_col + 2] <-
  temp$type[temp$task_id == (i - 34) &
              temp$consistency == "I"][1]# type

dataset[counter, i + count_col + 3] <-
  temp$operation[temp$task_id == (i - 34) &
                   temp$consistency == "I"][1] # operation

dataset[counter, i + count_col + 4] <-
  temp$consistency[temp$task_id == (i - 34) &
                     temp$consistency == "I"][1] # consistency

dataset[counter, i + count_col + 5] <-
  temp$correct_response[temp$task_id == (i - 34) &
                          temp$consistency == "I"][1] # correct_response
      }
      
      else{
        # get calculate time with time_elapsed - rt
        dataset[counter, i + count_col] <- # time
          round(as.numeric(temp$rt[temp$task_id == (i - 34)][!is.na(temp$rt[temp$task_id == (i - 34)])][1]))
        
        dataset[counter, i + count_col + 1] <- translate( # key_press
          temp$key_press[temp$task_id == (i - 34)][!is.na(temp$rt[temp$task_id == (i - 34)])][1],
          temp$key_press[temp$task_id == (i - 34)][!is.na(temp$rt[temp$task_id == (i - 34)])][2])
          
        dataset[counter, i + count_col + 2] <- # type
          temp$type[temp$task_id == (i - 34)][!is.na(temp$rt[temp$task_id == (i - 34)])][1]
        
        dataset[counter, i + count_col + 3] <- # operation
          temp$operation[temp$task_id == (i - 34)][!is.na(temp$rt[temp$task_id == (i - 34)])][1]
        
        dataset[counter, i + count_col + 4] <- # consistency
          temp$consistency[temp$task_id == (i - 34)][!is.na(temp$rt[temp$task_id == (i - 34)])][1]
        
        dataset[counter, i + count_col + 5] <- # correct_response
          temp$correct_response[temp$task_id == (i - 34)][!is.na(temp$rt[temp$task_id == (i - 34)])][1]
      }
      
      # counting up columns of each ID
      count_col = count_col + 5
    }
    else if (i <= 110) {
      # RT02
      # columns 83 - 110
      # if RT's available and not empty
      if (validate(toString(temp$responses[163]))
          &&
          fromJSON(toString(temp$responses[163]))[paste("RT02_", RTnum[i -
                                                                       82], sep = "")] != "") {
        # write data
        dataset[counter, i + 240] <-
          fromJSON(toString(temp$responses[163]))[paste("RT02_", RTnum[i - 82], sep = "")]
      }
      #else NA
      else{
        dataset[counter, i + 240] <- NA
      }
    }
    else if (i <= 138) {
      # RT03
      # columns 111 - 138
      # if RT's available and not empty
      if (validate(toString(temp$responses[165]))
          &&
          fromJSON(toString(temp$responses[165]))[paste("RT03_", RTnum[i -
                                                                       110], sep = "")] != "") {
        # write data
        dataset[counter, i + 240] <-
          fromJSON(toString(temp$responses[165]))[paste("RT03_", RTnum[i -
                                                                         110], sep = "")]
      }
      #else NA
      else{
        dataset[counter, i + 240] <- NA
      }
    }
    else if (i <= 194) {
      # readingtest
      # columns 139 - 194
      # if value exist´write
      if (!is.null(temp$button_pressed[i + 28])) {
        dataset[counter, i + 240] <-
          temp$button_pressed[i + 28]
      }
      else{
        #else NA
        # get results
        dataset[counter, i + 240] <- NA
      }
    }
    
    # last 3 rows -> Kontroll
    if (i >= nrow(temp) - 2) {
      # if Kontroll is available and not empty
      if (validate(toString(temp$responses[nrow(temp) - 2]))
          &&
          !is.null(fromJSON(toString(temp$responses[nrow(temp) - 2]))[[kontroll[nrow(temp) + 1 - i]]])) {
        # write kontroll in 195, 196, 197
        dataset[counter, kontrollnum[nrow(temp) + 1 - i] + 240] <-
          fromJSON(toString(temp$responses[nrow(temp) - 2]))[kontroll[nrow(temp) + 1 - i]]
      }
      #else NA
      else{
        dataset[counter, (195 + 240):(197 + 240)] <- NA
      }
    }
  }
  
  # cleaning tempprary and running variables
  rm(i, temp, count_col)
}

# dataset now contains all observations for all participants, participant number
# for each observation is now stored in "
write.csv(dataset,
          "Ziel_vollständi_gerundet.csv",
          row.names = FALSE)
