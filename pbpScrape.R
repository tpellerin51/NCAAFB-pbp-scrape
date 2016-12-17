install.packages("jsonlite")
install.packages("stringdist")
library(jsonlite)
library(stringdist)

sampleJSON = "http://data.ncaa.com/game/football/fbs/2014/09/06/air-force-wyoming/pbp.json"
baseJSON = "http://data.ncaa.com/game/football/fbs/"

#list of all games (date and teams involved) in years of interest. should get online later
gamesList2014.2015 = read.csv() # year will be called 15 in /octonion. cant find right now :(
gamesList2015.2016 = read.csv()

# youll need to download this file
gamesList2016.2017 = read.csv("fbs2017.csv")

#eventualy this needs to get automated... find a website to scrape or some thing
# This funtion takes a season schedule and makes it have a nice to use table
formatGames = function(seasonSchedule1){
  #seasonSchedule = gamesList2016.2017
  seasonSchedule = cbind(seasonSchedule1, jsonDate=" ", jsonExtension = " ", HomeTeam = " ", AwayTeam=" ")
  
  date = unlist(str_split(seasonSchedule$Date, "/"))
  lendate = length(date)
  month = date[1:lendate%%3==1]
  day = date[1:lendate%%3==2]
  year = date[1:lendate%%3==0]
  
  #make sure month and day have correct number of characters
  for (i in 1:length(month)){
    if (nchar(month[i]) <=1)
      month[i] = paste("0", month[i],sep="")
    if (nchar(day[i]) <=1)
      day[i] = paste("0",day[i], sep ="")
    seasonSchedule$Date[i] = paste(day[i], month[i], year[i], sep = "/")
    seasonSchedule$jsonDate[i] = paste(year[i], month[i], day[i], sep = "/")
    
    #code home and away team. notice that we just base case neutral for now
    if(seasonSchedule$Location[i] == "H" || seasonSchedule$Location[i] == "N"){
      seasonSchedule$HomeTeam[i] = seasonSchedule$Team[i]
      seasonSchedule$AwayTeam[i] = seasonSchedule$Opp[i]
    }
    if(seasonSchedule$Location[i] == "A" ){
      seasonSchedule$HomeTeam[i] = seasonSchedule$Opp[i]
      seasonSchedule$AwayTeam[i] = seasonSchedule$Team[i]
    }
    
    #this begins formatting the home and away team names to get put into the json extension
    team1 = gsub(" ", "-", tolower(seasonSchedule$AwayTeam[i]))
    team2 = gsub(" ","-",tolower(seasonSchedule$HomeTeam[i]))
    
    # remove ampersand, parenthesis, periods from names
    if(grepl(".",team1) || grepl(".",team2)){
      team1 = gsub("[.]","",team1)
      team2 = gsub("[.]","",team2)
    }
    if(grepl("(",team1) || grepl("(",team2)){
      team1 = gsub("[(]","",gsub("[)]","",team1))
      team2 = gsub("[(]","",gsub("[)]", "-",team2))
    }
    if(grepl("&",team1) || grepl("&",team2)){
      team1 = gsub("[&]","",team1)
      team2 = gsub("[&]","",team2)
    }
    
    #build json extension.
    seasonSchedule$jsonExtension[i] =  paste(seasonSchedule$jsonDate[i],"/", team1,"-",team2,sep="")
    
    #check that we got the json extension right for the neutral site games. fix it if not
    if(seasonSchedule$Location[i] == "N"){
      if (url.exists(paste(baseJSON,seasonSchedule$jsonExtension[i], sep = ""))==FALSE){
        seasonSchedule$jsonExtension[i] =  paste(seasonSchedule$jsonDate[i],"/", team2,"-",team1,sep="")
      }
    }
  }
  
  return(seasonSchedule)
}

#gets and writes in a single game of data into a basic table. Needs unpacking
scrapePBPgame = function(jsonPath){
  fullGame = NULL
  json2 <- fromJSON(jsonPath)
  
  #loop through each quarter
  for (k in 1:4){
    fullDrive = NULL
    #Loop through each offensive drive or special teams play in the quarter
    for (i in 1:length(json2$periods$possessions[[k]]$plays)){
      driveChunk = cbind(offenseNCAAcode=json2$periods$possessions[[k]]$team[i],
                driveStartTime=json2$periods$possessions[[k]]$time[i],json2$periods$possessions[[k]]$plays[[i]])
      fullDrive = rbind(fullDrive,driveChunk)
    }
    gameChunk = cbind(Quarter = json2$periods$title[[k]],fullDrive)
    fullGame=rbind(fullGame,gameChunk)
  }
  #put on all the meta data
  jsonMeta = json2$meta$teams
  
  #initialize NULLs to append to
  HomeTeamCode = NULL
  AwayTeamCode = NULL
  offenseAbbr = NULL
  defenseAbbr = NULL
  for (i in 1:length(fullGame$Quarter)){
    #add in home and away team NCAA codes
    if(jsonMeta$homeTeam[1] == "true"){
      HomeTeamCode = c(HomeTeamCode, jsonMeta$id[1])
      AwayTeamCode = c(AwayTeamCode, jsonMeta$id[2])
    }
    if(jsonMeta$homeTeam[1] == "false"){
      HomeTeamCode = c(HomeTeamCode, jsonMeta$id[2])
      AwayTeamCode = c(AwayTeamCode, jsonMeta$id[1])
    }
    #add in offense and defense abbreviations... these are not same as the ones used by score reporterss.
    if(HomeTeamCode[i]==fullGame$offenseNCAAcode[i]){
      offenseAbbr = c(offenseAbbr,jsonMeta$sixCharAbbr[1])
      defenseAbbr = c(defenseAbbr,jsonMeta$sixCharAbbr[2])
    }
    if(AwayTeamCode[i]==fullGame$offenseNCAAcode[i]){
      offenseAbbr = c(offenseAbbr,jsonMeta$sixCharAbbr[2])
      defenseAbbr = c(defenseAbbr,jsonMeta$sixCharAbbr[1])
    }
  }
  #put these new columns on the table
  fullGame = cbind(HomeTeamCode, AwayTeamCode, offenseAbbr, defenseAbbr, fullGame)
  
  #fill in score for every play
  for (i in 2:length(fullGame$Quarter)){
    if (fullGame$visitingScore[i]==""){
      fullGame$visitingScore[i] = fullGame$visitingScore[i-1]
      fullGame$homeScore[i]=fullGame$homeScore[i-1]
    }
  }
  #split up drive text into down, distance and spot on the field
  allDriveText = fullGame$driveText
  
  #initialize null to append to
  down = NULL
  distance = NULL
  spot = NULL
  
  #make vectors of all data for each of down, distance and spot for each game.
  for (i in 1:length(allDriveText)){
    driveText = unlist(strsplit(allDriveText[i]," "))
    down = c(down,driveText[1])
    distance = c(distance,driveText[3])
    spot = c(spot,driveText[5])
  }
  fullGame = cbind(fullGame,down,distance,spot)
  
  return(fullGame)
}

wyomingAirForce = scrapePBPgame(sampleJSON)
emptyJSONscrape = fromJSON(baseJSON)

#this bit interprets the entry "spot" and adds a value to the table for distance to goal line
interpretSpot = function(gameTable){
  
  locations = gameTable$spot
  # set up nulls to append to
  teamCode = NULL
  spotOnHalf = NULL
  for (h in 1:length(locations)){
    code = NA
    halfSpot = NA
    loc = locations[h]
    if (is.na(loc)==FALSE){
      for (g in 1:nchar(loc)){
        #check to see if this character is a number
        if (grepl(unlist(strsplit(loc,""))[g],"1234567890")==TRUE){
          #if the gth character is numeric, split up the team code and distance to goal, break loop
          code = substr(loc, 1, g-1)
          halfSpot = substr(loc, g, nchar(loc))
          halfSpot = as.numeric(halfSpot)
          break
        }
      }
    }else{
    #making these NA since there was no value given
    code = NA
    halfSpot = NA
    }
    #put team code and spot on their half on the list.
    teamCode = c(teamCode, code)
    spotOnHalf = c(spotOnHalf, halfSpot)
  }
  #record the short code used by score reported
  mysteryCode1=teamCode[1]
  mysteryCode2=NULL
  for (q in 1:length(teamCode)){
    if (teamCode[q] != mysteryCode1){
      mysteryCode2 = teamCode[q]
      break
    }
  }
  #match the mystery code here to the short code provided
  specialCode1=NULL
  specialCode2=NULL
  #look at string distance between mystery code and offense 
  dist1 = stringdist(gameTable$offenseAbbr[1],mysteryCode1)
  dist2 = stringdist(gameTable$offenseAbbr[1],mysteryCode2)
  if (dist1 < dist2){
    specialCode1=mysteryCode1
    specialCode2=mysteryCode2
  } else{
    specialCode1=mysteryCode2
    specialCode1=mysteryCode1
  }
  # now, using the connection between the codes, make vector of linked codes 
  offenseShortCodes = NULL
  defenseShortCodes= NULL
  for (w in 1:length(locations)){
    offenseShort=NA
    defenseShort=NA
    if (is.na(locations[w])==FALSE){
      if (gameTable$offenseAbbr[w] ==specialCode1){
        offenseShort=specialCode1
        defenseShort=specialCode2
      } else{
        offenseShort=specialCode2
        defenseShort=specialCode1
      }
    }
    offenseShortCodes = c(offenseShortCodes,offenseShort)
    defenseShortCodes= c(defenseShortCodes, defenseShort)
  }
  distanceToGoal = NULL
  for (z in 1:length(locations)){
    distToGoal = NA
    if (is.na(teamCode[z])==FALSE){
      distToGoal = spotOnHalf[z]
      if(teamCode[z] == offenseShortCodes[z]){
        distToGoal = 100 - spotOnHalf[z]
      }
    }
    distanceToGoal = c(distanceToGoal,distToGoal)
  }
  
  tempTable = cbind(gameTable,distanceToGoal)
  return(tempTable)
}

tempSpotTable = interpretSpot(wyomingAirForce)

# Take the text about the play and find key word to record play type
addPlaytype = function(gameTable){
  #probably can just initialize play type to rush. these have the least info about them
  outputTable = cbind(gameTable, playType = "RUSH", extraPlayInfo = "")
  scoreText = outputTable$scoreText
  for (p in 1:length(scoreText)){
    playText = scoreText[p]
    if (grepl("[.]",playText))
      playText = gsub("[.]","", playText)
    playText = unlist(strsplit(tolower(scoreText[p])," "))
    
    #check to see if this is a pass, etc
    if (length(intersect(c("complete","complete.","incomplete", "incomplete.", "pass"),playText)>=1)){
      outputTable$playType[p] = "PASS"
    }
    if (length(intersect(c("run", "rush", "runs"),playText)>=1)){
      outputTable$playType[p] = "RUSH"
    }
    if (length(intersect(c("punt","punts"),playText)>=1)){
      outputTable$playType[p] = "PUNT"
    }
    if (length(intersect(c("field","goal","goal."),playText)>=2)){
      outputTable$playType[p] = "FIELD GOAL"
    }
    if (length(intersect(c("extra","point"),playText)>=2)){
      outputTable$playType[p] = "EXTRA POINT"
    }
    if (length(intersect(c("kicks","kickoff"),playText)>=1)){
      outputTable$playType[p] = "KICKOFF"
    }
    if (length(intersect(c("sack","sacked"),playText)>=1)){
      outputTable$playType[p] = "PASS"
      outputTable$extraPlayInfo[p] = "SACK"
    }
    if (length(intersect(c("safety"),playText)>=1)){
      outputTable$extraPlayInfo[p] = "SAFETY"
    }
    if (length(intersect(c("penalty"),playText)>=1)){
      outputTable$playType[p] = "PENALTY"
    }
    if (length(intersect(c("interception","intercepted"),playText)>=1)){
      outputTable$playType[p] = "PASS"
      outputTable$extraPlayInfo[p] = "INTERCEPTION"
    }
  }
  return(outputTable)
}

wyomingAirForce2 = addPlaytype(wyomingAirForce)

# parse json and fill in all information about a single game
doEntireGame = function(jsonPath){
  scrapedGame = scrapePBPgame(jsonPath)
  scrapedGame1 = interpretSpot(scrapedGame)
  scrapedGame2 = addPlaytype(scrapedGame1)
  return(scrapedGame2)
}

completedWyomingAirForce = doEntireGame(sampleJSON)

# This function takes a table of season match ups (teams and date) and
# returns a giant play by play table for the season
scrapePBPseason = function(scheduleTable){
  
  wholeSeason = NULL
  
  # loop through each game appending them to the end of the giant table
  for (k in 1:length(scheduleTable$jsonExtension)){
    useThisJSON = paste(baseJSON,scheduleTable$jsonExtension, sep ="")
    gametable = doEntireGame(useThisJSON)
    wholeSeason = rbind(wholeSeason,gametable)
  }
  return(wholeSeason)
}

formatted2016Season=NULL
#these wont work yet
#formatted2016Season = formatGames(gamesList2016.2017)
#formatted2015Season = formatGames(gamesList2015.2016) 
formatted2014Season = formatGames(gamesList2014.2015) 

EnireSeason2016.2017 = scrapePBPseason(formatted2016Season)