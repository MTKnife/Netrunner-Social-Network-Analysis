#Initialize variables.
CardName <- vector(mode = "character", length = 0)
FactionVector <- c("Haas-Bioroid", "Jinteki", "NBN", "The Weyland Consortium", "Neutral")
Faction <- factor(vector(length = 0), levels = FactionVector)
NoInfluence <- vector(mode = "logical", length = 0)
Cards <- data.frame(CardName, Faction, NoInfluence, stringsAsFactors = FALSE)
TruncPattern <- " \\(.*$"
MasterID <- as.integer(0)
FactionInVector <- c("HB", "Jinteki", "NBN", "Weyland", "NeutralCorp")

#Read in faction lists.
for(ListID in 1:5) {
  CurrentListFile <- paste0(FactionInVector[ListID],"List.txt")
  CurrentDeckList <- 
    readLines(con=paste0("C:/text/data science/social network analysis/project/",
              CurrentListFile), -1)
  ListLength <- length(CurrentDeckList)

#Add each card to the master list.
  for(LocalID in 1:ListLength) {
    CurrentCardName <- sub(TruncPattern, "", CurrentDeckList[LocalID])
    Cards[MasterID + LocalID,] <- list(CurrentCardName, FactionVector[ListID],
                                       FALSE)
    }
  MasterID <- MasterID + ListLength
  }

#Mark cards with no Influence (that is, those that can't be used out-of-
#  faction).
NoInfluenceVector <- c("Accelerated Beta Test", "Mandatory Upgrades", "Project
                       Vitruvius", "Braintrust", "Fetal AI", "Nisei MK II",
                       "AstroScript Pilot Program", "Breaking News",
                       "Restructured Datapool", "Government Contracts",
                       "Hostile Takeover", "Posted Bounty", "Project Atlas")
for(NoInfluenceID in 1:length(NoInfluenceVector)) {
  Cards[Cards[,"CardName"] == NoInfluenceVector[NoInfluenceID], "NoInfluence"] <- TRUE
  }

#Write CorpMasterList.txt.
write.csv(Cards,
          file = "C:/text/data science/social network analysis/project/CorpMasterList.txt",
          row.names = FALSE)