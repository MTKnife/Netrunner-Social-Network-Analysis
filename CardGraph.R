#Initalize variables, including output data frames for cards (nodes) and edges.

#"Decks is the number of decks that a card appears in.
CardName <- vector(mode = "character", length = 0)
FactionVector <- c("Haas-Bioroid", "Jinteki", "NBN", "The Weyland Consortium",
  "Neutral")
Faction <- factor(vector(length = 0), levels = FactionVector)
Decks <- vector(mode = "integer", length = 0)
Cards <- data.frame(CardName, Faction, Decks, stringsAsFactors = FALSE)

#"Edges" link two cards that appear at least once in the same deck.  "Type" is
#  a column with all values as "Undirected", so that Gephi treats all edges in
#  the data as undirected.  "RawWeight" is the number of decks in which two
#  cards appear together, while "Weight" depends on the option chosen below.
Source <- vector(mode = "integer", length = 0)
Target <- vector(mode = "integer", length = 0)
Type <- factor(vector(length = 0), levels = "Undirected")
RawWeight <- vector(mode = "integer", length = 0)
Weight <- vector(mode = "numeric", length = 0)
Edges <- data.frame(Source, Target, Type, RawWeight, Weight)

#To analyze multiple factions at once, edit the CurrentFactionVector.
CurrentFactionVector <- c("Jinteki")
TruncPattern <- " \\(.*$"
TotalDecks <- as.integer(0)
FactionDecks <- vector(mode = "integer", length = length(CurrentFactionVector))
CardID <- as.integer(0)
EdgeID <- as.integer(0)

#Read in master card list and identify cards that can't be used out-of-faction.
MasterCardList <- 
  as.data.frame(read.csv("C:/text/data science/social network analysis/project/CorpMasterList.txt",
  as.is = "CardName"))
NoInfluenceVector <- MasterCardList[MasterCardList[, "NoInfluence"], "CardName"]

#Read in decks.  This code assumes that each faction's decks are stored in a
#  separate folder, with file names the same for all factions ("Deck???.txt").
#  For analyzing multiple factions at once, the fixed iteration values in the
#  "for" loop would have to be replaced with variables specific to each Corp
#  (the current ones represent the Jinteki decks between the releases of A Study
#  in Static and Humanity's Shadow).
for(FactionNumber in 1:length(CurrentFactionVector)) {
  CurrentFaction <- CurrentFactionVector[FactionNumber]
  for(DeckID in
      178:233) {
    CurrentDeckFile <- paste0("Deck",DeckID,".txt")
    CurrentDeckList <- 
      readLines(con=paste0("C:/text/data science/social network analysis/project/",
                CurrentFaction,"/",CurrentDeckFile), -1)

#Check for illegal decks.  Any value of "Illegal" greater than zero indicates an
#  illegal deck; incrementing the variable (rather than just setting "Illegal"
#  to zero if any illegal condition is met) assists with debugging.
    Illegal <- 0
    TotalCards <-  as.integer(gsub("\\D", "",
                              CurrentDeckList[pmatch("Total Cards",
                              CurrentDeckList)]))
    if(TotalCards < 45) {Illegal <- Illegal + 1}

#Note that the deckbuilding app has already done some of the work for us, by
#  calculating total influence for each faction.
    Influence <- 0
    for(InfluenceFactionNumber in 1:(length(FactionVector) - 1)) {
      CheckFaction = FactionVector[InfluenceFactionNumber]
      if(CheckFaction != CurrentFaction) {
        Influence <- Influence + as.integer(gsub("\\D", "",
                                            CurrentDeckList[pmatch(paste0(" ",
                                            CheckFaction), CurrentDeckList)]))
        }
      }
    if(Influence > 15) {Illegal <- Illegal + 1}
    AgendaPoints <- as.integer(gsub("\\D", "",
                               CurrentDeckList[pmatch("Total Agenda",
                               CurrentDeckList)]))
    if(AgendaPoints %/% 2 != TotalCards %/% 5 + 1) {Illegal <- Illegal + 1}
    IllegalList <- MasterCardList[MasterCardList[, "CardName"] %in%
                                  NoInfluenceVector & MasterCardList[,
                                  "Faction"] != CurrentFaction, "CardName"]
    if(sum(pmatch(IllegalList, CurrentDeckList, nomatch = 0)) > 0) {
      Illegal <- Illegal + 1
      }

#Optional filter to analyze only decks of a single identity.
#    if(grepl("Personal Evolution",CurrentDeckList[5])) {
#      Illegal <- Illegal +1
#      }

#Check to see if each line represents a card, extract card names, add cards to
#  node matrix, and create a list of all the cards in the current deck (in the
#  form of their indices in the node matrix).  In the original deck file, line
#  5 always contains the identity card, and the line beginning with "Total
#  Agenda Points" is always the first non-blank line after the last card.
#  Note that we can't easily use rbind() here because rbind() used on an empty
#  data frame changes the column names and data types (and we can't know before
#  reading in the decks how many rows the data frame will have).
    if(Illegal == 0) {
      TotalDecks <- TotalDecks + 1
      FactionDecks[FactionNumber] <- FactionDecks[FactionNumber] + 1
      CurrentCardsList <- NULL
      line <- 5
      while (!grepl("Total Agenda", CurrentDeckList[line])) {
        if(line == 5 | grepl("#", CurrentDeckList[line], fixed=TRUE)) {
          CurrentCardName <- sub(TruncPattern, "", CurrentDeckList[line])
          CurrentCardID <- which(Cards[, "CardName"] == CurrentCardName)
          if(length(CurrentCardID) == 1) {
            Cards[CurrentCardID, "Decks"] <- Cards[CurrentCardID, "Decks"] + 1
            }
          else {
            CardID <- CardID + 1
            CurrentCardID <- CardID
		Cards[CurrentCardID,] <- list(CurrentCardName,
                                          MasterCardList[MasterCardList[,
                                          "CardName"] == CurrentCardName,
                                          "Faction"], 1)
            }
          CurrentCardsList <- c(CurrentCardsList, CurrentCardID)
          }
        line <- line+1
        }

#Extract new edges and add to raw weights of previously extracted ones.
      DistinctCards <- length(CurrentCardsList)
      for(Card1 in 1:(DistinctCards-1)) {
        for(Card2 in (Card1+1):DistinctCards) {
          if(CurrentCardsList[Card1] < CurrentCardsList[Card2]) {
            SourceID <- CurrentCardsList[Card1]
            TargetID <- CurrentCardsList[Card2]
            }
          else {
            SourceID <- CurrentCardsList[Card2]
            TargetID <- CurrentCardsList[Card1]
            }
          if(nrow(Edges[Edges[, "Source"] == SourceID & Edges[, "Target"] ==
             TargetID,]) == 0) {
            EdgeID <- EdgeID + 1
            Edges[EdgeID,] <- list(SourceID, TargetID, "Undirected", 1, 0)
            }
          else {
            Edges[Edges[, "Source"] == SourceID & Edges[, "Target"] == TargetID,
                  "RawWeight"] <- Edges[Edges[, "Source"] == SourceID & Edges[,
                                        "Target"] == TargetID, "RawWeight"] + 1
            }
          }
        }
      print(paste0(CurrentFaction, " Deck", DeckID, " read."))
      }
    else {print(paste0(CurrentFaction, " Deck", DeckID, " is illegal."))}
    }
  }

#Below, I've included three different options for weighting the edges; unfor-
#  tunately, with Gephi it's impossible to put all three in the same
#  output file, because the column used to weight the edges has to be called
#  "Weight" (and even changing the name of a column in Gephi is a pain,
#  requiring the column to be copied to a new one with the new name).
#  Another option is to edit the script to include all three options, then
#  edit the column names in the output CSV, but this also requires editing
#  the initialization of the "Edges" data frame above, as well as the
#  initial assignment of values to the the data frame, and these changes must
#  be made every time a new output column is added.
PMI <- function(CurrentEdgeID) log10(Edges[CurrentEdgeID, "RawWeight"] /
                         Cards[Edges[CurrentEdgeID, "Source"], "Decks"] /
                         Cards[Edges[CurrentEdgeID, "Target"], "Decks"] *
                         TotalDecks)
SignifPMI <- function(CurrentEdgeID) if(ppois(Edges[CurrentEdgeID,
                                        "RawWeight"],(Cards[Edges[CurrentEdgeID,
                                        "Source"], "Decks"] *
                                        Cards[Edges[CurrentEdgeID,
                                        "Target"], "Decks"] / TotalDecks)) >
                                        0.90) PMI(CurrentEdgeID) else 0

#Option 1:  "Weight" is the same as "RawWeight", that is, the number of decks
#  in which the two cards appear together.
#Edges[, "Weight"] <- Edges[, "RawWeight"]

#Option 2:  "Weight" is the pointwise mutual information (PMI) of each edge.
#Edges[, "Weight"] <- apply(as.matrix(1:nrow(Edges)), 1, PMI)

#Option 3:  Check raw edge weight for statistical significance using the Poisson
#  distribution; set weights of significant edges to the PMI, and weights of non-
#  significant edges to 0.
Edges[, "Weight"] <- apply(as.matrix(1:nrow(Edges)), 1, SignifPMI)

#Print statistics on decks input.  You could use an "lapply" for the Faction
#  totals, but the output would be ugly.
print(paste0("Total Legal Decks Input:  ", TotalDecks))
for(FactionNumber in 1:(length(CurrentFactionVector))) {
	print(paste0("   ", CurrentFactionVector[FactionNumber], ":  ",
            FactionDecks[FactionNumber]))
	}

#Write cards (nodes) and edges output files.  The name of the "CardName" column
#  has to be changed to "Label" for Gephi to read it properly.
colnames(Cards) <- list("Label", "Faction", "Decks")
write.csv(Cards,
          file = "C:/text/data science/social network analysis/project/JintekiCardsNew.txt",
          quote = FALSE)
write.csv(Edges,
          file = "C:/text/data science/social network analysis/project/JintekiEdgesNew.txt",
          quote = FALSE)
