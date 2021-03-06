## Netrunner-Social-Network-Analysis
*A class project to show which cards tends to be used together in Netrunner decks*

# What Is This About?

My coding at work (all Python at the moment) is entirely proprietary, and therefore I can't display it here.  Some of my hobbies are quite technical (lately, for instance, I've been learning Blender), but I prefer not to bring my work home with me.  The upshot is that I don't do a lot of data science coding that I can put up on GitHub.  Therefore, I've decided to put up this project I created for a class, Coursera's Social Network Analysis (https://www.coursera.org/course/sna), which is where I first learned R.

For the course's final project, I decided to look at decks in the game Netrunner (https://www.fantasyflightgames.com/en/products/android-netrunner-the-card-game/), a game in which two opponents, the "Corp" and the "Runner", build decks from a variety of available cards, and then pit them against one another.  The goal of the proejct was to see which cards tended to be used together, somewhat which might lend insights into deck-building strategy and players; psychology.  (For the sake of full disclosure, I need to admit that, despite some trying, I never managed actually to win a game of Netrunner, though normally I'm pretty good at games.)

# The Details

Let me say, up front, that there were easier ways to download decks and compile card lists.  I later mended my ways and implemented those easier approaches in a second version of the project, stored in its own repo, https://github.com/MTKnife/Netrunner-Social-Network-Analysis-2 (but, alas, never finished).

First, I had to get a hold of as many decks as possible.  I located the most popular repository of decks created by players, on CardGameDB (http://www.cardgamedb.com/index.php/netrunner/android-netrunner-submitted-decks).  To keep things simple, I limited myself to one Corp faction, Jinteki, and then, not knowing anything about web-scraping, I manually downloaded all the decks that had been uploaded for that faction.

I then wrote a script, MakeCorpMasterList.R, that used those decks (and a bit of other information) to compose a list of all the cards used in any of the decks.

Next, I wrote a script, CardGraph.R, that created a dataframe of cards from the master list, and ran through each of the Jinteki decks (excluding those that didn't follow the game rules) to find out which cards appeared in the same decks, and how often.  The dataframe represents the relationship between each pair of cards as a graph edge, with three different options for representing the strength of the edge.  In the most useful of the three, I calculated the pointwise mutual information coefficient (used often in natural language processing, among other domains), which expresses the co-occurence of two items ("vertices" in graph terms) by the ratio of the frequency that two items occur together over the frequency that that should appear together randomly, given the separate frequencies of each item--that is, two cards that don't show up very often in decks can have a very high PMI if they mostly show up together.  Though one option just saved all PMI's, the option used in the graphs saved only the edges with PMI's that were staistically significant (p > .05), which made for visualizations that were both cleaner and more meaningful.

Finally, I read the graph data into Gephi (http://gephi.github.io/), and used the Force Atlas 2 algorithm to create the visualizations you can see in the "Graphics" directory.  Note that, aside from the PMI (and the significance check on PMI), there's no numerical analysis here:  the point is to see which cards tend to be close to one another in the graph; unfortunately, there's some random variation in the way Gephi lays out the graph, but the pictures do yield meaningful insights.  My first two attempts, which I turned in for the project (I was in a hurry, lacking time due to visiting relatives) are the black-and-white "ForceAtlas2.png" and "ForceAtlas2a.png".  I later discovered how to make the prettier pictures you see in the remaining files.

In the graphics, the more often a card shows up, the bigger its circle is in the graph.  Colors represent card factions (since decks can draw from multiple factions), with native Jinteki cards as red.  "JintekiMid.png" displays decks using both of the "Identities" available at the time, "Personal Evolution" and "Replicating Perfection" (each deck has one and only one identity).  The other files display either the Personal Evolution or Replicating Perfection decks.  You'll notice "Mid" and "New" Gephi files--the latter were created with additional decks downloaded after the initial ones.

Since the decks were downloaded in early 2013, the database here is hopelessly outdated, and you really wouldn't want to update it by hand.  However, you could use the web-scraping scripts in the second repo to compile a more recent database of decks, and modify the scripts in this repo to analyze them.

Anyone who'd like to fork this and/or my second project is welcome to, but please drop me a note and let me know what you've done!
