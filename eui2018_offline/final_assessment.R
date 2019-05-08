
# Processing notes

require(rvest)
require(plyr)


## Debate over 3 days: 7th, 8th and 9th April. With a vote on April 9th ##

browseURL("https://api.parliament.uk/historic-hansard/commons/1975/apr/07/european-community-membership")

turn.data.final <- NULL
for (day in 7:9){
  url <- paste0("https://api.parliament.uk/historic-hansard/commons/1975/apr/0", day, "/european-community-membership")
  deb <- read_html(url)
  #### Parsing the debate ###
  mes <- html_nodes(deb, "div[class='hentry member_contribution']")
  speakers <- html_attr(html_node(mes, "cite a"), "title")
  # by speaker turn
  turns <- html_text(mes)
  turn_data <- data.frame(speaker=speakers, text=turns,
                          stringsAsFactors=FALSE)
  turn.data.final <- rbind.fill(turn.data.final, turn_data)
}
## Parse data by speaker ##
by_speaker <- split(turn.data.final$text, turn.data.final$speaker)
speaker_contribs <- unlist(lapply(by_speaker, paste, collapse="\n"))
speaker_data <- data.frame(speaker=names(by_speaker),
                    text=speaker_contribs,
                    stringsAsFactors=FALSE)
speaker_data <- speaker_data[order(speaker_data$speaker),]


## Auxiliary Information: Party Affiliation for each MP ##

mps <- read_html("https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_United_Kingdom_general_election,_October_1974")
browseURL("https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_United_Kingdom_general_election,_October_1974")
mps.table <- data.frame(mps %>% html_nodes('xpath'= ' [INSERT HERE XPATH ELEMENT OF TABLE] ') %>% html_table())
colnames(mps.table) <- mps.table[2,]
mps.table <- mps.table[-c(1:2),]

##################
#### Analysis ####
##################

## Topic Modelling using Quanteda ##

require(quanteda)

corpus_speaker <- corpus(speaker_data)


## Structural Topic Modelling using the STM package: let content vary as a function of Party Affiliation ##

# Creating a party assignment for each MP in the speaker_data object
speaker_data$party <- NULL
# We'll need to merge the MP names in speaker_data with the MP names in the Wikipedia table we scraped, 
# which gives us constituencies and party affiliations

# Step 1. Normalize MP name strings in our both speaker_data and mps.table objects, and merge.

speaker_data$mp_name <- gsub("^Mr |^Mrs |^Sir |^Hon. |^Dr ", "", speaker_data$speaker)
## Merge mp_name with mps.table$MP
#### Tips: 
# 1. use regular expressions to remove Dr, Sir, Hon, etc.
# 2. split last name and first name
# 3. convert everything to lowercase in both speaker_data and mps.table
# 4. Merge using the "merge" function in R. See this tutorial for any questions https://sejdemyr.github.io/r-tutorials/statistics/tutorial5.html
# 5. Alternatively, assign the party manually for each the 86 MPs in your speaker_data object


# Step 2. Create STM processed object, and run topic modelling:

require(stm)
processed <- textProcessor(speaker_data$text, metadata=speaker_data, language="english", stem=TRUE)






