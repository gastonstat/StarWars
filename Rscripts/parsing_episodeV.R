############################################################################
# Title:        parsing_episodeV.R
# Description:  Script to parse Star Wars Episode V (text file)
# Input file:   StarWars_EpisodeV_script.txt
# Author:       Gaston Sanchez
#               www.gastonsanchez.com
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
############################################################################

# set your working directory (don't use mine!)
# setwd("/Users/gaston/Documents/Gaston/GoogleSite/StarWars")

# read episode IV script in R (this is a character vector)
sw = readLines("StarWars_EpisodeV_script.txt")

# inspect first 70 lines
# you'll see that the first dialogue is from LUKE in line 64
sw[1:70]

# how many lines in input file
nlines = length(sw)
# we can start reading at line 30, for instance
i = 30

# auxiliary flag to indicate if the current line is part
# of the same dialogue 
current = FALSE

# write title line in output file
writeLines("STAR WARS - EPISODE 5: THE EMPIRE STRIKES BACK", "EpisodeV_dialogues.txt")

# while loop to extract character and dialogues
# you may get some errors, just ignore them and re-run
# the while loop as many times as needed
i = i + 1
while (i <= nlines)
{
  # if empty line
  if (sw[i] == "" || substr(sw[i], 1, 1) == " ") {
    current = FALSE
    i = i + 1  # next line
  } else {
    # EXTERIOR or INTERIOR (we don't want these lines)
    if (str_extract(sw[i], "\\w+") %in% c("INTERIOR", "EXTERIOR"))
    {
      current = FALSE
      i = i + 1
    } else {
      # good dialogue
      if (!current) {
        cat("\n", file="EpisodeV_dialogues.txt", append=TRUE)
        cat(sw[i], "", file="EpisodeV_dialogues.txt", append=TRUE)
        i = i + 1
        current = TRUE
      } else {
        cat("", sw[i], file="EpisodeV_dialogues.txt", append=TRUE)
        i = i + 1
        current = TRUE
      }
    }
  }
}


# =========================================================================
# Creating data table "SW_EpisodeIV.txt"
# =========================================================================

# how many lines in output file
system("wc -l EpisodeV_dialogues.txt")

# get vector of character names
SW5_chars = system("tail -n839 EpisodeV_dialogues.txt | cut -f1 -d':'", intern=TRUE)
# get vector of dialogue lines
SW5_diags = system("tail -n839 EpisodeV_dialogues.txt | cut -f2 -d':'", intern=TRUE)

# check character names
table(SW5_chars)
# remove apostrophes voices
SW5_chars = gsub("'S VOICE", "", SW5_chars)

# remove strings between parenthesis in dialogues
SW5_diags = gsub("\\((.*?)\\)", "", SW5_diags)

# remove extra white spaces at the beginning and the end of dialogues
SW5_diags = gsub("^\\s+", "", SW5_diags)
SW5_diags = gsub("\\s+$", "", SW5_diags)

# join characters and dialogues in one table
SW5 = cbind(character=SW5_chars, dialogue=SW5_diags)
# save SW5 in file 'SW_EpisodeV.txt'
write.table(SW5, file="SW_EpisodeV.txt")

# if you want to check the data table
# A = read.table("SW_EpisodeV.txt")
# head(A)
# tail(A)

