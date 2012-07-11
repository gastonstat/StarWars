############################################################################
# Title:        parsing_episodeIV.R
# Description:  Script to parse Star Wars Episode IV (text file)
# Input file:   StarWars_EpisodeIV_script.txt
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
sw = readLines("StarWars_EpisodeIV_script.txt")

# inspect first 70 lines
# you'll see that the first dialogue is from THREEPIO in line 52
sw[1:70]

# command to extract character name (just for demo purposes)
substr(sw[52], 21, nchar(sw[52]))
# command to extract dialogue text (just for demo purposes)
substr(sw[53], 11, nchar(sw[53]))

# we need these auxiliary strings to help us
# extract character names and their dialogues
b10 = "          "
b20 = "                    "

# how many lines in input file
nlines = length(sw)

# let's parse the entire script while extracting only the names of the
# characters and their dialogues. The output file is EpisodeIV_dialogues.txt

# write first line in output file
writeLines("STAR WARS - EPISODE 4: STAR WARS", "EpisodeIV_dialogues.txt")

# the first 50 lines don't contain dialogues
# start reading at line 50
i = 50

# while loop to extract character and dialogues
# you may get some errors, just ignore them and re-run
# the while loop as many times as needed
while (i <= nlines)
{
  # if empty line
  if (sw[i] == "") i = i + 1  # next line
  # if text line
  if (sw[i] != "")
  {
    # if script description
    if (substr(sw[i], 1, 1) != " ") i = i + 1   # next line
    if (nchar(sw[i]) < 10) i = i + 1  # next line
    # if character name
    if (substr(sw[i], 1, 20) == b20) 
    {
      if (substr(sw[i], 21, 21) != " ")
      {
        tmp_name = substr(sw[i], 21, nchar(sw[i], "bytes"))
        cat("\n", file="EpisodeIV_dialogues.txt", append=TRUE)
        cat(tmp_name, "", file="EpisodeIV_dialogues.txt", sep="\t", append=TRUE)
        i = i + 1        
      } else {
        i = i + 1
      }
    }
    # if dialogue
    if (substr(sw[i], 1, 10) == b10)
    {
      if (substr(sw[i], 11, 11) != " ")
      {
        tmp_diag = substr(sw[i], 11, nchar(sw[i], "bytes"))
        cat(tmp_diag, file="EpisodeIV_dialogues.txt", append=TRUE)
        i = i + 1
      } else {
        i = i + 1
      }
    }
  }
}


# =====================================================================
# Creating data table "SW_EpisodeIV.txt"
# =====================================================================

# how many lines in output file
system("wc -l EpisodeIV_dialogues.txt")

# get vector of character names 
SW4_chars = system("tail -n1010 EpisodeIV_dialogues.txt | cut -f1", intern=TRUE)
# get vector of dialogue lines
SW4_diags = system("tail -n1010 EpisodeIV_dialogues.txt | cut -f2", intern=TRUE)

# check character names
table(SW4_chars)
# remove voices
SW4_chars = gsub("'S VOICE", "", SW4_chars)

# join characters and dialogues in one table
SW4 = cbind(character=SW4_chars, dialogue=SW4_diags)

# save SW4 in file 'SW_EpisodeIV.txt'
write.table(SW4, file="SW_EpisodeIV.txt")

# if you want to check the data table
# A = read.table("SW_EpisodeIV.txt")
# head(A)
# tail(A)
