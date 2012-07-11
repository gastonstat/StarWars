############################################################################
# Title:        parsing_episodeVI.R
# Description:  Script to parse Star Wars Episode VI (text file)
# Input file:   StarWars_EpisodeVI_script.txt
# Author:       Gaston Sanchez
#               www.gastonsanchez.com
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
############################################################################

# set your working directory (don't use mine!)
# setwd("/Users/gaston/Documents/Gaston/GoogleSite/StarWars")

# read episode VI script in R (this is a character vector)
sw = readLines("StarWars_EpisodeVI_script.txt")

# inspect first 90 lines
# you'll see that the first dialogue is from SHUTTLE CAPTAIN in line 76
sw[1:90]

# command to extract character name (just for demo purposes)
substr(sw[76], 31, nchar(sw[76]))
# command to extract dialogue text (just for demo purposes)
substr(sw[77], 16, nchar(sw[77]))

# we need these auxiliary strings to help us
# extract character names and their dialogues
b15 = "               "
b30 = "                              "

# how many lines in input file
nlines = length(sw)

# let's parse the entire script while extracting only the names of the
# characters and their dialogues. The output file is EpisodeVI_dialogues.txt

# write title line in output file
writeLines("STAR WARS - EPISODE 6: RETURN OF THE JEDI", "EpisodeVI_dialogues.txt")

# the first 70 lines don't contain dialogues
# so we can start reading at line 70 (for instance)
i = 70

# while loop to extract character and dialogues
# (probably there's a better way to parse the file instead of
# using my crazy nested if-then-elses, but this works for me)
while (i <= nlines)
{
  # if empty line
  if (sw[i] == "") i = i + 1  # next line
  # if text line
  if (sw[i] != "")
  {
    # if uninteresting stuff
    if (substr(sw[i], 1, 1) != " ") {
      i = i + 1   # next line
    } else {
      if (nchar(sw[i]) < 10) {
        i = i + 1  # next line
      } else {
        if (substr(sw[i], 1, 5) != " " && substr(sw[i], 6, 6) != " ") {
          i = i + 1  # next line
        } else {
          # if character name
          if (substr(sw[i], 1, 30) == b30) 
          {
            if (substr(sw[i], 31, 31) != " ")
            {
              tmp_name = substr(sw[i], 31, nchar(sw[i], "bytes"))
              cat("\n", file="EpisodeVI_dialogues.txt", append=TRUE)
              cat(tmp_name, "", file="EpisodeVI_dialogues.txt", sep="\t", append=TRUE)
              i = i + 1        
            } else {
              i = i + 1
            }
          } else {
            # if dialogue
            if (substr(sw[i], 1, 15) == b15)
            {
              if (substr(sw[i], 16, 16) != " ")
              {
                tmp_diag = substr(sw[i], 16, nchar(sw[i], "bytes"))
                cat("", tmp_diag, file="EpisodeVI_dialogues.txt", append=TRUE)
                i = i + 1
              } else {
                i = i + 1
              }
            }
          }
        }
      }
    }    
  }
}


# =====================================================================
# Creating data table "SW_EpisodeVI.txt"
# =====================================================================

# how many lines in output file
system("wc -l EpisodeVI_dialogues.txt")

# get vector of character names
SW6_chars = system("tail -n676 EpisodeVI_dialogues.txt | cut -f1", intern=TRUE)
# get vector of dialogue lines
SW6_diags = system("tail -n676 EpisodeVI_dialogues.txt | cut -f2", intern=TRUE)

# check character names
table(SW6_chars)
# remove strings between parenthesis in SW6_chars
SW6_chars = gsub("\\((.*?)\\)", "", SW6_chars)
# remove extra white spaces in SW6_chars
SW6_chars = gsub("\\s+$", "", SW6_chars)

# remove strings between parenthesis in dialogues
SW6_diags = gsub("\\((.*?)\\)", "", SW6_diags)
# remove extra white spaces at the beginning of dialogues
SW6_diags = gsub("^\\s+", "", SW6_diags)

# remove last line (useless)
SW6_chars = SW6_chars[-length(SW6_chars)]
SW6_diags = SW6_diags[-length(SW6_diags)]

# fix DEATH STAR CONTROLLER dialogue (this is the only exception)
SW6_diags[2] = SW6_diags[3]

# join characters and dialogues in one table
SW6 = cbind(character=SW6_chars[-3], dialogue=SW6_diags[-3])

# save SW6 in file 'SW_EpisodeVI.txt'
write.table(SW6, file="SW_EpisodeVI.txt")

# if you want to check the data table
# A = read.table("SW_EpisodeVI.txt")
# head(A)
# tail(A)

