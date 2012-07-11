## Star Wars Arc Diagram
=============================================
by [Gaston Sanchez](http://www.gastonsanchez.com/)

**Motivation**
The main idea behind this project is to reproduce, as much as possible, an arc-diagram in R like the one depicted in [Similar Diversity](http://similardiversity.net/) (by Philipp Steinweber and Andreas Koller). 

**Long story short**
I think I saw the arc-diagram of ***Similar Diversity*** for the first time back in 2010. Every time I contemplated that diagram, both amazed and amused, I always ended up wondering how the hell did Philipp and Andreas do it. Finally one day, I couldn't stand my questions anymore so I decided to try to make my own arc-diagram in R... and this project is the result of such attempt. Although I haven't been able to obtain an identical representation of the Similar Diversity arc diagram, I'm really happy with what I got and I guess it's pretty much similar.

**Source**
In my case, I decided to use the movie scripts from the original Star Wars trilogy (episodes IV, V, and VI) as the text data for my analysis. I found the movie scripts at [Ben and Grove](http://www.benandgrover.com/scripts.asp) and [corkey.net](http://corky.net/scripts/) where you can find other scripts if you want to play with other movies.

----------------------------------

## Analysis recipe:

### Step 1) Text Parsing
Parse the text data of the movie scripts to extract the dialogues of each character. The final output files are the extracted dialogues in table format (one table per movie)
```
Movie script text files         | Parsed files             | Data table files
------------------------------- | ------------------------ | -----------------
StarWars_EpisodeIV_script.txt   | EpisodeIV_dialogues.txt  | SW_EpisodeIV.txt
StarWars_EpisodeV_script.txt    | EpisodeV_dialogues.txt   | SW_EpisodeV.txt
StarWars_EpisodeVI_script.txt   | EpisodeVI_dialogues.txt  | SW_EpisodeVI.txt
```
The R scripts for this step are:
```
1. parsing_episodeIV.R
2. parsing_episodeV.R
3. parsing_episodeVI.R
```
------------------------------------
### Step 2) Identify top characters
Once we have the dialogues in table format, the next step is to identify the most talkative characters of the trilogy. This implies performing a frequency analysis to detect the *most talkative* characters, that is, the characters with the greatest number of dialogues in english. Unfortunately, Artoo and Chewie are excluded since they don't speak english. The R script for this step is:
```
get_top_characters.R
```
The output file is:
```
top_chars_by_eps.txt
```
------------------------------------
### Step 3) Network between top characters
Having identified the top characters (ie most talkative ones) of the trilogy, the following step is to get a network between them. The way I get the network is by looking at associations between the words that appear in the dialogues of the top characters. This process implies a text mining analysis with the help of the ```tm``` package. In turn, the network is obtained by using the ```igraph``` package. The R script for this step is:
```
get_top_characters_network.R
```
The output files are:
```
top_chars_network.txt
weight_edges_graph1.txt
weight_edges_graph2.txt
```
For exploratory purposes, we can get some preliminary visualizations using some of the plotting functions:
```
1. arcDiagram.R
2. arcPies.R
3. arcBands.R
```
------------------------------------
### Step 4) Identify top terms of top characters
The next step is to identify the top terms said by the top characters, according to the episodes in which they appear. The results obtained in this phase are used for plotting the bar-charts below each character in the final arc-diagram. The R script for this step is:
```
get_terms_by_episodes.R
```
The output file is:
```
top_char_terms.txt
```
------------------------------------
### Step 5) Ultimate Arc-Diagram
The last step is to get the final arc-diagram containing the top characters, their participation by episode, and the associated top terms from their dialogues. The size of the bands around each character reflect how much they talk in the movies, as well as their participation in each episode. The width of the arcs connecting two characters reflect the number of words in common. In turn, the color of the arcs reflect the association between two characters depending on the similarity of the words in their dialogues. The R script for this step is:
```
ultimate_arc_diagram.R
```
The plotting function is:
```
arcBandBars.R
```
