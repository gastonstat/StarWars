## Visualizing Star Wars Movie Scripts
======================================

**Description:**
Perform a statistical text analysis on the Star Wars scripts from Episodes IV, V, and VI, in order to obtain an arc-diagram representation (more or less equivalent to the one in [Similar Diversity](http://similardiversity.net/)) with the most talkative characters of the Star Wars trilogy.

<br>

### R scripts
**Parsing scripts:** R scripts to parse the movie scripts, extract the characters & dialogues, and produce the data tables:
```
1. parsing_episodeIV.R
2. parsing_episodeV.R
3. parsing_episodeVI.R
```
**Analysis scripts:** R scripts for the performed analysis (run them sequentially):
```
1. get_top_characters.R
2. get_top_characters_network.R
3. get_terms_by_episodes.R
4. ultimate_arc_diagram.R
```
**Functions:** R functions for plotting different types of arc diagrams (these functions are used in some of the above analysis scripts):
```
1. arcDiagram.R
2. arcPies.R
3. arcBands.R
4. arcBandBars.R
```

<br>

### Text Files 
**Movie Scripts:** These are the movie scripts (raw text data) which are parsed to produced the dialogues files:
```
1. StarWars_EpisodeIV_script.txt
2. StarWars_EpisodeV_script.txt
3. StarWars_EpisodeVI_script.txt
```
**Text dialogues:** These are <em>intermediate</em> files containing the extracted dialogues from the movie scripts:
```
1. EpisodeIV_dialogues.txt
2. EpisodeV_dialogues.txt
3. EpisodeVI_dialogues.txt
```
**Input tables:** These are the input files (in data table format) used for the text mining analysis:
```
1. SW_EpisodeIV.txt
2. SW_EpisodeV.txt
3. SW_EpisodeVI.txt
```
**Output tables:** These are the output files (in data table format), produced in the text mining analysis, that are used to get the different arc-diagrams:
```
1. top_chars_by_eps.txt
2. top_chars_network.txt
3. top_char_terms.txt
4. weight_edges_graph1.txt
5. weight_edges_graph2.txt
```
<br>

#### PS:
Keep in mind that my main motivation for this project was to replicate, as much as possible, the arc diagram of Similar Diversity by Philipp Steinweber and Andreas Koller. I'm sure there are a lot of parts in the analysis that can be made faster, more efficient, and simpler. However, my quota of spare time is very limited and I haven't had enough time to write the ideal code.