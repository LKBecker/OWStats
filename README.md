# OW Stats

OWStats is a simple collection of two scripts to review your Overwatch performance.

  - DownloadStats.py, a simple Python script to download your stats from OWAPI.com (and by extension from the Overwatch website). 
  - OWStatsAnalysis.R, which creates graphs and calculations from that data.

# Data download
Run DownloadStats.py using Python 3.7, after replacing
```python
Stack = [ "YOURTAGHERE-No#JustDashes-1111" ]
```
with your own BattleNet tag(s), e.g.
```python
Stack = [ "ReapersCat-1111" ] #or
Stack = [ "ReapersCat-1111", "DVaCups-1234" ]
```
and run the script.

# Graphing
We use RStudio to execute the RScript and capture the GGplot2 output.