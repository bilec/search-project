# search-project

```
stack build
stack run 
```

## Requirements
1. parse - it needs collection.jl file in folder where it is being run
2. pageRank - it needs webPageInfo.txt file in folder where it is being run
3. invertedIndex - same as for pageRank
4. search - it needs pageRank.txt and invertedIndex.txt files in folder where it is being run

## Usage
When you start application menu will appear and you can choose from :
1. Parse collection.jl file
2. Calculate pageRank
3. Calculate reverse index
4. Search

After whichever operation you chose this menu will appear again.

## Sources
pageRank: 
 - https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.258.9919&rep=rep1&type=pdf
 - http://cs.brown.edu/courses/cs016/static/files/assignments/projects/GraphHelpSession.pdf
 - https://michaelnielsen.org/blog/using-your-laptop-to-compute-pagerank-for-millions-of-webpages/

## Dataset
https://www.kaggle.com/datasets/aldebbaran/html-br-collection