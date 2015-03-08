#!/usr/bin/python

import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS

def makeworldcloud(fileName):
	with open ("temp/"+fileName, "r") as myfile:
		data=myfile.readlines()

	stopwords = STOPWORDS.copy()
	stopwords.add('food')
	stopwords.add('restaurant')
	stopwords.add('place')
	stopwords.add('good')
	stopwords.add('vegas')


	text = ""
	for line in data:
	    # remove white space
	    line = line.strip()
	    # everything is lower case
	    line = line.lower()
	    # remove puncuation and numbers
	    line = line.translate(None, ",?.!/;:\"")
	    for word in line.split():
	    	if word.isalpha() and word not in STOPWORDS:
	    		text += word + " "

	wordcloud = WordCloud(
		font_path='/Users/stump/Library/Fonts/CabinSketch-Bold.ttf',
		stopwords=stopwords,
		background_color='black'
		).generate(text)

	return wordcloud


goodWC = makeworldcloud("good-reviews.txt")
badWC = makeworldcloud("bad-reviews.txt")

plt.imshow(goodWC)
plt.axis("off")
plt.savefig("good.png")

plt.imshow(badWC)
plt.axis("off")
plt.savefig("bad.png")

