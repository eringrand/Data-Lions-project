#!/usr/bin/python

import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS

def makeworldcloud(fileName, stopwords):
	with open ("temp/"+fileName, "r") as myfile:
		data=myfile.readlines()

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


stopwords = STOPWORDS.copy()
#stopwords.add('food')
#stopwords.add('restaurant')
#stopwords.add('place')
#stopwords.add('good')
#stopwords.add('vegas')
#stopwords.add('great')
stopwords.add('u')

goodWC = makeworldcloud("good-reviews.txt", stopwords)
badWC = makeworldcloud("bad-reviews.txt", stopwords)

plt.imshow(goodWC)
plt.axis("off")
plt.savefig("good-nodel.png")

plt.imshow(badWC)
plt.axis("off")
plt.savefig("bad-nodel.png")

stopwords = STOPWORDS.copy()
stopwords.add('food')
stopwords.add('restaurant')
stopwords.add('place')
stopwords.add('good')
stopwords.add('vegas')
stopwords.add('great')
stopwords.add('u')

goodWC = makeworldcloud("good-reviews.txt", stopwords)
badWC = makeworldcloud("bad-reviews.txt", stopwords)

plt.imshow(goodWC)
plt.axis("off")
plt.savefig("good.png")

plt.imshow(badWC)
plt.axis("off")
plt.savefig("bad.png")