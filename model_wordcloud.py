# read in table of word, freq
import numpy as np
import pandas as pd
import math
import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS

data = pd.read_csv("WordProbs.csv", header = 0)

good = data[(data['Coefficients'] < 0)]
bad = data[(data['Coefficients'] > 0)]

bad['Freq'] = abs(bad['Coefficients']) * 100. 
good['Freq'] = abs(good['Coefficients']) * 100. 


def totext(good):
	w = list(good['Words'])
	f = list(good['Freq'])

	text = ""
	for i in range(len(w)-1):
		wi = np.array(w[i])
		fi = math.floor(f[i])
		wrep = np.repeat(wi, fi, axis=0)	
		ws = str(wrep)
		text += ws

	text = text.strip()
	text =  text.translate(None, "[].\'")

	stopwords = STOPWORDS.copy()
	stopwords.add('marginally')
	stopwords.add('demographics')

	#stopwords.add('food')
	#stopwords.add('restaurant')
	#stopwords.add('place')
	#stopwords.add('good')
	#stopwords.add('vegas')

	wordcloud = WordCloud(
		font_path='/Users/stump/Library/Fonts/CabinSketch-Bold.ttf',
		stopwords=stopwords,
		background_color='black'
		).generate(text)

	return wordcloud

goodtext = totext(good)
badtext = totext(bad)

plt.imshow(goodtext)
plt.axis("off")
plt.savefig("good-model.png")

plt.imshow(badtext)
plt.axis("off")
plt.savefig("bad-model.png")

