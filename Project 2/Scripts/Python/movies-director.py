from operator import itemgetter
import numpy as np
import heapq
import ast
import re

movies_director = {}
md_reader = file.readlines(file("director_movies_processed.txt", "rb"))

movies = []
directors = []
i = 1
for md in md_reader:
    content = md.strip().split("\t\t")
    if len(content) > 1:
        directors.extend([content[0]] * (len(content) - 1))
        movies.extend(map(str.strip, content[1:]))
    i += 1

movies_director = dict(zip(movies, directors))

print movies_director["The Matrix (1999)"]

mg_reader = file.readlines(file("movies-names-greater-5.csv", "rb"))

directors_greater_5 = []
for mg in mg_reader:
    movie_name = mg.strip()[1:-1]
    if movie_name in movies_director:
        directors_greater_5.append(movies_director[mg.strip()[1:-1]])
    else:
        directors_greater_5.append("Unknown")

#writer = file("directors_greater_5.csv", "wb")
#for key in directors_greater_5:
#    writer.write(str(key) + "\n")

top100_reader = file.readlines(file("top100.txt", "rb"))
directors_names = []
for t1 in top100_reader:
    movie = t1.split("\t")[1:2][0].strip()
    if movie in movies_director:
        directors_names.append(movies_director[movie])
    else:
        directors_names.append("Pata nahi")

directors_names = set(directors_names)
feature_directors = []

for key in directors_greater_5:
    if key in directors_names:
        feature_directors.append(1)
    else:
        feature_directors.append(0)


writer = file("feature_director.csv", "wb")
for key in feature_directors:
    writer.write(str(key) + "\n")