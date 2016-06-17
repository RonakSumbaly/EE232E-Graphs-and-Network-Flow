## QUESTION 7
## get ratings of movies with greater than 5 actors

movies_names = []
m_reader = file.readlines(file("rating-names.csv", "rb"))  # csv after pre-processing
g_reader = file.readlines(file("rating-values.csv", "rb"))

rating_values = []

for m in m_reader:
    movies_names.append(m.strip())

for n in g_reader:
    rating_values.append(n.strip())

movie_rating_dict = dict(zip(movies_names, rating_values))

mg_reader = file.readlines(file("movies-names-greater-5.csv", "rb"))

genre_movie_greater = []
count_unknown = 0   # unknown genre
for m in mg_reader:
    if m.strip() in movie_rating_dict:
        genre_movie_greater.append(movie_rating_dict[m.strip()])
    else:
        genre_movie_greater.append(0)
        count_unknown += 1

writer = file("rating-list-greater-5.csv","wb")
for key in genre_movie_greater:
    writer.write(str(key) + "\n")


print count_unknown