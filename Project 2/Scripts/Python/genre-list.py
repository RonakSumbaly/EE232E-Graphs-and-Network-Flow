## QUESTION 5
## get genres of movies with greater than 5 actors

movies_names = []
m_reader = file.readlines(file("processed_movie_names.csv", "rb"))  # csv after pre-processing
g_reader = file.readlines(file("processed_movie_genres.csv", "rb")) 

genre_names = []

for m in m_reader:
    movies_names.append(m.strip())

for n in g_reader:
    genre_names.append(n.strip())

movie_genre_dict = dict(zip(movies_names, genre_names))

mg_reader = file.readlines(file("movies-names-greater-5.csv", "rb"))

genre_movie_greater = []
count_unknown = 0  # unknown genre
for m in mg_reader:
    if m.strip() in movie_genre_dict:
        genre_movie_greater.append(movie_genre_dict[m.strip()])
    else:
        genre_movie_greater.append("Unknown")
        count_unknown += 1

writer = file("genre-list-greater-5.csv","wb")
for key in genre_movie_greater:
    writer.write(str(key) + "\n")

