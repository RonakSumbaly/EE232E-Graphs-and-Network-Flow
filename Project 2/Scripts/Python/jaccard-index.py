## QUESTION 4
## Calculate jaccard-index between movies with greater than 5 actors

actors_movies = list()
reader = file.readlines(file("actors-of-movies-5.csv", "rb"))

for row in reader:
  content = set(row.strip().split(','))
  actors_movies.append(content)

output = list()

for i in xrange(len(actors_movies)):
    print (i+1)
    for j in xrange(i,len(actors_movies)):
        if i==j:
            continue
        n = len(actors_movies[i].intersection(actors_movies[j]))
        if n == 0:
            continue
        JI = (n / float(len(actors_movies[i]) + len(actors_movies[j]) - n))
        output.append(str(i+1) + "," + str(j+1) + "," + str(JI))

writer = file("jaccard-index.csv","wb")
for key in output:
    writer.write(str(key) + "\n")