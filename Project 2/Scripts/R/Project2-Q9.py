import numpy as np
import math
from sklearn import  linear_model

actors_reader = file.readlines(file("../../project2.data/CSV.Data/actors-of-movies-5.csv", "rb"))
movie_reader = file.readlines(file("../../project2.data/CSV.Data/movies-names-greater-5.csv", "rb"))

movie_to_actor_dict = {}
actor_to_movie_dict = {}
i = 0
for ar, mr in zip(actors_reader, movie_reader):
    print i
    actors = ar.strip().split(",")
    movie = mr.strip()[1:-1]
    movie_to_actor_dict[movie] = actors

    for a in actors:
        a = a[1:-1]

        if a in actor_to_movie_dict:
            actor_to_movie_dict[a].append(movie)
        else:
            actor_to_movie_dict[a] = [movie]
    i += 1

rating_reader = file.readlines(file("../../project2.data/CSV.Data/rating-list-greater-5.csv", "rb"))
print movie_to_actor_dict["Batman v Superman: Dawn of Justice (2016)"]

movie_rating_dict = {}

for mo , mr in zip(movie_reader, rating_reader):
    movie_rating_dict[mo.strip()[1:-1]] = float(mr.strip())

print movie_rating_dict["Batman v Superman: Dawn of Justice (2016)"]

actor_rating = {}

i = 0
for a in actor_to_movie_dict.keys():
    print i
    v = []
    for m in actor_to_movie_dict[a]:
        v.append(movie_rating_dict[m])
    actor_rating[a] = sum(v)/len(v)
    i += 1

movie_to_actor_rating = {}

i = 0
for m in movie_rating_dict.keys():
    print i
    v = []
    for a in movie_to_actor_dict[m]:
        v.append(float(actor_rating[a[1:-1]]))
    movie_to_actor_rating[m] = sorted(v,reverse=True)[:5]
    i += 1

X = []
Y = []
for m in movie_rating_dict.keys():
    if movie_rating_dict[m] != 0:
        X.append(movie_to_actor_rating[m])
        Y.append(movie_rating_dict[m])


batman_features = movie_to_actor_rating["Batman v Superman: Dawn of Justice (2016)"]
mission_features = movie_to_actor_rating["Mission: Impossible - Rogue Nation (2015)"]
minions_features = movie_to_actor_rating["Minions (2015)"]

X =  np.array(X)
Y =  np.array(Y)

# Create linear regression object
regr = linear_model.LinearRegression()

# Train the model using the training sets
regr.fit(X, Y)

# The coefficients
print('Coefficients: \n', regr.coef_)

print("Predicted value for Batman v Superman: Dawn of Justice (2016) = ", regr.predict(batman_features))
print("Predicted value for Mission: Impossible - Rogue Nation (2015) = ", regr.predict(mission_features))
print("Predicted value for Minions (2015) = ", regr.predict(minions_features))

print("Average Predicted value for Batman v Superman: Dawn of Justice (2016) = ", np.mean(np.array(batman_features)))
print("Average Predicted value for Mission: Impossible - Rogue Nation (2015) = ", np.mean(np.array(mission_features)))
print("Average Predicted value for Minions (2015) = ", np.mean(np.array(minions_features)))

v = []
for a in movie_to_actor_dict["Batman v Superman: Dawn of Justice (2016)"]:
    if float(actor_rating[a[1:-1]]) != 0:
        v.append(float(actor_rating[a[1:-1]]))

print("All Neighbor Average Predicted value for Batman v Superman: Dawn of Justice (2016) = ",  np.mean(v))

v = []
for a in movie_to_actor_dict["Mission: Impossible - Rogue Nation (2015)"]:
    if float(actor_rating[a[1:-1]]) != 0:
        v.append(float(actor_rating[a[1:-1]]))

print("All Neighbor Average Predicted value for Mission: Impossible - Rogue Nation (2015) = ",  np.mean(v))

v = []
for a in movie_to_actor_dict["Minions (2015)"]:
    if float(actor_rating[a[1:-1]]) != 0:
        v.append(float(actor_rating[a[1:-1]]))

print("All Neighbor Average Predicted value for Minions (2015) = ",  np.mean(v))