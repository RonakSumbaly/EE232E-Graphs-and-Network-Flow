from operator import itemgetter
import numpy as np
import heapq
import ast

page_rank = {}
pg_reader = file.readlines(file("page-rank-actors.csv", "rb"))

for pg in pg_reader:
    content = pg.strip().split()
    page_rank[content[0]] = float(ast.literal_eval(content[1]))

ma_reader = file.readlines(file("actors-of-movies-5.csv", "rb"))

actor_id = []

for ma in ma_reader:
    actor_id.extend(ma.strip().split(","))

actor_id = np.ravel(np.ravel(np.array(actor_id)))
print actor_id
actor_id = set(actor_id)

for ac in actor_id:
    if ac in page_rank:
        continue
    else:
        page_rank[ac] = 0


mb_reader = file.readlines(file("actors-of-movies-5.csv", "rb"))

output = []
for mb in mb_reader:
    page_rank_values = itemgetter(*mb.strip().split(","))(page_rank)
    output.append(heapq.nlargest(5, page_rank_values))

i = 0
writer = file("page_rank_movies_greater_5.csv","wb")
for key in output:
    i += 1
    writer.write(str(key)[1:len(str(key))-1] + "\n")

print i