import csv
from itertools import groupby
import string
import itertools

#mydict = {}
locations = list()
# assuming Python 2
with open("processed_only_values.csv", "rb") as fp_in:
    reader = file.readlines(fp_in)
    i = 0
    for row in reader:
        print i
        content = row.split(',')
        if len(content) >=5:
            locations.append(i)
        i += 1
        #if not row in mydict:
        #    mydict[row.strip()] = i
        #else:
        #    mydict[row].append(i)
        #i += 1
file.close(fp_in)

#print mydict.keys()[1]
#locations = list()
#with open("movies-greater-name.csv", "rb") as fp_in:
#    reader = file.readlines(fp_in)
#    i = 0
#    for row in reader:
#        print i
#        locations.append(mydict["\""+row.strip()+"\""])
#        i += 1


#writer = file("processed_only_names.csv","wb")
#for key,value in mydict.items():
#    writer.write(str(key) + "\n")

#content = list()
#with open("processed_only_values.csv") as fp:
#    reader = csv.reader(fp, delimiter = ",")
#    i = 0
#    for row in reader:
#        print i
#        content.append(list(itertools.combinations(row, 2)))
#        i += 1

#C = [item for sublist in content for item in sublist]
#outfile = file("combinations.csv", "wb")
#for item in C:
#    print item
#    outfile.write(str(item[0]) + " " + str(item[1])+"\n")

#with open("combinations.csv", "rb") as fp_in:
 #   reader = csv.reader(fp_in, delimiter = " ")
#    i = 0
#    for row in reader:
#        print row
#        if not "-".join(row) in mydict:
#            mydict["-".join(row)] = 1
#        else:
#            mydict["-".join(row)] += 1
#        i += 1

#writer = file("weights.csv","wb")
#for key,value in mydict.items():
#    writer.write(" ".join(str.split(key,"-")) + " " + str(value) + "\n")
