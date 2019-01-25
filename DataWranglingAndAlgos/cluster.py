import sys
import csv
import math
inputs = sys.argv
if not len(inputs) is 3:
	print("Input error call program as following")
	print("python cluster.py data.csv clusternum")
	exit()



votes = ["Nay", "Yea", "", "Not Voting", "Present"]

class Rep:
	def __init__(self, idnum):
		self.idnum = idnum
		self.votes = []
		self.dem = False

reps = []
clusternum = int(inputs[2])

with open(inputs[1]) as trainFile:
	csvReader = csv.reader(trainFile)
	idcount = -1
	for row in csvReader:
		idcount += 1
		rLen = len(row) - 1
		temp = Rep(idcount)
		temp.dem = row[rLen] == "Democrat"
		for ind in range(rLen):
			temp.votes.append(row[ind])
		# EndFor
		reps.append(temp)
	# EndFor
# EndOpen

numreps = len(reps)

clusters = [[rep] for rep in reps]

def dist(rep1, rep2):
	dtot = 0
	aVotes = rep1.votes
	bVotes = rep2.votes
	for ind in range(rLen):
		if not (aVotes[ind] == bVotes[ind]):
			dtot += 1
	return dtot

def clustdist(ind1, ind2):
	clst1 = clusters[ind1]
	clst2 = clusters[ind2]
	totDist = 0
	clustCount = 0
	for aRep in clst1:
		for bRep in clst2:
			totDist+= dist(aRep, bRep)
			clustCount += 1
	return float(totDist)/clustCount

def merge(ind1, ind2):
	for rep in clusters[ind2]:
		clusters[ind1].append(rep)
	clusters.pop(ind2)

def refactor():
	mindist = 10000
	indL = -1
	indR = -1
	cLen = len(clusters)
	for ind1 in range(cLen):
		for ind2 in range(ind1+1, cLen):
			diff = clustdist(ind1, ind2)
			if diff <= mindist:
				indL = ind1
				indR = ind2
				mindist = diff
			if mindist == 0:
				break;
	merge(indL, indR)

while len(clusters) > clusternum:
	#print(len(clusters))
	refactor()

for cluster in clusters:
	printstr = ""
	for ind in range(len(cluster)-1):
		print(int(cluster[ind].idnum), end = ",")
	print(int(cluster[len(cluster)-1].idnum))




# Line number = ID number + 1