import sys
import csv
import math
inputs = sys.argv

DEBUG = False

if not len(inputs) is 3:
	print("Input error call program as following")
	print("python congress_nbc.py arg1.csv arg2.csv")
	exit()

votes = ["Nay", "Yea", "", "Not Voting", "Present"]
rLen = -1
dLst = []
rLst = []
eLst = []
bigLst = []
with open(inputs[1]) as trainFile:
	csvReader = csv.reader(trainFile)
	rnum = 0
	for row in csvReader:
		bigLst.append(row)
		rnum += 1
		if rLen is -1:
			rLen = len(row)
			dLst = [{} for i in range(rLen)]
			rLst = [{} for i in range(rLen)]
			eLst = [{} for i in range(rLen)]
		dem = len(row[-1]) is 8
		for ind in range(rLen):
			choice = row[ind]
			eHsh = eLst[ind]
			if choice in eHsh:
				eHsh[choice] += 1
			else:
				eHsh[choice] = 1
			if dem:
				dHsh = dLst[ind]
				if choice in dHsh:
					dHsh[choice] += 1
					continue
				else:
					dHsh[choice] = 1
					continue
			else:
				rHsh = rLst[ind]
				if choice in rHsh:
					rHsh[choice] += 1
					continue
				else:
					rHsh[choice] = 1
					continue
		# EndFor
	# EndFor
# EndOpen

# List of indexes we are still looking at to arrange
tLst = range(len(bigLst))
bLst = [0 for i in range(len(bigLst))]

class treeNode:
	def __init__(self):
		# Index of values that it decides on
		self.ind = -1
		# Hashtable, key = vote, value = childNode to go to when voting the vote
		self.children = {vote: None for vote in votes}
		self.party = None
		self.odds = -1
	def addChild(self, child, vote):
		self.children[vote] = child
	def setInd(self, ind):
		self.ind = ind
	def setVals(self, party, odds):
		self.party = party
		self.odds = odds


#Treemaking


# Input examples to look at in the big list in a [][] format like bigLst, the list of index of attributes that remain to look at
# and the examples of the parent
# Returns the treeNode that starts the segment pertaining to the examples
def treeGen(examples, attributes, parentExs):
	# Checking for end conditions
	if len(examples) is 0:
		if DEBUG:
			print "no Exs"
		dCount = 0
		rCount = 0
		for row in parentExs:
			if len(row[-1]) is 8:
				dCount +=1
			else:
				rCount +=1
		if dCount > rCount:
			ans = treeNode()
			ans.setVals("Democrat", float(dCount)/(dCount + rCount))
			return ans
		else:
			ans = treeNode()
			ans.setVals("Republican", float(rCount)/(dCount + rCount))
			return ans

	if len(attributes) is 0:
		if DEBUG:
			print "no Exs"
		dCount = 0
		rCount = 0
		for row in examples:
			if len(row[-1]) is 8:
				dCount +=1
			else:
				rCount +=1
		if dCount > rCount:
			ans = treeNode()
			ans.setVals("Democrat", float(dCount)/(dCount + rCount))
			return ans
		else:
			ans = treeNode()
			ans.setVals("Republican", float(rCount)/(dCount + rCount))
			return ans


	#Getting data for Importance Calc
	dLst = [{} for i in range(rLen)]
	rLst = [{} for i in range(rLen)]
	eLst = [{} for i in range(rLen)]
	for row in examples:
		dem = len(row[-1]) is 8
		for ind in range(rLen):
			choice = row[ind]
			eHsh = eLst[ind]
			if choice in eHsh:
				eHsh[choice] += 1
			else:
				eHsh[choice] = 1
			if dem:
				dHsh = dLst[ind]
				if choice in dHsh:
					dHsh[choice] += 1
					continue
				else:
					dHsh[choice] = 1
					continue
			else:
				rHsh = rLst[ind]
				if choice in rHsh:
					rHsh[choice] += 1
					continue
				else:
					rHsh[choice] = 1
					continue
		# EndFor
	#EndFor

	def dAt(ind, key):
		if key in dLst[ind]:
			return dLst[ind][key]
		else:
			return 0
	def rAt(ind, key):
		if key in rLst[ind]:
			return rLst[ind][key]
		else:
			return 0
	def eAt(ind, key):
		if key in eLst[ind]:
			return eLst[ind][key]
		else:
			return 0

	dCount = dAt(rLen - 1, "Democrat")
	rCount = rAt(rLen - 1, "Republican")
	eCount = dCount + rCount
	if DEBUG:
		print(dCount, rCount, eCount)

	if dCount is eCount:
		if DEBUG:
			print "did it--------"
		ans = treeNode()
		ans.setVals("Democrat", 1.0)
		return ans

	if rCount is eCount:
		if DEBUG:
			print "did it r --------"
		ans = treeNode()
		ans.setVals("Republican", 1.0)
		return ans

	

	def log(num):
		return math.log(num)/math.log(2)

	def B(q):
		p = 1-q
		return -(q*log(q) + p*log(p))


	impVals = [0.0 for i in range(len(attributes))]
	totgain = 0
	for ind in range(len(attributes)):
		nid = attributes[ind]
		gain = 1.0
		for vote in votes:
			d = dAt(nid, vote)
			r = rAt(nid, vote)
			e = eAt(nid, vote)
			if e is 0 or d is 0 or r is 0:
				continue
			else:
				# print e
				# print "/"
				# print eCount
				# print ""
				# print float(d)/e
				# print ""
				gain -= float(e)/eCount * B(float(d)/e)

		impVals[ind] = gain
	mVal = max(impVals)
	if mVal < 0.01:
		if DEBUG:
			print "not sig"
		dCount = 0
		rCount = 0
		for row in examples:
			if len(row[-1]) is 8:
				dCount +=1
			else:
				rCount +=1
		if dCount > rCount:
			ans = treeNode()
			ans.setVals("Democrat", float(dCount)/(dCount + rCount))
			return ans
		else:
			ans = treeNode()
			ans.setVals("Republican", float(rCount)/(dCount + rCount))
			return ans
	ans = treeNode()
	newInd = attributes[impVals.index(mVal)]
	ans.setInd(newInd)
	newAttrs = []
	for val in attributes:
		if val is newInd:
			continue
		newAttrs.append(val)

	if DEBUG:
		nhsh = {}
		for ex in examples:
			setA = (ex[newInd], ex[-1])
			if setA in nhsh:
				nhsh[setA]+=1
			else:
				nhsh[setA] = 1
		print nhsh

	for vote in votes:
		newexs = []
		for ex in examples:
			if ex[newInd] == vote:
				newexs.append(ex)
		ans.addChild(treeGen(newexs, newAttrs, examples), vote)
	return ans



with open(inputs[2]) as testFile:
	head = treeGen(bigLst, range(len(bigLst[0])-1), bigLst)
	csvReader = csv.reader(testFile)
	for row in csvReader:
		itern = head
		while itern:
			ind = itern.ind
			if ind == -1:
				print str(itern.party)+","+str(itern.odds)
				break
			itern = itern.children[row[ind]]

	




