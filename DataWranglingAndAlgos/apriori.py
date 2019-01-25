import sys
import csv
import math
inputs = sys.argv
if not len(inputs) is 3:
	print("Input error call program as following")
	print("python apriori.py data.csv support%")
	exit()


votmat = []
supper = float(inputs[2])/100

with open(inputs[1]) as trainFile:
	csvReader = csv.reader(trainFile)
	rownum = -1
	for row in csvReader:
		rownum += 1
		votmat.append([])
		rLen = len(row)
		for vote in row:
			votmat[rownum].append(vote)
		# EndFor
	# EndFor
# EndOpen

finalqueue = []
curqueue = []


for ind in range(rLen):
	curqueue.append([ind])



def sup(collist):
	yCount = 0
	for ind in range(rownum):
		yCount += 1
		for colind in collist:
			if not (votmat[ind][colind] == "Yea"):
				yCount -= 1
				break;
	return float(yCount)/rownum


def purge():
	ind = 0
	while ind < len(curqueue):
		if sup(curqueue[ind]) < supper:
			curqueue.pop(ind)
			continue
		ind+=1


def record():
	for lst in curqueue:
		finalqueue.append(lst)

def merge(lstA, lstB):
	lst = []
	for item in lstA:
		lst.append(item)
	for item in lstB:
		if not item in lst:
			lst.append(item)
	return lst

def same(lstA, lstB):
	lstA.sort()
	lstB.sort()
	return lstA == lstB

def inLst(item, lst):
	for check in lst:
		if same(item, check):
			return True
	return False

def removeRepeats(mLst):
	aLst = []
	for item in mLst:
		if not inLst(item, aLst):
			aLst.append(item)
	return aLst


def nextLst(smlst):
	mLst = []
	for x in range(len(smlst)):
		for y in range(x+1, len(smlst)):
			mLst.append(merge(smlst[x], smlst[y]))
	return removeRepeats(mLst)

purge()
curqueue = nextLst(curqueue)

#Section that does as project spect
while len(curqueue) > 0:
	purge()
	#print(curqueue)
	record()
	curqueue = nextLst(curqueue)

# #The code that only does two
# purge()
# record()

# #end of code that does two



print(finalqueue)

for lst in finalqueue:
	llen = len(lst)
	for ind in range(llen):
		if ind == llen-1:
			print("->" + str(lst[ind]))
		else:
			print(lst[ind], end = ",")

exit()

