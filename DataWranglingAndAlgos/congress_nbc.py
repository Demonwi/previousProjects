import sys
import csv
import math
inputs = sys.argv
if not len(inputs) is 3:
	print("Input error call program as following")
	print("python congress_nbc.py arg1.csv arg2.csv")
	exit()

votes = ["Nay", "Yea", "", "Not Voting", "Present"]
rLen = -1
dLst = []
rLst = []
eLst = []
with open(inputs[1]) as trainFile:
	csvReader = csv.reader(trainFile)
	rnum = 0
	for row in csvReader:
		rnum += 1
		# if rnum > 300:
		# 	print "stopped train"
		# 	break
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
	
# testing 


	# print rnum
	# dCount = dLst[-1]["Democrat"]
	# rCount = rLst[-1]["Republican"] 
	# eCount = dCount + rCount
	# for row in csvReader:
	# 	dProb = 1.0
	# 	rProb = 1.0
	# 	for ind in range(rLen-1):
	# 		choice = row[ind]
	# 		dHsh = dLst[ind]
	# 		rHsh = rLst[ind]
	# 		if choice in dHsh:
	# 			dProb *= float(dHsh[choice]+1)/dCount
	# 		else:
	# 			dProb *= 1.0/dCount
	# 		if choice in rHsh:
	# 			rProb *= float(rHsh[choice]+1)/rCount
	# 		else:
	# 			rProb *= 1.0/rCount
	# 	ep = dProb+rProb
	# 	dCert = dProb/ep
	# 	rCert = rProb/ep
	# 	if dCert < rCert:
	# 		# wtf = wonderful true false
	# 		wtf = len("Republican") is len(row[-1])
	# 		print wtf
	# 		if not wtf:
	# 			print (dCert,rCert)
	# 			print row[-1]
	# 	else:
	# 		wtf = len("Democrat") is len(row[-1])
	# 		print wtf
	# 		if not wtf:
	# 			print (dCert, rCert)
	# 			print row[-1]
	# print("Trained")
	# print(dLst[1])
	# print(rLst[1])
	# print(eLst[1])
	# print(dhsh)
	# print(rhsh)
# EndOpen



with open(inputs[2]) as testFile:
	csvReader = csv.reader(testFile)
	rows = 0
	dCount = dLst[-1]["Democrat"]
	rCount = rLst[-1]["Republican"] 
	eCount = dCount + rCount
	for row in csvReader:
		dProb = 1.0
		rProb = 1.0
		for ind in range(rLen-1):
			choice = row[ind]
			dHsh = dLst[ind]
			rHsh = rLst[ind]
			if choice in dHsh:
				dProb *= float(dHsh[choice]+1)/dCount
			else:
				dProb *= 1.0/dCount
			if choice in rHsh:
				rProb *= float(rHsh[choice]+1)/rCount
			else:
				rProb *= 1.0/rCount
		ep = dProb+rProb
		dCert = dProb/ep
		rCert = rProb/ep
		if dCert < rCert:
			print "Republican,"+str(rCert)
		else:
			print "Democrat,"+str(dCert)





