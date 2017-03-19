import codecs
import collections
import sys
import os
from os import listdir

#this script takes N texts of the form
#freq\tpattern
#and transforms them into a format suitable to be used with the CA_factominer script

if len(sys.argv) > 1:
	# Usage: python main.py input
	input_dir = sys.argv[1]


else:
	print "input dir"

files = os.listdir( input_dir )

model = dict()
counter = 1
freqs = collections.defaultdict(dict)

for input_file in files:
	text = input_file[:-4]
	print text
	
	f = codecs.open(input_dir + "/" + input_file, encoding="latin-1")
#	f = open(input_dir + "/" + input_file)

	for line in f:
		line = line.strip()
		cols = line.split("\t")
		pattern_name = ""
		if cols[1] not in model:
			pattern_name = str(counter)
			counter +=1
			model[cols[1]]=pattern_name
		else: 
			pattern_name = model[cols[1]]
		freqs[text]["Pattern_"+pattern_name] = int(cols[0])
			
	f.close()

o = codecs.open("Model.txt", "w", "utf-8")


#print the Model.txt file in sorted order of value (number of pattern id)
for k in sorted(model, key=lambda i: int(model[i])):
	o.write(k + "\t" +  str(model[k])+"\n")
o.close()

try:
    os.stat("Models")
except:
    os.mkdir("Models")  

for f in freqs:
	text_model_file = f+"_Model.txt"
	o = codecs.open("Models/"+text_model_file, "w", "utf-8")
	o.write("Pattern_ID" + "\t" +  f + "\n")
	for p in freqs[f]:
		o.write(p + "\t" + str(freqs[f][p])+"\n")
	o.close()



#o = codecs.open("out", "w", "utf-8") # uncomment for option (2)
