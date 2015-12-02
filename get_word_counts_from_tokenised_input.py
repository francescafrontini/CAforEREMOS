import codecs
import sys


if len(sys.argv) > 1:
	# Usage: python main.py input
	input_file = sys.argv[1]

else:
	print "pass input"

	#sys.exit()

file = codecs.open(input_file, "r", encoding="utf-8")
wordcount={}
for word in file.read().split():
    if word not in wordcount:
        wordcount[word] = 1
    else:
        wordcount[word] += 1
        
        
file.close()

o = open(input_file+"_count", 'w') # uncomment for option (1)

for k,v in wordcount.items():
	o.write(k.encode("utf-8") + "\t" + str(v) + "\n")


o.close()
