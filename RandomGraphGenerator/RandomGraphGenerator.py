from sys import argv
from random import randint

# generates a graph with n vertices and random number of random edges (up to binomial(n,2)) to a text file
# uses the write format used by the graph tool from the main project
# uses Python's randint function (based on the Mersenne Twister algorithm)

# Usage:
# python3 RandomGraphGenerator.py [output textfile name] [number of vertices]
# if the textfile doesn't exist, it's created; if it exists, it gets overwritten
# if the n value is negative, it is treated as 0; if it can't be casted to int, an error message is printed and the program doesn't create any output file

if(len(argv)!=3):
    print("Wrong number of arguments!\n\n")

else:
    try:
        fileName = argv[1]
        n = int(argv[2])
        
        if(n<0):
            n=0
        
        maxNumberOfEdges = (n*(n-1))//2
        
        file = open(fileName,'w')
        file.write(str(n)+"\n")
        
        numberOfEdges = randint(0,maxNumberOfEdges)
        
        for i in range(numberOfEdges):
            file.write(str(randint(1,n))+" "+str(randint(1,n))+"\n")
    
    except:
        print("Couldn't cast the argument "+argv[1]+" to integer!\n\n")