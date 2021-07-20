# https://realpython.com/python-csv/#reading-csv-files-with-csv

import csv

with open('GA/vaccinations.csv') as csv_file:
    table1=[]
    csv_reader = csv.reader(csv_file, delimiter=',')
    for row in csv_reader:
        table1.append(row)

with open('results/myOut.finalBests.csv') as csv_file:
    table2=[]
    csv_reader = csv.reader(csv_file, delimiter=',')
    for row in csv_reader:
        table2.append(row)

#print (table1)
#print (table2)

newTable=[]
newTable.append([])

newTable[0].append("n")
for j in range(1,3):
    newTable[0].append(eval(table1[0][j]))

for i in range(1,6):
    newTable.append([])
    for j in range(0,2):
        newTable[i].append(eval(table1[i][j]))
    for j in range(0,7):
        newTable[i].append(eval(table2[1][2+j+(i-1)*7]))
newTable.append([])

newTable[6].append(eval(table1[6][0]))
newTable[6].append(table1[6][1])


with open('GA/vaccinationsPostGA.csv', mode='w') as ga:
    ga_writer = csv.writer(ga, delimiter=',', quotechar='"',\
                           quoting=csv.QUOTE_NONNUMERIC)

    for i in range(0,7):
        ga_writer.writerow(newTable[i])

for i in range(1,6):
    print("%g & %g & %.2g & %.2g & %.2g & %.2g & %.2g & %.2g & %.2g \\\\" %  \
          (newTable[i][0],newTable[i][1],newTable[i][2],newTable[i][3],\
           newTable[i][4],newTable[i][5],newTable[i][6],newTable[i][7],newTable[i][8]))

print("\n\n")

for i in range(1,6):
    print("%g,%g,%.2g,%.2g,%.2g,%.2g,%.2g,%.2g,%.2g" %  \
          (newTable[i][0],newTable[i][1],newTable[i][2],newTable[i][3],\
           newTable[i][4],newTable[i][5],newTable[i][6],newTable[i][7],newTable[i][8]))


print("\n\n[")

for i in range(1,6):
    print("[%g,%g,%.2g,%.2g,%.2g,%.2g,%.2g,%.2g,%.2g]," %  \
          (newTable[i][0],newTable[i][1],newTable[i][2],newTable[i][3],\
           newTable[i][4],newTable[i][5],newTable[i][6],newTable[i][7],newTable[i][8]))
print("]")

   
