west = ["WA","OR","CA","MT","ID","WY","NV","UT","AZ","CO","NM","AK","HI"]
midwest = ["ND","SD","NE","KS","MN","IA","MO","WI","IL","MI","IN","OH"]
northeast = ["NY","PA","VT","NH","ME","MA","CT","RI","NJ"]
south = ["OK","TX","AR","LA","KY","TN","MS","AL","WV","MD","DE","VA","NC","GA","SC","FL","DC"]
f = open("location.csv","r")
o = open("output.csv","w")
f.readline()
i = 1
for line in f:
    o.write(str(i) + ",")
    tmp = line.rstrip().split(",")[2]
    states = tmp.strip('" ').split("-")
    for state in states:
        if state in west:
            o.write("0,")
        elif state in midwest:
            o.write("1,")
        elif state in northeast:
            o.write("2,")
        elif state in south:
            o.write("3,")
        else:
            o.write("NA: " + state + ",")
    o.write("\n")

f.close()
o.close()
    
