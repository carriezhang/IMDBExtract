import urllib
import time
from bs4 import BeautifulSoup
base  = "http://www.imdb.com"
url1 = "/title/tt0111161/?ref_=chttp_tt_1"
f1 = open("url.txt","r") # Read from url file
f2 = open("genre.txt","w") #save genre information for each film
f3 = open("rating.txt","w") # save rating counts
f4 = open("genreCounts.txt","w") # save genre counts

f3.write("Total Rating,Total Review,Total Critic" + "\n")
count = 0
genreSet = {}
for line in f1:
    url1 = line.rstrip()
    page = urllib.request.urlopen(base + url1)
    soup = BeautifulSoup(page.read())
    genre = soup.find_all("span",{"class" : "itemprop", "itemprop" : "genre"})
    for g in genre:
        tmp = str(g.contents).strip("[]'")
        if tmp not in genreSet:
            genreSet[tmp] = 1
        else:
            genreSet[tmp] += 1
        f2.write(tmp)
        f2.write(",")
    f2.write("\n")
    nRate = soup.find_all("span",{"itemprop" : "ratingCount"})
    for n in nRate:
        tmp = str(n.contents)
        f3.write(tmp.replace(",", "").strip("[]'"))
        f3.write(",")


    nReview = soup.find_all("span",{"itemprop" : "reviewCount"})
    for n in nReview:
        tmp = str(n.contents)
        f3.write(tmp.replace(",", "").split()[0].strip("[]'"))
        f3.write(",")
    f3.write("\n")
    count += 1
    time.sleep(5) # delays for 5 seconds
    print(count) # Indicate the number of pages possessed

for g in genreSet:
    f4.write(g)
    f4.write(",")
    f4.write(str(genreSet[g]))
    f4.write("\n")
f1.close()
f2.close()
f3.close()
f4.close()
