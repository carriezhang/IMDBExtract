import urllib
from bs4 import BeautifulSoup
# open the top 250 films page from IMDB
url = "http://www.imdb.com/chart/top?ref_=nv_ch_250_4"
page = urllib.request.urlopen(url)
soup = BeautifulSoup(page.read())
movies = soup.find_all(class_ = "titleColumn")

# File to save informations
f0 = open("url.txt","wb") # Save url of 250 films
f1 = open("movie.txt","wb") # Save movie name
f2 = open("rate.txt","wb") # Save movie rating
f3 = open("year.txt","wb") # Save time information
for movie in movies:
    for link in movie.find_all("a",href = True):
        name = link.contents
        f0.write(str(link["href"]).encode("utf-8"))       
        f0.write("\n".encode("utf-8"))
        f1.write(str(name[0]).encode("utf-8"))       
        f1.write("\n".encode("utf-8"))
        
for movie in movies:
    for block in movie.find_all("span",{"name" : "ir"}):
        f2.write(str(block["data-value"]).encode('utf-8') )
        f2.write("\n".encode("utf-8"))
        
for movie in movies:
    for block in movie.find_all("span",{"class" : "secondaryInfo"}):
        f3.write(str(block["data-value"]).encode('utf-8') )
        f3.write("\n".encode("utf-8"))

f0.close()
f1.close()
f2.close()
f3.close()
