from bs4 import BeautifulSoup
import urllib2

lettera = "z"

with open("./_IT/" + lettera + ".txt") as f:
    for line in f:
        if "( VERB" in line:
            verbo = line.split("(")[0].strip()
            #verbo = "handicappi"

            print(verbo)
            contents = urllib2.urlopen("https://www.i-verbi.it/_xhr_AVmis.php?rawText=" + verbo).read()
            soup = BeautifulSoup(contents, 'html.parser')
            lemma = soup.find_all('strong')

            _lemma = ""
            if len(lemma) > 1:
                _lemma = str(lemma[1]).replace("<strong>", "").split("<br/>")[0].strip()

            tables = soup.find_all('table')

            _verbo = ""
            _diatesi = []
            _modo = []
            _tempo = []
            _persona = []

            for table in tables[::2]:
                localSoup = BeautifulSoup(str(table), 'html.parser')
                contentTable = localSoup.find_all("tr")

                for value in contentTable[1::2]:
                    internalSoup = BeautifulSoup(str(value), 'html.parser')
                    exactValue = localSoup.find_all("strong")

                    _diatesi.append(str(exactValue[0]).replace("<strong>", "").replace("</strong>", ""))
                    _modo.append(str(exactValue[1]).replace("<strong>", "").replace("</strong>", ""))
                    _tempo.append(str(exactValue[2]).replace("<strong>", "").replace("</strong>", "").replace("<br/>", " "))
                    _persona.append(str(exactValue[3]).replace("<strong>", "").replace("</strong>", "").replace("<br/>", "-"))

            _diatesi = list(set(_diatesi))
            _modo = list(set(_modo))
            _tempo = list(set(_tempo))
            _persona = list(set(_persona))

            if _lemma != "":
                with open("./IT/" + lettera + ".txt", "a") as f:
                    # , ("avere", "(hai)", "presente", "indicativo", "2a singolare")
                    f.write("\n, (\"" + _lemma + "\", \"(" + verbo + ")\", \"" + '#'.join(_tempo) + "\", \"" + '#'.join(_modo) + "\", \"" + '#'.join(_persona) + "\", \"" + '#'.join(_diatesi) + "\")")
