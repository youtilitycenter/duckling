# coding=utf-8

from bs4 import BeautifulSoup
import urllib2
from rreplace import rreplace


lettere = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "L", "M", "N", "O", "P",
           "R", "S", "T", "U", "V"]

for lettera in lettere:
    print(lettera)
    with open("../Dictionary/Verb/IT/Values/Irregular/" + lettera + ".hs") as f:
        for line in f:
            if "verbs :: [(Text, String)]" in line:
                with open("../Dictionary/Verb/IT/Values/Irregular/" + lettera + "_.hs", 'a') as f:
                    f.write("verbs :: [(Text, String, Text, Text)]\n")

            elif "\"" in line and "[" in line:
                parts = line.split(",")

                newLine = ""
                for i, part in enumerate(parts):
                    if i != 3 and i != 5:
                        if i > 0:
                            newLine += "," + part
                        else:
                            newLine += part
                newLine += ")"
                with open("../Dictionary/Verb/IT/Values/Irregular/" + lettera + "_.hs", 'a') as f:
                    f.write(newLine + "\n")

            elif "\"" in line:
                parts = line.split(",")

                newLine = ""
                for i, part in enumerate(parts):
                    if i != 4 and i != 6:
                        if i > 0:
                            newLine += "," + part
                        else:
                            newLine += part
                newLine += ")"

                with open("../Dictionary/Verb/IT/Values/Irregular/" + lettera + "_.hs", 'a') as f:
                    f.write(newLine + "\n")

            else:
                with open("../Dictionary/Verb/IT/Values/Irregular/" + lettera + "_.hs", 'a') as f:
                    f.write(line)
