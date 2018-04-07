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
            if "\"" in line:
                parts = line.split("\"")
                # 5
                newLine = ""
                for i, part in enumerate(parts):
                    if i == 5:
                        if part == "PRESENTE":
                            newLine += "present"
                        elif "PASSATO" in part:
                            newLine += "past"
                        elif part == "IMPERFETTO":
                            newLine += "imperfect"
                        elif "FUTURO" in part:
                            newLine += "future"
                        elif "#" in part:
                            if part.split("#")[0] == "PRESENTE":
                                newLine += "present"
                            elif "PASSATO" in part.split("#")[0]:
                                newLine += "past"
                            elif part.split("#")[0] == "IMPERFETTO":
                                newLine += "imperfect"
                            elif "FUTURO" in part.split("#")[0]:
                                newLine += "future"
                            else:
                                newLine += part
                        else:
                            newLine += part
                    elif i == 7 and False:
                        if "#" in part or "-" in part:
                            newLine += "nothing"
                        elif "1a s" in part:
                            newLine += "1s"
                        elif "2a s" in part:
                            newLine += "2s"
                        elif "3a s" in part:
                            newLine += "3s"
                        elif "1a p" in part:
                            newLine += "1p"
                        elif "2a p" in part:
                            newLine += "2p"
                        elif "3a p" in part:
                            newLine += "3p"
                        else:
                            newLine += "nothing"
                    else:
                        newLine += part

                    if i != 8:
                        newLine += "\""

                with open("../Dictionary/Verb/IT/Values/Irregular/" + lettera + "_.hs", 'a') as f:
                    f.write(newLine)

            else:
                with open("../Dictionary/Verb/IT/Values/Irregular/" + lettera + "_.hs", 'a') as f:
                    f.write(line)
