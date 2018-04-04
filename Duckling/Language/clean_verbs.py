# coding=utf-8

from bs4 import BeautifulSoup
import urllib2
from rreplace import rreplace

lettera = "z"

verbsAdded = []

with open("./IT/" + lettera + ".txt") as f:
    for line in f:
        if line.strip() != "":
            verbo = line.split("\"")[1].strip()
            #verbo = "handicappi"
            if verbo not in verbsAdded:
                verbsAdded.append(verbo)
                print(verbo)

                with open("./IT/_" + lettera + ".txt", 'a') as f:
                    # , ("avere", "(hai)", "presente", "indicativo", "2a singolare")
                    root = verbo
                    if root.endswith('are') or root.endswith('ere') or root.endswith('ire'):
                        root = root[:-3]

                    f.write("\n, (\"" + verbo + "\", \"(" + verbo + "|" + root + "(e|a|ò|o|i)(rò$|rai$|rà$|remo$|rete$|ranno$|i$|sti$|mmo$|ste$|rono$|to$|ta$|ti$|te$|amo$|te$|no$|vo$|vi$|va$|vamo$|vate$|vano$|rei$|resti$|rebbe$|remmo$|reste$|rebbero$)?)\")")

    print(len(verbsAdded))
