# Versicherungsapp
## Einleitung
Dieser Show Case (Version 5) ist für eine Versicherung oder andere Unternehmen, welche an einem Chatbot mit Textklassifikation interessiert sind.

Hierbei wird eine Anfrage des Kunden direkt zu vier Formularen der Bereiche **"Household**", **"Car"**, **"Health"** oder **"Others"** klassifiziert. "Others" stellt zum Beispiel eine Anfrage bezüglich einer Haftpflichtversicherung dar.

Für die Ausführung wird eine installierte Version von [R](https://www.r-project.org/) (Version >= 3.0.2) und am Besten [RStudio](https://www.rstudio.com/products/rstudio/#Desktop) benötigt.

Hier gibt es Dokumentationen für [R](https://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-install-R-for-Windows_003f) und für [RStudio Shiny](https://www.rstudio.com/products/shiny/).


Alle vorherigen Versionen dieser App sind bereits im Repository [Chatbot](https://github.com/acinorev/Chatbot) enthalten. Zugriff haben derzeit Matthias Fuhr, Matthias Leopold und Veronica Pohl.

### Gebrauch
Nach Öffnen des R-files "app.R" in RStudio müssen alle R Packages (siehe unten in "Package Information") installiert werden. Dies geschieht z.B. für "shiny" mit dem Befehl
```
install.packages("shiny")
```
im RStudio Bedienpult (console).

Danach kann die App durch Klicken des Buttons "Run App" gestartet werden.

<img align="center" src="RunApp.png">

Hierbei kann zwischen dem Öffnen in einem neuen Fenster, im Browser und im RStudio Viewer gewählt werden.

#### Hinweise zur Texteingabe:
* Die Klassifikation erfolgt nur, wenn mindestens 3 Worte einer Klasse erkannt sind (z.B. für "Car": Toyota, back, parked) oder wenn der Unterschied erkannter Worte der Klassen mindestens 2 Worte sind (z.B. 4 Worte aus Klasse "Car", 2 Worte aus Klasse "Health" => "Car")
* Um Vor- und Nachnamen zu erkennen benötigt man direkt zuvor UNBEDING "Kind regards,"!
* Telefonnummern müssen 10-stellige Zahlen ohne Unterbrechung sein, z.B. 0796932792
* Versicherungsnummern müssen die Form "3Zahlen-3Zahlen.1Zahl" haben, z.B. 467-836.1

Beispiele für jede Klassifizierung sind in der Datei "input.txt" zu finden!

### Inhalt der App
Diese Version der Versicherungsapp sieht gestartet so aus:

<img align="center" src="Versicherungsapp.png">

Implementiert sind:

* Darstellung des Handy-Fotos im Hintergrund
* Anordnung der jeweiligen Panels, so dass alle vier Formulare rechts präsent sind.
* Kleine Icons für jedes Formular
* Drei Dictonaries ( Haushaltsgegenstände für Klasse "Household", Autonamen für Klasse "Car", alle Teile eines Körpers für Klasse "Health")
* Transformation des eingegebenen Textes zu Kleinbuchstaben
* Löschen aller Satzzeichen (. , -)
* Löschen aller Stopworte der englischen Sprache
* Klassifikation des Textes durch Anzahl der vorhandenen Wörter bzw. Wortteile.
* Erkennung der Versicherungsnummer anhand der speziellen Form ("3Zahlen-3Zahlen.1Zahl")
* Erkennung des Vor- und Nachnamens durch den Satzteil "Kind regards,"
* Erkennung der Telefonnummer anhand der speziellen Form (10-stellig)
* Berechnung der Email Adresse anhand des Vor- bzw. Nachnamens: VornameNachname@company.com
* Bei Betätigung des Buttons "Upload Image" Anzeige des jeweilig zugehörigen Bildes
* Bei Betätigung des Buttons "Send" Texterkennung, Reaktion des Panels "Bot's response", "Your message is classified as" und der Wahl des Formulars (mit Erhellen bzw. Ergrauen des jeweiligen Feldes). Ausserdem Einfüllen der vorhandenen Information des gegebenen Textinhaltes. Bei Klassifikation "Others": Link zur Webseite des [Zubot](https://zuehlke-bot.herokuapp.com/) (Zuehlke Bot) darstellen und in einem extra Fenster darstellen.
* zwei Standard-Antworten des Chatbots
* Box für jede Klasse für Kommentare

Anregungen für weiterführende Funktionen

* Antwort des Chatbots bei fehlenden Angaben
* Anzeige eines Chatverlaufes
* Textklassifikation mit anderer Methode (z.B naive Bayes) als Anzahl der vorhandenen Wortteile
* Adresse, Geburtstag in Formular hinzufügen
* Angegebene Zeit und Ort in Formular verarbeiten
* Anhand von Trainingsdaten Text klassifizieren

### R Paket Information


| Paket Name | Version oder höher | 
| ---------- |--------------------| 
| NLP        |      0.1-11        | 
| shiny      |      1.0.5         |  
| tm         |      0.7-3         |   

                
               
        
