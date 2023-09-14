Deskriptive Statistiken
===

Deskriptive Statistiken ermöglicht es dem Benutzer, grundlegende deskriptive Statistiken, Histogramme und Dichtediagramme, Korrelationsdiagramme, Boxplots und Häufigkeitstabellen zu erhalten.

### Eingaben
-------

#### Eingabe-Box
- Variablen: Alle Variablen von Interesse.
- Aufteilung: Es kann nach einer kategorischen Variable aufgeteilt werden, z.B. nach einer experimentellen Bedingung.
- Deskriptive Tabelle transponieren: Transponiert die Haupttabelle
- Häufigkeitstabellen: Zeigt eine Häufigkeitstabelle für jede Variable an.
  - Maximale Anzahl verschiedener Werte: Häufigkeitstabellen werden nur für Variablen gezeigt, die weniger einzigartige Beobachtungen haben als die angegebene Anzahl.
- Stängel-Blatt-Tabellen: Eine Tabelle, die alle numerischen Beobachtungen von klein nach groß anzeigt. Die Beobachtungen werden aufgeteilt in einen "Stamm" mit den erste(n) Ziffer(n), und ein "Blatt" mit der nachfolgenden Ziffer.
  - Skalierung: Steuert die Länge der Tabelle. Erhöhen der Skalierung ergibt eine gröbere Tabelle, und dagegen ergibt das Verringern der Skalierung eine geglättete Tabelle.

### Diagramme
- Verteilungsdiagramme: Für kontinuierliche Variablen wird ein Histogramm gezeigt. Für nominale und ordinale Variablen wird eine Häufigkeitsverteilung gezeigt.
  - Dichte anzeigen (nur für kontinuierliche Variablen): Zeigt die Dichte basierend auf einer nichtparametrischen Dichteschätzung.
- Korrelationsdiagramme: Für kontinuierliche Variablen werden Histogramme, Dichtediagramme und Streudiagramme gezeigt.
- Boxplots: Für kontinuierliche Variablen wird ein Boxplot gezeigt.
  - Ausreißer beschriften: Optional werden die Ausreißer beschriftet. Ausreißer basieren auf dem Interquartilsabstand (IQR), d.h. [25. Perzentil] - 1,5 × IQR und [75. Perzentil] + 1,5 × IQR.
  - Farbe: Anzeigen in Farbe.
    - Enthält auswählbare Boxplot-, Violin- und Jitter-Elemente zur Anzeige der Verteilung der Daten.
  - Pareto-Diagramme: Können nur für nominale und ordinale Variablen gezeigt werden. Zeigt eine absteigend sortierte Häufigkeitsverteilung. Standardmäßig wird eine kumulative Linie hinzugefügt, die den prozentualen Beitrag jedes Faktors/ jeder Stufe innerhalb der Variable angibt.
  - Pareto-Regel: Zeichnet zwei zusätzliche Linien, eine vertikal und eine horizontal, die sich schneiden und an der Höhe der eingegebenen Prozentzahl stoppen (skaliert mit der kumulativen Linie). Standardmäßig ist dies auf 95% eingestellt. Dies kann auf den gewünschten Prozentsatz angepasst werden.
- Likert-Diagramme: Können nur für nominale und ordinale Variablen gezeigt werden. Zeigt ein horizontal gestapeltes Balkendiagramm; für jede zusätzlich hinzugefügte Variable eine neue Leiste generiert.
  - Annehmen, dass alle Variablen dieselbe Stufen haben: Zeigt ein einzelnes Diagramm mit allen Variablen. Die Variablen müssen die gleiche Anzahl von Stufen haben.
  - Einstellbare Schriftgröße für vertikale Achse: Steuert die Schriftgröße der Variablennamen auf der vertikalen Achse. Standardmäßig wird die normale Schriftgröße verwendet; es sind drei weitere Größen verfügbar.
- Dichtediagramme: Können nur für metrische (kontinuierliche) Variablen gezeigt werden. Verwendet die Kerndichteschätzung, um die Verteilung einer numerischen Variable zu visualisieren.
  - Getrennte Dichten: Durch das Einfügen einer nominalen oder ordinalen Variable in dieses Feld werden verschiedene Verteilungen gezeigt, die den verschiedenen Stufen der Variable entsprechen.
  - Transparenz: Einstellbare Transparenz für die Flächenfarbe unterhalb der Dichtelinie (einstellbar von 0 bis 100).

### Statistiken
- Zentrale Tendenz (nur für kontinuierliche Variablen):
  - Modus: Modus (Modalwert) der Datenpunkte; wenn mehr als ein Modus vorhanden ist, wird nur der erste berichtet. Für nominale und ordinale Daten ist der Modus der am häufigsten beobachtete Wert. Für kontinuierliche Daten ist der Modus der Wert mit der höchsten Dichteschätzung (siehe 'Verteilungsdiagramme' -> 'Dichte anzeigen'). Falls eine Fußnote zur Multimodalität für kontinuierliche Variablen angegeben wird, empfehlen wir, die Daten zu visualisieren, um die Multimodalität zu überprüfen.
  - Median: Median der Datenpunkte.
  - Mittelwert: Arithmetischer Mittelwert der Datenpunkte.
- Percentil-Werte:
  - Quartile: Zeigt das 25., 50. und 75. Perzentil der Datenpunkte an.
  - Schnittpunkte für x gleiche Gruppen: Zeigt die Schnittpunkte an, die die Daten in x gleich große Gruppen einteilen; Standardeinstellung ist 4 gleich große Gruppen.
  - Perzentile: Zeigt das x-te Perzentil an; Perzentilwerte müssen durch Komma getrennt sein.
- Streuung (nur für kontinuierliche Variablen):
  - Std.-Abweichung: Standardabweichung der Datenpunkte.
  - Variationskoeffizient: Der Variationskoeffizient gibt uns die relative Streuung der Daten an im Gegensatz zur Standardabweichung, die die absolute Streuung angibt. Dazu wird die Standardabweichung durch den Mittelwert dividiert, sodass die Einheit herausgekürzt wird.
  - MAD: Median der absoluten Abweichungen der Datenpunkte.
  - MAD robust: Robuster Median der absoluten Abweichungen der Datenpunkte, angepasst durch einen Faktor für asymptotische Normalität.
  - IQR: Interquartilsabstand der Datenpunkte; 75. Perzentil - 25. Perzentil.
  - Varianz: Varianz der Datenpunkte.
  - Wertebereich: Wertebereich der Datenpunkte; Maximum - Minimum.
  - Minimum: Minimalwert der Datenpunkte.
  - Maximum: Maximalwert der Datenpunkte. 
- Verteilung:
  - Schiefe: Schiefe der Verteilung der Datenpunkte.
  - Wölbung: Wölbung (Kurtosis) der Verteilung der Datenpunkte.
  - Shapiro-Wilk-Test
  - Summe: Summe der Datenpunkte.
- Inferenz:
  - Std.-Fehler Mittelwert: Standardfehler des Mittelwerts.
  - Konfidenzintervall des Mittelwerts:
    - Breite: Breite des Konfidenzintervalls.
    - Methode: Wie soll das Konfidenzintervall berechnet werden? Standardmäßig wird ein `T-Modell` verwendet, das die gleichen Ergebnisse wie ein T-Test für eine Stichprobe führt. Alternative Optionen sind ein normales Modell (\\\\(\\bar{x} \\pm z_{95}\\times SE\\\\)) oder `Bootstrap`.
  - Konfidenzintervall für der Std.-Abweichung: ein Konfidenzintervall für der Standardabweichung auf der Grundlage von Bootstrap-Stichproben.
  - Konfidenzintervall für der Varianz: ein Konfidenzintervall für der Varianz auf der Grundlage von Bootstrap-Stichproben.
  - Bootstrap Stichprobenanzahl: die Anzahl der Bootstrap-Stichproben.

### Ausgabe
-------
#### Deskriptive Statistiken
- Gültig: Anzahl der gültigen Fälle.
- Fehlend: Anzahl der fehlenden Werte.
- Modus: Modus der Datenpunkte.
- Median: Median der Datenpunkte.
- Mittelwert: Arithmetisches Mittel der Datenpunkte.
- Std.-Fehler Mittelwert: Standardfehler des Mittelwerts.
- Standardabweichung: Standardabweichung der Datenpunkte.
- MAD: Median der absoluten Abweichungen
- IQR: Interquartilsabstand
- Varianz: Varianz der Datenpunkte.
- Schiefe: Schiefe der Verteilung der Datenpunkte.
- Std.-Fehler Schiefe: Standardfehler der Schiefe.
- Kurtosis: Kurtosis der Verteilung der Datenpunkte.
- Std.-Fehler Wölbung: Standardfehler der Wölbung.
- Shapiro-Wilk: Wert der Shapiro-Wilk-Statistik.
- P-Wert Shapiro-Wilk: p-Wert der Shapiro-Wilk-Statistik.
- Wertebereich: Wertebereich der Datenpunkte.
- Minimum: Minimalwert der Datenpunkte.
- Maximum: Maximalwert der Datenpunkte.
- Quartile: 25., 50. und 75. Perzentil der Datenpunkte.
- Schnittpunkte für x gleiche Gruppen: Schnittpunkte, die die Daten in x gleich große Gruppen unterteilen.
- Perzentile: Zeigt die x-ten Perzentile an.
- Summe: Summe der Datenpunkte.

#### Verteilungsdiagramme
- Zeigt für kontinuierliche Variablen ein Histogramm und die Anpassung eines nichtparametrischen Dichteschätzers.
- Für nominale und ordinale Variablen wird eine Häufigkeitsverteilung gezeigt.

#### Korrelationsdiagramm
- Zeigt eine Matrix von Diagrammen zwischen kontinuierlichen Variablen an, mit Streudiagrammen zwischen den Variablen außerhalb der Diagonaleinträge und Histogrammen und Dichtediagrammen in den Diagonaleinträgen.
 Die Linie stellt die Anpassung eines Polynoms 1., 2., 3. oder 4. Ordnung dar (die Auswahl basiert auf dem Bayesschen Informationskriterium; Schwarz, 1978).

#### Boxplots
- Zeigt für kontinuierliche Variablen einen Boxplot. Optional werden die Ausreißer beschriftet. Die Ausreißer basieren auf dem Interquartilabstand (IQR), d. h. [25. Perzentil] - 1,5 × IQR und [75. Perzentil] + 1,5 × IQR. Kann auch in Farbe angezeigt werden und verfügt über auswählbare Boxplot-, Violin- und Jitter-Elemente zur Darstellung der Verteilung der Daten. Dies kann durch eine kategorische Variable wie die Versuchsbedingung aufgeteilt werden.

#### Pareto-Diagramme
- Zeigt die Anzahl der einzelnen Faktoren/Stufen innerhalb der Variablen in absteigender Reihenfolge an. Die y-Achse stellt die Häufigkeit (Anzahlen als graue Balken) jedes Faktors/jeder Stufe dar, die x-Achse stellt die Faktoren/Stufen der Variablen in einer geordneten Reihenfolge dar.
- Standardmäßig wird eine kumulative Linie gezeichnet, die den proportionalen Beitrag eines jeden Faktors anzeigt. Eine zweite vertikale Achse auf der rechten Seite des Diagramms skaliert mit dieser kumulativen Linie und beschriftet sie mithilfe von Prozentsätzen.
- Wenn 'Pareto-Regel' aktiviert ist, erlauben die beiden neuen Linien eine präzisere Bewertung des Beitrags der Faktoren/Stufen zum Gesamtbeitrag (in Prozent), bei Verwendung angepasster Eingabezahlen.

#### Likert-Diagramme
- Zeigt ein horizontal gestapeltes Balkendiagramm, das den Beitrag der Stufen innerhalb einer Variablen in Prozent anzeigt. Die Reihenfolge der Stufen hängt von der definierten Reihenfolge in der JASP-Datentabelle ab. Eine Legende unterhalb des Diagramms gibt einen Überblick über die Stufen und ihre jeweiligen Farben im Diagramm.
  - Die y-Achse steht für die verwendeten Variablen, die x-Achse für die Prozentsätze. Der prozentuale Anteil aller unteren Stufen (unterhalb der mittleren Stufe) und oberen Stufen (oberhalb der mittleren Stufe) wird auf der jeweiligen Seite des Diagramms dargestellt.
  - Das Diagramm zeigt die Prozentsätze auf der x-Achse in beide Richtungen als positiv an. Der Grund für die gewählte Darstellung (in zwei Richtungen) liegt in der Nützlichkeit des Diagramms in der Umfrageforschung, in der die Stufen oft nach dem Likertsystem geordnet sind (z. B. hoch - niedrig, wahrscheinlich - unwahrscheinlich, Zustimmung - Ablehnung). Daher enthält das Diagramm eine Aufteilung zwischen den Stufen bei ihrem Median.
  - Die Anzahl der Variablenstufen bestimmt die Anzahl der dargestellten Stufen. Die Ebenen stellen die prozentuale Verteilung der Stufen der untersuchten Variable dar.
  - Wenn die Variablen eine ungerade Anzahl von Stufen enthalten, wird die mittlere Stufe als grauer Block in der Mitte des gestapelten Balkendiagramms dargestellt, wobei ihr prozentualer Anteil oben steht.
- Verfügbare Schriftgrößen: normal, klein, mittel, groß.

#### Dichtediagramme
- Zeigt die Verteilung einer numerischen Variable. Falls andere nominale oder ordinale Variablen enthalten sind, werden verschiedene Verteilungen, die verschiedene Werte der anderen Variablen repräsentieren, im selben Diagramm dargestellt.
- Die y-Achse stellt die Wahrscheinlichkeitsdichte für die Kerndichteschätzung dar (Wahrscheinlichkeit pro Einheit auf der x-Achse), die x-Achse stellt die verwendeten Variablen dar.
- Das Erscheinungsbild kann mit Farbpalette und der Transparenz angepasst werden.

#### Häufigkeitstabellen (nominale und ordinale Variablen)
- Zeigt eine Häufigkeitstabelle für jede Variable an.
  - Häufigkeit: Häufigkeit des Auftretens der einzelnen Werte.
  - Prozent: Prozentsatz des Auftretens der einzelnen Werte.
  - Prozent gültig: Gültiger Prozentsatz des Auftretens der einzelnen Werte.
  - Prozent kumulativ: Kumulierter Prozentsatz.

#### Stamm- und Blatttabellen
- Zeigt die Streubreite einer Variable.
  - Stamm: die erste(n) Ziffer(n).
  - Blatt: die erste Ziffer nach dem Stamm.

### Referenzen
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics, 6*, 461-464.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

### R-Pakete
---
- ggplot2
- ggrepel
- grid
- stats

