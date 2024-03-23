Beschrijvende statistieken
===

Met beschrijvende statistieken kunt u basis beschrijvende statistieken verkrijgen zoals histogrammen, dichtheidsgrafieken, correlatiegrafieken, boxplots en frequentietabellen. 

### Invoer
-------

#### Invoerveld 
- Variabelen: Alle variabelen waarin u geïnteresseerd bent. 
- Splits: Variabelen kunnen worden gesplitst aan de hand van een categorische variabele zoals een experimentele conditie. 
- Tabel met beschrijvingen transponeren: transponeert de eerste tabel.
- Frequentietabellen: Geeft frequentietabellen weer voor elke variabele.
  - Hoogste aantal verschillende waarden: Frequentietabellen worden getoond voor alle variabelen die minder unieke observaties hebben dan het gespecificeerde aantal.
- Steelbladdiagram: Een tabel die alle numerieke observaties van klein naar groot toont. The observaties zijn gesplitst in een "stam", de eerste getal(len), en een "blad", het daaropvolgende getal.
  - schaal: Controleert de grootte van de tabel. Het verhogen van de schaal leidt tot een grovere weergave van de variabele, terwijl het verlagen van de schaal leidt tot een afgevlakte weergave.

### Grafieken 
- Verdelingsgrafieken: Voor een continue variabele wordt een histogram weergegeven. Voor nominale en ordinale variabelen wordt een frequentieverdeling weergegeven.
  - Geef dichtheid weer (alleen continue variabelen): Geeft dichtheid weer op basis van een nonparametrische dichtheidschatter. 
- Correlatiediagram: Geeft voor een continue variabele histogrammen, dichtheidsgrafieken en spreidingsdiagrammen weer. 
- Boxplots: Geeft voor continue variabelen een boxplot weer.
  - Label uitschieters: De uitschieters worden gelabeld. Uitschieters worden gebaseerd op de interkwartielafstand (IQR). Bijv., [25e percentiel] - 1.5 × IQR en [75e percentiel] +  1.5 × IQR.
  - Kleur: Geeft weer in kleur.
	- Heeft selecteerbare boxplot, viool- en jitterelementen voor het weergeven van de verdeling van de data. 
- Pareto plots: Kan alleen worden weergegeven voor nominale en ordinale variabelen. Geeft een geordende aflopende frequentieverdeling weer. Standaard wordt een cumulatieve lijn toegevoegd die de proportionele bijdrage van elke factor/niveau binnen de variabele aangeeft.
  - Parteo-regel: Tekent twee extra lijnen, één verticaal en één horizontaal, die elkaar snijden en stoppen op de hoogte van het ingevoerde getal (schaalt mee met cumulatieve lijn). Standaard is deze ingesteld op 95%. Dit kan worden veranderd in het gewenste percentage.
- Likert plots: Kan alleen worden weergegeven voor nominale en ordinale variabelen. Geeft een horizontaal gestapelde staafdiagram weer, waarbij een nieuwe staaf wordt gegenereerd voor elke extra variabele die wordt toegevoegd.
  - Veronderstel dat alle variabelen dezelfde niveaus hebben: Geeft één plot weer die alle variabelen bevat. Variabelen hebben hetzelfde aantal niveaus.
  - Instelbare lettergrootte voor verticale as: Regelt de lettergrootte van variabelenamen op de verticale as. Standaard wordt de normale lettergrootte gebruikt, maar er zijn nog drie andere groottes beschikbaar.
- Dichtheidsplots: Kan alleen worden weergegeven voor schaal (continue) variabelen. Gebruikt de kernel dichtheidsschatting om de verdeling van een numerieke variabele te visualiseren. 
  - Gescheiden dichtheden: Door een nominale of ordinale variabele in dit vak te plaatsen, worden verschillende verdelingen die overeenkomen met de verschillende niveaus van de variabele weergegeven.
  - Transparantie: Instelbare transparantie voor de kleur binnen het gebied onder de dichtheidslijn (varieert van 0 tot 100).

### Statistieken
- Percentielwaarden: 
  - Kwartielen: Geeft het 25e, 50e en 75e percentiel van de datapunten. 
  - Snij punten af voor x gelijke groepen: Geeft de punten aan die de data in x gelijke groepen opdelen; de standaardoptie is 4 gelijke groepen. 
  - Percentielen: Geeft het xde percentiel aan; percentielwaarden moeten met komma's worden gescheiden. 
- Centrale tendens (alleen continue variabelen):
  - Gemiddelde: Rekenkundig gemiddelde van de datapunten.
  - Mediaan: Mediaan van de datapunten.
  - Modus: Modus van de datapunten; als er meerdere modi zijn, wordt slechts de eerste gerapporteerd. Voor nominale en ordinale data is de modus de meest voorkomende geobserveerde waarde. Voor continue data wordt de waarde met de grootste geschatte dichtheid gerapporteerd (zie 'Verdelingsgrafieken' -> 'Geef dichtheid weer'). Wanneer een voetnoot over multimodaliteit wordt weergeven voor continue variabelen raden wij aan om de data te visualizeren om multimodaliteit te controleren.
  - Som: De som van de datapunten. 
- Spreiding (alleen voor continue variabelen): 
  - Std. Afwijking. De standaarddeviatie van de datapunten. 
  - MAD: Mediaan absolute afwijking van de datapunten. 
  - MAD robuust: Mediaan absolute afwijking van de datapunten, bijgesteld door een factor voor asymptotische normale consistentie. 
  - IQR: Interkwartielafstand van de datapunten; 75e percentiel - 25e percentiel. 
  - Variantie: Variantie van de datapunten. 
  - Bereik: De spreidingsbreedte van de datapunten; maximum - minimum. 
  - Minimum: Minimale waarde van de datapunten. 
  - Maximum: Maximale waarde van de datapunten. 
- Verdeling: 
  - Scheefheid: De scheefheid van de verdeling van de data punten.
  - Gepiektheid: De gepiektheid (kurtosis) van de verdeling van de datapunten. 
  - Shapiro-Wilk toets: Een toets om de normaliteit van de verdeling te toetsen.
- Inferentie:
  - S.E. Gemiddelde: De standaardfout van het gemiddelde.
  - Betrouwbaarheidsinterval voor het gemiddelde:
    - Breedte: breedte of the confidence interval.
    - Methode: Hoe moet het betrouwbaarheidsinterval berekent worden? Als standaard wordt een `T-model` gebruikt dat dezelfde resultaten oplevert als een T-Toets voor Eén gemiddelde. Alternatieve opties zijn een normaal model (\\\\(\\bar{x} \\pm z_{95}\\times SE\\\\)) of `Bootstrap`.
  - Betrouwbaarheidsinterval voor de standaarddeviatie: een betrouwbaarheidsinterval voor de standaarddeviatie gebaseerd op bootstrap samples.
  - Betrouwbaarheidsinterval voor de variantie: een betrouwbaarheidsinterval voor de variantie gebaseerd op bootstrap samples.
  - Bootstrap samples: het aantal bootstrap samples.
- Associatie matrix:
  - Covariantie: Covariantie waarde.
  - Correlatie: Pearson's correlatie coefficient.
  - Use: Hoe om te gaan met ontbrekende waarden? 
    - Alles: gebruik alle observaties; dit kan leiden tot NA's als er data ontbreken.
    - Complete observaties: enkel complete observaties (rijen) worden gebruikt.
    - Paarsgewijze complete observaties: gebruik paarsgewijs complete observaties. Dit kan leiden tot matrices die niet positief semi-definiet zijn.


### Uitvoer
-------
#### Beschrijvende statistieken
- Geldig: Het aantal geldige waarnemingen. 
- Ontbrekend: Het aantal ontbrekende waarden. 
- Gemiddelde: Rekenkundig gemiddelde van de datapunten.
- Std. fout van het gemiddelde: De standaardfout van het gemiddelde. 
- Mediaan: Mediaan van de datapunten.
- Modus: Modus van de datapunten; als er meerdere modi zijn, wordt slechts de eerste gerapporteerd. 
- Standaardafwijking: De standaarddeviatie van de datapunten. 
- MAD: Mediaan absolute afwijking van de datapunten. 
- MAD robuust: Mediaan absolute afwijking van de datapunten, bijgesteld door een factor voor asymptotische normale consistentie. 
- IQR: Interkwartielafstand van de datapunten; 75e percentiel - 25e percentiel. 
- Variantie: Variantie van de datapunten.
- Scheefheid: De scheefheid van de verdeling van de datapunten.
- Std. fout van scheefheid: De standaardfout van de scheefheid. 
- Gepiektheid: De gepiektheid (kurtosis) van de verdeling van de datapunten. 
- Std. Fout van Gepiektheid: De standaardfout van de kurtose. 
- Shapiro-Wilk: waarde van de Shapiro-Wilk statistiek.
- P-waarde van Shapiro-Wilk: p-waarde van de Shapiro-Wilk statistiek.
- Bereik: De spreidingsbreedte van de datapunten; maximum - mimimum. 
- Minimum: Minimale waarde van de datapunten. 
- Maximum: Maximale waarde van de datapunten.
- Som: De som van de datapunten. 
- Percentielen: Geeft het xde percentiel aan. 

#### Verdelingsgrafieken
- Geeft voor continue variabelen een histogram en de passing van een niet parametrische dichtheidsschatting weer. 
- Geeft voor nominale en ordinale variabelen een frequentieverdeling weer. 

#### Correlatie grafiek
- Geeft een matrix van grafieken weer tussen continue variabelen met spreidingsdiagrammen tussen de variabelen in de niet-diagonaal-cellen, en histogrammen en dichtheidsplots in de cellen op de diagonaal. De lijn representeert de passing van een 1e-, 2e-, 3e- of 4e-orde polynomiaal (de selectie is gebaseerd op het Bayesiaanse informatiecriterium ; Schwarz, 1978).

#### Boxplots
- Geeft voor continue variabelen een boxplot weer. De uitschieters kunnen gelabeld worden op basis van de interkwartiel afstand (IQR), bijv., [25e percentiel] - 1.5 × IQR en [75e percentiel] + 1.5 × IQR. Kan ook in kleur worden weergegeven en heeft selecteerbare boxplot-, viool- en jitterelementen voor het weergeven van de verdeling van de data. Dit kan worden gesplitst op basis van een categorische variabele zoals een experimentele conditie. 

#### Pareto plots
- Toont de tellingen van elke factor/niveau binnen de variabele in een aflopende volgorde. De y-as vertegenwoordigt de frequentie (tellingen als grijze balken) van elke factor/niveau, de x-as vertegenwoordigt de factoren/niveaus van de variabele in een geordende volgorde.
- Standaard wordt een cumulatieve lijn getekend die de proportionele bijdrage van elke factor aangeeft. Een tweede verticale as aan de rechterkant van de grafiek schaalt mee met deze cumulatieve lijn en geeft percentages weer om de beschrijving van de cumulatieve lijn mogelijk te maken.
- Als de "Pareto-regel" is ingeschakeld, maken de twee nieuwe lijnen een nauwkeuriger beoordeling mogelijk van de bijdrage van factoren/niveau's aan de totale bijdrage (in procenten) door verschillende invoernummers te gebruiken.

#### Likert plots
- Toont een horizontaal gestapeld staafdiagram dat de bijdrage van niveaus binnen een variabele in procenten weergeeft. De volgorde van de niveaus hangt af van de gedefinieerde volgorde in de JASP-gegevenstabel. Een legenda onder de grafiek geeft een overzicht van de niveaus en hun respectievelijke kleuren in de grafiek.
  - De y-as staat voor de gebruikte variabelen, de x-as voor de percentages. De procentuele bijdrage van alle lagere-orde- (onder het middelste niveau) en hogere-orde- (boven het middelste niveau) niveaus wordt aan hun respectieve kant van de grafiek weergegeven.
  - De grafiek geeft de percentages op de x-as in beide richtingen positief weer. De reden voor de gekozen weergave (in twee richtingen) is de bruikbaarheid van de grafiek in enquête-onderzoek waar de niveaus vaak een likert-volgorde volgen (b.v. hoog - laag, waarschijnlijk - onwaarschijnlijk, eens - oneens). Daarom bevat de grafiek een splitsing tussen niveaus bij hun mediaan.
  - Het aantal variabele niveaus bepaalt het aantal weergegeven lagen. Lagen geven de procentuele verdeling van de niveaus van de onderzochte variabele weer.
  - Als de variabelen een ongelijk aantal niveaus bevatten, wordt het middelste niveau weergegeven als een grijs blok in het midden van de gestapelde balk met zijn procentuele bijdrage erbovenop.
- Beschikbare lettergrootten: normaal, klein, middelgroot, groot.

#### Dichtheidsplots
- Geeft de verdeling van een numerieke variabele weer. Indien andere nominale of ordinale variabelen zijn opgenomen, worden verschillende verdelingen die verschillende waarden van de andere variabele vertegenwoordigen, in dezelfde plot weergegeven.
- De y-as vertegenwoordigt de waarschijnlijkheidsdichtheid voor de kernel dichtheidsschatting (waarschijnlijkheid per eenheid op de x-as), de x-as vertegenwoordigt de gebruikte variabelen.
- Het uiterlijk kan worden gemanipuleerd door het kleurenpalet en de transparantie aan te passen.

#### Frequentietabellen (voor categorische variabelen) 
- Geeft een frequentietabel voor elke variabele weer. 
  - Frequentie: Frequentie van het voorkomen van elke waarde.
  - Percentage: Percentage van voorkomen van elke waarde.
  - Geldig percentage: Percentage van het voorkomen van elke geldige waarde.
  - Cumulatieve percentage: Het cumulatieve percentage. 

#### Steelbladdiagram
- Toont de spreiding van een variabele.
  - Stam: de eerste getallen.
  - Blad: het eerste getal na de stam.

### Referenties
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics, 6*(2), 461-464.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

### R Packages
---
- ggplot2
- ggrepel
- grid
- stats

