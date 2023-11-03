# PG-Tools

PG-Tools ist ein textbasiertes CLI Programm zur Entwicklung und Einbindung von
Programmgraphen im Kontext der formalen Sicherheitsanalyse.

Ziel ist es, ein anwenderfreundliches, robustes und zukunftssicheres System zu entwickeln,
dass möglichst im Hintergrund bleibt und Platz für das Wesentliche lässt: Das Modellieren.

## Beispielprojekt

Damit klarer wird, welches System wir uns vorstellen, demonstrieren wir im Folgenden ein kleines
Beispielprojekt. Es handelt sich um eine leicht modifizierte Version 
des "Weidezaun"-Projektes aus der Vorlesung.
Die Ausgaben sind teilweise noch von Hand erstellt, da die Implementierung noch
nicht weit genug fortgeschritten ist.

### Die Entwicklungsumgebung

Da wir vorhaben, ein CLI-Programm zu erstellen, das mit "Plain Text" arbeitet,
ist jede Entwicklungsumgebung denkbar.
Das folgende Bild zeigt also nur ein Beispiel für eine Möglichkeit.
(Mehr dazu [hier](#warum-cli-und-plain-text))

![alt text](./definition.png)

Links im Bild sieht man die Definition eines Programmgraphen über die
Ruby DSL. Rechts sieht man die generierte Grafik. Die Grafik wird aktualisiert, sobald
man `pg-tools show` ausführt, so wie unten abgebildet.

### Validitätsüberprüfungen

Oben im Bild ist die Definition von Spezifikationen zur Validitätsprüfung.
Führt man `pg-tools test` aus, erhält man die unten dargestellte Ausgabe.
(Das Feature ist noch nicht implementiert. Tatsächlich wären hier einige Tests fehlgeschlagen.)
Die Syntax ist vom beliebten Test-Framework [Rspec](https://rspec.info/) inspiriert. 
Im Fehlerfall wird der Ablauf ausgegeben, der die Formel verletzt.

Nach der Fehlerintegration könnte man Fehler beispielsweise so ausschließen:

```ruby
BEP = 150
errors = [ :BreaksError, :SensorError ]
# ...
no_errors = errors.map { |error| "G #{error} == no" }.join(" && ")
specify "The train" do
    assuming "there are no errors" => no_errors do
        it "reaches BEP" => :"G Tain.position > #{BEP}"
    end
end
```

Zudem kann man die Ruby DSL so erweitern, dass Fehlerautomaten
im internen Datenmodell von gewöhnlichen Komponenten unterschieden werden können.
Damit kann man eine knappe Syntax anbieten und die DCCA vollautomaitsch durchführen:

```ruby
# Erzeugt Fehlerautomaten für die Bremse & den Zugsensor
persistent error :Breaks
transient error :Sensor

# Validität unter Ausschluss von Fehlern
# 'no_erros' kann hier automatisch generiert wrden
specify "The train" do
    assuming "there are no errors" => no_errors do
        it "reaches BEP" => :"G Tain.position > #{BEP}"
    end
end

# Definition einer Gefährdung für die DCCA. Ausführbar mit: 'pg-tools dcca'
hazard "Train on unsecured railroad crossing" \
    => :"Barrier.angle > #{barrier_closed_angle} && Train.position >= #{train_pos_gep} && Train.position <= #{tain_pos_sp}"
```

## Language Server

Language Servers ermöglichen es, effizient Sprach-Features zu implementieren. Dazu gehören zum Beispiel:
- Code-Vervollständigung
- Fehler-Überprüfung und -diagnose
- Springen zur Definition einer Funktion

Wir möchten versuchen, pg-tools in einen Language Server zu integrieren. Allerdungs wissen wir mangels eigener Erfahrung nicht, ob das mit angemessenem Aufwand möglich ist und können
deshalb nicht versprechen, dass dieses Fearture am Ende im Produkt enthalten ist.

Durch diese Integration möchten wir den Entwickler:innen ermöglichen:
- Sich auf die Modellierung der Graphen zu konzentrieren, anstatt wieder und wieder die gleiche Variable auszuschreiben
- Sofort zu sehen, was falsch ist, anstatt stundenlang selbst nach Fehlern zu suchen
- Somit möglichst produktiv zu arbeiten

Weitere große Vorteile der Verwendung eines Language Servers sind Wiederverwendbarkeit und Plattformunabhängigkeit.
Wiederverwendbarkeit wird dadurch erreicht, dass man die Sprach-Features nur einmal definieren muss
und diese dann über wohldefinierte Schnittstellen vom Client aus aufrufen kann.
Plattformunabhängigkeit wird dadurch erreicht, dass man Language Clients in jedem erdenklichen Text-Editor bzw. IDE implementieren kann.
Beispielhaft soll ein Language Client für Visual Studio Code implementiert werden.
Dieser Editor ist für alle großen Betriebssysteme - Linux, Mac und Windows verfügbar.
Durch die angesprochene Plattformunabhängigkeit wollen wir einen Mehrwert gegenüber dem
aktuell verwendeten Produkt schaffen, welches nur auf Windows über MS VisualStudio verwendet werden kann.

Zudem ist es wahrscheinlich möglich, die oben angesprochenen Validitätsüberprüfungen direkt
über den Language Server abzufragen und in Echtzeit zur Verfügung zu stellen.

# Features

Im Folgenden haben wir einige Ideen für Features aufgelistet:

- Einlesen von Modellen über Ruby-DSL, JSON und YAML
- Ausgeben von Modellen in JSON, YAML, PlantUML
- Integration von NuSMV, Prism und weiterer Model Checker.
- Simulation von Modellen und Ausgabe als Video oder GIF
- Integriertes Test-Framework zur Validitätsprüfung
- Deklarative Syntax zur Verwendung der "LTL-Pattern" aus der Vorlesung
- Automatische DCCA
- Installation über ein Kommando (`gem install pg-tools`)
- Einfache Einarbeitung (mit Kommando `pg-tools init`)
- Ansprechende Dokumentation
- Konfigurationsmöglichkeiten  
- Hilfreiche Fehlermeldungen
- Unterstützung eines Language Servers für pg-tools
- Implementierung eines Language Clients für Visual Studio Code

# Warum CLI und Plain Text

Das Verständnis eines Systems ist zwar die Grundlage der Modellierung, aber nur ihr Anfang.
Erst durch die Konzeptualisierung ist es möglich, Modelle festzuhalten, zu kommunizieren, 
Denkfehler aufzudecken und sie maschinell zu verarbeiten.

Mit den folgenden Argumenten wollen wir darstellen, warum die Verwendung von "Plain Text" und eines CLI-Programms
für die Konzeptualisierung bzw. Festschreibung von Modellen besser geeignet ist als eine Graphische
Lösung.
(Mit "Pain Text" meinen wir den Ansatz, dass alle Projektdateien ausschließlich Text enthaten,
der schon für sich und ohne die Verwendung von Tools verständlich ist)

## Unkomplizierte Eingabe

Graphische Darstellungen sind sehr nützlich, um Modelle zu verstehen.
Dabei liegt ihr Vorteil aber in der effizienten *Aufnahme* von Informationen,
durch den Betrachter.

Bei der *Ausgabe* der eignenen Gedanken können graphische Editoren oft hinderlich sein.
Nebensächlichkeiten, wie das Layout und eine umständliche Navigation in der UI
stehen dem eigentlichen Ziel - der Modellierung - eher im Weg.

Wir wollen die Verständlichkeit einer graphischen Ausgabe
mit der unkomplizierten Eingabe über Text verbinden.

## Einfache Zusammenarbeit

Tools wie Git(-Hub) sind auf die Verarbeitung von Text ausgelegt.
Versionskontrolle, Code Reviews und das Beheben von Merge-Konflikten
funktionieren deutlich besser mit "Plain Text".

## Zeitlosigkeit & Technologie-Unabhängigkeit

Die Wartung und Instandhaltung von von UI-basierten Programmen ist nur mit großem Aufwand möglich.
Bibliotheken veralten und Design Trends ändern sich. Während Programme wie Eclipse schlecht gealtert sind, 
erfreuen sich Command Line Tools wie "git" oder "make" nach wie vor großer Beliebtheit.

Trotz des Aufkommens neuer Technologien stellen CLI Programme und und die Verwendung von
Plain Text eine Konstante dar.

## Einfache Installation

Die Leichtgewichtigkeit eines CLI-Programms vereinfacht die Installation und lässt weniger
Spielraum für Fehler. Durch minimale Abhängigkeiten kann die Funktionalität
auf verschiednen Systemen sichergestellt werden.

## Automatisierung und Flexibilität

CLI-Programme können einfach in Arbeitsabläufe eingebunden werden.
Beispielsweise wäre es möglich, Tests für Modelle zu schreiben, die mittles "continuous integration"
für jeden Pull Request auf GitHub ausgeführt werden.
