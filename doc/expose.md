# PG-Tools

PG-Tools ist ein textasiertes CLI Programm zur Entwicklung und Einbindung von
Programmgraphen im Kontext der formalen Sicherheitsanalyse.

Ziel ist es, ein anwenderfreundliches, robustes und zukunftssicheres System zu entwickeln,
dass möglichst im Hintergrund bleibt und Platz für das Wesentliche lässt: Das Modellieren.

## Beispielprojekt

Damit klarer wird welches System wir uns vorstellen, wird im Folgenden ein kleines
Beispielprojekt vogestellt. Es handelt sich um eine leicht modifizierte Version 
des "Weidezaun" projektes aus der Vorlesung.
Die Ausgaben sind teilweise noch von Hand erstellt weil die Implementierung noch
nicht weit genug fortgeschritten ist. d

### Die Entwicklungsumgebung

Da wir vorhaben ein CLI Programm zu erstellen, das mit "Plain Text" arbeitet,
ist jede Entwicklungsumgebung denkbar.
Das folgende Bild zeigt also nur ein Beispiel für eine Möglichkeit.
(Mehr dazu [hier](#warum-cli-und-plain-text))

![alt text](./definition.png)

Links auf dem Bild sieht man die definition eines Programmgraphen über die
Ruby DSL. Rechts sieht man die erzeugte Grafik. Die Grafik wird ernuert, sobald
man `pg-tools show` ausführt, so wie unten abgebildet.

### Validitätsüberprüfungen

Oben im Bild ist die Definition von Spezifikationen zur Validitätsprüfung.
Führt man `pg-tools test` aus, erhält man die unten dargestellte Ausgabe.
(Das Feature ist noch nicht implementiert. Tatsächlich wären hier einige tests fehlgeschlagen.)
Die Syntax ist vom beliebten Test-Framework [Rspec](https://rspec.info/) inspiriert. 
Im Fehlerfall würde der Ablauf ausgegeben, der die Formel verletzt.

Nach der Fehlerintegration könntem man Fehler beispielsweise so ausschließen:

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

Zudem könnte man die Ruby DSL so erweitern, dass Fehlerautomaten
im internen Datenmodell von gewöhnlichen Komponenten unterschieden werden können.
Damit könnte man eine knappe Syntax anbieten und die DCCA vollautomaitsch durchführen:

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

# Features

Im Folgenden einige Ideen für Features:

- Einlesen von Modellen über Ruby-DSL, Json und Yaml
- Ausgeben von Modellen in Json, Yaml, PlantUML
- Integration von NuSMV, Prism und weiterer model checker.
- Simulation von Modellen und Ausgabe als video/gif
- Integriertes Test-Framework zur Validitätsprüfung
- Deklarative syntax zur Verwendung der "LTL-Pattern" aus der Vorlesung
- Automatische DCCA
- Installation über ein Kommando (`gem install pg-tools`)
- Einfache Einarbeitung (mit Kommando `pg-tools init`)
- Ansprechende Dokumentation
- Konfigurationsmöglichkeiten  
- Hilfreiche Fehlermeldungen

# Warum CLI und Plain Text

Das Verständnis eines Systems ist zwar die Grundlage der Modellierung, aber nur ihr Anfang.
Erst durch die Konzeptualisierung, ist es möglich Modelle festzuhalten, zu kommunizieren, 
Denkfehler aufzudecken und sie maschinell zu verarbeiten.

Mit den folgenden Argumenten wollen wir Darstellen, warum die Verwendung von "Plain Text" und eines CLI Programms
für die Konzeptualisierung/Festschreibung von Modellen besser geeignet ist, als eine Graphische
Lösung.
(Mit "Pain Text" meinen wir den Ansatz, dass alle Projektdateien ausschließlich Text enthaten,
der schon für sich und ohne die Verwendung von Tools verständlich ist)

## Unkomplizierte Eingabe

Graphischen Darstellungen sind sehr nützlich um Modelle zu verstehen.
Dabei liegt ihr Vorteil aber in der Effizienten *Aufnahme* von Informationen,
durch den Betrachter.

Bei der *Ausgabe* der eignenen Gedanken können graphische Editoren oft hinderlich sein.
Nebensächlichkeiten, wie das Layout und eine umständliche Navigation im UI
stehen dem eigentlichen Ziel, der Modellierung, eher im Weg.

Wir wollen die Verständlichkeit einer graphischen Ausgabe,
mit der unkomplizierten Eingabe über Text verbinden.

## Einfache Zusammenarbeit

Tools wie Git(-hub) sind auf die verarbeitung von Text ausgelegt.
Versionskontrolle, code reviews und das Beheben von merge Konflikten
funktionieren deutlich besser mit "Plain Text".

## Zeitlosigkeit & Technologie unabhängigkeit

Die Wartung und Instandhaltung von von UI-basierten Programmen ist nur mit Großem Aufwand möglich.
Bibliotheken veralten und Design trends ändern sich. Während Programme wie Eclipse schlecht gealtert sind, 
erfreuen sich command line tools wie "git" oder "make" weiterhin großer Beliebtheit.

Trotz des Aufkommens neuer Technologien stellen CLI Programme und und die Verwendung von
Plain Text eine Konstante dar.

## Einfache Installation

Die Leichtgewichtigkeit eines CLI Programms vereinfacht die Installation und lässt weniger
Spielraum für Fehler. Durch minimale Abhängigkeiten kann die Funktionalität
auf verschiednen systemen sichergestellt werden.

## Automatisierung und Flexibilität

CLI programme können einfach in Arbeitsabläufe eingebunden werden.
Beispielsweise wäre es möglich tests für Modelle zu schreiben, die mittles "continuous integration"
für jedem Pull Request auf Github ausgeführt werden.
