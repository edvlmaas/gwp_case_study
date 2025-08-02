1940          Starting year ## start with assumed ground-breaking for agriculture until intensification
2002          Last year
site.100  Site file name
0             Labeling type ## all defaults turned off
-1            Labeling year
-1.00         Microcosm
-1            CO2 Systems
-1            pH effect
-1            Soil warming
0             N input scalar option (0 or 1)
0             OMAD scalar option (0 or 1)
0             Climate scalar option
1             Initial system
SORL          Initial crop ## sorghum
              Initial tree

Year Month Option
1       Block ## Sorghum, low yield, no fertilizer
1959    Last year
1       Repeats # of years
1940    Output starting year
12      Output month
1       Output interval
F       Weather choice
basic_eq.wth
1 89 CULT K			## Mar 30
1 120 CULT I		## Apr 30
1 145 CULT C		## May 25
1 161 CROP SORG	## Jun 10 - Grain sorghum
1 161 PLTM 			## Jun 10
1 319 HARV G90S	## Oct 23 - Harvest grains and 90% straw
-999 -999 X
3       Block ## Switch to cotton
1979    Last year
1       Repeats # of years
1960    Output starting year
12      Output month
1       Output interval
C       Weather choice ## Continue
1 118 CULT K  ## Apr 28
1 140 CULT ROW  ## May 20
1 140 CROP COTL ## May 20
1 140 PLTM  ## May 20
1 290 HARV G90S  ## Oct 17
-999 -999 X
4       Block ## Switch to cotton
2001    Last year
1       Repeats # of years
1980    Output starting year
12      Output month
1       Output interval
C       Weather choice ## Continue
1 118 CULT K  ## Apr 28
1 118 FERT (1N)  ## Apr 28
1 140 CULT ROW  ## May 20
1 140 CROP COTL ## May 20
1 140 PLTM  ## May 20
1 290 HARV G90S  ## Oct 17
-999 -999 X
5       Block ## All include rye at end as transistion year
2002    Last year
1       Repeats # of years
2002    Output starting year
12      Output month
1       Output interval
C       Weather choice ## Continue
1 118 CULT K  ## Apr 28
1 118 FERT (2.2N)  ## Apr 28
1 140 CULT ROW  ## May 20
1 140 CROP COT ## May 20
1 140 PLTM  ## May 20
1 290 HARV G90S  ## Oct 17
1 341 CROP RGA
-999 -999 X
