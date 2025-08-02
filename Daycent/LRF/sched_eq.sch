0             Starting year
4000          Last year
site.100	  Site file name
0             Labeling type ## keep 0, no C labeling
-1            Labeling year ## keep 0
-1.00         Microcosm ## must stay -1 for DayCent (deprecated)
-1            CO2 Systems ## leave off - C4 plants not as affected by CO2 increase
-1            pH effect ## leave off, assume no pH shifting
-1            Soil warming #? investigate - difference in soil temperature between grassland and cropped land
0             N input scalar option (0 or 1) ## leave off, do not scale inputs
0             OMAD scalar option (0 or 1) ## leave off, do not scale inputs
0             Climate scalar option ## leave 0, will create own climate simulations
1             Initial system ## 1 for crop or grassland
GI5           Initial crop ## mixed grass, 75% warm, 25% cool season
              Initial tree

Year Month Option
1            Block ## Estimated avg 7-year fire frequency (5-10 year occurance)
4000         Last year
7            Repeats # years
1            Output starting year #? start with output all years, then later reduce or set to 4000
1            Output month
100.000      Output interval ## set to 100 will reduce the number of records output (every 100 years)
F            Weather choice
basic_eq.wth
  1 120 CROP GI5      
  1 120 FRST       ## May 1-ish
  1 135 GRAZ GM    ## Moderate grazing
  1 166 GRAZ GM
  1 196 GRAZ GM
  1 227 GRAZ GM
  1 258 GRAZ GM
  1 273 LAST       ## Sept 30-ish
  1 273 SENM
  2 120 CROP GI5   
  2 120 FRST       ## May 1-ish
  2 135 GRAZ GM    ## Moderate grazing
  2 166 GRAZ GM
  2 196 GRAZ GM
  2 227 GRAZ GM
  2 258 GRAZ GM
  2 273 LAST       ## Sept 30-ish
  2 273 SENM
  3 120 CROP GI5   
  3 120 FRST       ## May 1-ish
  3 135 GRAZ GM    ## Moderate grazing
  3 166 GRAZ GM
  3 196 GRAZ GM
  3 227 GRAZ GM
  3 258 GRAZ GM
  3 273 LAST       ## Sept 30-ish
  3 273 SENM
  4 120 CROP GI5   
  4 120 FRST       ## May 1-ish
  4 135 GRAZ GM    ## Moderate grazing
  4 166 GRAZ GM
  4 196 GRAZ GM
  4 227 GRAZ GM
  4 258 GRAZ GM
  4 273 LAST       ## Sept 30-ish
  4 273 SENM
  5 120 CROP GI5   
  5 120 FRST       ## May 1-ish
  5 135 GRAZ GM    ## Moderate grazing
  5 166 GRAZ GM
  5 196 GRAZ GM
  5 227 GRAZ GM
  5 258 GRAZ GM
  5 273 LAST       ## Sept 30-ish
  5 273 SENM
  6 120 CROP GI5   
  6 120 FRST       ## May 1-ish
  6 135 GRAZ GM    ## Moderate grazing
  6 166 GRAZ GM
  6 196 GRAZ GM
  6 227 GRAZ GM
  6 258 GRAZ GM
  6 273 LAST       ## Sept 30-ish
  6 273 SENM
  7 120 CROP GI5   
  7 120 FRST       ## May 1-ish
  7 135 GRAZ GM    ## Moderate grazing
  7 152 FIRE H     ## June 1-ish
  7 166 GRAZ GM
  7 196 GRAZ GM
  7 227 GRAZ GM
  7 258 GRAZ GM
  7 273 LAST       ## Sept 30-ish
  7 273 SENM
-999 -999 X
