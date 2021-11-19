__includes
["species.nls"
"output.nls"
"import.nls"
"disturbance.nls"]

turtles-own
[
  real
  opt
  height
  stem-diameter
  crown-diameter
  crown-height
  crown-radius
  s ;; height-diameter realtionship
  g ;; growth parameter
  b2
  b3
  biomass
  B0
  B1
  agemx
  hmax
  dmax
  growth-rate
  projection-area
  CR0
  CR1
  LAI ;; leaf-area index
  L0
  L1
  M0 ;; dbh mortalily species constant
  M1 ;; dbh mortalily species constant
  DDmax ;; maximum growing degree-days
  DDmin ;; minimum growing degree-days
  light
  my-neighbors
  num-neighbors
  dominant
  temp_effect
  age
  patch_count
  Dmin ;; minimum diameter setting for new trees
  MD ;; diameter mortality
  covered-patches
  who-covered-patches
  disperse-radius
  c-rate
  disperse-kernal
  lmax ;; maximum height at top of the canopy layer each layer is a meter
  lmin ;; minimum height layer where canopy starts
  n-layer
  shade-tolerant ;; bool if false shade intolerent
  opttemp ;; if DEGD falls in between min and max for this is set to true for growth and seed growth and establish
  open-light-area
  no-covered-patches
  wood-weight
  size-prop
  species-sum-light
  percent-energy
  envirn-input
  PPA ;; pollen production age
  can-produce

  ;;; Ellenberg Indicator Values (EIV's) ;;;
  ;; for the time being all factor are worth the same effect on growth i.e 20%
  El ;; light effect
  Ef ;; soil moisture
  Er ;; acidity effect
  E_n ;; soil nitrogen
  Es ;; soil salt
  neighbor_trees
  dominant_neighbor
  neighbors1
  neighbors2
  open-patches
]

patches-own
[
  seed-pool
  patch_neighbor
  tree_cov_no
  which-trees
  canopy_patches
  disperse_patches
  covered
  original-soil
  new-growth
  whoList
  neighbor-final
  canopy-count
  _turtle
]


Globals
[
  DEGD
  TJan_max
  TJan_min
  TJul_max
  TJul_min
  TJul
  TJan
  year
  envirn
  tmax ;; max temp
  tmin ;; min temp
  af-days ;; number of days of frost per month
  rain
  Nseed
  leftover-seeds
  human-population
  animal-population
  elm-active
  pine-active
  oak-active
  alder-active
  hazel-active
  birch-active
  lime-active
  ash-active
  rand-pick
  rand-pick-1
  random-func
  bar-chart
  which-species
  elm-monitor
  elm-monitor-all
  pine-monitor
  pine-monitor-all
  oak-monitor
  oak-monitor-all
  alder-monitor
  alder-monitor-all
  hazel-monitor
  hazel-monitor-all
  birch-monitor
  birch-monitor-all
  lime-monitor
  lime-monitor-all
  ash-monitor
  ash-monitor-all
  elm-prob
  pine-prob
  oak-prob
  alder-prob
  hazel-prob
  birch-prob
  lime-prob
  ash-prob
  total-trees
  opened-patches
  overall-biomass ;;current years biomass
  last-bio ;; previous yeas biomass
  removed-biomass ;; biomass removed by management
  bio-change
]

to init-datasets
   if ticks = 0 [
     reset-timer ]
   load-datasets
end

to load-datasets
   import-tmax
   import-tmin
   ;import-rain
   ;import-frost
end

to setup-species-1
  setup-species
end

to species-setup
    set dominant_neighbor true
    set stem-diameter 1
    set growth-rate dmax / agemx

    let hmax1 hmax * 100
    set b2 2 * (( hmax1 - 137 ) / dmax)
    set b3 ( hmax1 - 137 ) / (dmax * dmax)

    growth
   ;set stem-diameter stem-diameter + growth-rate
   get-height-2
   ;height-powerlaw
   calculate-biomass
   calculate-crown-radius
   set crown-diameter crown-radius * 2
   set size crown-diameter
   set light 1
end

to reset-sums
  ifelse %-tree
  [set %-elm 0 set %-pine 0 set %-alder 0 set %-oak 0 set %-lime 0 set %-birch 0 set %-ash 0 set %-hazel 0]
  [set num-elm 0 set num-pine 0 set num-alder 0 set num-oak 0 set num-lime 0 set num-birch 0 set num-ash 0 set num-hazel 0]
end

to percent-tree-setup
  ;; ensure the first 7 trees remain within tree total
  ;; hazel to have it's own percentage of the trees as the majority of pollen data define hazel type as percentage of total tree pollen

  if %-elm + %-pine + %-alder + %-oak + %-lime + %-birch + %-ash > 100 [user-message ("Error: Exceeded % input. Ensure % sum <= 100!")]

  create-elm ((%-elm / 100) * n-trees )
  create-pine (n-trees  * (%-pine / 100))
  create-alder (n-trees * (%-alder / 100))
  create-oak (n-trees  * (%-oak / 100))
  create-hazel (n-trees  * (%-hazel / 100))
  create-lime! (n-trees  * (%-lime / 100))
  create-birch (n-trees  * (%-birch / 100))
  create-ash (n-trees  * (%-ash / 100))

  set num-elm count elm
  set num-pine count pine
  set num-alder count alder
  set num-oak count oak
  set num-hazel count hazel
  set num-lime count lime!
  set num-birch count birch
  set num-ash count ash

end

to present-species
  ifelse count elm > 0 [set elm-active true][set elm-active false]
  ifelse count pine > 0 [set pine-active true][set pine-active false]
  ifelse count oak > 0 [set oak-active true][set oak-active false]
  ifelse count alder > 0 [set alder-active true][set alder-active false]
  ifelse count hazel > 0 [set hazel-active true][set hazel-active false]
  ifelse count birch > 0 [set birch-active true][set birch-active false]
  ifelse count lime! > 0 [set lime-active true][set lime-active false]
  ifelse count ash > 0 [set ash-active true][set ash-active false]
end

to setup
  ca
  ;set-current-directory "F:\\Netlogo\\Elm Decline Model\\Local Scale"
  ask patches [set _turtle false]
  set overall-biomass 10
  set total-trees n-trees

  ifelse %-tree
  [
    percent-tree-setup

    if  %-elm > 0 [set elm-active true]
    if %-pine > 0 [set pine-active true]
    if %-oak > 0 [set oak-active true]
    if %-alder > 0 [set alder-active true]
    if %-hazel > 0 [set hazel-active true]
    if %-birch > 0 [set birch-active true]
    if %-lime > 0 [set lime-active true]
    if %-ash > 0 [set ash-active true]
  ]
  [
    create-elm num-elm
    create-pine num-pine
    create-alder num-alder
    create-oak num-oak
    create-hazel num-hazel
    create-lime! num-lime
    create-birch num-birch
    create-ash num-ash

    if num-elm > 0 [set elm-active true]
    if num-pine > 0 [set pine-active true]
    if num-oak > 0 [set oak-active true]
    if num-alder > 0 [set alder-active true]
    if num-hazel > 0 [set hazel-active true]
    if num-birch > 0 [set birch-active true]
    if num-lime > 0 [set lime-active true]
    if num-ash > 0 [set ash-active true]
  ]

  ;if num-elm > 0 or %-elm > 0 [set elm-active true]
  ;if num-pine > 0 or %-pine > 0 [set pine-active true]
  ;if num-oak > 0 or %-oak > 0 [set oak-active true]
  ;if num-alder > 0 or %-alder > 0 [set alder-active true]
  ;if num-hazel > 0 or %-hazel > 0 [set hazel-active true]
  ;if num-birch > 0 or %-birch > 0 [set birch-active true]
  ;if num-lime > 0 or %-lime > 0 [set lime-active true]
  ;if num-ash > 0 or %-ash > 0 [set ash-active true]

  ask turtles [
    setup-species
    set stem-diameter random dmax
    setup-age
   ; setxy random-pxcor random-pycor of [one-of patches with [_turtle = false]]
    setxy random-pxcor random-pycor
    while [any? other turtles-here] [
    setxy random-pxcor random-pycor
   ]
    ask patch-here [set _turtle true]
  ]


  ask turtles [
    set disperse-kernal 1
    set dominant_neighbor true
    set growth-rate dmax / agemx ;;simple growth function
    let hmax1 hmax * 100
    set b2 2 * (( hmax1 - 137 ) / dmax)
    set b3 ( hmax1 - 137 ) / (dmax * dmax)
   ; print b2
   ; print b3
    growth
   ;;set stem-diameter stem-diameter + growth-rate
   get-height-4
   ;height-powerlaw
   calculate-biomass
   calculate-crown-radius
   set crown-diameter crown-radius * 2
   set size crown-diameter
  ]
  ask patches [
    set pcolor one-of [65 54 55 64]
    set canopy_patches false
    set original-soil pcolor
    set canopy-count 0
    set disperse_patches false
  ]
  set elm-monitor 0
  set elm-monitor-all 0
  set pine-monitor 0
  set pine-monitor-all 0
  set oak-monitor 0
  set oak-monitor-all 0
  set alder-monitor 0
  set alder-monitor-all 0
  set hazel-monitor 0
  set hazel-monitor-all 0
  set birch-monitor 0
  set birch-monitor-all 0
  set lime-monitor 0
  set lime-monitor-all 0
  set ash-monitor 0
  set ash-monitor-all 0
  init-seedpool-FORMIND
  set year 0
  general-manage-setup
  file-close-all
  reset-ticks

end

to go
  init-datasets
  set opened-patches count patches with [canopy_patches = false]
  ask turtles [set age age + 1
  set opttemp false]
  degree-days
  present-species
  grow
  ;print DEGD
  ;print tmax
  ask turtles [
    if age > agemx [die]]
  ;ask tree2s [write "optimum growth =" print opt ]
  ;ask turtles [ifelse age >= PPA[set can-produce true][set can-produce false]]
  establish-neighborhood
  light-potential
  calc-light-effect
  disperse-seeds
  crowding-mortality
  output-data
  species-coop-2
  calculate-bio-overall
  calc-bio-changes
  if ticks mod coppice-increment = 0 [manage]
  calc-removed-biomass
  if ticks = 500 [stop]
  tick
end

to grow
  calculate-envirn
  ask turtles [
   temperature-effect
   ;set stem-diameter stem-diameter + growth-rate
   if stem-diameter <= Dmax [growth]

   get-height-4
   ;height-powerlaw
   calculate-biomass
   calculate-crown-radius
   set crown-diameter crown-radius * 2
   set size crown-diameter
   calculate-LAI
   calculate-crown-height
   ;create-temporary-plot-pen (word who)
   ;set-plot-pen-color red
   FAREAST-mortality
   ;plotxy stem-diameter height
  ]

end

to calculate-bio-overall
  set last-bio overall-biomass
  set overall-biomass sum [biomass] of turtles
end

to calc-bio-changes
  set bio-change (overall-biomass - last-bio)
end

to calc-removed-biomass
  let current-bio sum [biomass] of turtles
  set removed-biomass (overall-biomass - current-bio)
end

to growth

  set envirn temp_effect
  let part1 (G * stem-diameter)
  ;write "part 1 = " print part1
  let part2  (b2 * stem-diameter)
  ;write "part 2 = " print part2
  let part3  (b3 * stem-diameter ^ 2)
  ;write "part 3 = " print part3
  let part4 137 + part2 - part3
  ;write "part 4 = " print part4
  let part5  ( stem-diameter * part4) / (dmax * (hmax * 100))
  ;write "part 5 = " print part5
  let part6 ( 1 - part5)
  ;write "part 6 = " print part6
  let part7 part1 * part6
  ;write "part 7 = " print part7

  let part8 (3 * (b2 * stem-diameter))
  ;write "part 8 = " print part8
  let part9 (4 * (b3 * (stem-diameter ^ 2)))
  ;write "part 9 = " print part9
  let part10 274 + part8 - part9
  ;write "part 10 = "print part10

  set opt part7 / part10

  ifelse natural
  [set envirn-input (temp_effect + percent-energy) / 2]
  [set envirn-input (temp_effect + light) / 2]

  ;if light = 0 [ set envirn-input 0 ]
  ;print envirn-input

  set real opt * envirn-input

  set stem-diameter stem-diameter + real

  ;print "Optimal growth"  show upper
  ;print "Actual growth"  show real
  ;if dominant_neighbor = false [ set stem-diameter stem-diameter +  ]
  ;if dominant_neighbor = true [set stem-diameter stem-diameter + real]
  ;ifelse en [set stem-diameter stem-diameter + real - envirn ]
                ;[set stem-diameter stem-diameter + opt ]
end

to growth_original

  ;set hmax hmax * 100
  let part1 (g * stem-diameter)
  let part2  (b2 * stem-diameter)
  let part3  (b3 * (stem-diameter * stem-diameter))
  let part4 part2 - part3 + 137
  let part5  ( stem-diameter * part4) / (dmax * hmax)
  let part6 ( 1 - part5)
  let part7 part1 * part6

  set opt (part7 / ((274 + (3 * (b2 * stem-diameter))) - (4 * (b3 * (stem-diameter * stem-diameter)))))
  set real opt * temp_effect

  ;print "Optimal growth"  show upper
  ;print "Actual growth"  show real

  ifelse en [set stem-diameter stem-diameter + real]
            [set stem-diameter stem-diameter + opt]
end


to growth2

  let upper ((1 - (stem-diameter * height)) / (dmax * hmax))

  let lower (g * stem-diameter) * upper

  set opt (lower) / ((274 + (3 * (b2 * stem-diameter))) - (4 * (b3 * (stem-diameter * stem-diameter))))
  set real opt * envirn
;print opt
  set stem-diameter stem-diameter + opt
end


to get-height ;; FAREAST polynomial growth
 ; set b2 2 * (( hmax - 137 ) / dmax)
  ;set b2 b2_1 * 2
 ; set b3 ( hmax - 137 ) / (dmax * dmax )

  set height 137 + ((b2 * stem-diameter) - (b3 * (stem-diameter *  stem-diameter)))
end

to get-height-3 ;; Widlowski 2003
  set b2 (200 * ( hmax - 1.37 )) / dmax
  set b3 b2 / (2 * dmax )

  set height 1.37 + ((b2 * stem-diameter) - (b3 * (stem-diameter *  stem-diameter)))
end

to get-height-2

  ;set b2 2 * (( hmax - 137 ) / dmax)
  ;set b3 ( hmax - 137 ) / (dmax * dmax )

  ;set year year + 1
  ;let height_2 (1.3 + (hmax - 1.3)) * (( 1 - exp( - (1 * stem-diameter) / (hmax - 1.3))))
  ;set height (1.37 + (hmax - 1.37)) * ( 1 - e ^  (  b3 * stem-diameter))

 ;set height height_2

  ;;FAREAST exponential equation

  ;let part1 ( - ( s * stem-diameter))
   ; / (hmax - 1.3))
  let part1 ( - ( s * stem-diameter) / (hmax - 1.3))
  let part3 1 - e ^ (part1)
  ;show part3
  let part4 (hmax - 1.3) * part3
  let part5 1.3 + part4
  set height part5 * 100
end

to get-height-4 ;; exponential function form SORTIE
  set height hmax * (1 - e ^ (-(s / hmax) * stem-diameter)) * 100
end

to height-powerlaw
  set height 3 * (stem-diameter ^ 0.4)
end

to calculate-biomass

   ;;; biomass coefficients (Alberti 2005)

   set biomass (B0 * (stem-diameter ^ B1))
   set wood-weight (biomass / 1000) * 700
   ;print wood-weight
end

to calculate-crown-height
  set crown-height 0.4 * height
end

to calculate-crown-radius

  ;set crown-radius CR0 * (stem-diameter ^ CR1)
  set crown-radius (c-rate * stem-diameter) / 2
end

to calculate-LAI
  set LAI (L0 * (stem-diameter ^ L1))
end

to setup-age
  if stem-diameter > 0 [
  let a1 dmax / stem-diameter
  let a2 1 / a1

  let a3 agemx * a2
  set age round a3 - 20
  ]
end

to calculate-layers
  ask turtles [
    ;; linear based approach
    set lmax height / 100
    set lmin height - crown-height
    set n-layer lmax - lmin

  ;;distribute LAI evenly between these layers
    let Li LAI / n-layer ]

end

to degree-days

  let base 5
  let sinus 58.1 ;; rounded to the nearest .1 from Bortkin 1972
  let sinus_2 116.2
  set TJul (TJul_max + TJul_min) / 2
  set TJan (TJan_max + TJan_min) / 2

  ;write "TJul max = " print TJul_max
  ;write "TJul min = " print TJul_min
  ;write "TJan max = " print TJan_max
  ;write "TJan min = " print TJan_min

  ;write "TJul = " print TJul
  ;write "TJul = " print TJul

  let DEGD_p1 (sinus * (TJul - TJan))
  ;write "DEGD1 = " print DEGD_p1

  let DEGD_p2 182.5 * (5 - ((TJul + TJan) / 2))
  ;write "DEGD2 = " print DEGD_p2

  let DEGD_p3 (5 - ((TJul + TJan) / 2)) ^ 2
  ;write "DEGD3 = " print DEGD_p3

  let DEGD_p4  DEGD_p3 / (TJul - TJan)
  ;write "DEGD4 = " print DEGD_p4


  let DEGD_p5 sinus_2 * DEGD_p4
  ;write "DEGD5 = " print DEGD_p5

  set DEGD DEGD_p1 - DEGD_p2 + DEGD_p5
  ;write "DEGD = " print DEGD

  ;write "-----------------------" print ""
end

to temperature-effect

  let temp_p1 4 * ((DDmax - DEGD) * (DEGD - DDmin))
  let temp_2 (DDmax - DDmin) ^ 2

  set temp_effect temp_p1 / temp_2
  ;print temp_effect
  if temp_effect < 0 [
    set temp_effect 0.1
    ;set temp_effect temp_effect * (- 1)
    ;set temp_effect 1 - temp_effect
  ]
   ; set temp_effect 1 - temp_effect ]

  if const-temp [set temp_effect 1]
  ;;print temp_effect
end

to calculate-envirn
  set envirn random-float 1
  ;show envirn
end

to init-seedpool-FORMIND
  set Nseed int 25000
  ask patches[
    let seed-pool-float Nseed / count patches
    set seed-pool int seed-pool-float
  ]
  set leftover-seeds remainder Nseed count patches
    while [ leftover-seeds > 0 ]
     [ ask one-of patches [
        set seed-pool seed-pool + 1
        set leftover-seeds leftover-seeds - 1
     ]
  ]

  ;show leftover-seeds
  ;show count seed-pool

end

to disperse-seeds-FORMIND
  if ticks mod 12 = 0 [
    ask turtles [
      let A_disp size
      set patch_count count patches in-radius A_disp / 2
    ]
  ]
end

to FAREAST-mortality
  if real < (opt / 10) [
    let death random 1000
    if death < 368 [die]
  ]
end

to crowding-mortality
  ;; firstly identifies that only one tree can grow per patch
  ask patches [
    while [count turtles-here >= 2 ] [
     ask one-of turtles-here [die]
   ]
  ]
  ;; secondly if tree diameter is 1 metre or greater, kills/ restricts growth of trees on neighboring patches
  ask turtles [ if stem-diameter >= 100 [set dominant_neighbor true]]
  ask turtles [if dominant_neighbor = true [ask turtles-on neighbors [die]]]

end

to calc-light-effect
  let shade_intolerant 2.24
  let shade_tolerant 1

  let EiV-factor 0

  ask turtles [
    if El <= 5 and El > 3 [
      set EiV-factor 1 ;; shade and light
    ]
    if El <= 3 [
      set EiV-factor 2 ;; shade tolerant, prefers shade
    ]
    if El > 5 [
      set EiV-factor 3 ;; needs light for growth
    ]
  ]

  ask turtles [
    ifelse EIV [
    set light 0
    if EiV-factor = 1 and open-light-area > 30 and open-light-area <= 70 [
      set light 0.8 ]
    if EiV-factor = 1 and open-light-area <= 30 [
      set light 0.3]
    if EiV-factor = 1 and open-light-area > 70 [
      set light 1]
    if EiV-factor = 2 and open-light-area > 10 and open-light-area <= 50 [
      set light 0.8 ]
    if EiV-factor = 2 and open-light-area <= 10 [
      set light 0.1 ]
     if EiV-factor = 2 and open-light-area > 50 [
      set light 0.5 ]
    if EiV-factor = 3 and open-light-area > 50 and open-light-area <= 100 [
      set light 1 ]
    if EiV-factor = 3 and open-light-area <= 50  [
      set light 0.5 ]
    if open-light-area = 0 [
        set light 0 ]  ]
    [set light open-light-area / 100]
  ]

end

to light-potential
  ;; the highest tree of a cluster will receive the higher percentage of incoming light, with lower tree incrementing down on the percentage
  ;; and also how much of there crown is open to light, i.e how much the tree is be shaded by the upper trees
  ;establish-neighborhood
  ask turtles [
    set dominant_neighbor false]
    ;if count other turtles in-radius crown-radius = 0 [set dominant_neighbor true ]]
  ask turtles [
    ask neighbors2 with-max [height] [set dominant_neighbor true ]]
  gen-light-area

end

to establish-neighborhood
  ask patches [set whoList []]
  ask patches [set patch_neighbor patch_neighbor - patch_neighbor]
  ask patches [set canopy_patches false]

  ask turtles [set who-covered-patches[]]
  ask turtles [set covered-patches (patches) in-radius crown-radius
               set no-covered-patches count (patches) in-radius crown-radius
               set who-covered-patches lput covered-patches who-covered-patches]
  ask turtles [ ask covered-patches [

    set canopy_patches true
    set patch_neighbor patch_neighbor + 1

    set tree_cov_no myself
    set whoList lput tree_cov_no whoList
    set canopy-count canopy-count + 1
    ;print turtle-set whoList
  ]]

ask turtles [
    set neighbors1 [self] of turtle-set [whoList] of (patches) in-radius crown-radius
    set neighbors1 remove-duplicates neighbors1
    set neighbors1 remove self neighbors1
    set neighbors1 lput self neighbors1
    set neighbors2 turtle-set neighbors1
  ]

  ask patches [ ifelse canopy_patches = true
    [if canopy [set pcolor blue]]
      [set pcolor original-soil]]
  ask turtles [
    set my-neighbors other turtles in-radius crown-radius
    set num-neighbors [who] of my-neighbors
  ]


end

to gen-light-area
  ask turtles [ if no-covered-patches > 0 [
    set open-patches count patches with [patch_neighbor = 1] in-radius crown-radius
  let percent-open (open-patches * 100) / no-covered-patches
    set open-light-area percent-open

  ]
    if dominant_neighbor = true [
      set open-light-area 100
      set open-patches no-covered-patches]

    ;print open-light-area
  ]
end

to disperse-seeds
  dispersal-kernal
  ask patches [set disperse_patches false]
  ask turtles [ set disperse-radius (patches) in-radius (crown-radius + disperse-kernal)]
  ask turtles [ ask disperse-radius [
    if canopy_patches = false [ if disperse-area [set pcolor white ]]]]
  ask turtles [ ask disperse-radius [
    set disperse_patches true ]]
  grow-sapling
end


to grow-sapling
  ;; firstly needs to identify the composition of the woodland
  set elm-monitor 0
  set pine-monitor 0
  set oak-monitor 0
  set alder-monitor 0
  set hazel-monitor 0
  set birch-monitor 0
  set lime-monitor 0
  set ash-monitor 0

  set bar-chart []

  calc-sprout-probabilty
  let grow-counter 0
  let pick-species 0

  while [grow-counter < 200] [
    set rand-pick-1 random 1000
    ;print rand-pick-1

    if length random-func = 1 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]

    if length random-func = 2 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]
    if length random-func = 2 and rand-pick-1 > item 0 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func [ set pick-species item 1 which-species]

    if length random-func = 3 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]
    if length random-func = 3 and rand-pick-1 > item 0 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func [ set pick-species item 1 which-species]
    if length random-func = 3 and rand-pick-1 > item 0 random-func + item 1 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func [ set pick-species item 2 which-species]

    if length random-func = 4 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]
    if length random-func = 4 and rand-pick-1 > item 0 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func [ set pick-species item 1 which-species]
    if length random-func = 4 and rand-pick-1 > item 0 random-func + item 1 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func [ set pick-species item 2 which-species]
    if length random-func = 4 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func [ set pick-species item 3 which-species]

    if length random-func = 5 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]
    if length random-func = 5 and rand-pick-1 > item 0 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func [ set pick-species item 1 which-species]
    if length random-func = 5 and rand-pick-1 > item 0 random-func + item 1 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func [ set pick-species item 2 which-species]
    if length random-func = 5 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func [ set pick-species item 3 which-species]
    if length random-func = 5 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func [ set pick-species item 4 which-species]

    if length random-func = 6 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]
    if length random-func = 6 and rand-pick-1 > item 0 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func [ set pick-species item 1 which-species]
    if length random-func = 6 and rand-pick-1 > item 0 random-func + item 1 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func [ set pick-species item 2 which-species]
    if length random-func = 6 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func [ set pick-species item 3 which-species]
    if length random-func = 6 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func [ set pick-species item 4 which-species]
    if length random-func = 6 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func [ set pick-species item 5 which-species]

    if length random-func = 7 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]
    if length random-func = 7 and rand-pick-1 > item 0 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func [ set pick-species item 1 which-species]
    if length random-func = 7 and rand-pick-1 > item 0 random-func + item 1 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func [ set pick-species item 2 which-species]
    if length random-func = 7 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func [ set pick-species item 3 which-species]
    if length random-func = 7 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func [ set pick-species item 4 which-species]
    if length random-func = 7 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func [ set pick-species item 5 which-species]
    if length random-func = 7 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func + item 6 random-func [ set pick-species item 6 which-species]

    if length random-func = 8 and rand-pick-1 <= item 0 random-func [set pick-species item 0 which-species]
    if length random-func = 8 and rand-pick-1 > item 0 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func [ set pick-species item 1 which-species]
    if length random-func = 8 and rand-pick-1 > item 0 random-func + item 1 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func [ set pick-species item 2 which-species]
    if length random-func = 8 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func [ set pick-species item 3 which-species]
    if length random-func = 8 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func [ set pick-species item 4 which-species]
    if length random-func = 8 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func [ set pick-species item 5 which-species]
    if length random-func = 8 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func + item 6 random-func [ set pick-species item 6 which-species]
    if length random-func = 8 and rand-pick-1 > item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func + item 6 random-func and rand-pick-1 <= item 0 random-func + item 1 random-func + item 2 random-func + item 3 random-func + item 4 random-func + item 5 random-func + item 6 random-func + item 7 random-func [ set pick-species item 7 which-species]

    if count patches with [canopy_patches = false and disperse_patches = true] >= 3 [

      if num-elm > 0 and pick-species = 1 [
        set elm-monitor elm-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-elm 1 [setup-species species-setup]]]
        ask patches [set new-growth false]]

      if num-hazel > 0 and pick-species = 5 [
        set hazel-monitor hazel-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-hazel 1 [setup-species species-setup]]]
        ask patches [set new-growth false]
      ]

      if num-pine > 0 and pick-species = 3 [
        set pine-monitor pine-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-pine 1 [setup-species species-setup]]]
        ask patches [set new-growth false]]

      if num-alder > 0 and pick-species = 2 [
        set alder-monitor alder-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-alder 1 [setup-species species-setup]]]
        ask patches [set new-growth false]
      ]

      if num-ash > 0 and pick-species = 8 [
        set ash-monitor ash-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-ash 1 [setup-species species-setup]]]
        ask patches [set new-growth false]
      ]

      if num-oak > 0 and pick-species = 6 [
        set oak-monitor oak-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-oak 1 [setup-species species-setup]]]
        ask patches [set new-growth false]
      ]

      if num-lime > 0 and pick-species = 7 [
        set lime-monitor lime-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-lime! 1 [setup-species species-setup]]]
        ask patches [set new-growth false]
      ]

      if num-birch > 0 and pick-species = 4 [
        set birch-monitor birch-monitor + 1
        ask one-of patches with [canopy_patches = false and disperse_patches = true] [ set new-growth true]
        ask patches [if new-growth = true [sprout-birch 1 [setup-species species-setup]]]
        ask patches [set new-growth false]
      ]
    ]

     set grow-counter grow-counter + 1
  ]

  set elm-monitor-all elm-monitor-all + elm-monitor
  set pine-monitor-all pine-monitor-all + pine-monitor
  set alder-monitor-all alder-monitor-all + alder-monitor
  set hazel-monitor-all hazel-monitor-all + hazel-monitor
  set oak-monitor-all oak-monitor-all + oak-monitor
  set lime-monitor-all lime-monitor-all + lime-monitor
  set ash-monitor-all ash-monitor-all + ash-monitor
  set birch-monitor-all birch-monitor-all + birch-monitor

end

to check-growth-temp
  ask turtles [ if DEGD > DDmin and DEGD < DDmax[set opttemp true]]
end

to calc-sprout-probabilty

  let active_species []
  let species-percent 0
  let sum-prob []
  set random-func []
  set which-species []

  if elm-active = true [
    set active_species lput 1 active_species
    set species-percent species-percent + 1
    set elm-prob [temp_effect] of one-of elm
    set sum-prob lput elm-prob sum-prob
  ]

  if pine-active = true [
    set active_species lput 2 active_species
    set species-percent species-percent + 1
    set pine-prob [temp_effect] of one-of pine
    set sum-prob lput pine-prob sum-prob
  ]

  if oak-active = true [
    set active_species lput 3 active_species
    set species-percent species-percent + 1
    set oak-prob [temp_effect] of one-of oak
    set sum-prob lput oak-prob sum-prob
  ]

  if alder-active = true [
    set active_species lput 4 active_species
    set species-percent species-percent + 1
    set alder-prob [temp_effect] of one-of alder
    set sum-prob lput alder-prob sum-prob
  ]

  if hazel-active = true [
    set active_species lput 5 active_species
    set species-percent species-percent + 1
    set hazel-prob [temp_effect] of one-of hazel
    set sum-prob lput hazel-prob sum-prob
  ]

  if birch-active = true [
    set active_species lput 6 active_species
    set species-percent species-percent + 1
    set birch-prob [temp_effect] of one-of birch
    set sum-prob lput birch-prob sum-prob
  ]

  if lime-active = true [
    set active_species lput 7 active_species
    set species-percent species-percent + 1
    set lime-prob [temp_effect] of one-of lime!
    set sum-prob lput lime-prob sum-prob
  ]

  if ash-active = true [
    set active_species lput 8 active_species
    set species-percent species-percent + 1
    set ash-prob [temp_effect] of one-of ash
    set sum-prob lput ash-prob sum-prob
  ]

  ;print sum-prob

  let species-sum-prob sum sum-prob
  ;print species-sum-prob
  ;print active_species

  ;;if ticks mod 12 = 0 [
  ;;calc-sprout-envirn
  check-growth-temp
  set rand-pick one-of active_species

  set species-percent precision (1 / species-percent) 3
  ;print species-percent
  ;print rand-pick

  if elm-active = true [
    set elm-prob precision (elm-prob * species-percent) 3
    ;print elm-prob
    set elm-prob elm-prob * 1000
    set random-func lput elm-prob random-func
    set which-species lput 1 which-species
  ]

  if alder-active = true [
    set alder-prob precision (alder-prob * species-percent) 3
    ;print alder-prob
    set alder-prob alder-prob * 1000
    set random-func lput alder-prob random-func
    set which-species lput 2 which-species
  ]

  if pine-active = true [
    set pine-prob precision (pine-prob * species-percent) 3
    ;print pine-prob
    set pine-prob pine-prob * 1000
    set random-func lput pine-prob random-func
    set which-species lput 3 which-species
  ]

  if birch-active = true [
    set birch-prob precision (birch-prob * species-percent) 3
    ;print birch-prob
    set birch-prob birch-prob * 1000
    set random-func lput birch-prob random-func
    set which-species lput 4 which-species
  ]

  if hazel-active = true [
    set hazel-prob precision (hazel-prob * species-percent) 3
    ;print hazel-prob
    set hazel-prob hazel-prob * 1000
    set random-func lput hazel-prob random-func
    set which-species lput 5 which-species
  ]

  if oak-active = true [
    set oak-prob precision (oak-prob * species-percent) 3
    ;print oak-prob
    set oak-prob oak-prob * 1000
    set random-func lput oak-prob random-func
    set which-species lput 6 which-species
    ]

  if lime-active = true [
    set lime-prob precision (lime-prob * species-percent) 3
    ;print lime-prob
    set lime-prob lime-prob * 1000
    set random-func lput lime-prob random-func
    set which-species lput 7 which-species
  ]

  if ash-active = true [
    set ash-prob precision (ash-prob * species-percent) 3
    ;print ash-prob
    set ash-prob ash-prob * 1000
    set random-func lput ash-prob random-func
    set which-species lput 8 which-species
  ]

  ;print which-species
  ;print random-func

end

to dispersal-kernal
  ask turtles [
    if height <= 200 [set disperse-kernal ( - crown-radius )]
    if height <= 1000 and height > 200  [set disperse-kernal 1]
    if height > 1000 and height <= 3000 [set disperse-kernal 2]
    if height > 3000 [set disperse-kernal 3]
  ]
end

to display-soil-moisture
  ask turtles [
    set hidden? true ]
  let mHigh max [canopy-count] of patches
  let mLow min [canopy-count] of patches
  ask patches [ set pcolor scale-color red canopy-count mLow mHigh]
end


to species-coop

  let largest-elm max [no-covered-patches] of elm
  let largest-alder max [no-covered-patches] of alder
  let largest-pine max [no-covered-patches] of pine

  ask alder [set size-prop ((no-covered-patches * 100) / largest-alder) ]


  let alder-sum sum [size-prop] of alder
  let pine-sum sum [size-prop] of pine
  let elm-sum sum [size-prop] of elm

  let alder-light sum [open-light-area] of alder ;; overall light patches
  let pine-light sum [open-light-area] of pine
  let elm-light sum [open-light-area] of elm

  ask alder [ set species-sum-light (alder-light * (size-prop / alder-sum))] ;; need this to be divide no of patches covered by the tree by overall light patches *
  ask alder [ if no-covered-patches >= 1 [set percent-energy ((species-sum-light / no-covered-patches) * 100)]]

  ;print alder-light
  ;print alder-sum
  ;print largest-alder
end

to species-coop-2

  let largest-elm sum [no-covered-patches] of elm
  let largest-alder sum [no-covered-patches] of alder
  let largest-pine sum [no-covered-patches] of pine

  let alder-light sum [open-patches] of alder ;; overall light patches
  let pine-light sum [open-patches] of pine
  let elm-light sum [open-patches] of elm

  ask alder [if largest-alder >= 1 [set percent-energy alder-light / largest-alder]]
  ask pine [if largest-pine >= 1 [set percent-energy pine-light / largest-pine]]
  ask elm [if largest-elm >= 1 [set percent-energy elm-light / largest-elm]]

end

to generate-pollen-diagram

end

;;;; SIMULATED ANTHOPROGENIC DISTURBANCE ;;;;

;;; pick a random area and clear trees in that locale
;;; species specific clearing, i'e ideal material for whatever process
;;; for site based simulations, simulate clearing in perimiter around the site

to reduce-stand
  ask turtles [
    if xcor < 0 [die]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
304
28
912
637
-1
-1
6.0
1
10
1
1
1
0
0
0
1
-49
50
-49
50
0
0
1
ticks
30.0

BUTTON
12
24
75
57
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
81
25
144
58
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
12
68
75
101
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
6
152
96
185
En
En
1
1
-1000

MONITOR
175
12
242
57
No. Trees
count turtles
17
1
11

BUTTON
81
69
215
102
NIL
crowding-mortality
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
4
114
98
147
canopy
canopy
1
1
-1000

SWITCH
109
151
227
184
disperse-area
disperse-area
1
1
-1000

SLIDER
10
354
149
387
num-elm
num-elm
0
1000
0.0
1
1
NIL
HORIZONTAL

SLIDER
11
393
149
426
num-pine
num-pine
0
1000
0.0
1
1
NIL
HORIZONTAL

SLIDER
11
431
150
464
num-alder
num-alder
0
1000
0.0
1
1
NIL
HORIZONTAL

SLIDER
11
468
151
501
num-oak
num-oak
0
1000
600.0
1
1
NIL
HORIZONTAL

SLIDER
11
505
152
538
num-lime
num-lime
0
1000
0.0
1
1
NIL
HORIZONTAL

SLIDER
11
545
154
578
num-ash
num-ash
0
1000
0.0
1
1
NIL
HORIZONTAL

SLIDER
11
584
154
617
num-birch
num-birch
0
1000
0.0
1
1
NIL
HORIZONTAL

SLIDER
12
624
153
657
num-hazel
num-hazel
0
1000
0.0
1
1
NIL
HORIZONTAL

CHOOSER
6
197
144
242
Dataset
Dataset
"Whitby" "Durham" "Bradford" "Eskdalemuir" "Oxford"
4

SWITCH
109
114
212
147
EIV
EIV
1
1
-1000

SLIDER
4
252
148
285
num-humans
num-humans
0
100
0.0
1
1
NIL
HORIZONTAL

PLOT
943
15
1364
324
Total
time
no. of
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot count elm\n"
"pen-1" 1.0 0 -16777216 true "" "plot count alder"
"pen-2" 1.0 0 -2674135 true "" "plot count pine"
"pen-3" 1.0 0 -817084 true "" "plot count hazel"
"pen-4" 1.0 0 -10022847 true "" "plot count oak"
"pen-5" 1.0 0 -14439633 true "" "plot count lime!"
"pen-6" 1.0 0 -15302303 true "" "plot count birch\n"
"pen-7" 1.0 0 -5207188 true "" "plot count ash\n"

MONITOR
947
350
1004
395
No. Elm
count elm
17
1
11

MONITOR
1008
350
1067
395
No. Pine
count pine
17
1
11

MONITOR
1071
350
1131
395
No. Alder
count alder
17
1
11

TEXTBOX
954
283
996
380
-
80
105.0
1

TEXTBOX
1321
342
1471
360
NIL
11
0.0
1

TEXTBOX
1020
284
1170
381
-
80
15.0
1

TEXTBOX
1083
284
1233
381
-
80
0.0
1

MONITOR
946
450
1003
495
Elm Rec
elm-monitor
17
1
11

MONITOR
1010
449
1070
494
Pine Rec
pine-monitor
17
1
11

MONITOR
1073
449
1134
494
Alder Rec
alder-monitor
17
1
11

SLIDER
371
670
543
703
manage%
manage%
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
9
709
181
742
max-height%
max-height%
0
100
52.0
1
1
%
HORIZONTAL

SLIDER
191
670
363
703
max-DBH%
max-DBH%
0
100
10.0
1
1
%
HORIZONTAL

CHOOSER
563
670
701
715
Species
Species
"Elm" "Hazel" "Lime" "Birch" "Ash" "Pine" "Alder" "Oak"
5

SLIDER
10
669
182
702
min-height%
min-height%
0
100
62.0
1
1
%
HORIZONTAL

SLIDER
191
707
363
740
min-DBH%
min-DBH%
0
100
10.0
1
1
%
HORIZONTAL

PLOT
938
625
1363
907
Recruit total
time
no. of
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot elm-monitor-all"
"pen-1" 1.0 0 -2674135 true "" "plot pine-monitor-all"
"pen-2" 1.0 0 -16777216 true "" "plot alder-monitor-all"
"pen-3" 1.0 0 -817084 true "" "plot hazel-monitor-all"
"pen-4" 1.0 0 -10022847 true "" "plot oak-monitor-all"
"pen-5" 1.0 0 -5207188 true "" "plot ash-monitor-all"
"pen-6" 1.0 0 -15302303 true "" "plot birch-monitor-all"

PLOT
1383
14
1808
325
Biomass
time
no. of
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot sum [biomass] of elm"
"pen-1" 1.0 0 -16777216 true "" "plot sum [biomass] of alder"
"pen-2" 1.0 0 -2674135 true "" "plot sum [biomass] of pine"
"pen-3" 1.0 0 -817084 true "" "plot sum [biomass] of hazel"
"pen-4" 1.0 0 -5207188 true "" "plot sum [biomass] of ash"
"pen-5" 1.0 0 -15302303 true "" "plot sum [biomass] of birch"
"pen-6" 1.0 0 -10022847 true "" "plot sum [biomass] of oak"

BUTTON
78
755
171
788
Water Content
display-soil-moisture
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
9
754
72
787
park
maintain-open-areas
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
16
836
119
869
NIL
species-coop
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
152
196
242
229
natural
natural
1
1
-1000

SWITCH
9
793
130
826
prop-recruit
prop-recruit
1
1
-1000

PLOT
1380
625
1783
906
DEGD
time
DEGD
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -4079321 true "" "plot DEGD"

SLIDER
373
709
545
742
coppice-increment
coppice-increment
0
20
10.0
1
1
NIL
HORIZONTAL

MONITOR
1135
350
1195
395
No. Hazel
count hazel
17
1
11

MONITOR
1137
449
1196
494
Hazel Rec
hazel-monitor
17
1
11

TEXTBOX
1143
284
1293
381
-
80
26.0
1

MONITOR
1198
351
1258
396
No. Oak
count oak
17
1
11

MONITOR
947
400
1004
445
% Elm
(count elm * 100) / (count turtles - count hazel)
17
1
11

MONITOR
1008
399
1065
444
% Pine
(count pine * 100) / (count turtles - count hazel)
17
1
11

MONITOR
1072
399
1130
444
% Alder
(count alder * 100) / (count turtles - count hazel)
17
1
11

MONITOR
1136
398
1195
443
% Hazel
(count hazel * 100) / (count turtles - count hazel)
17
1
11

MONITOR
1198
398
1258
443
% Oak
(count oak * 100) / (count turtles - count hazel)
17
1
11

MONITOR
1199
449
1258
494
Oak Rec
oak-monitor
17
1
11

MONITOR
1261
351
1319
396
No. Lime
count lime!
17
1
11

MONITOR
1324
351
1385
396
No. Birch
count birch
17
1
11

MONITOR
1389
351
1447
396
No. Ash
count ash
17
1
11

MONITOR
1261
398
1318
443
% Lime
(count lime! * 100) / (count turtles - count hazel)
17
1
11

MONITOR
1325
398
1385
443
% Birch
(count birch * 100) / (count turtles - count hazel)
17
1
11

SLIDER
156
312
248
345
n-trees
n-trees
0
2000
1000.0
1
1
NIL
HORIZONTAL

MONITOR
1389
398
1447
443
% Ash
(count ash * 100) / (count turtles - count hazel)
17
1
11

MONITOR
1261
449
1319
494
Lime Rec
lime-monitor
17
1
11

MONITOR
1326
449
1386
494
Birch Rec
birch-monitor
17
1
11

MONITOR
1389
449
1448
494
Ash Rec
ash-monitor
17
1
11

TEXTBOX
1204
283
1234
346
-
80
123.0
1

TEXTBOX
1270
283
1299
343
-
80
64.0
1

TEXTBOX
1627
424
1777
442
NIL
11
0.0
1

TEXTBOX
1332
283
1360
347
-
80
74.0
1

TEXTBOX
1393
283
1423
343
-
80
36.0
1

SWITCH
158
273
248
306
%-tree
%-tree
1
1
-1000

SLIDER
154
354
291
387
%-elm
%-elm
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
154
393
291
426
%-pine
%-pine
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
155
431
291
464
%-alder
%-alder
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
155
468
293
501
%-oak
%-oak
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
155
506
295
539
%-lime
%-lime
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
156
545
297
578
%-ash
%-ash
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
156
584
299
617
%-birch
%-birch
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
156
624
300
657
%-hazel
%-hazel
0
100
0.0
1
1
%
HORIZONTAL

BUTTON
158
234
226
267
NIL
reset-sums
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
948
518
1005
563
PPA Elm
count elm with [age >= PPA]
17
1
11

MONITOR
1011
518
1069
563
PPA Pine
count pine with [age >= PPA]
17
1
11

MONITOR
1071
517
1134
562
PPA Alder
count alder with [age >= PPA]
17
1
11

MONITOR
1138
516
1197
561
PPA Hazel
count hazel with [age >= PPA]
17
1
11

MONITOR
1200
516
1260
561
PPA Oak
count oak with [age >= PPA]
17
1
11

MONITOR
1261
516
1319
561
PPA Lime
count lime! with [age >= PPA]
17
1
11

MONITOR
1326
516
1384
561
PPA Birch
count birch with [age >= PPA]
17
1
11

MONITOR
1393
516
1452
561
PPA Ash
count ash with [age >= PPA]
17
1
11

MONITOR
948
567
1005
612
% PPA
(count elm with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

MONITOR
1012
567
1069
612
% PPA
(count pine with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

MONITOR
1072
567
1133
612
% PPA
(count alder with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

MONITOR
1138
567
1195
612
% PPA
(count hazel with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

MONITOR
1200
566
1257
611
% PPA
(count oak with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

MONITOR
1261
566
1318
611
% PPA
(count lime! with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

MONITOR
1326
565
1383
610
% PPA
(count birch with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

MONITOR
1393
565
1453
610
% PPA
(count ash with [age >= PPA]  * 100) / (count turtles with [age >= PPA] - count hazel with [age >= PPA])
17
1
11

BUTTON
174
755
279
788
NIL
reduce-stand
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1458
328
1658
478
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot opened-patches"

PLOT
1459
479
1658
623
plot 2
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot overall-biomass\n"

SWITCH
292
758
410
791
const-temp
const-temp
0
1
-1000

PLOT
1663
329
1863
479
plot 3
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks > 1 [plot removed-biomass]\n"

MONITOR
1670
499
1780
544
NIL
removed-biomass
17
1
11

MONITOR
1673
559
1776
604
NIL
opened-patches
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="behaviour-test" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count elm</metric>
    <metric>count pine</metric>
    <metric>count alder</metric>
    <metric>count oak</metric>
    <metric>count ash</metric>
    <metric>count birch</metric>
    <metric>count hazel</metric>
    <enumeratedValueSet variable="natural">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-oak">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-height%">
      <value value="62"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-lime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-lime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-DBH%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-birch">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-elm">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-tree">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species">
      <value value="&quot;Hazel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-ash">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-alder">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-hazel">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="En">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-height%">
      <value value="52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-elm">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-recruit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coppice-increment">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hazel">
      <value value="480"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="canopy">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manage%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-trees">
      <value value="800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-pine">
      <value value="240"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Dataset">
      <value value="&quot;Eskdalemuir&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-ash">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-humans">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-birch">
      <value value="240"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disperse-area">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-oak">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-DBH%">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-alder">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EIV">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-pine">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="No-management" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>overall-biomass</metric>
    <enumeratedValueSet variable="natural">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-height%">
      <value value="62"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-lime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-lime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-birch">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-DBH%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-elm">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-tree">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species">
      <value value="&quot;Elm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-ash">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-alder">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="En">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-hazel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-height%">
      <value value="52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-elm">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-recruit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coppice-increment">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hazel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="canopy">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manage%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-trees">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-pine">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Dataset">
      <value value="&quot;Oxford&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-ash">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="const-temp">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-humans">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-birch">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disperse-area">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-DBH%">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-alder">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EIV">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-pine">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="DBH-management" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>removed-biomass</metric>
    <metric>overall-biomass</metric>
    <metric>opened-patches</metric>
    <metric>count pine</metric>
    <enumeratedValueSet variable="natural">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-height%">
      <value value="62"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-lime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-lime">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-birch">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-DBH%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-elm">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-tree">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species">
      <value value="&quot;Pine&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-ash">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-alder">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="En">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-hazel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-height%">
      <value value="52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-elm">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-recruit">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coppice-increment">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-hazel">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="canopy">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="manage%" first="2" step="2" last="20"/>
    <enumeratedValueSet variable="n-trees">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-pine">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Dataset">
      <value value="&quot;Oxford&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-ash">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="const-temp">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-humans">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-birch">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disperse-area">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-DBH%">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-alder">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EIV">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-pine">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
