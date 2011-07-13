globals [middle rightExit]
turtles-own [
  
  knowledge 
  team 
  leaderTurtle 
  closestAisle 
  reachAisle 
  reachConcourse 
  leadership 
  reachExit 
  closestExit 
  height 
  weight 
  age 
  sex 
  child
  flockmates         ;; agentset of nearby turtles
  nearest-neighbor   ;; closest one of our flockmates
  groupLeader
  expandGroup
  parkingLot
  family
  ]


patches-own [ptype pheight exit exitNumber]
to setup 
  
  clear-all
  ask patches [set pcolor violet]
  import-pcolors nameMap
  set rightExit 0
  set middle patch (max-pxcor / 2) (max-pycor / 2)
  ifelse teams = false [ask patches with [
      shade-of? pcolor turquoise or shade-of? pcolor cyan or shade-of? pcolor sky] [
      set ptype "seat" if (round (distance middle) mod (rowDensity) = 0  ) [
        sprout 1 [ 
          set parkingLot one-of [0 1 2 3] set family turtles-here set expandGroup true set shape "person" set groupLeader false set knowledge 0 getAttributes  set reachExit false set closestExit Nobody set leaderTurtle Nobody set reachConcourse false set closestAisle Nobody set reachAisle false set flockmates turtles-here set size 3 set team color] ]]] [
    ask patches with [
      shade-of? pcolor turquoise or shade-of? pcolor cyan or shade-of? pcolor sky][
       set ptype "seat" if (round (distance middle) mod (rowDensity) = 0 and pycor > (max-pycor / 2 )) [
         sprout 1 [
           set parkingLot one-of [0 1 2 3] set family turtles-here set expandGroup true set shape "person" set groupLeader false set knowledge 0 getAttributes set reachExit false set closestExit Nobody set leaderTurtle Nobody set reachConcourse false set closestAisle Nobody set flockmates turtles-here set reachAisle false set size 3 set color orange set team color ] ]]
    ask patches with [
      shade-of? pcolor turquoise or shade-of? pcolor cyan or shade-of? pcolor sky][
       set ptype "seat" if (round (distance middle) mod (rowDensity) = 0 and pycor < (max-pycor / 2) ) [
         sprout 1 [ 
           set parkingLot one-of [0 1 2 3] set family turtles-here set expandGroup true set shape "person" set groupLeader false set knowledge 0 getAttributes set reachExit false set closestExit Nobody set leaderTurtle Nobody  set reachConcourse false set closestAisle Nobody set flockmates turtles-here set reachAisle false set size 3 set color black set team color] ]]]
  ask patches with [
    shade-of? pcolor yellow] [
    set ptype "aisle" set pheight distance middle]
  ask patches with [
    shade-of? pcolor green or shade-of? pcolor lime] [
    set ptype "field" set pheight 0]
  ask patches with [
    shade-of? pcolor gray] [
    set ptype "concourse" set pheight 300]
  ask patches with [
    shade-of? pcolor red or shade-of? pcolor magenta or shade-of? pcolor pink] [
    set ptype "wall"]
  ask patches with [
    shade-of? pcolor orange] [
    set ptype "upperLevel"]
  ask patches with [
    pcolor = white] [
    set ptype "exit"]
  ask patches with [
    pcolor = 0] [
    set ptype "nothing"]
  ask patches with [
    pcolor = violet] [
    set ptype "outside"]
  while [count patches with [ptype = 0] > 0] [
    ask patches with [ptype = 0] [
      ask one-of patches in-radius 1 [
        let tempType ptype let tempColor pcolor ask myself[
          set ptype tempType set pcolor tempColor]]]]
 
  if seatDensity != 50 [ask n-of ((count Turtles) / seatDensity) turtles [die]]
  
  ask patches with [ptype = "exit"] [ifelse (pxcor < 36 and pycor > 36) [set exitNumber 0] [ifelse (pxcor > 36 and pycor < 36) [set exitNumber 1] [ifelse (pxcor > 36 and pycor > 280) [set exitNumber 2] [set exitNumber 3]]]] 
;  if globalNotification = true [
;    ask patches with [ptype = "exit"] [
;      ask turtles in-radius 150 [
;        set closestExit myself]]]

  if globalNotification = true [
    ask turtles [set closestExit ([one-of patches in-radius 5] of min-one-of patches with [ptype = "exit"] [distance myself]) ]]
  
  if police = true [
    ask n-of policeCount (patches with [ptype = "concourse" and not any? patches in-radius 2 with [ptype != "concourse"] ]) [ask patches in-radius 1 with [ptype = "concourse"] [set ptype "police" set pcolor blue set exit (min-one-of patches with [ptype = "exit"] [distance myself])]]]
  
  ask turtles [let famSize random 5 if count turtles in-radius 5 >= famsize [set family n-of  famSize turtles in-radius 6 ask family [set family [family] of myself set parkingLot [parkingLot] of myself ]]]
  ask turtles [set family (turtle-set turtles-here family) set flockmates family]
end

to getAttributes
  ;0 is male, 1 is female
  set sex random 2
  ;assumes roughly two adults to every child at the stadium
  set child (random 3 < 2)
  ifelse child = false [
    set age 2 + abs round random-normal 40 15 
    ifelse sex = 0 [
      set height 2 + abs round random-normal 69.5 3] [
      set height 2 + abs round random-normal 64 3 ]][
      set age 2 + abs round random-normal 10 5 
      ifelse sex = 0 [
        set height 2 + abs round random-normal 50 4][
        set height 2 + abs round random-normal 47 4]] 
  millerFormula
end

to penOn
  ask turtles[pen-down]
end

to penOff
  ask turtles[pen-up]
end

;replace with classification tree: leader, peer, no relation
to-report socialComparison [agent1 agent2]
  let numericalComparison 0
  if [color] of agent1 != [color] of agent2 [set numericalComparison (numericalComparison - 30)]
  if [child] of agent1 = [child] of agent2 [set numericalComparison (numericalComparison + 30)]
  if [sex] of agent1 = [sex] of agent2 [set numericalComparison (numericalComparison + 30)]
  set numericalComparison (numericalComparison + (20 - abs (.25 * ([height] of agent1 - [height] of agent2))))
  set numericalComparison (numericalComparison + (20 - abs (.25 * ([weight] of agent1 - [weight] of agent2))))
  set numericalComparison (numericalComparison + (20 - abs (.5 * ([age] of agent1 - [age] of agent2))))
  report numericalComparison
end

to go
  ;if ticks = 250 [set preferExit false]
  if count turtles = 0 [stop]
  tick
  if (ticks mod upperLevelRate) = 0 [
    ask n-of upperLevelQuantity patches with [
      ptype = "upperLevel" and count turtles-here = 0] [
      sprout 1 [
        set parkingLot one-of [0 1 2 3] set family turtles-here set expandGroup true set shape "person" getAttributes set flockmates turtles-here set groupLeader false set reachExit false set closestExit Nobody set leaderTurtle Nobody set reachConcourse true set closestAisle Nobody set reachAisle true set size 3 set color (one-of list orange black) set team color]]]
  ask turtles [ ifelse reachAisle = false [  
          if closestAisle = Nobody and leaderTurtle = Nobody[
            locateAisle] moveToAisle] [
            ifelse reachConcourse = false [
              moveToConcourse] [
              escapeBuilding ]]]

 
end

to millerFormula
  ;Miller formula for adult weight
  ;Luscombe weight formula for estimating children's weight
  ifelse child = false or age > 13[
    let tempHeight height ifelse sex = 0 [
      ifelse height >= 60 [
        set weight (weight + 124) 
        set tempHeight (tempHeight - 60) 
        set weight (tempHeight * 3)][set weight 124]][
      ifelse height >= 60 [
        set weight (weight + 117) set tempHeight (tempHeight - 60) set weight (tempHeight * 3)][
        set weight 117]]][let weightkg ((3 * age) + 7) set weight (weightkg * 2.2)]
  

end
; making it so that height must be positive allows me to "disable" an aisle by setting height to 0 or negative
to locateAisle
  let aisle (min-one-of patches in-radius 10 with [ptype = "aisle" and pheight > 0] [distance myself])
  ifelse aisle = Nobody [ 
    set leaderTurtle one-of turtles with [distance myself < 10 and closestAisle != Nobody] 
    if leaderTurtle != Nobody [
      let a Nobody ask leaderTurtle [
        set a closestAisle] 
      set closestAisle a  
      set leaderTurtle Nobody]] [set closestAisle aisle set leaderTurtle Nobody ] 
  
end

to moveToAisle
  ifelse closestAisle = Nobody [ set heading (random 360) locateAisle
      jump 1] [face closestAisle jump 1]
  if collisions = true [
    if count turtles-here > 1 [jump -1 locateAisle]]
  let dest one-of patches in-radius 2 with [ptype = "aisle" and count turtles-here = 0]
  if dest != nobody [move-to dest]    
  if ptype = "aisle" [set reachAisle true]
end

to moveToConcourse
  ;field is included to take into account a fiew patches that wind up being shades of green unintentionally
  let nextStep max-one-of patches in-radius 3 with [ptype = "aisle" or ptype = "concourse" or ptype = "field"] [pheight] 
  if nextStep != Nobody [
  face nextStep ]
  jump 1
  if collisions = true [
    if count turtles-here > 1 [jump -1 set heading (heading + random 45)] ]
  let dest one-of patches in-radius 2 with [ptype = "concourse" and count turtles-here = 0]
  if dest != nobody [move-to dest]
  if ptype = "concourse" [set reachConcourse true]
end

to locateExit
; 70 is typical human cone of vision

  set closestExit (one-of patches in-cone 50 70 with [ptype = "exit" and (preferExit = false or exitNumber = [parkingLot] of myself)])
  if closestExit = nobody and any? patches in-cone 10 70 with [ptype = "police"] [
    ifelse preferExit = false [set closestExit [exit] of one-of patches in-cone 10 70 with [ptype = "police"]][
      if any? patches in-cone 10 70 with [ptype = "police" and exitNumber = [parkingLot] of myself] [set closestExit [exit] of one-of patches in-cone 10 70 with [ptype = "police" and exitNumber = [parkingLot] of myself]]]]
  
  if closestExit != Nobody [let temp nobody
      ask closestExit  [set temp one-of patches in-radius 8 with [ptype = "exit"]]
      set closestExit temp 
      face closestExit]  
      
end 

to learnExit 
  if learningType = "local" [
  if any? turtles in-radius leaderVolume with [closestExit != nobody and (preferExit = false or parkingLot = [parkingLot] of myself)][
      ifelse preferExit = false [ set closestExit [closestExit] of one-of turtles in-radius leaderVolume with [closestExit != nobody]] [
        set closestExit [closestExit] of (one-of turtles in-radius leaderVolume with [closestExit != nobody and parkingLot = [parkingLot] of myself])]
  let temp nobody
  ask closestExit  [set temp one-of patches in-radius 5 with [ptype = "exit"]]
  set closestExit temp
  ]]
  
  if learningType = "flockmates" [
  if any? flockmates with [groupLeader = true] [if distance one-of flockmates with [groupLeader = true] < leaderVolume [
  set closestExit [closestExit] of one-of flockmates with [groupLeader = true]] ]
  ; nobody in group knows, find out from people in small area
  if groupLeader = true [if closestExit = nobody [
    ifelse preferExit = false [if any? turtles in-radius leaderVolume with [closestExit != nobody][ 
      set closestExit [closestExit] of (one-of turtles in-radius leaderVolume with [closestExit != nobody])]] [if
      any? turtles in-radius leaderVolume with [closestExit != nobody and parkingLot = [parkingLot] of myself][
      set closestExit [closestExit] of (one-of turtles in-radius leaderVolume with [closestExit != nobody and parkingLot = [parkingLot] of myself])]]
       ]]
  ]
  if closestExit = nobody and any? patches in-cone 10 70 with [ptype = "police"] [
    ifelse preferExit = false [set closestExit [exit] of one-of patches in-cone 10 70 with [ptype = "police"]][
      if any? patches in-cone 10 70 with [ptype = "police" and exitNumber = [parkingLot] of myself] [set closestExit [exit] of one-of patches in-cone 10 70 with [ptype = "police" and exitNumber = [parkingLot] of myself]]]]
  
  if closestExit != Nobody [let temp nobody
      ask closestExit  [set temp one-of patches in-radius 8 with [ptype = "exit"]]
      set closestExit temp 
      face closestExit]  
  
end


 
to escapeBuilding

;if count flockmates = 0 [set flockmates turtles-here]

;if not any? flockmates [set heading (heading + random (60) - random 120)]
if closestExit = nobody and flockCommunication = true and any? flockmates with [groupLeader = true and closestExit != nobody] [set closestExit [closestExit] of one-of flockmates with [groupLeader = true and closestExit != nobody]]
if closestExit = nobody and learningType != "none" [learnExit]
if closestExit = Nobody and groupLeader = true [locateExit] 
;if closestExit != Nobody [ face (closestExit) 
 ; if distance closestExit <= 15 [set heading ( heading + random 50 - random 100)] 
  ;ask other flockmates [set heading [heading] of myself + random 50 - random 100]]

flock
let safe avoid_obstacles
ifelse safe = true [fd 1][ifelse closestExit != nobody [face closestExit set heading (heading + random 30 - random 40)] [set heading (heading + random 30 - random 40)]
  if any? patches in-cone 2 150 with [count turtles-here = 0 and (ptype = "concourse" or (preferExit = false and ptype = "exit" or (ptype = "exit" and exitNumber = [parkingLot] of myself))) ] [
    move-to one-of patches in-cone 2 150 with [count turtles-here = 0 and (ptype = "concourse" or (preferExit = false and ptype = "exit" or (ptype = "exit" and exitNumber = [parkingLot] of myself))) ]]]
;fd 1
if any? patches in-radius 3 with [ptype = "exit"] [if [exitNumber] of one-of patches in-radius 3 with [ptype = "exit"] = parkingLot and preferExit = true [set rightExit (rightExit + 1)] set reachExit true die]

end

to flock  ;; turtle procedure
  
  ;flockmates too far away, find new flockmates
  ;if count flockmates in-radius maxGroupSize < count flockmates [set expandGroup true set flockmates turtles-here set groupLeader false]
  
  ;if expandGroup = true [if random 10 < 2 [ask flockmates [set expandGroup false]]]
  if count flockmates > maxGroupSize [ask flockmates [set expandGroup false]]
  if count flockmates < maxGroupSize and expandGroup = true [find-flockmates]

  ;if any? flockmates with [socialComparison self myself <= 50] [set expandGroup true set flockmates turtles-here set groupLeader false]
  
  if not any? flockmates with [groupLeader = true] [locateExit]
  
  if any? other flockmates
    [ find-nearest-neighbor
      ifelse distance nearest-neighbor <= 1
        [ separate ]
        [ align
          cohere ] ]
    
  if leadershipFunction > 70 [makeLeader]
  
end
to makeLeader 
  set groupLeader false
  ask flockmates [set groupLeader false]
  ask max-one-of flockmates [leadershipFunction][set groupLeader true]
end

to-report leadershipFunction
  let leaderValue 0
  if child = false [set leaderValue (leaderValue + 50)]
  set leaderValue (leaderValue + .5 * age)
  set leaderValue (leaderValue + .5 * weight)
  if count flockmates = 1 [set leaderValue 0]
  report leaderValue
end

to find-flockmates  ;; turtle procedure
;  let newFlockmates other turtles in-radius (maxGroupSize)  with [socialComparison self myself > 20 and reachConcourse = true and expandGroup = true and (preferExit = false or parkingLot = [parkingLot] of myself)]
;  ask newFlockmates [ask myself [set flockmates (turtle-set flockmates [flockmates] of myself)]]
;  
;  ask flockmates [set flockmates [flockmates] of myself]
;  if count flockmates with [groupLeader = true] > 1 [ask flockmates [set groupLeader false]]

 set flockmates turtles in-radius (maxGroupSize)  with [;socialComparison self myself > 20 and 
   reachConcourse = true and expandGroup = true and (preferExit = false or parkingLot = [parkingLot] of myself)]
 ask flockmates [set flockmates [flockmates] of myself]
 if count flockmates with [groupLeader = true] > 1 [ask flockmates [set groupLeader false]]
end

to find-nearest-neighbor ;; turtle procedure
  set nearest-neighbor min-one-of flockmates [distance myself]
end

;;; SEPARATE

to separate  ;; turtle procedure
  turn-away ([heading] of nearest-neighbor) 4.5
end

;;; ALIGN

to align  ;; turtle procedure
  turn-towards average-flockmate-heading  1
end

to-report average-flockmate-heading  ;; turtle procedure
  ;; We can't just average the heading variables here.
  ;; For example, the average of 1 and 359 should be 0,
  ;; not 180.  So we have to use trigonometry.
  let x-component sum [sin heading] of flockmates
  let y-component sum [cos heading] of flockmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

;;; COHERE

to cohere  ;; turtle procedure
  turn-towards average-heading-towards-flockmates 1
end

to-report average-heading-towards-flockmates  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180
  let x-component mean [sin (towards myself + 180)] of flockmates
  let y-component mean [cos (towards myself + 180)] of flockmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

;;; HELPER PROCEDURES

to turn-towards [new-heading max-turn]  ;; turtle procedure
  turn-at-most (subtract-headings new-heading heading) max-turn
end

to turn-away [new-heading max-turn]  ;; turtle procedure
  turn-at-most (subtract-headings heading new-heading) max-turn
end

;; turn right by "turn" degrees (or left if "turn" is negative),
;; but never turn more than "max-turn" degrees
to turn-at-most [turn max-turn]  ;; turtle procedure
  ifelse abs turn > max-turn
    [ ifelse turn > 0
        [ rt max-turn ]
        [ lt max-turn ] ]
    [ rt turn ]
end

to-report avoid_obstacles
    let i 0
    while [([ptype] of patch-ahead i = "concourse" or ([ptype] of patch-ahead i = "exit" and (preferExit = false or [exitNumber] of patch-ahead i = parkingLot )) and i <= 10)]
    [set i (i + 1) ]
    if ([ptype] of patch-ahead i != "concourse" and ([ptype] of patch-ahead i != "exit" or (preferExit = true and [ptype] of patch-ahead i = "exit" and [exitNumber] of patch-ahead i != parkingLot)))
    [
        if [ptype] of patch-at-heading-and-distance (heading - 5) (i + 1) != "concourse" and ([ptype] of patch-at-heading-and-distance (heading - 5) (i + 1) != "exit" or (preferExit = true and [ptype] of patch-at-heading-and-distance (heading - 5) (i + 1) = "exit" and [exitNumber] of patch-at-heading-and-distance (heading - 5) (i + 1) != parkingLot))
        [
            ifelse [ptype] of patch-at-heading-and-distance (heading + 5) (i + 1) != "concourse" and ([ptype] of patch-at-heading-and-distance (heading + 5) (i + 1) != "exit" or (preferExit = true and [ptype] of patch-at-heading-and-distance (heading + 5) (i + 1) = "exit" and [exitNumber] of patch-at-heading-and-distance (heading + 5) (i + 1) != parkingLot))
            [ 
                ifelse random 1 = 0
                [ set i 0 rt 40 ]
                [ set i 0 lt 40 ]
            ]
            [ifelse heading <= 180 and [ptype] of patch-at-heading-and-distance (heading - 5 ) ( i + 1 ) = "wall" [set i 0 rt 60 ][set i 0 lt 60]]
        ]
    ]
   ifelse count [other turtles-here] of patch-ahead 1 = 0  and ([ptype] of patch-ahead 1 = "concourse" or (preferExit = true and [ptype] of patch-ahead 1 = "exit" and [exitNumber] of patch-ahead 1 = parkingLot)) [report true] [report false]
end

@#$#@#$#@
GRAPHICS-WINDOW
312
10
1124
687
-1
-1
2.0
1
5
1
1
1
0
0
0
1
0
400
0
322
0
0
1
ticks

BUTTON
78
34
141
67
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
8
34
74
67
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

CHOOSER
6
88
239
133
nameMap
nameMap
"stadium.gif" "stadium4Exits.gif" "stadium4Exits4Staircases.gif" "stadium4ExitsSquare.gif"
1

SLIDER
5
271
177
304
rowDensity
rowDensity
5
30
30
1
1
NIL
HORIZONTAL

SWITCH
8
139
111
172
teams
teams
0
1
-1000

TEXTBOX
9
310
262
366
lower values on this slider leads to more agents & higher density (counterintuitive I know)
11
0.0
1

SLIDER
11
357
183
390
seatDensity
seatDensity
3
50
50
1
1
NIL
HORIZONTAL

TEXTBOX
36
391
186
419
values here work as expected
11
0.0
1

SWITCH
115
139
231
172
collisions
collisions
0
1
-1000

SWITCH
8
183
174
216
globalNotification
globalNotification
1
1
-1000

SLIDER
11
423
185
456
upperLevelQuantity
upperLevelQuantity
0
200
0
1
1
NIL
HORIZONTAL

TEXTBOX
30
464
180
492
set to 0 if wish to stop new agents\n
11
0.0
1

SLIDER
12
496
184
529
upperLevelRate
upperLevelRate
25
200
25
1
1
NIL
HORIZONTAL

SLIDER
14
539
186
572
maxGroupSize
maxGroupSize
2
30
5
1
1
NIL
HORIZONTAL

CHOOSER
13
221
151
266
learningType
learningType
"local" "flockmates" "none"
1

SLIDER
11
580
183
613
leaderVolume
leaderVolume
3
50
25
1
1
NIL
HORIZONTAL

SWITCH
175
227
278
260
police
police
1
1
-1000

SLIDER
14
626
186
659
policeCount
policeCount
5
60
40
1
1
NIL
HORIZONTAL

MONITOR
215
402
281
447
NIL
rightExit
17
1
11

SWITCH
189
496
307
529
preferExit
preferExit
1
1
-1000

BUTTON
149
33
222
66
NIL
penOn\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
228
31
302
64
NIL
penOff
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
13
665
198
698
flockCommunication
flockCommunication
1
1
-1000

@#$#@#$#@
WHAT IS IT?
-----------
My model is a simulation of a stadium evacuation in an emergency situation. It explores crowd behavior and focuses on the act of grouping within a crowd. Groups form based on a number of factors and characteristics. Within each group there is a leader who is more cognicent of his or her surroundings than the rest of the members of the group. This leader is responsible for leading the group towards an exit.


HOW IT WORKS
------------
This section could explain what rules the agents use to create the overall behavior of the model.


HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.


THINGS TO NOTICE
----------------
Groups form and continue to grow with a 33% probability until they reach a critical mass as defined by the user. The leader is originally the founder of the group until a qualified "leader" is found in the group. The leader is the person who looks for the exit, communicates to some members in the group, and between groups. Leaders speak at a particular volume which determines a radius within which the leader can tell other groupmates information. Movement is essentially flocking behavior with built in obstacle avoidance.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


CREDITS AND REFERENCES
----------------------
COPYRIGHT NOTICE FOR FLOCKING
Copyright 1998 Uri Wilensky. All rights reserved.
Permission to use, modify or redistribute this model is hereby granted, provided that both of the following requirements are followed: a) this copyright notice is included. b) this model will not be redistributed for profit without permission from Uri Wilensky. Contact Uri Wilensky for appropriate licenses for redistribution for profit.
This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML). The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.
This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2002.
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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.1.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Flock Sizes" repetitions="4" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>ticks</metric>
    <steppedValueSet variable="maxGroupSize" first="2" step="2" last="16"/>
  </experiment>
  <experiment name="LeaderVolume" repetitions="4" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>ticks</metric>
    <steppedValueSet variable="leaderVolume" first="3" step="2" last="21"/>
  </experiment>
  <experiment name="PoliceCount" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>ticks</metric>
    <steppedValueSet variable="policeCount" first="10" step="2" last="50"/>
  </experiment>
  <experiment name="prefer exit" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>ticks</metric>
    <enumeratedValueSet variable="preferExit">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GroupSize and LeaderVolume" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>ticks</metric>
    <steppedValueSet variable="leaderVolume" first="3" step="3" last="24"/>
    <steppedValueSet variable="maxGroupSize" first="5" step="5" last="30"/>
  </experiment>
  <experiment name="maxGroupSize" repetitions="4" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>ticks</metric>
    <steppedValueSet variable="maxGroupSize" first="2" step="2" last="20"/>
  </experiment>
  <experiment name="Longer LeaderVolume" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>ticks</metric>
    <steppedValueSet variable="leaderVolume" first="3" step="2" last="25"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
