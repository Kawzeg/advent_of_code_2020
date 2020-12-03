require 'format/printf'
raw =: freads 'input'

NB. Cut (https://code.jsoftware.com/wiki/Vocabulary/semidot1) at newlines
NB. Uses the last character (a newline) as frets
NB. Unboxing with > to get a table. There won't be fill because of the input format
NB. Converting to 1 and 0 with '#'=
data =: '#'=>;._2 raw

NB. i.#data - Array from 0 to length of data - 1
NB. y|.                Rotate input according to y (the "slope")
NB.    ^:              Rotate multiple times - Gives an array of all rotations from 0 to needed length
NB.        (#data)%{.y Find how many times to rotate by dividing length of the data by the head of y
NB.      <.            Floor the result
NB.   ({."1^:2) Take the head of the head of the table to get the "current" spot
NB. +/          Sum the list
f =: 3 : '+/({."1^:2) y|.^:(i.<.(#data)%{.y) data'"1
] part1 =: f 1 3

'Part 1 result: %d' printf part1

] slopes =: 1 1 , 1 3 , 1 5 , 1 7 ,: 2 1

] part2 =: */ f slopes
'Part 2 result: %d' printf part2

NB. Shorter:
in =: '#'=>;._2 freads 'input'
f =: 3 : '+/({."1^:2) y|.^:(i.<.(#in)%{.y) in'"1
echo f 1 3
echo */ f 1 1 , 1 3 , 1 5 , 1 7 ,: 2 1
