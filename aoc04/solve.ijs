raw =: freads 'input'

NB. Make a hook to avoid writing raw twice:
NB.                       raw,LF Make the last LF a double LF          (y)
NB.           (2#LF)&E.          Indices of double LF                  (v)
NB.    <;._2                     Make boxes using the indices as frets (u)
NB.         ~                    Reverse parameters to fit the hook
NB. (u~ v) y makes it a hook, result will be: y u~ (v y)
NB. p =: (v y) u y would be the equivalent without a hook, ~ isn't necessary then
p =: (<;._2~((2#LF)&E.)) raw,LF
fields =: >;:'byr iyr eyr hgt hcl ecl pid'

NB. For each passport, get the indices of each field
NB. Use +/ to find whether each field is present at all
NB. Use */ to find whether all fields are present
NB. Use +/ To get the sum of all valid passports
NB. Part 1 Solution
echo +/*/|:+/|:fields E."1"1 2 >p

NB. Part 2
NB. Get the element of y that follows x, if present.
NB. If not present, returns y
f =: 4 : '(1+(y i.x)){^:((y i.x)<#y) y'
NB. Convert values that aren't atoms or lists of length 1 to '0'
g =: '0'"_^:([:1&<[:#[:$])
NB. Split y into words, return the word following x
ex =: 4 : 'g>(<x) f (;:y)'
NB. Helper lists
hex =: '0123456789abcdef'
d =: '0123456789'

NB. Validation verbs
NB. (1920&<: * 2002&>:) is a fork returning if both conditions are true
NB. ". converts to a number
byr =: [:(1920&<:*2002&>:)[:".'byr:'&ex
iyr =: [:(2010&<:*2020&>:)[:".'iyr:'&ex
eyr =: [:(2020&<:*2030&>:)[:".'eyr:'&ex

NB. Could be better
NB. Reverse the list, drop/take the first 2 elements
NB.     (150&<:*193&>:)                      Similar to the years above
NB.                                  'mc'&-: checks that the first two characters match
cmV =: ((150&<:*193&>:)@".@|.@(2&}.)*'mc'&-:@(2&{.))@|.
inV =: ((59&<:*76&>:  )@".@|.@(2&}.)*'ni'&-:@(2&{.))@|.
NB. One of them might be 1
hgtV =: cmV+inV
hgt =: [:hgtV'hgt:'&ex

NB. Turn y into a list of indices in the hex list defined above (16 if not present in hex)
NB. Expect that the sum of indices under 16 is 6
NB. Yes, '123g123' would be matched as valid by this
NB. But that doesn't appear in the puzzle input, so, oh well.
hclV =: 3 : '6=+/<&16 hex i.y'
NB. '#' is an atom, but the boxes in ex are lists, therefore 1$'#'
NB. Make sure the value after 'hcl:' is '#'
NB. Run the value after '#' through hclV
hcl =: ([:(1$'#')&-:'hcl:'&ex)*[:hclV(1$'#')&ex

NB. Make a list of three-letter strings
cs =: 7 3$'ambblubrngrygrnhzloth'
NB. Built-in Member verb e. does all the work of checking if the eyecolor is in cs
ecl =: [:(e.&cs)'ecl:'&ex
NB. Check that the pid has 9 members of d
NB. Yes, has the same issue as hcl where it doesn't check that the total number of digits is 9
pid =: [:9&-:[:+/[:e.&d'pid:'&ex

NB. Gerund of all validation verbs
vs =: byr`iyr`eyr`hgt`hcl`ecl`pid
NB. Apply all verbs of the gerund and multiply the results together
valid =: [:*/vs`:0
NB. Part 2 Solution
echo +/valid"1 >p
NB. Alternative to the gerund is a fork train:
valid2 =: byr*iyr*eyr*hgt*hcl*ecl*pid
+/valid2"1 >p

NB. {:: is useful, remember it!
x =: 0{::p
