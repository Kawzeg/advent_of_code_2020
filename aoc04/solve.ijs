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
echo +/*/|:+/|:fields E."1"1 2 >p NB. Part 1
