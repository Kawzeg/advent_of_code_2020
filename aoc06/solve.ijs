raw =: freads 'input'

NB. Add LF before and after input
NB. Two LF mark the end of a record
NB. The first LF is used as a fret for intervals in part 2
q =: (<;._2~((2#LF)&E.)) LF,raw,LF
abc =: 'abcdefghijklmnopqrstuvwxyz'
NB. .Part 1
echo +/+/abc&e."1(>q)

NB. Part 2
NB.                        ];.1 Cut into intervals using the first LF as a fret
NB.             abc&e."1        Make an alphabet mask for each line, 1 if the letter appears
NB.            +/               Sum each letter, now we have a list of how often each letter appeared
NB.      #=(u)                  A fork finding out which characters appeared in every line
NB. At the end, sum all columns and rows 
echo +/+/([: (#=([:+/abc&e."1)) ];.1)@>q
