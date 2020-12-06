#!/usr/bin/bash
awk 'BEGIN{RS="\n\n"}{
gsub(/\n/,"")
gsub(/./,"& ")
n = split($0, a, FS)
$0=""
j=0
delete seen
for (i=1; i<=n; i++)
    if (!seen[a[i]]++)
       j++
sum += j
}
END {print sum}' input
