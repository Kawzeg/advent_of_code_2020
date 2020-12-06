#!/usr/bin/bash

echo Part 1
awk 'BEGIN {RS="\n\n"}
{
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

echo Part 2
awk 'BEGIN {RS="\n\n"; sum=0}
{
gsub(/\n/," ")
n = split($0, a, FS)
$0=""
k=1
delete seen
for (i=1; i<=n; i++) {
    gsub(/./,"& ",a[i])
    m = split(a[i], b, FS)
    for (j=1; j<=m; j++)
        seen[b[j]]+=1
}
for (q in seen)
    if (seen[q] == n)
       sum++
}
END {print sum}
' input
