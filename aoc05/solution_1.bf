>
at y
>++++++[<++++++>-] $
>>>,

<<<<[->>>>-<<<<]>>>>
#
While input isn't $
[
>++++++[<++++++>-] restore input
#
<<<<<
+> 1
>++++++[<+++++++++++>-] B
>> temp cells
----------[++++++++++>,----------] read input until newline
<[<]<  t0
Convert input to binary
[-]  t0
>[-] t1
>[<+>-]+ x
<<<[>>-<+<-]
>[<+>-]
>[>-<[-]]
<  t0
[-]
>[-]
>>[<<+>>-]+
<<<<[>>-<+<-]
>[<+>-]
>[>>-<<[-]]
< t0
[-]
>[-]
>>>[<<<+>>>-]+
<<<<<[>>-<+<-]
>[<+>-]
>[>>>-<<<[-]]
< t0
[-]
>[-]
>>>>[<<<<+>>>>-]+
<<<<<<[>>-<+<-]
>[<+>-]
>[>>>>-<<<<[-]]
< t0
[-]
>[-]
>>>>>[<<<<<+>>>>>-]+
<<<<<<<[>>-<+<-]
>[<+>-]
>[>>>>>-<<<<<[-]]
< t0
[-]
>[-]
>>>>>>[<<<<<<+>>>>>>-]+
<<<<<<<<[>>-<+<-]
>[<+>-]
>[>>>>>>-<<<<<<[-]]
< t0
[-]
>[-]
>>>>>>>[<<<<<<<+>>>>>>>-]+
<<<<<<<<<[>>-<+<-]
>[<+>-]
>[>>>>>>>-<<<<<<<[-]]
< t0

<++++++++++++++++ B to R
> t0
[-]
>[-]
>>>>>>>>[<<<<<<<<+>>>>>>>>-]+
<<<<<<<<<<[>>-<+<-]
>[<+>-]
>[>>>>>>>>-<<<<<<<<[-]]
< t0
[-]
>[-]
>>>>>>>>>[<<<<<<<<<+>>>>>>>>>-]+
<<<<<<<<<<<[>>-<+<-]
>[<+>-]
>[>>>>>>>>>-<<<<<<<<<[-]]
< t0
[-]
>[-]
>>>>>>>>>>[<<<<<<<<<<+>>>>>>>>>>-]+
<<<<<<<<<<<<[>>-<+<-]
>[<+>-]
>[>>>>>>>>>>-<<<<<<<<<<[-]]
< t0

Multiply binary input by y
>  t1
>>>>>>>>>>[<<<<<<<<<<+>>>>>>>>>>-] x
<<<<<<<<<<[<<<
  [>>>>>>>>>>>>>+<<<<<<<<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]
>  t1
>>>>>>>>>[<<<<<<<<<+>>>>>>>>>-] x
<<<<<<<<<[<<<
  [>>>>>>>>>>>>+<<<<<<<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]
>  t1
>>>>>>>>[<<<<<<<<+>>>>>>>>-] x
<<<<<<<<[<<<
  [>>>>>>>>>>>+<<<<<<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]
>  t1
>>>>>>>[<<<<<<<+>>>>>>>-] x
<<<<<<<[<<<
  [>>>>>>>>>>+<<<<<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]

>  t1
>>>>>>[<<<<<<+>>>>>>-] x
<<<<<<[<<<
  [>>>>>>>>>+<<<<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]

>  t1
>>>>>[<<<<<+>>>>>-] x
<<<<<[<<<
  [>>>>>>>>+<<<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]

>  t1
>>>>[<<<<+>>>>-] x
<<<<[<<<
  [>>>>>>>+<<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]

>  t1
>>>[<<<+>>>-] x
<<<[<<<
  [>>>>>>+<<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]

>  t1
>>[<<+>>-] x
<<[<<<
  [>>>>>+<<<+<<-]>>[<<+>>-]
>-] t1
Double y
<<<[>>+>+<<<-]
>>>[<<<+>>>-]
<[<<+>>-]

>  t1
>[<+>-] x
<[<<<
  [>>>>+<<+<<-]>>[<<+>>-]
>-] t1

Sum the number
>[<+>-]
>[<<+>>-]
>[<<<+>>>-]
>[<<<<+>>>>-]
>[<<<<<+>>>>>-]
>[<<<<<<+>>>>>>-]
>[<<<<<<<+>>>>>>>-]
>[<<<<<<<<+>>>>>>>>-]
>[<<<<<<<<<+>>>>>>>>>-]
>[<<<<<<<<<<+>>>>>>>>>>-]
  <<<<<<<<<<

<<[-] Zero R
<[-] Zero y
<[>+>+<<-]>>[-<<+>>] Copy the current max
>>[-<<+>>]<< Move the new possible max
 z = new larger old
[>+
    <<[- >>[-] >+ <<<]
    >>[- >>+<<]
    >[- <<<+>>>]
   <<<- >-]
if z then < old minus y new

>>>>[<<+>+>-]<<[>>+<<-]
>[

<<<
[<->-] code
>>>[-]]

>[-]<<<<[-]


>++++++[<++++++>-] $
>>>,

<<<<[->>>>-<<<<]>>>>

] End while

<<<<<
>>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-
<+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++
<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<