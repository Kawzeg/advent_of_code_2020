bits 16
segment code

..start:

  ;; Set up data segment
  mov ax, data
  mov ds, ax
  mov es, ax
;;; Set up stack
  mov ax,stack
  mov ss,ax
  mov sp,stacktop
  mov bp,stacktop


;;; Load Program from 'input' file into program:
  ;; Open input file
  mov ah, 0x3d                  ; Open Handle
  mov al, 0                     ; Read
  mov dx, input_file
  int 0x21
  jc exit                       ; CF=1 if error
  mov bx, ax                    ; File handle

  ;; Read file into memory
  mov ah, 0x3f                  ; Read Handle
  mov dx, input                 ; Target buffer
  mov cx, 0x1000                ; Number of bytes to read
  int 0x21

  ;; Copy line into buffer
  mov cx, 0
  mov si, input
;;; Parse program
  mov di, program
parse_loop:
  call read_line
  call write_line
  cmp ax, 1                     ; EOF flag
  je exec

  call parse_line
  jmp parse_loop

exec:
  ;;  DEBUG print parameters
  mov si, program
  lodsw                         ; Skip Instruction
  mov dx, di
  mov cx, 1
  mov bx, 1
  mov ah, 0x40
  int 0x21
  lodsw


;;; Execute program until it loops

;;; Print result

exit:
  ;;  Terminate
  mov ax,0x4c00
  int 0x21

;;; Parses the line from line to es:di
;;; Increments di by 4
parse_line:
  push ax
  push bx
  push cx
  push si

  mov si, line
  mov al, [esi]

  cmp al, 0                     ; eof
  je eof
  cmp al, "j"                   ; jmp
  je jmp
  cmp al, "a"                   ; acc
  je acc
nop:                            ; nop / else
  mov al, 0x00
  jmp param
acc:
  mov al, 0x01
  jmp param
jmp:
  mov al, 0x02
  jmp param
param:                          ; Parse parameter
  add si, 4
  call parse_parameter          ; returns parameter in dx

  stosb                         ; Write al to edi, inc di
  inc di                        ; Skip reserved byte
  mov ax, dx                    ; Write parameter to program
  stosw                         ; Write ax to edi, di+=2

  mov ax, 0
pl_ret:
  pop si
  pop cx
  pop bx
  pop ax
  ret

;;; Parse the parameter at ds:si
;;; Increments si to point after the newline
;;; Returns the parameter in dx
parse_parameter:
  push ax
  push bx
  push cx

  mov dx, 0

  lodsb
  cmp al, "+"
  je .loop
  mov dx, 1                     ; Need to negate the result at the end
;;; Read until end of line (0 byte)
.loop:
  lodsb                         ; load esi into al, inc si
  cmp al, 0
  je .end
  jmp .loop
.end:

  std                           ; Move backwards
  mov bx, 1
  mov cx, 0

.digit_loop:
  dec si                        ; Point at last number
  lodsb                         ; load esi into al, dec si
  sub al, "0"                   ; ASCII to integer
  cmp al, 0
  jl .dl_end                    ; End of digits
  mul bx
  add cx, ax
  mov ax, 10
  mul bx
  mov bx, ax
  jmp .digit_loop
.dl_end:
  cmp dx, 0                     ; neg cx if number didn't start with a "+"
  je .ret
  neg cx

.ret:
  mov dx, cx
  cld                           ; Move forwards
  pop cx
  pop bx
  pop ax
  ret

zero_line:
  push di
  push si

  mov si, zero
  mov di, line
  times 0x40 movsw

  pop si
  pop di
  ret

;;; Copies bytes until \n from es:si into the line buffer
;;; Increments si to point at the next line
;;; Returns 1 in ax if eof was reached
read_line:
  push bx
  push cx
  push di

  call zero_line                ; Empty line buffer

  mov cx, 0
  mov di, line
read_loop:
  lodsb                         ; load ds:si into al, inc si
  cmp al, `\n`
  je read_end
  cmp al, 0                     ; EOF
  je eof
  stosb                         ; Store al in es:di, increment di
  inc cx

  jmp read_loop
read_end:
  mov ax, 0
  jmp ret
eof:
  mov ax, 1
ret:

  pop di
  pop cx
  pop bx
  ret

;;; Prints the contents of the line buffer up to \n to stdout
write_line:
  push dx
  push cx
  push bx
  push ax
  push si

  mov si, line

  mov cx, 0
.loop:
  lodsb
  cmp al, 0
  je .end
  inc cx                        ; Number of chars in the line
  jmp .loop
.end:
  ;; int 21h,ah=40: write to handle
  mov dx, line                  ; line buffer
  mov bx, 1                     ; stdout
  mov ah, 0x40                  ; write cx bytes from dx to bx
  int 0x21
  call write_newline

  pop si
  pop ax
  pop bx
  pop cx
  pop dx
  ret

write_newline:
  push dx
  push cx
  push bx
  push ax

  ;; int 21h,ah=40: write to handle
  mov dx, newline
  mov cx, 1
  mov bx, 1
  mov ah, 0x40
  int 0x21

  pop ax
  pop bx
  pop cx
  pop dx
  ret

segment data
  ;; Program will live here
program:
  times 0x1000 dd 0
  ;; Input file name
input_file:
  db "input", 0
  ;; Buffer for input file content
input:
  db "input file"
  db 0x10
  times 0x1000 dd 0
  ;; Buffer for single lines
line:
  times 0x100 dw 0
hello:
  db `Hello world!`
newline:
  db `\n`
zero:
  times 0x100 dw 0
segment stack stack
  resb 0xff
stacktop:
