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
  cmp ax, 1                     ; EOF flag
  je exec

  call parse_line
  jmp parse_loop

exec:
  ;;  DEBUG print the program
  mov si, program

  mov dx, si
  mov cx, 0x100
  mov bx, 1
  mov ah, 0x40
  int 0x21

  mov bp, sp
  sub sp, 4                     ; Allocate space for accumulator
  mov word [bp-4], 0
  mov word [bp-2], 0
  xor bx, bx
exec_loop:
  mov ax, si
  call print_result
  call write_newline

  xor ax, ax
  mov al, [seen+bx]

  cmp al, 0
  jne .done
  mov al, [seen+bx]
  cmp al, 0
  jne .done
  mov byte [seen+bx], 1
  inc bx
  lodsw                         ; Fetch instruction
  cmp al, "a"                   ; acc
  je .acc
  cmp al, "n"                   ; nop
  je .nop
  cmp al, "j"                   ; jmp
  je .jmp
  jmp .nop
.acc:
  lodsw                         ; Fetch Parameter
  add [bp-2], ax
  adc word [bp-4], 0

  jmp .end
.jmp:
  lodsw                         ; Fetch Parameter
  times 4 add si, ax
  add bx, ax
  jmp .end
.nop:
  lodsw                         ; Fetch Parameter
.end:
  jmp exec_loop

.done:

  call  write_newline
  mov ax, [bp-4]
  call print_result

  call write_newline

  mov ax, [bp-2]
  call print_result

;;; Execute program until it loops

;;; Print result

exit:
  ;;  Terminate
  mov ax,0x4c00
  int 0x21

print_result:
  push ax
  push cx
  push dx


  xor dx, dx
  mov cx, 10
  div cx
  test ax, ax
  je .end
  call print_result             ; Print rest of ax
.end:
  mov ax, dx
  add ax, "0"
  call print_char


  pop dx
  pop cx
  pop ax
  ret

print_char:
  push ax
  push dx

  mov dl, al
  mov ah, 0x02
  int 0x21

  pop dx
  pop ax
  ret

;;; Parses the line from line to es:di
;;; Increments di by 4
parse_line:
  push ax
  push bx
  push cx
  push si

  mov si, line
  mov al, [esi]

  cmp al, "j"                   ; jmp
  je .jmp
  cmp al, "a"                   ; acc
  je .acc
.nop:                            ; nop / else
  mov al, "n"
  jmp .param
.acc:
  mov al, "a"                  ; a
  jmp .param
.jmp:
  mov al, "j"
  jmp .param
.param:                          ; Parse parameter
  add si, 3
  call parse_parameter          ; returns parameter in dx

  stosb                         ; Write al to edi, inc di
  inc di                        ; Skip reserved byte
  mov ax, dx                    ; Write parameter to program
  stosw                         ; Write ax to edi, di+=2

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
  pushf

;;; Read until end of line buffer (0 byte)
.loop:
  lodsb                         ; load esi into al, inc si
  cmp al, 0
  je .end
  jmp .loop
.end:

  std                           ; Move backwards
  mov bx, 1
  mov cx, 0
  sub si, 2                     ; Go to last digit

  .digit_loop:
  mov ax, 0                     ; ah had results from the last multiplication
  lodsb                         ; load esi into al, dec si
  sub al, "0"                   ; ASCII to integer
  jl .dl_end                    ; End of digits

  mov dx, 0
  mul bx
  mov dx, 0
  add cx, ax
  mov ax, 10
  mul bx
  mov dx, 0
  mov bx, ax
  jmp .digit_loop
.dl_end:
  add al, "0"
  cmp al, "+"                     ; neg cx if number didn't start with a "+"
  je .ret
  call write_line
  neg cx

.ret:
  mov dx, cx

  popf
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
  pushf

  cld

  mov si, line

  mov cx, 0
.loop:
  lodsb
  cmp al, 0
  je .end
  inc cx                        ; Number of bytes in the line
  jmp .loop
.end:
  ;; int 21h,ah=40: write to handle
  mov dx, line                  ; line buffer
  mov bx, 1                     ; stdout
  mov ah, 0x40                  ; write cx bytes from dx to bx
  int 0x21
  call write_newline

  popf
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
seen:
  times 0x1000 db 0
zero:
  times 0x100 dw 0

segment stack stack
  resb 0xff
stacktop:
