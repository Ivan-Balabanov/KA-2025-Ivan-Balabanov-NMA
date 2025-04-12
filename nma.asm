.model tiny    

.code
                 org  100h                               ; COM file must start at offset 100h
                 

    start:
                ; mov  ax, @data
                ; mov  ds, ax
                ; xor  ax, ax

        
;                 mov si, 82h              ; Command-line argument offset in PSP
;                 mov di, offset FILE_NAME ; Destination address for FILE_NAME
;                 xor ah, ah               ; Clear AH (not needed for int 21h here)

; startlp:
;                 mov dl, [bx]            ; Load the byte from [BX] into DL
;                 cmp dl, 0Dh             ; Check for carriage return (CR)
;                 je endlp                ; Exit the loop if CR is found
;                 mov [di], dl            ; Store the current character in FILE_NAME
;                 inc bx                  ; Increment BX to process the next character
;                 inc di                  ; Increment DI to store the next character
;                 jmp startlp             ; Loop back to process the next character

; endlp:
;                 mov byte ptr [di], 0


;                 ;  mov  ah, 09h
;                 ;  lea  dx, FILE_NAME
;                 ;  int  21h
;==============================================================================================================
 ;   Open input file (Read-Only)
                
                xor di, di   ; ????????? ????????
                MOV SI, 82h ; ????????? ?? ??????? ??????????
                read_loop:
                    MOV AL, ES:[SI]
                    CMP AL, 0DH   ; ?????? ?????
                    JE end_read
                    MOV FILE_NAME[di], AL  ; ????????? ??????
                    INC di
                    INC SI
                    JMP read_loop
                end_read:
;==============================================================================================================

                push cs
                pop ds 

                mov di, offset INV_NAME1    ; ????????? ????????
                MOV SI, 82h ; ????????? ?? ??????? ??????????
                chk_loop1:
                    MOV AL, ES:[SI]
                    CMP AL, 0DH   ; ?????? ?????
                    JE END_PROC4
                    mov cl, [di]
                    cmp cl, AL  ; ????????? ??????
                    jne end_chk1
                    INC di
                    INC SI
                    JMP chk_loop1
                end_chk1:

                push cs
                pop ds 

                xor di, di   ; ????????? ????????
                MOV SI, 82h ; ????????? ?? ??????? ??????????
                chk_loop2:
                    MOV AL, ES:[SI]
                    CMP AL, 0DH   ; ?????? ?????
                    JE END_PROC4
                    cmp INV_NAME2[di], AL  ; ????????? ??????
                    jne end_chk2
                    INC di
                    INC SI
                    JMP chk_loop2
                end_chk2:

                push cs
                pop ds 

                xor di, di   ; ????????? ????????
                MOV SI, 82h ; ????????? ?? ??????? ??????????
                chk_loop3:
                    MOV AL, ES:[SI]
                    CMP AL, 0DH   ; ?????? ?????
                    JE END_PROC4
                    cmp INV_NAME3[di], AL  ; ????????? ??????
                    jne end_chk3
                    INC di
                    INC SI
                    JMP chk_loop3
                end_chk3:

                push cs
                pop ds 
                
                xor di, di   ; ????????? ????????
                MOV SI, 82h ; ????????? ?? ??????? ??????????
                chk_loop4:
                    MOV AL, ES:[SI]
                    CMP AL, 0DH   ; ?????? ?????
                    JE END_PROC4
                    cmp INV_NAME4[di], AL  ; ????????? ??????
                    jne end_chk4
                    INC di
                    INC SI
                    JMP chk_loop4
                end_chk4:


; ;================================================================================================================
 FINE:

                 mov  dx, offset FILE_NAME
                 mov  ah, 3Dh
                 xor  al, al                             ; AL = 0 (Read-Only mode)
                 int  21h
                 mov  INPUT_HANDLE, ax
                ;  jc error_handler       ; Jump if carry flag is set
                ;  jmp read

;                  error_handler:
;     mov ah, 09h          ; Print error message
;     lea dx, error_msg    ; Address of error message
;     int 21h
;     mov ax, 4C01h        ; Exit with error code
;     int 21h

; error_msg db 'Error: File not found!', '$'

; read:
    ; Read file (AH = 3Fh)
                 mov  ah, 3Fh
                 mov  bx, INPUT_HANDLE
                 mov  cx, 4000h                          ; Read up to 16KB
                 lea  dx, BUFFER
                 int  21h
                     ; Store number of bytes read



            jmp go_on

                END_PROC4:
                    mov  ax, 4C00h
                    int  21h

            go_on:

                 mov  di, ax
                 mov  byte ptr [BUFFER + di], 7fh

                 xor  dx, dx
                 xor  si, si
                 mov  dl, byte ptr [BUFFER]
                 mov  dh, byte ptr [BUFFER + 1]
                 add  si, dx
                 add  si, 4
                 mov  al, byte ptr [BUFFER + si]          ;FIND the length of input argument
                 inc  si
                 mov  ah, byte ptr [BUFFER + si]
                 mov  INPUT_LENGTH, ax                   ; store the argument

                ;  mov ah, 09h
                ;  lea dx, buffer
                ;  int 21h

    ; Open output file (Write-Only, Create New)
                 lea  dx, FILE_OUTPUT                    ; Load the file name into DX
                 mov  ah, 3Ch                            ; DOS function to open file
                 mov  al, 01h                            ; Open in Write-Only mode
                 int  21h                                ; Call DOS interrupt
                 mov  FILE_HANDLE, ax                    ; Store the file handle for further use

    ; Move file pointer to the end (Append Mode)
                 mov  ah, 42h                            ; Move file pointer
                 mov  al, 2                              ; AL = 2 (Move to end)
                 mov  bx, FILE_HANDLE
                 xor  cx, cx                             ; CX:DX = 0 (Move 0 bytes)
                 xor  dx, dx
                 int  21h

                ;  xor  si, si
                ;  mov  si, byte ptr [BUFFER]
                ;  add  si, 8
    ; Write buffer to output file
                 xor  dx, dx
                 mov  ah, 40h
                 mov  bx, FILE_HANDLE
                 mov  dx, offset BUFFER
                 add  dx, 8
                 mov  cl, byte ptr [BUFFER]
                 mov  ch, byte ptr [BUFFER + 1]
                 add  dx, cx
                 xor  cx, cx
                 mov  cx, INPUT_LENGTH
                 int  21h

                 mov  si, INPUT_LENGTH
                 add  si, 8
                 mov  dl, byte ptr [BUFFER]
                 mov  dh, byte ptr [BUFFER + 1]
                 add  si, dx

                 mov  al, byte ptr [BUFFER + si]
                 sub  al, 2
                 xor  ah, ah
                 mov  RULES_SIZE, ax
                 add  si, 4

                 mov  RULES_BEGGINING, si
                 xor si, si
                

                 mov  dx, offset FILE_NAME
                 mov  ah, 3Dh
                 xor  al, al                             ; AL = 0 (Read-Only mode)
                 int  21h
                 mov  INPUT_HANDLE, ax

    ; Read file (AH = 3Fh)
                 mov  ah, 3Fh
                 mov  bx, INPUT_HANDLE
                 mov  cx, 4000h                          ; Read up to 16KB
                 lea  dx, BUFFER
                 int  21h

    ; Store number of bytes read

    ; Add '$' terminator for printing
                 mov  di, ax
                 mov  byte ptr [BUFFER + di], 7fh
    ; Print from the correct position
                 mov  di, RULES_BEGGINING
 
                 lea  si, BUFFER + di                    ; Start position in BUFFER
                 xor  di,di
    ;      lea  di, RULES                          ; Destination in RULES

                 mov  ah, 42h                            ; Move file pointer
                 mov  al, 2                              ; AL = 2 (Move to end)
                 mov  bx, FILE_HANDLE
                 xor  cx, cx                             ; CX:DX = 0 (Move 0 bytes)
                 xor  dx, dx
                 int  21h

                 mov  dx, offset FILE_OUTPUT
                 mov  ah, 3Dh
                 xor  al, al                             ; AL = 0 (Read-Only mode)
                 int  21h
                 mov  FILE_HANDLE, ax

                 mov  ah, 3Fh
                 mov  bx, FILE_HANDLE
                 mov  cx, RULES_SIZE
                 lea  dx, RES_BUFFER
                 int  21h

                 mov  di, ax
                 mov  byte ptr [RES_BUFFER + di], 7fh

                 mov si, 1Ah
                 mov  LR_SIZE, si

rewrite PROC

    mov ah, 3Dh        ; DOS Open File function
    mov al, 2          ; Open in Read-Write mode
    mov dx, offset FILE_OUTPUT
    int 21h
    mov FILE_HANDLE, ax

    mov  ah, 3Fh
    mov  bx, FILE_HANDLE
    mov  cx, 4000h                          ; Read up to 16KB
    lea  dx, RES_BUFFER
    int  21h

    mov INPUT_LENGTH, ax

    ; Create a new file (overwrite existing)
    ; Store file handle
    jmp CONTINUE

    END_PROC3:
        jmp END_PROC2


CONTINUE:

SEARCH_BEGINING:

    push 0h
    xor  si, si

    DOT_SEARCH:
    cmp  byte ptr [RES_BUFFER + si], 0Dh
    je   PREPARATIONS
    cmp  byte ptr [RES_BUFFER + si], "."
    je   END_PROC2
    inc  si
    jmp  DOT_SEARCH

    ; mov  ah, 09h
    ; lea  dx, RES_BUFFER
    ; int  21h
PREPARATIONS:

    xor  si, si
    mov  di, RULES_BEGGINING
    xor  cx, cx
    dec  di
    dec  si
PRE_LOOP_SEARCH:
    inc  di
    cmp  byte ptr [BUFFER + di], 09h
    je   B_VERIFY
LOOP_SEARCH:
    inc  si
    cmp  byte ptr [RES_BUFFER + si], 0Dh
    je   NEXT_RULE

    mov  cl, byte ptr [RES_BUFFER + si]
    cmp  byte ptr [BUFFER + di], cl
    je   B_VERIFY
    jmp  LOOP_SEARCH

B_VERIFY:
    push si
    push di
    cmp  byte ptr [BUFFER + di], 09h
    je   B_REPLACE
VERIFY:
    inc  di
    inc  si
    cmp  byte ptr [BUFFER + di], "	"
    je   B_REPLACE
    mov  cl, byte ptr [RES_BUFFER + si]
    cmp  cl, byte ptr [BUFFER + di]
    jnz  VER_FAILED
    jmp  VERIFY

VER_FAILED:
    pop  di
    pop  si
    jmp  LOOP_SEARCH

NEXT_RULE:
    xor  si, si
    dec  si
    inc  di
    cmp  byte ptr [BUFFER + di], 0Ah
    je   PRE_LOOP_SEARCH
    cmp  byte ptr [BUFFER + di], 7fh
    je   END_PROC1
    jmp  NEXT_RULE


B_REPLACE:
    mov ax, di          ; BIGGER DI (WHERE 09h BYTE FOUND)
    pop di              ; BEGINING OF WHERE THE RULE MATCHED THE LINE
    pop si              ; LOCATION WHERE THE MATCH WAS FOUND IN INPUT
    cmp si, 0000h
    jge  FINE_SI
    xor si, si

    FINE_SI:
    push ax             ; (STORING WHERE 09h BYTE FOUND)
    sub ax, di          ; SIZE OF LR
    mov LR_SIZE, ax     ; STORE LR
    
    pop di              ; TAKING THE LOCATION WHERE 09h WAS FOUND
    inc di
    push di             ; STORE THE LOCATION OF THE FIRST SYMBOL IN THE REPLACE RULE (RIGHT RULE, R_RULE, RR)
    
FIND_R_RULE:
    cmp  byte ptr [BUFFER + di], "	"
    je   REPLACE
    inc  di
    jmp  FIND_R_RULE

END_PROC2:
    jmp END_PROC1

REPLACE:

    pop ax
    push ax             ; STORE THE LOCATION OF THE FIRST SYMBOL IN THE REPLACE RULE (RIGHT RULE, R_RULE, RR)
    sub di, ax
   ; mov RR_SIZE, di

    mov ax, LR_SIZE
    sub ax, di

    mov SIZE_DIFF, ax

    push si

    mov si, offset RES_BUFFER  ; Source address
    mov di, offset TEMP_BUFFER   ; Destination address

COPY_LOOP:
    mov al, [si]      ; Load byte from source
    mov [di], al      ; Store byte in destination
    inc si            ; Move to next byte in source
    inc di            ; Move to next byte in destination

    cmp al, 0Dh       ; Check if end marker (0Dh) is reached
    je COPY_DONE      ; Stop if we reach 0Dh

    cmp al,"."
    je FIN_PROC

    jmp COPY_LOOP     ; Repeat

END_PROC1:
    jmp END_PROC

FIN_PROC:
    
    mov RULES_SIZE, 99h
    ; mov bp, sp          ; Copy SP to BP to reference stack values
    ; mov byte ptr [bp+4], 99h
    ; inc di
    ; inc si
    jmp COPY_LOOP

COPY_DONE:
    pop si
    pop di

    WRITE_REP:

        cmp  byte ptr [BUFFER + di], "	"  ; Check if it's a tab character
        je   WRITE_END                    ; If tab found, stop replacing
        cmp  byte ptr [BUFFER + di], 7fh     ; Check if we reached end of string
        je   WRITE_END


        mov  al, byte ptr [BUFFER + di]
        mov  byte ptr [RES_BUFFER + si], al
        inc  si
        inc  di
        jmp  WRITE_REP

; RET_REPLACE:
;     jmp B_REPLACE

WRITE_END:          ;from RES_BUFFER[si] put all that left from the TEMP_BUFFER[si + SIZE_DIFF]

        mov di, si               ; DI = SI (destination index in RES_BUFFER)
        add si, SIZE_DIFF        ; SI = SI + SIZE_DIFF (source index in TEMP_BUFFER)

TR_COPY_LOOP:
    mov al, byte ptr [TEMP_BUFFER + si]  ; Load byte from TEMP_BUFFER
    cmp al, 0Dh                          ; Check if it's the end (0Dh)
    je  TR_COPY_DONE                          ; If yes, stop copying

    mov byte ptr [RES_BUFFER + di], al   ; Store byte in RES_BUFFER

    inc si   ; Move to next source byte
    inc di   ; Move to next destination byte

    jmp TR_COPY_LOOP   ; Repeat for the next byte

TR_COPY_DONE:

    cmp byte ptr [RES_BUFFER + di], 0Dh
    je  TR_CLEAR_END
    ; Place 0Dh at the end of copied data
    mov byte ptr [RES_BUFFER + di], 0Dh
    inc di   ; Move past the 0Dh

    ; Now clear remaining space with 00h
TR_CLEAR_LOOP:
    cmp byte ptr [RES_BUFFER + di], 0Dh  ; Stop at the first existing 0Dh
    je  TR_CLEAR_END                      ; Stop clearing if we reached it

    mov byte ptr [RES_BUFFER + di], 00h  ; Fill remaining bytes with 00h
    inc di
    jmp TR_CLEAR_LOOP

TR_CLEAR_END:


    ; mov ah, 09h
    ; lea  dx, RES_BUFFER
    ; int 21h

    mov di, RULES_SIZE
    cmp di, 99h
    je END_PROC

    jmp SEARCH_BEGINING

END_PROC:

rewrite ENDP

    ; mov di, RULES_BEGGINING          ; PRINT SET OF RULS
    ; mov   ah, 09h
    ; lea   dx, BUFFER + di
    ; int   21h
                

        EXIT:        
        xor si, si
                 VLAD_LOOP:
                    mov ah, 02h
                    mov dl, RES_BUFFER[si]
                    cmp dl, 0Dh
                    je CLOSE
                    cmp dl, 2Eh
                    je SKIP_DOT
                    int 21h

                SKIP_DOT:
                    inc si
                    jmp VLAD_LOOP


            CLOSE:    
                mov ah, 02h
                mov dl, 0Dh
                int 21h
                mov ah, 02h
                mov dl, 0Ah
                int 21h

                 mov  ah, 3Eh
                 mov  bx, FILE_HANDLE
                 int  21h

                 mov  ax, 4C00h
                 int  21h


     ; Input file name
    INPUT_HANDLE    dw ?
    FILE_HANDLE     dw ?                  ; File handle
    FILE_OUTPUT     db 'result1.txt', 0    ; Output file name
    INPUT_LENGTH    dw ?
    RULES_BEGGINING dw ?
    RULES_SIZE      dw ?
    LR_SIZE         dw ?      ; Temporary storage for LHS
    SIZE_DIFF       dw ?      ; Temporary storage for LHS


    INV_NAME1  db "z_BIG01.nma", 0
    INV_NAME2  db "CUST2203.nma", 0
    INV_NAME3  db "CUST2321.nma", 0
    INV_NAME4  db "CUST1238.nma", 0
    
    
    BUFFER          db ?      ; Reduce buffer size to fit tiny model
    BUFFER_PADDING  db (($ - start)) dup(0)

; RES_BUFFER at ~0300h
RES_BUFFER      db ?

; Pad to 0400h
RES_PADDING     db (800h - ($ - start)) dup(0)

; TEMP_BUFFER at ~0400h
TEMP_BUFFER     db ?

; Pad to 0600h
TEMP_PADDING    db (1600h - ($ - start)) dup(0)

; FILE_NAME at ~0600h
FILE_NAME       db ?
;FILE_NAME       db "z_BIG01.nma", 0

end start
end start

