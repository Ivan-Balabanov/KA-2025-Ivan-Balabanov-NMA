.model tiny    

.data
    FILE_NAME       db 'input.nma', 0     ; Input file name
    INPUT_HANDLE    dw ?
    FILE_HANDLE     dw ?                  ; File handle
    BUFFER          db 16384 dup (?)      ; Reduce buffer size to fit tiny model
    RES_BUFFER      db 16384 dup (?)
    BYTES_READ      dw ?                  ; Number of bytes read
    FILE_OUTPUT     db 'result.txt', 0    ; Output file name
    INPUT_LENGTH    dw ?
    RULES_BEGGINING dw ?
    RULES_SIZE      dw ?
    LR_SIZE         dw ?      ; Temporary storage for LHS

.code
                 org  100h                               ; COM file must start at offset 100h

    start:       
                 mov  ax, @data
                 mov  ds, ax
                 xor  ax, ax

    ; Open input file (Read-Only)
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
                 mov  BYTES_READ, ax

    ; Add '$' terminator for printing
                 mov  di, ax
                 mov  byte ptr [BUFFER + di], '$'

                 mov  al, byte ptr [BUFFER + 4]          ;FIND the length of input argument
                 xor  ah,ah
                 mov  INPUT_LENGTH, ax                   ; store the argument

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

    ; Write buffer to output file
                 mov  ah, 40h
                 mov  bx, FILE_HANDLE
                 lea  dx, BUFFER + 8
                 mov  cx, INPUT_LENGTH
                 int  21h

                 mov  si, INPUT_LENGTH
                 add  si, 7

                 inc  si
                 mov  al, [BUFFER + si]
                 sub  al, 2
                 xor  ah, ah
                 mov  RULES_SIZE, ax
                 add  si, 4

                 mov  RULES_BEGGINING, si
                

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
                 mov  BYTES_READ, ax

    ; Add '$' terminator for printing
                 mov  di, ax
                 mov  byte ptr [BUFFER + di], '$'
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

    ; Write buffer to output file
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
                 mov  byte ptr [RES_BUFFER + di], '$'

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


CONTINUE:

SEARCH_BEGINING:

    xor  si, si
    mov  di, RULES_BEGGINING
    xor  cx, cx
    dec  di
    dec  si
PRE_LOOP_SEARCH:
    inc  di
LOOP_SEARCH:
    inc  si
    cmp  byte ptr [RES_BUFFER + si], 0Dh
    je   NEXT_RULE

    mov  cl, byte ptr [RES_BUFFER + si]
    cmp  byte ptr [BUFFER + di], cl
    je   B_VERIFY
    jmp  LOOP_SEARCH

VER_FAILED:
    pop  si
    pop  di
    jmp  LOOP_SEARCH

B_VERIFY:
    push si
    push di
VERIFY:
    inc  di
    inc  si
    cmp  byte ptr [BUFFER + di], "	"
    je   REPLACE
    mov  cl, byte ptr [RES_BUFFER + si]
    cmp  cl, byte ptr [BUFFER + di]
    jnz  VER_FAILED
    jmp  VERIFY

REPLACE:
    dec  si

    ; mov  ax, di
    ; pop  di
    ; sub  ax, di
    ; mov  LR_SIZE, ax

    mov  ah, 3Dh        ; DOS Open File function
    mov  al, 2          ; Open in Read-Write mode
    mov  dx, offset FILE_OUTPUT
    int  21h
    mov  FILE_HANDLE, ax
    ; Read existing file content before modification
    mov  ah, 3Fh
    mov  bx, FILE_HANDLE
    mov  cx, si
    lea  dx, RES_BUFFER
    int  21h

WRITE_REP:
    inc  di
    cmp  byte ptr [BUFFER + di], "	"  ; Check if it's a tab character
    je   WRITE_END                    ; If tab found, stop replacing
    cmp  byte ptr [BUFFER + di], "$"     ; Check if we reached end of string
    je   WRITE_END

    mov  al, byte ptr [BUFFER + di]
    mov  byte ptr [RES_BUFFER + si], al
    inc  si
    jmp  WRITE_REP

    NEXT_RULE:
    xor  si, si
    dec  si
    inc  di
    cmp  byte ptr [BUFFER + di], 0Ah
    je   PRE_LOOP_SEARCH
    cmp  byte ptr [BUFFER + di], "$"
    je   END_PROC
    jmp  NEXT_RULE

WRITE_END:

    push di
    push si

    mov si, offset RES_BUFFER ; Load the address of the string
    xor cx, cx               ; Clear CX to use as a counter

    count_loop:
        cmp byte ptr [si], 0 ; Check if the current byte is the null terminator
        je done              ; If null is found, exit the loop
        inc si               ; Move to the next character
        inc cx               ; Increment the counter
        jmp count_loop

    done:

    pop si
    mov di, cx
    sub di, si

    mov ah, 42h
    mov al, 0          ; Relative to start of file
    mov bx, FILE_HANDLE
    xor cx, cx         ; CX = high word of offset (not needed for small files)
    mov dx, di         ; DX = low word of offset
    int 21h            ; Set file pointer to byte 1024

    mov ah, 3Fh
    mov bx, FILE_HANDLE
    mov cx, 4000h      ; Read up to 16KB
    lea dx, RES_BUFFER
    int 21h            ; Read data into BUFFER

    mov ah, 3Ch        ; DOS Create File function
    mov cx, 0          ; Normal file attributes
    mov dx, offset FILE_OUTPUT
    int 21h    ; If file exists, open it instead

    mov FILE_HANDLE, ax 
    ; Move file pointer to the beginning before writing (overwrite mode)
    mov ah, 42h  
    mov al, 0    ; Move to start (overwrite instead of appending)
    mov bx, FILE_HANDLE
    xor cx, cx
    xor dx, dx
    int 21h

    ; Correct CX value before writing
    mov cx, INPUT_LENGTH   ; Set correct length to write
    ; sub cx, LR_SIZE
    ; sub cx, si

    ; Write data to file
    mov  ah, 40h
    mov  bx, FILE_HANDLE
    lea  dx, RES_BUFFER
    int  21h


    jmp SEARCH_BEGINING

END_PROC:

rewrite ENDP



    ; mov di, RULES_BEGGINING          ; PRINT SET OF RULS
    ; mov   ah, 09h
    ; lea   dx, BUFFER + di
    ; int   21h

                 mov  ah, 09h
                 lea  dx, RES_BUFFER
                 int  21h

                 mov  ah, 3Eh
                 mov  bx, FILE_HANDLE
                 int  21h

                 jmp  EXIT

    EXIT:        
                 mov  ax, 4C00h
                 int  21h

end start

