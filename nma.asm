.model tiny    
.386              ; Ensure 16-bit compatibility (Optional for TASM)

.data  
    FILE_NAME   db  'input.nma', 0     ; Input file name
    FILE_HANDLE dw  ?                  ; File handle
    BUFFER      db  16384 dup (?)       ; Reduce buffer size to fit tiny model
    BYTES_READ  dw  ?                   ; Number of bytes read
    FILE_OUTPUT db  'result.txt', 0     ; Output file name
    INPUT_LENGTH dw ?
    RULES_BEGGINING dw ?
    RULES       dw  16384 dup (?)       ; Reduce rule storage to fit memory
    LHS         db  256 dup (?)         ; Temporary storage for LHS
    RHS         db  256 dup (?)         ; Temporary storage for RHS

.code
org 100h        ; COM file must start at offset 100h  

start:          
    mov ax, @data
    mov ds, ax
    xor ax, ax

    ; Open input file (Read-Only)
    mov dx, offset FILE_NAME
    mov ah, 3Dh
    xor al, al        ; AL = 0 (Read-Only mode)
    int 21h
    mov FILE_HANDLE, ax 

    ; Read file (AH = 3Fh)
    mov ah, 3Fh
    mov bx, FILE_HANDLE
    mov cx, 4000h       ; Read up to 16KB
    lea dx, BUFFER
    int 21h

    test ax, ax
;    jz CLOSE_INPUT

    ; Store number of bytes read
    mov BYTES_READ, ax

    ; Add '$' terminator for printing
    mov di, ax
    mov byte ptr [BUFFER + di], '$'

    mov al, byte ptr [BUFFER + 4] ;FIND the length of input argument
    xor ah,ah
    mov INPUT_LENGTH, ax ; store the argument

    ; Open output file (Write-Only, Create New)
    lea dx, FILE_OUTPUT
    mov ah, 3Ch         ; DOS function to create file
    xor cx, cx          ; Normal file attributes
    int 21h
    mov FILE_HANDLE, ax

    ; Move file pointer to the end (Append Mode)
    mov ah, 42h         ; Move file pointer
    mov al, 2           ; AL = 2 (Move to end)
    mov bx, FILE_HANDLE
    xor cx, cx          ; CX:DX = 0 (Move 0 bytes)
    xor dx, dx
    int 21h

    ; Write buffer to output file
    mov ah, 40h
    mov bx, FILE_HANDLE
    lea dx, BUFFER + 8
    mov cx, INPUT_LENGTH
    int 21h

    mov si, INPUT_LENGTH
    add si, 8

    mov byte ptr [BUFFER + si], '$'

    ;jmp CLOSE_INPUT
    
    mov byte ptr [BUFFER + si], 0


    inc si
    add si, 3
    mov RULES_BEGGINING, si


    mov dx, offset FILE_NAME
    mov ah, 3Dh
    xor al, al        ; AL = 0 (Read-Only mode)
    int 21h
    mov FILE_HANDLE, ax 

    ; Read file (AH = 3Fh)
    mov ah, 3Fh
    mov bx, FILE_HANDLE
    mov cx, 4000h       ; Read up to 16KB
    lea dx, BUFFER
    int 21h

    test ax, ax
;    jz CLOSE_INPUT

    ; Store number of bytes read
    mov BYTES_READ, ax

    ; Add '$' terminator for printing
    mov di, ax
    mov byte ptr [BUFFER + di], '$'
    ; Print from the correct position
    mov si, RULES_BEGGINING
 ;  mov cx, BYTES_READ

 xor di,di
 
    lea di, BUFFER + si 
    mov RULES, di
    mov ah, 09h
    lea dx, RULES
    int 21h                    

    jmp EXIT

CLOSE_INPUT:
    mov ah, 3Eh
    mov bx, FILE_HANDLE
    int 21h

    ; Print the buffer to StdOut
    mov ah, 09h
    lea dx, BUFFER + 8
    int 21h

    ; Exit
    mov ax, 4C00h
    int 21h
    ret

EXIT:
    mov ax, 4C00h
    int 21h

end start
