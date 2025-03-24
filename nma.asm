.model tiny    

.code
                FILE_NAME   db  'input.nma', 0     ; Input file name
                FILE_HANDLE dw  ?                 ; File handle
                BUFFER      db  100h dup (?)      ; Buffer for reading file data
                BYTES_READ  dw  ?                 ; Number of bytes read
                FILE_OUTPUT db  'result.txt', 0   ; Output file name

start:          
                mov ax, @data
                mov ds, ax
                xor ax, ax

                ; Open input file (Read-Only)
                mov dx, offset FILE_NAME
                mov ah, 3Dh
                xor al, al        ; AL = 0 (Read-Only mode)
                int 21h
                jc ERROR_OPEN
                mov FILE_HANDLE, ax 

READ_LOOP:
                ; Read file (AH = 3Fh)
                mov ah, 3Fh
                mov bx, FILE_HANDLE
                mov cx, 100h       ; Read up to 256 bytes
                lea dx, BUFFER
                int 21h
                jc ERROR_READ

                ; If AX == 0, EOF reached
                test ax, ax
                jz CLOSE_INPUT

                ; Store number of bytes read
                mov BYTES_READ, ax

                ; Add '$' terminator for printing
                mov di, ax
                mov byte ptr [BUFFER + di], '$'

                ; Open output file (Write-Only, Create New)
                lea dx, FILE_OUTPUT
                mov ah, 3Ch         ; DOS function to create file
                xor cx, cx         
                int 21h
                jc ERROR_OPEN
                mov FILE_HANDLE, ax  

                ; Move file pointer to the end (Append Mode)
                mov ah, 42h         ; Move file pointer
                mov al, 2           ; AL = 2 (Move to end)
                mov bx, FILE_HANDLE
                xor cx, cx          
                xor dx, dx
                int 21h

                ; Write buffer to output file
                mov ah, 40h
                mov bx, FILE_HANDLE
                lea dx, BUFFER
                mov cx, BYTES_READ
                int 21h
                jc ERROR_WRITE

                ; Close output file
                mov ah, 3Eh
                mov bx, FILE_HANDLE
                int 21h

                jmp READ_LOOP       ; Read next chunk up to failiure (TOCHECK)

CLOSE_INPUT:
                ; Close input file
                mov ah, 3Eh
                mov bx, FILE_HANDLE
                int 21h

                ; Print the buffer to StdOut
                mov ah, 09h
                lea dx, BUFFER
                int 21h

                ; Exit
                mov ax, 4C00h
                int 21h

ERROR_OPEN:
                mov ah, 09h
                mov dx, OFFSET MSG_OPEN_ERROR
                int 21h
                jmp EXIT

ERROR_READ:
                mov ah, 09h
                mov dx, OFFSET MSG_READ_ERROR
                int 21h
                jmp EXIT

ERROR_WRITE:
                mov ah, 09h
                mov dx, OFFSET MSG_WRITE_ERROR
                int 21h
                jmp EXIT

EXIT:
                mov ax, 4C01h
                int 21h

MSG_OPEN_ERROR  db 'Error opening file!$'
MSG_READ_ERROR  db 'Error reading file!$'
MSG_WRITE_ERROR db 'Error writing file!$'

end start
