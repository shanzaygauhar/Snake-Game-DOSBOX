[org 0x0100]
jmp start

;08FE
;14 = points
oldisr: dd 0
size1: dw 20
size_level2: dw 40
size_level3: dw 40
pos: times 240 dw 0
direc: dw 1			;1=right  2=left   3=up	  4=down
ticks: dw 0
speed: dw 6
sec: dw 0
secflag: dw 0
collision: dw 0					;collison flag
points: dw 0

fruits: dw 0x0999, 0x0205, 0x0406, 0x0EF5, 0x0CA2, 0x0AEC, 0x030C, 0x0EEA
findex: dw 0
fdi: dw 0
fruitflag: dw 0
negt: dw 0
level2_call: dw 0
level3_call: dw 0
level1_display: db "LEVEL 1"
level2_display: db "LEVEL 2"
level3_display: db "LEVEL 3"
fatalfruits: dw 2090, 3010, 988, 2390, 1074, 3500, 3096, 1028, 3340, 2598 		;positions
fataldi: dw 0

fatalarr: dw 0x870F, 0x832A, 0x8777, 0x83F7					;chars
farrindex: dw 0
fatalflag: dw 0

;;;;;;;;;;;;;;;;;;;;;;Timer lives variable;;;;;;;;;;;;;;;;;;;;;;;;;;
min: dw	3
seconds: dw 60
life: dw 3
tickcount: dw 0
message1: db "GAME OVER!! BETTER LUCK NEXT TIME!"
message2: db "CONGRATULATIONS AND Ma Sha Allah! You Won!"
FRUIT_TICK: dw 322

;;;;;;;;;;;;;;;;;;;;;;Sound Check;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sound:
	push cx
	push dx
	push bx
	push ax
	
	mov cl,bl
	mov dx,2000          ; Number of iterations
	mov bx,1             ; Frequency

	mov al, cl    		 ; Magic Number (use this binary number only)
	out 43H, al        	 ; Send it to the initializing port 43H Timer 2.

next_frequency:         	 ; This is were we will jump back to 2000 times.

	mov ax, bx           ; Move our Frequency value into AX.
	out 42H, al          ; Send LSB to port 42H.
	mov  al, ah           ; Move MSB into AL  
	out 42H, al          ; Send MSB to port 42H.
	in  al, 61H          ; Get current value of port 61H.
	or  al, 00000011B    ; OR AL to this value, forcing first two bits high.
	out 61H, al          ; Copy it to port 61H of the PPI Chip
                         ; to turn ON the speaker.
	mov cx, 150          ; Repeat loop 150 times
delay_loop:              ; Here is where we loop back too.
loop    delay_loop       ; Jump repeatedly to DELAY_LOOP until CX = 0

	add bx,1              ; Incrementing the value of BX lowers 
                         ; the frequency each time we repeat the
                         ; whole routine
	sub dx,1               ; Decrement repeat routine count
	cmp dx, 0            ; Is DX (repeat count) = to 0
	jnz next_frequency   ; If not jump to NEXT_FREQUENCY
                         ; and do whole routine again.
                         ; Else DX = 0 time to turn speaker OFF

	in al,61H           ; Get current value of port 61H.
	and al,11111100B     ; AND AL to this value, forcing first two bits low.
	out 61H,al          ; Copy it to port 61H of the PPI Chip
                         ; to
	pop ax
	pop bx
	pop dx
	pop cx
	ret
	
sound2:
	push ax
	push bx
	push cx
	
       mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax, 7670        ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 25          ; Pause for duration of note.
pause1:
		mov cx, 1000
pause2:
		dec cx
		jnz pause2
		
        dec     bx
        jnz     pause1
		
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.
		pop cx
		pop bx
		pop ax
		ret

Soundf:
pusha
        mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax,1297       ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 25          ; Pause for duration of note.
pause11:
		mov cx, 1000
pause22:
		dec cx
		jnz pause22
		
        dec     bx
        jnz     pause11
		
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.
popa
		ret


;;;;;;;;;;;;;;;;;;;;;;Fruits and its Checks;;;;;;;;;;;;;;;;;;;;;;;;;;
MaxRand: dw 3838
MinRand: dw 322

Rng:

push ax
push bx
push dx
push di
push es

push cs
pop ds

mov ax, 0xb800
mov es, ax

generate_again:
mov ah, 2ch  			; Set function code
int 21h      			; get time from MS-DOS
						; DH=seconds, DL=hundredths of second
mov al, 00			;current sec
out 0x70, al 

D3: 
in al, 0x71

add ax, dx
add ax, 160
add ax, [cs:findex]
add ax, [cs:size1]
add ax, [tickcount]
add ax, [ticks]

mov dx, 0
mov bx,[FRUIT_TICK]
div bx
shl dx, 1

	cmp dx, [MaxRand]
	jg update
	
	cmp dx,[MinRand]
	jnl check_2_
	
	mov ax,320
	add dx,ax
	jmp check_2_
	
update:
	mov dx, 3838
	mov ax,160
	sub dx,ax

check_2_:	
mov di, dx
mov ax, [es:di]

	cmp ax, 0x062A
	jne check_3_
	
	;jmp generate_again
	add dx,18
	jmp check_2_

check_3_:
	mov di, dx
	mov ax, [es:di]
	mov bh, 01100110b
	mov bl, 0x2A
	cmp ax, bx
	jne check_4_
	
	;jmp generate_again
	add dx,18
	jmp check_3_

check_4_:
	mov di, dx
	mov ax, [es:di]
	
	cmp ax, 0x08FE
	jne left
	
	;jmp generate_again
	add dx,18
	jmp check_4_
left:
	mov word[cs:fdi], dx

pop es
pop di
pop dx
pop bx
pop ax
ret

PrintFruit:

	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	;push ds
	
	push cs
	pop ds
	call FruitCheck
	
	mov ax, 0xb800
	mov es, ax
	mov di, 16
	mov byte[es:di], 'P'
	add di, 2
	mov byte[es:di], 'o'
	add di, 2
	mov byte[es:di], 'i'
	add di, 2
	mov byte[es:di], 'n'
	add di, 2
	mov byte[es:di], 't'
	add di, 2
	mov byte[es:di], 's'
	add di, 2
	mov byte[es:di], ':'
	add di, 2
	mov si, [cs:points]
	call printnum

	cmp word[cs:fruitflag], 0
        jne present
        mov word[cs:fruitflag], 1
        call Rng
	add word[cs:findex], 2
	cmp word[cs:findex], 16
	jne present
	mov word[cs:findex], 0
	

present:
	mov di, [cs:fdi]
	mov ax, 0xb800
	mov es, ax
	mov ax, [es:di]
	cmp word[es:di], 0x08FE
	jne cool
	call Rng
	jmp present
cool:   mov bx, fruits
        add bx, [cs:findex]
        mov dx, [bx]
	mov [es:di], dx

	;pop ds	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret

FruitCheck:

	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	push ds
	
	push cs
	pop ds

	mov bx, pos
	mov cx, [cs:size1]
	sub cx, 1
	shl cx, 1
	add bx, cx
	
	mov dx, [cs:fdi]
	cmp dx, [bx]
	jne noeat2

	call Soundf
	
	add word[cs:points], 4
	mov bx, pos
	mov ax, [bx+2]							;second last
	
	sub ax, [bx]
	cmp ax, 0
	jl negative								
	
        mov word[cs:negt], 0
	jmp ahead

negative:
	mov word[cs:negt], 1
	mov bx, pos
	mov ax, [bx]
	sub ax, [bx+2]

ahead:	mov bx, pos
	mov cx, [cs:size1]
	sub cx, 1
	shl cx, 1
	add bx, cx
	jmp go_

noeat2: jmp noeat
	
go_:	mov word[cs:fruitflag], 0			;to generate new fruit
	mov bx, pos
	mov cx, [cs:size1]
	sub cx, 1
	shl cx, 1
	add bx, cx					;bx points to head
	
	;mov dx, [bx]
	;mov [bx+8], dx
	
mov cx, [cs:size1]

shiftpos: mov dx, [bx]
	  mov word[bx+8], dx
	  sub bx, 2
	  loop shiftpos

mov bx, pos
add bx, 6
mov cx,4

newpos: mov dx, [bx+2]
	cmp word[cs:negt], 1
	je isnegative
	cmp ax, 2
	jne down_
	sub dx, 2
	jmp done
down_:  sub dx, 160
	jmp done

isnegative:
	cmp ax, 2
	jne up_
	add dx, 2
	jmp done
up_:    add dx, 160

done:	mov [bx], dx
	sub bx, 2
	loop newpos

	add word[cs:size1], 4


noeat:
	
	
	pop ds	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax

	ret

FatalCheck:
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	push ds
	
	push cs
	pop ds


	mov bx, pos
	mov cx, [cs:size1]
	sub cx, 1
	shl cx, 1
	add bx, cx
	
	mov ax, [bx]
	mov bx, fatalfruits
	add bx, [cs:fataldi]
	cmp ax, [bx]
	jne noissue2
;;;;
	mov word[cs:direc], 1
	sub word[life],1
	mov word[min], 3
	mov word[seconds],60
	mov bl, 01111111b
	call Sound
	
	cmp word[level3_call],1
	jne check_thiis
	mov word[level3_call],0
	jmp golevel1m
	
check_thiis:	
	cmp word[level2_call],1
	jne golevel1m
	mov word[level2_call],0
	jmp golevel1m

noissue2: call Fatal
	  jmp eexitt	

golevel1m:
	call clrscr
	call Screen_Display
	mov word[cs:collision], 1
	mov bx, pos
	mov cx, [cs:size1]
	mov ax, 1940


restore_positionn: 
  
	mov [bx], ax
        add ax, 2
        add bx, 2
        loop restore_positionn

	cmp word[cs:life],0
	jne eexitt
	
	cld ; auto increment mode
		mov ah, 0x0e
		mov di, 2100
		mov si,message1
		mov cx, 34
		
nextcharr: 
		lodsb ; load next char in al
		stosw ; print char/attribute pair
	loop nextcharr ; repeat for the whole string

eexitt:

	pop ds	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax

	ret

Fatal:
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	push ds
	
	push cs
	pop ds

	mov ax, 0xb800
	mov es, ax

	
	cmp word[cs:fatalflag], 0
	je nochange
	add word[cs:fataldi], 2
	cmp word[cs:fataldi], 20
	jne hh
	mov word[cs:fataldi], 0
hh:	add word[cs:farrindex], 2
	mov word[cs:fatalflag], 0
	cmp word[cs:farrindex], 8
	jne jjj
	mov word[cs:farrindex], 0
jjj:
nochange:
	mov bx, fatalfruits
	add bx, [cs:fataldi]
	mov di, [bx]
	
check99:
	mov ax, word[es:di]
	cmp ax, 0x08FE				;snake body
	jne check88
	jmp next99	

check88:mov ax, word[es:di]
	cmp ax, 0x062A				;borders
	jne check77
	jmp next99

check77:cmp di, [cs:fdi]			;fruits
	jne check66
	jmp next99

check66:mov ax, word[es:di]
	cmp ax, 0x662A
	jne noprob
	jmp next99

next99:	add word[cs:fataldi], 2
	cmp word[cs:fataldi], 20
	jne yay
	mov word[cs:fataldi], 0
	mov bx, fatalfruits
	add bx, [cs:fataldi]
	mov di, [bx]
yay:	jmp check99
	
noprob: mov bx, fatalarr
	add bx, [cs:farrindex]
	mov ax, [bx]
	mov [es:di], ax

	pop ds	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax

	ret

;;;;;;;;;;;;;;;;;;;;;;LIFE CHECK AND TIME DISPLAY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;Boundary Check;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CollisionCheck:
	push ax
	push es
	push bx
	push cx
	push si
	push di
	
	mov ax, 0xb800
	mov es,ax
	
	cmp word[es:si], 0x062A			;asterik ascii
	je penalty
	
	cmp word[es:si], 0x08FE		
	je penalty
	
	mov ah, 01100110b
	mov al, 0x2A
	
	cmp word[es:si],ax
	jne exit_

	
penalty:
	mov word[cs:direc], 1
	sub word[life],1
	mov word[min], 3
	mov word[seconds],60
	mov bl, 01111111b
	call Sound
	
	cmp word[level3_call],1
	jne check_this
	mov word[level3_call],0
	jmp golevel1
	
check_this:	
	cmp word[level2_call],1
	jne golevel1
	mov word[level2_call],0
	jmp golevel1

exit_: jmp eexit	

golevel1:
	call clrscr
	call Screen_Display
	mov word[cs:collision], 1
	mov bx, pos
	mov cx, [cs:size1]
	mov ax, 1940


restore_position: 
  
	mov [bx], ax
        add ax, 2
        add bx, 2
        loop restore_position

	cmp word[cs:life],0
	jne eexit
	
	cld ; auto increment mode
		mov ah, 0x0e
		mov di, 2100
		mov si,message1
		mov cx, 34
		
nextchar: 
		lodsb ; load next char in al
		stosw ; print char/attribute pair
	loop nextchar ; repeat for the whole string
		
		

	
	
eexit:
	pop di
	pop si
	pop cx
	pop bx
	pop es
	pop ax
	ret
printnum: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx

	mov ax, 0xb800
	mov es, ax 
	mov ax, si
	mov bx, 10
	mov cx, 0 

nextdigit: 
	mov dx, 0 
	div bx 
	add dl, 0x30 
	push dx
	inc cx 
	cmp ax, 0 
	jnz nextdigit

nextpos: 
	pop dx 
	mov dh, 0x07 
	mov [es:di], dx 
	add di, 2 
	loop nextpos 

pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret

timer:
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	push ds
	
        push cs
        pop ds

		
		cmp word[life],0
		jle  terminate
		
		cmp word[size1],240
		je winner_
		
		call Screen_Display
		mov ax, 0xb800
		mov es,ax
		
		inc word[tickcount]
		
		cmp word[tickcount],18
		jl minutes

		mov word[tickcount],0
		sub word[seconds],1
		jmp checksec
		
terminate:
		cld ; auto increment mode
		mov ah, 0x07
		mov di, 1340
		mov si,message1
		mov cx, 33
		
NextChar: 
		lodsb ; load next char in al
		stosw ; print char/attribute pair
	loop NextChar ; repeat for the whole string
			jmp end2
	
winner_: jmp winner	
checksec:
		cmp word[seconds],0
		jne minutes
		
		mov word[seconds],60
		
		cmp word[min],0
		jg subt

		cmp word[size1],240
		jmp winner
		
		mov word[min], 3
		sub word[life],1
		
		mov bl,01110111b
		call Sound
		
		cmp word[life],0
		jle  terminate
		jmp minutes
                
subt:		sub word[min], 1

minutes:	
		mov al, [min]
		mov ah,0
		
		mov bx, 16
		
		mov di, 144
		mov si, ax
                
		call printnum

		add di, 2
		mov word[es:di], 0x073A
		add di, 4
		
Print:	        cmp word[seconds], 9
                jnle j
		mov word[es:di], 0x0730
                add di, 2

j:	        mov al, [seconds]
		mov ah,0
		mov si, ax
		
		call printnum
		jmp end2
winner:
	cld ; auto increment mode
		mov ah, 0x07
		mov di, 1340
		mov si,message2
		mov cx, 42
		
NextCharacter: 
		lodsb ; load next char in al
		stosw ; print char/attribute pair
	loop NextCharacter ; repeat for the whole string
			jmp end2
end2:
	pop ds	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;SCREEN DISPLAY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SCREEEEEENNN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
clrscr:
	push es
	push ax
	push cx
	push di
	
	mov word[cs:fatalflag], 1
	mov ax, 0xb800
	mov es, ax 			; point es to video base
	xor di, di 			; point di to top left column
	
	mov ax, 0x0720 			; space char in normal attribute
	mov cx, 2000 			; number of screen locations

	cld 				; auto increment mode
	rep stosw 			; clear the whole screen
	
	;call timer
	
	pop di
	pop cx
	pop ax
	pop es
	ret

Screen_Display:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di

	mov ax, 0xb800
	mov es,ax

	mov cx, 7
	mov di,74
	mov si, level1_display
	mov ah, 0x04
	
show:
	lodsb
	stosw
	loop show
	
	cmp word[cs:size1],40
	jl borders
	
	;jg LEVEL3
	cmp word[cs:level2_call],0
	jne LEVEL3
	
	mov word[cs:level2_call],1
	call sound2
	add word[cs:points],4
	call clrscr
	call level2
	jmp borders
LEVEL3:
	cmp word[cs:size1],52
	jl borders

	cmp word[cs:level3_call],0
	jne borders
	mov word[cs:level3_call],1
	;mov word[cs:level2_call],0
	call sound2
	add word[cs:points],4
	call clrscr
	call level3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BORDERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
borders:
	mov cx,80
	mov ax, 0x062A
	mov di, 160
	
	cld
	rep stosw	
	
	mov di, 3840
	mov cx,80
	mov ax, 0x062A
	
	cld
	rep stosw
	
	mov di, 160
	mov si, 318
	
BORDERS:
	mov word[es:di],0X062A
	mov word[es:si],0X062A
	add di,160
	add si,160
	cmp si,3998
	jle BORDERS
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HEART;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	mov cx, 6
	mov di,0
space:
	mov word[es:di], 0x0720
	add di,2
	loop space	
	
	
	mov cx, [life]
	mov di, 0
	cmp cx, 0
	jle end
	
HEART:
	mov word[es:di], 0x0403
	add di,2
	mov word[es:di], 0x0720
	add di,2
	loop HEART

		

end:
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret
	
level2:
	push si
	push di
	push es
	push ax
	push bx
	push cx
	push dx
	
	mov ax, 0xb800
	mov es, ax
	
	mov cx, 7
	mov di,74
	mov ah, 0x04
	mov si, level2_display

show2:
		lodsb
		stosw
		loop show2
		
	mov bx, pos
	mov cx, [cs:size1]
	mov ax, 1940

restore_position1: 
  
	mov [bx], ax
        add ax, 2
        add bx, 2
        loop restore_position1
	
continue:
		
	mov di, 560
	mov cx,7
	mov ah,01100110b
	mov al, 0x2A
	
obstacle1:
	mov[es:di], ax
	add di,160
	loop obstacle1

	mov di, 2600
	mov cx,7
obstacle5:
	mov[es:di], ax
	add di,160
	loop obstacle5
	
end__:
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop di
	pop si
	ret

level3:
	push si
	push di
	push es
	push ax
	push bx
	push cx
	push dx
	
	mov ax, 0xb800
	mov es, ax
	
	mov ax, 0xb800
	mov es, ax
	
		
	mov cx, 7
	mov di,74
	mov si,level3_display
	mov ah, 0x04
show3:
		lodsb
		stosw
		loop show3
		
	mov bx, pos
	mov cx, [cs:size1]
	mov ax, 1940

restore_position2: 
  
	mov [bx], ax
        add ax, 2
        add bx, 2
        loop restore_position2
		

	mov cx, 5
	mov di,650
	mov ah,01100110b
	mov al, 0x2A
l_3:
	mov [es:di], ax
	add di,162
	loop l_3
	
	mov di, 560
	mov cx,7
	mov ah,01100110b
	mov al, 0x2A
	
obstacle2:
	mov[es:di], ax
	add di,160
	loop obstacle2

	mov di, 2600
	mov cx,7
obstacle3:
	mov[es:di], ax
	add di,160
	loop obstacle3
	
	mov di, 3080
	mov cx,6
obstacle4:
	mov[es:di], ax
	add di,2
	loop obstacle4
	
	
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop di
	pop si
	ret

printt:

push ax
push bx
push cx
push dx
push es
push si
push di
push ds

push cs
pop ds

xor ax, ax
mov ax, 0xb800
mov es, ax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SNAKE PRINTING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print:
	   
;call FruitCheck
mov cx, [cs:size1]
sub cx, 1
mov bx, pos

mov di,[bx]
mov word[es:di],0x0720

move: mov dx, [bx+2]
      mov [bx], dx
      add bx, 2
      loop move
	  

mov bx, pos
mov cx, [cs:size1]
sub cx, 1
shl cx, 1
add bx, cx

cmp word[cs:direc], 1
jne checkleft

	mov si,[bx]
	add si, 2
	call CollisionCheck

	cmp word[cs:collision], 1
	je prnt
	add word[bx], 2
	
jmp prnt

checkleft: cmp word[cs:direc], 2
		jne checkup
		
		mov si,[bx]
		sub si, 2
		call CollisionCheck
	   cmp word[cs:collision], 1
	   je prnt
           sub word[bx], 2
           jmp prnt

checkup  : cmp word[cs:direc], 3
	   jne checkdown
	   
	   mov si,[bx]
	   sub si, 160
		
		call CollisionCheck
            cmp word[cs:collision], 1
	   je prnt
           sub word[bx], 160
           jmp prnt

checkdown: cmp word[cs:direc], 4
	   jne prnt
	   
	   mov si,[bx]
           add si, 160
	   call CollisionCheck
           cmp word[cs:collision], 1
	   je prnt
           add word[bx], 160
		   

prnt:

mov word[cs:collision], 0
mov bx, pos
mov cx, [cs:size1]
sub cx, 1


l1:	mov al, 0xFE
	mov ah, 0x08
   	mov di, [bx]
	mov [es:di], ax
	cmp di, 3998
	jg skip
	add bx, 2
	loop l1

mov al, 0x4F
mov ah, 0x04
mov di, [bx]
mov [es:di], ax


skip:	pop ds
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;KEY BOARD ROUTINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
kbisr:
        push ax
		push ds
		push es
		push si

        push cs
        pop ds
	
		in al, 0x60
	
		cmp al, 0x4D     ;right key
		jne next
        mov word[cs:direc], 1				;right direc
	
        jmp skip2
	

next:   cmp al, 0x4B	 ;left key
		jne next2
		mov word[cs:direc], 2				;left direc
	
        jmp skip2
	
	
next2:  cmp al, 0x48			;up key
		jne next3
		mov word[cs:direc], 3			;up direc
		
        jmp skip2
	
next3:  cmp al, 0x50	;down key
		jne skip2
        mov word[cs:direc], 4			;down direc

skip2: 	pop si
		pop es
        pop ds
        pop ax
		mov al,0x20
		out 0x20, al
		iret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;INT 8 TIMER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
mysnakeisr:

	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	push ds


    push cs
    pop ds

	add word[FRUIT_TICK],6
	cmp word[FRUIT_TICK], 4000
	jnge move_ahead_
	
	mov word[FRUIT_TICK],322
	
move_ahead_:
	cmp word[life],0
	jle lol
	
	call timer 
	add word[cs:secflag], 1
   	 cmp word[cs:secflag], 18
	 jl go
  	  mov word[cs:secflag], 0
  	  add word[cs:sec], 1

go:     
	add word[cs:ticks], 1
        cmp word[cs:sec], 20
        jl slow
        mov word[cs:sec], 0

spcheck:cmp word [cs:speed], 1
	jle slow
	sub word[cs:speed], 1


slow:   mov bx, [cs:speed]
        mov ax, [cs:ticks]
        div bl
        cmp ah, 0
        jne lol
        cmp word[cs:ticks], 18
        jle x
        mov word[cs:ticks], 0
x:      ;call clrscr
        call printt
	call PrintFruit
	;call Fatal
	call FatalCheck
	
	 
       
lol:    
 	mov al, 0x20			;End of signal
	out 0x20, al 
	pop ds	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax


	iret 
	

start:

mov cx, 20
mov ax, 1940
mov bx, pos

save:   mov [bx], ax
        add ax, 2
        add bx, 2
        loop save

call clrscr

xor ax,ax
mov es, ax

cli
mov word[es:8*4], mysnakeisr
mov[es:8*4+2],cs
	
mov word[es:9*4],kbisr
mov[es:9*4+2], cs
sti
	
mov dx,start
add dx,15
mov cl,4
shr dx,cl
mov ax, 0x3100
int 21h