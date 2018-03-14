;; Created by Amir Arbel and Ronen Finish
;; @file codebase.scheme
;; Contains start of code for .asm generation.
(define gen-all-code
  (lambda (library_functions constant_table_init fvar_table_init constant_table_assignment fvar_table_assignment generated_code)
    (string-append
      start-of-code
      constant_table_init
      fvar_table_init
      start-main
      constant_table_assignment
      fvar_table_assignment
      generated_code
      end-of-code
      library_functions
    )
  )
)


(define start-of-code (string-append
  ;=====Taken from https://www.cs.bgu.ac.il/~comp181/wiki.files/scheme.s by Mayer Goldberg
"%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_INTEGER 3
%define T_FRACTION 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10
%define T_VECTOR 11

%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32

%define TYPE_BITS 4
%define WORD_SIZE 64

%define MAKE_LITERAL(type, lit) ((lit << TYPE_BITS) | type)

%macro RET_TRUE 0
 mov rax, sobTrue
 leave
 ret
%endmacro%

%macro RET_FALSE 0
 mov rax, sobFalse
 leave
 ret
%endmacro%

%macro TYPE 1
	and %1, ((1 << TYPE_BITS) - 1)
%endmacro

%macro DATA 1
	sar %1, TYPE_BITS
%endmacro

%macro DATA_UPPER 1
	shr %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
%endmacro

%macro DATA_LOWER 1
	sal %1, ((WORD_SIZE - TYPE_BITS) >> 1)
	DATA_UPPER %1
%endmacro

%define MAKE_LITERAL_PAIR(car, cdr) (((((car - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (cdr - start_of_data)) << TYPE_BITS) | T_PAIR)

;;; MAKE_MALLOC_LITERAL_PAIR target-address, car-address, cdr-address
%macro MAKE_MALLOC_LITERAL_PAIR 3
  push rax
  push rbx
  push rcx
  mov rcx, %2
  sub rcx, start_of_data
  sal rcx, 30
  mov rbx, %3
  sub rbx, start_of_data
  or rcx, rbx
  sal rcx, 4
  or rcx, T_PAIR
  mov rax, %1
  mov qword[rax], rcx
  pop rcx
  pop rbx
  pop rax
%endmacro

%define MAKE_LITERAL_FRACTION(num, den) (((((num - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (den - start_of_data)) << TYPE_BITS) | T_FRACTION)

;;; MAKE_MALLOC_LITERAL_FRACTION target-address, numerator-address, denominator-address
%macro MAKE_MALLOC_LITERAL_FRACTION 3
  push rax
  push rbx
  push rcx
  mov rcx, %2
  sub rcx, start_of_data
  sal rcx, 30
  mov rbx, %3
  sub rbx, start_of_data
  or rcx, rbx
  sal rcx, 4
  or rcx, T_FRACTION
  mov rax, %1
  mov qword [rax], rcx
  pop rcx
  pop rbx
  pop rax
%endmacro

%macro DEBUG_TYPE 1
  push r11
  mov r11, %1
  TYPE r11
  sal r11, 4
  or r11, T_INTEGER
  push r11
  call write_sob_if_not_void
  add rsp, 8
  pop r11
 %endmacro

%macro CAR 1
	DATA_UPPER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro CDR 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

;;; MAKE_LITERAL_CLOSURE target, env, code
%macro MAKE_LITERAL_CLOSURE 3
	push rax
	push rbx
	mov rax, %1
	mov qword [rax], %2
  sub qword [rax], start_of_data
	sal qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)
  lea rbx,  [rax + 8]
  sub rbx,  start_of_data
	or  qword [rax], rbx
	sal qword [rax], TYPE_BITS
	or  qword [rax], T_CLOSURE
	mov qword [rax + 8], %3
	pop rbx
	pop rax
%endmacro

%macro CLOSURE_ENV 1
	DATA_UPPER %1
	add %1, start_of_data
%endmacro

%macro CLOSURE_CODE 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro MAKE_LITERAL_STRING 1+
	dq (((((%%LstrEnd - %%Lstr) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Lstr - start_of_data)) << TYPE_BITS) | T_STRING)
	%%Lstr:
	db %1
	%%LstrEnd:
%endmacro

%macro MAKE_LITERAL_STRING_REGS 2
	shl %2, 30
	mov rax, %2
	or rax, %1
	sub rax, start_of_data
	shl rax, TYPE_BITS
	or rax, T_STRING
%endmacro

%macro STRING_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro STRING_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; STRING_REF dest, src, index
%macro STRING_REF 3
	push rbx
	mov rbx, %2
	STRING_ELEMENTS rbx
	add rbx, %3
  ; return in rax
  mov rax, 0
  mov al, byte [rbx]
	pop rbx
%endmacro

%macro MAKE_LITERAL_VECTOR 1+
	dq ((((((%%VecEnd - %%Vec) >> 3) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Vec - start_of_data)) << TYPE_BITS) | T_VECTOR)
	%%Vec:
	dq %1
	%%VecEnd:
%endmacro

%macro MAKE_LITERAL_VECTOR_REGS 2
	sar %2, 3
	sal %2, 30
	mov rax, %2
	or rax, %1
	sub rax, start_of_data
	sal rax, TYPE_BITS
	or rax, T_VECTOR
%endmacro

%macro VECTOR_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro VECTOR_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; VECTOR_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro VECTOR_REF 3
	mov %1, %2
	VECTOR_ELEMENTS %1
	lea %1, [%1 + %3*8]
	mov %1, qword [%1]
	mov %1, qword [%1]
%endmacro

;;%1/%2 - %3/%4
%macro PLUS 4
  ;;%1*%4
	mov rax, %1
  mul %4
  mov %1, rax

  ;;%3*%2
  mov rax, %3
  mul %2
  mov %3, rax

  ;;%2*%4
  mov rax, %2
  mul %4
  mov %2, rax

  add %1, %3
  mov r10, %1
  mov r11, %2
%endmacro

;;%1/%2 - %3/%4
%macro MINUS 4
  ;;%1*%4
	mov rax, %1
  mul %4
  mov %1, rax

  ;;%3*%2
  mov rax, %3
  mul %2
  mov %3, rax

  ;;%2*%4
  mov rax, %2
  mul %4
  mov %2, rax

  sub %1, %3
  mov r10, %1
  mov r11, %2
%endmacro

; [%1 / %2] * [%3 / %4]
%macro MULTIPLY 4
  ;;%1*%3
	mov rax, %1
  mul %3
  mov r10, rax

  ;;%2*%4
  mov rax, %2
  mul %4
  mov r11, rax
%endmacro

; [%1 / %2]  / [%3 / %4] => r10 / r11
; equiv. [%1 / %2] * [%4 / %3] =>
; r10 = %1 * %4
; r11 = %2 * %3
%macro DIVIDE 4
  ;;%1*%4
	mov rax, %1
  mul %4
  mov r10, rax

  ;;%3*%2
  mov rax, %3
  mul %2
  mov r11, rax
%endmacro

%macro IABS 1
	cmp %1, 0
	jge %%cont
	neg %1
	%%cont:
%endmacro

; Can't use rax rbx rcx or rdx for parameters here
%macro REDUCER 2
  ; %1 - nominator, %2 - denominator
  ; Shall be known hencefort as x / y
  ; Backup x => rcx
  mov rcx, %1
  ; abs(x) => %1
  IABS(%1)
  ; After div operation, rdx holds remainder.
  mov rdx, 0
  ; rax - dividend
  mov rax, %1
  ; rbx - divisor
  mov rbx, %2
  ; while remainder != 0
  %%GCD_loop:
  ; divide rax by rbx
  div rbx
  ; check remainder
  cmp rdx, 0
  je %%GCD_loop_end
  ; divisor becomes the dividend
  mov rax ,rbx
  ; remainder becomes the divisor
  mov rbx, rdx
  ; reset remainder
  mov rdx, 0
  jmp %%GCD_loop
  %%GCD_loop_end:

  ; Reduce numerator.
  mov rax, rcx
  ; divide x by gcd(x, y) (reduced fraction of x / y)
  div rbx
  ; %1 - reduced numerator.
  mov %1, rax

  ; Reduce denominator.
  mov rax, %2
  div rbx
  ; %2 - reduced numerator.
  ; %2 - reduced denominator.
  mov %2, rax
%endmacro

;; ALLOCATOR(n)
;; Allocates n bytes.
;; Allocated memory start pointer returned in rax.
%macro ALLOCATE 1
  push rdi
  push rbx
  push rcx
  push rdx
  mov rdi, %1
  call malloc
  pop rdx
  pop rcx
  pop rbx
  pop rdi
%endmacro

%define SOB_UNDEFINED MAKE_LITERAL(T_UNDEFINED, 0)
%define SOB_VOID MAKE_LITERAL(T_VOID, 0)
%define SOB_NIL MAKE_LITERAL(T_NIL, 0)
%define SOB_TRUE MAKE_LITERAL(T_BOOL, 1)
%define SOB_FALSE MAKE_LITERAL(T_BOOL, 0)

%define param(offset) qword [rbp + offset]

struc scmframe
.old_rbp: resq 1
.ret_addr: resq 1
.env: resq 1
.arg_count: resq 1
.A0: resq 1
.A1: resq 1
.A2: resq 1
.A3: resq 1
.A4: resq 1
.A5: resq 1
endstruc

%define old_rbp param(scmframe.old_rbp)
%define ret_addr param(scmframe.ret_addr)
%define env param(scmframe.env)
%define arg_count param(scmframe.arg_count)
%define A0 param(scmframe.A0)
%define A1 param(scmframe.A1)
%define A2 param(scmframe.A2)
%define A3 param(scmframe.A3)
%define A4 param(scmframe.A4)
%define A5 param(scmframe.A5)
%define An(n) qword [rbp + 8 * (n + 4)]

section .bss\n

start_of_data:
  resq 1

  extern exit, printf, malloc, puts
  global main, write_sob, write_sob_if_not_void\n"
    ; === saved data (constant table) starts here
  )
)

(define start-main (string-append
  "end_of_locals:\n\n"

  "section .text\n"
  "main:\n"
    "nop\n"
    ; Create dummy stack.
    "push 0\n"
    "push 0\n"
    "push exitProgram\n"
    "push rbp\n"
    "mov rbp, rsp\n"
    "; Assign malloc pointer to start of data.\n"
    "ALLOCATE(1)\n"
    "mov qword[start_of_data], rax\n"
  )
)

(define end-of-code (string-append
  "exitProgram:\n"
  "pop rbx\n"
  "mov rax, 0\n"
  "call exit\n"
  "ret \n"
  "write_sob_undefined:\n"
  	"push rbp\n"
  	"mov rbp, rsp\n"

  	"mov rax, 0\n"
  	"mov rdi, .undefined\n"
  	"call printf\n"

  	"leave\n"
  	"ret\n"

  "section .data\n"
  ".undefined:\n"
  	"db \"#<undefined>\", 0\n"

  "write_sob_integer:\n"
  	"push rbp\n"
  	"mov rbp, rsp\n"

  	"mov rsi, qword [rbp + 8 + 1*8]\n"
  	"sar rsi, TYPE_BITS\n"
  	"mov rdi, .int_format_string\n"
  	"mov rax, 0\n"
  	"call printf\n"

  	"leave\n"
  	"ret\n"

  "section .data\n"
  ".int_format_string:\n"
  	"db \"%ld\", 0\n"
"int_format_string2:\n"
  "db \"%ld\", 0\n"

  "write_sob_char:\n"
  	"push rbp\n"
  	"mov rbp, rsp\n"

  	"mov rsi, qword [rbp + 8 + 1*8]\n"
  	"DATA rsi\n"

  	"cmp rsi, CHAR_NUL\n"
  	"je .Lnul\n"

  	"cmp rsi, CHAR_TAB\n"
  	"je .Ltab\n"

  	"cmp rsi, CHAR_NEWLINE\n"
  	"je .Lnewline\n"

  	"cmp rsi, CHAR_PAGE\n"
  	"je .Lpage\n"

  	"cmp rsi, CHAR_RETURN\n"
  	"je .Lreturn\n"

  	"cmp rsi, CHAR_SPACE\n"
  	"je .Lspace\n"
  	"jg .Lregular\n"

  	"mov rdi, .special\n"
  	"jmp .done\n"

  ".Lnul:\n"
  	"mov rdi, .nul\n"
  	"jmp .done\n"

  ".Ltab:\n"
  	"mov rdi, .tab\n"
  	"jmp .done\n"

  ".Lnewline:\n"
  	"mov rdi, .newline\n"
  	"jmp .done\n"

  ".Lpage:\n"
  	"mov rdi, .page\n"
  	"jmp .done\n"

  ".Lreturn:\n"
  	"mov rdi, .return\n"
  	"jmp .done\n"

  ".Lspace:\n"
  	"mov rdi, .space\n"
  	"jmp .done\n"

  ".Lregular:\n"
  	"mov rdi, .regular\n"
  	"jmp .done\n"

  ".done:\n"
  	"mov rax, 0\n"
  	"call printf\n"

  	"leave\n"
  	"ret\n"

  "section .data\n"
  ".space:\n"
  	"db \"#space\", 0\n"
  ".newline:\n"
  	"db \"#newline\", 0\n"
  ".return:\n"
  	"db \"#return\", 0\n"
  ".tab:\n"
  	"db \"#tab\", 0\n"
  ".page:\n"
  	"db \"#page\", 0\n"
  ".nul:\n"
  	"db \"#nul\", 0\n"
  ".special:\n"
  	"db \"#x%02x\", 0\n"
  ".regular:\n"
  	"db \"#\\%c\", 0\n"

  "write_sob_void:\n"
  	"push rbp\n"
  	"mov rbp, rsp\n"

  	"mov rax, 0\n"
  	"mov rdi, .void\n"
  	"call printf\n"

  	"leave\n"
  	"\n"

  "section .data\n"
  ".void:\n"
  	"db \"#<void>\", 0\n"

  "write_sob_bool:\n"
  	"push rbp\n"
  	"mov rbp, rsp\n"

  	"mov rax, qword [rbp + 8 + 1*8]\n"
  	"cmp rax, SOB_FALSE\n"
  	"je .sobFalse\n"

  	"mov rdi, .true\n"
  	"jmp .continue\n"

  ".sobFalse:\n"
  	"mov rdi, .false\n"

  ".continue:\n"
  	"mov rax, 0\n"
  	"call printf\n"

  	"leave\n"
  	"ret
    \n"
;; Created by Amir Arbel and Ronen Finish

  "section .data\n"
  ".false:\n"
  	"db \"#f\", 0\n"
  ".true:\n"
  	"db \"#t\", 0\n"

  "write_sob_nil:\n"
  	"push rbp\n"
  	"mov rbp, rsp\n"

  	"mov rax, 0\n";; Created by Amir Arbel and Ronen Finish

  	"mov rdi, .nil\n"
  	"call printf\n"

  	"leave\n"
  	"ret
    \n"

  "section .data\n"
  ".nil:\n"
  	"db \"()\", 0\n"

  "write_sob_string:\n"
  	"push rbp\n"
  	"mov rbp, rsp\n"

  	"mov rax, 0\n"
  	"mov rdi, .double_quote\n"
  	"call printf\n"

  	"mov rax, qword [rbp + 8 + 1*8]\n"
  	"mov rcx, rax\n"
  	"STRING_LENGTH rcx\n"
  	"STRING_ELEMENTS rax
    \n"

  ".loop:
  	cmp rcx, 0
  	je .done
  	mov bl, byte [rax]
  	and rbx, 0xff

  	cmp rbx, CHAR_TAB
  	je .ch_tab
  	cmp rbx, CHAR_NEWLINE
  	je .ch_newline
  	cmp rbx, CHAR_PAGE
  	je .ch_page
  	cmp rbx, CHAR_RETURN
  	je .ch_return
  	cmp rbx, CHAR_SPACE
  	jl .ch_hex

  	mov rdi, .fs_simple_char
  	mov rsi, rbx
  	jmp .printf
    \n"

  ".ch_hex:
  	mov rdi, .fs_hex_char
  	mov rsi, rbx
  	jmp .printf

  .ch_tab:
  	mov rdi, .fs_tab
  	mov rsi, rbx
  	jmp .printf

  .ch_page:
  	mov rdi, .fs_page
  	mov rsi, rbx
  	jmp .printf

  .ch_return:
  	mov rdi, .fs_return
  	mov rsi, rbx
  	jmp .printf

  .ch_newline:
  	mov rdi, .fs_newline
  	mov rsi, rbx

  .printf:
  	push rax
  	push rcx
  	mov rax, 0
  	call printf
  	pop rcx
  	pop rax

  	dec rcx
  	inc rax
  	jmp .loop

  .done:
  	mov rax, 0
  	mov rdi, .double_quote
  	call printf

  	leave
  	ret
    \n"

  "section .data
  .double_quote:
    db '\"', 0
  .fs_simple_char:
  	db \"%c\", 0
  .fs_hex_char:
  	db \"\\x%01x;\", 0
  .fs_tab:
  	db \"\\t\", 0
  .fs_page:
  	db \"\\f\", 0
  .fs_return:
  	db \"\\r\", 0
  .fs_newline:
  	db \"\\n\", 0

  write_sob_pair:
  	push rbp
  	mov rbp, rsp

  	mov rax, 0
  	mov rdi, .open_paren
  	call printf
  	mov rax, qword [rbp + 8 + 1*8]
  	CAR rax
  	push rax
  	call write_sob
  	add rsp, 1*8
  	mov rax, qword [rbp + 8 + 1*8]
  	CDR rax
  	push rax
  	call write_sob_pair_on_cdr
  	add rsp, 1*8
  	mov rdi, .close_paren
  	mov rax, 0
  	call printf

  	leave
  	ret
    \n"

  "section .data
  .open_paren:
  	db \"(\", 0
  .close_paren:
  	db \")\", 0\n

  write_sob_pair_on_cdr:
  	push rbp
  	mov rbp, rsp

  	mov rbx, qword [rbp + 8 + 1*8]
  	mov rax, rbx
  	TYPE rbx
  	cmp rbx, T_NIL
  	je .done
  	cmp rbx, T_PAIR
  	je .cdrIsPair
  	push rax
  	mov rax, 0
  	mov rdi, .dot
  	call printf
  	call write_sob
  	add rsp, 1*8
  	jmp .done
    \n"

  ".cdrIsPair:
  	mov rbx, rax
  	CDR rbx
  	push rbx
  	CAR rax
  	push rax
  	mov rax, 0
  	mov rdi, .space
  	call printf
  	call write_sob
  	add rsp, 1*8
  	call write_sob_pair_on_cdr
  	add rsp, 1*8
    \n"

  ".done:
  	leave
  	ret

  section .data
  .space:
  	db \" \", 0
  .dot:
  	db \" . \", 0\n

  write_sob_vector:
  	push rbp
  	mov rbp, rsp

  	mov rax, 0
  	mov rdi, .fs_open_vector
  	call printf

  	mov rax, qword [rbp + 8 + 1*8]
  	mov rcx, rax
  	VECTOR_LENGTH rcx
  	cmp rcx, 0
  	je .done
  	VECTOR_ELEMENTS rax

  	push rcx
  	push rax
  	mov rax, qword [rax]
  	push qword [rax]
  	call write_sob
  	add rsp, 1*8
  	pop rax
  	pop rcx
  	dec rcx
  	add rax, 8

  .loop:
  	cmp rcx, 0
  	je .done

  	push rcx
  	push rax
  	mov rax, 0
  	mov rdi, .fs_space
  	call printf

  	pop rax
  	push rax
  	mov rax, qword [rax]
  	push qword [rax]
  	call write_sob
  	add rsp, 1*8
  	pop rax
  	pop rcx
  	dec rcx
  	add rax, 8
  	jmp .loop
    \n"

  ".done:
  	mov rax, 0
  	mov rdi, .fs_close_vector
  	call printf

  	leave
  	ret
    \n"

  "section	.data
  .fs_open_vector:
  	db \"#(\", 0
  .fs_close_vector:
  	db \")\", 0
  .fs_space:
  	db \" \", 0
    \n"

  "write_sob_symbol:
	push rbp
	mov rbp, rsp

	mov rax, qword [rbp + 8 + 1*8]
  ; Get string address
	DATA rax
  add rax, start_of_data
	mov rax, qword [rax]

	mov rcx, rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	mov rdi, .simple_char
	mov rsi, rbx

	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop
.done:
	leave
	ret

section .data
	.simple_char:
		db \"%c\", 0
    \n"

    "write_sob_fraction:
    	push rbp
    	mov rbp, rsp

    	mov rax, qword [rbp + 8 + 1*8]
    	CAR rax
    	push rax
    	call write_sob
    	add rsp, 1*8

    	mov rax, 0
    	mov rdi, .slash
    	call printf

    	mov rax, qword [rbp + 8 + 1*8]
    	CDR rax
    	push rax
    	call write_sob
    	add rsp, 1*8

    	leave
    	ret

    section	.data
    .slash:
    	db \"/\", 0\n"
  "write_sob_closure:
  	push rbp
  	mov rbp, rsp

  	mov rsi, qword [rbp + 8 + 1*8]
  	mov rdx, rsi
  	CLOSURE_ENV rsi
  	CLOSURE_CODE rdx
  	mov rdi, .closure
  	mov rax, 0
  	call printf

  	leave
  	ret

  section .data
  .closure:
  	db \"#<closure [env:%p, code:%p]>\", 0
    \n"

  "write_sob:
  	mov rax, qword [rsp + 1*8]
  	TYPE rax
  	jmp qword [.jmp_table + rax * 8]
    \n"

  "section .data
  .jmp_table:
  	dq write_sob_undefined, write_sob_void, write_sob_nil
  	dq write_sob_integer, write_sob_fraction, write_sob_bool
  	dq write_sob_char, write_sob_string, write_sob_symbol
  	dq write_sob_closure, write_sob_pair, write_sob_vector
    \n"

  "section .text
  write_sob_if_not_void:
  	mov rax, qword [rsp + 1*8]
  	cmp rax, SOB_VOID
  	je .continue

  	push rax
  	call write_sob
  	add rsp, 1*8
  	mov rax, 0
  	mov rdi, .newline
  	call printf
    \n"

  ".continue:
  	ret
  section .data
  .newline:
  	db CHAR_NEWLINE, 0
    \n"

"
    error:
	   db 'error: not a closure'
  	 db 0 ;<- zero byte marks end of string
"
  "L_die_not_closure2:\n"
  "mov rdi, error \n"
  "call puts\n"
  "L_die_not_closure:\n"
  "mov rdi, error \n"
  "call puts\n"
  "\tmov rax, 0x3c\n"
  "\tsyscall\n"

  )
)
