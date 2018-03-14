%define T_UNDEFINED 0%define T_VOID 1%define T_NIL 2%define T_INTEGER 3%define T_FRACTION 4%define T_BOOL 5%define T_CHAR 6%define T_STRING 7%define T_SYMBOL 8%define T_CLOSURE 9%define T_PAIR 10%define T_VECTOR 11%define CHAR_NUL 0%define CHAR_TAB 9%define CHAR_NEWLINE 10%define CHAR_PAGE 12%define CHAR_RETURN 13%define CHAR_SPACE 32%define TYPE_BITS 4%define WORD_SIZE 64%define MAKE_LITERAL(type, lit) ((lit << TYPE_BITS) | type)%macro TYPE 1and %1, ((1 << TYPE_BITS) - 1)%endmacro%macro DATA 1sar %1, TYPE_BITS%endmacro%macro DATA_UPPER 1sar %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)%endmacro%macro DATA_LOWER 1sal %1, ((WORD_SIZE - TYPE_BITS) >> 1)DATA_UPPER %1%endmacro%define MAKE_LITERAL_PAIR(car, cdr) (((((car - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (cdr - start_of_data)) << TYPE_BITS) | T_PAIR)%macro CAR 1DATA_UPPER %1add %1, start_of_datamov %1, qword [%1]%endmacro%macro CDR 1DATA_LOWER %1add %1, start_of_datamov %1, qword [%1]%endmacro%macro MAKE_LITERAL_CLOSURE 3push raxpush rbxmov rax, %1mov qword [rax], %2 - start_of_datashl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)lea rbx, [rax + 8 - start_of_data]or qword [rax], rbxshl qword [rax], TYPE_BITSor qword [rax], T_CLOSUREmov qword [rax + 8], %3pop rbxpop rax%endmacro%macro CLOSURE_ENV 1DATA_UPPER %1add %1, start_of_data%endmacro%macro CLOSURE_CODE 1DATA_LOWER %1add %1, start_of_datamov %1, qword [%1]%endmacro%macro MAKE_LITERAL_STRING 1+dq (((((%%LstrEnd - %%Lstr) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Lstr - start_of_data)) << TYPE_BITS) | T_STRING)%%Lstr:db %1%%LstrEnd:%endmacro%macro STRING_LENGTH 1DATA_UPPER %1%endmacro%macro STRING_ELEMENTS 1DATA_LOWER %1add %1, start_of_data%endmacro%macro STRING_REF 3push raxmov rax, %2STRING_ELEMENTS raxadd rax, %3mov %1, byte [rax]pop rax%endmacro%macro MAKE_LITERAL_VECTOR 1+dq ((((((%%VecEnd - %%Vec) >> 3) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Vec - start_of_data)) << TYPE_BITS) | T_VECTOR)%%Vec:dq %1%%VecEnd:%endmacro%macro VECTOR_LENGTH 1DATA_UPPER %1%endmacro%macro VECTOR_ELEMENTS 1DATA_LOWER %1add %1, start_of_data%endmacro%macro VECTOR_REF 3mov %1, %2VECTOR_ELEMENTS %1lea %1, [%1 + %3*8]mov %1, qword [%1]mov %1, qword [%1]%endmacro%define SOB_UNDEFINED MAKE_LITERAL(T_UNDEFINED, 0)%define SOB_VOID MAKE_LITERAL(T_VOID, 0)%define SOB_FALSE MAKE_LITERAL(T_BOOL, 0)%define SOB_TRUE MAKE_LITERAL(T_BOOL, 1)%define SOB_NIL MAKE_LITERAL(T_NIL, 0)section .datastart_of_data:section .bssextern exit, printf, scanfglobal main, write_sob, write_sob_if_not_voidsection .text
main:
	nop
	; setup a fake closure just to see how it prints:
	mov rax, 0x1234
	sal rax, 30
	or rax, sob6 + 8 - start_of_data
	sal rax, 4
	or rax, T_CLOSURE
	mov qword [sob6], rax
	mov qword [sob6 + 8], main

	; printing the fake closure:
	push qword [sob6]
	call write_sob_if_not_void
	add rsp, 1*8

	; printing a vector:
	push qword [sobVec1]
	call write_sob_if_not_void
	add rsp, 1*8

	; will void print??
	push qword SOB_VOID
	call write_sob_if_not_void
	add rsp, 1*8

	ret
write_sob_undefined:push rbpmov rbp, rspmov rax, 0mov rdi, .undefinedcall printfleaveretsection .data.undefined:db "#<undefined>", 0write_sob_integer:push rbpmov rbp, rspmov rsi, qword [rbp + 8 + 1*8]sar rsi, TYPE_BITSmov rdi, .int_format_stringmov rax, 0call printfleaveretsection .data.int_format_string:db "%ld", 0write_sob_char:push rbpmov rbp, rspmov rsi, qword [rbp + 8 + 1*8]DATA rsicmp rsi, CHAR_NULje .Lnulcmp rsi, CHAR_TABje .Ltabcmp rsi, CHAR_NEWLINEje .Lnewlinecmp rsi, CHAR_PAGEje .Lpagecmp rsi, CHAR_RETURNje .Lreturncmp rsi, CHAR_SPACEje .Lspacejg .Lregularmov rdi, .specialjmp .done.Lnul:mov rdi, .nuljmp .done.Ltab:mov rdi, .tabjmp .done.Lnewline:mov rdi, .newlinejmp .done.Lpage:mov rdi, .pagejmp .done.Lreturn:mov rdi, .returnjmp .done.Lspace:mov rdi, .spacejmp .done.Lregular:mov rdi, .regularjmp .done.done:mov rax, 0call printfleaveretsection .data.space:db "#space", 0.newline:db "#newline", 0.return:db "#return", 0.tab:db "#tab", 0.page:db "#page", 0.nul:db "#nul", 0.special:db "#x%02x", 0.regular:db "#%c", 0write_sob_void:push rbpmov rbp, rspmov rax, 0mov rdi, .voidcall printfleavessection .data.void:db "#<void>", 0write_sob_bool:push rbpmov rbp, rspmov rax, qword [rbp + 8 + 1*8]cmp rax, SOB_FALSEje .sobFalsemov rdi, .truejmp .continue.sobFalse:mov rdi, .false.continue:mov rax, 0call printfleaveretsection .data.false:db "#f", 0.true:db "#t", 0write_sob_nil:push rbpmov rbp, rspmov rax, 0mov rdi, .nilcall printfleaveretsection .data.nil:db "()", 0write_sob_string:push rbpmov rbp, rspmov rax, 0mov rdi, .double_quotecall printfmov rax, qword [rbp + 8 + 1*8]mov rcx, raxSTRING_LENGTH rcxSTRING_ELEMENTS rax.loop:
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
  	jmp .printf.ch_hex:
  	mov rdi, .fs_hex_char
  	mov rsi, rbx
  	jmp .printf.ch_tab:
  	mov rdi, .fs_tab
  	mov rsi, rbx
  	jmp .printf.ch_page:
  	mov rdi, .fs_page
  	mov rsi, rbx
  	jmp .printf.ch_return:
  	mov rdi, .fs_return
  	mov rsi, rbx
  	jmp .printf.ch_newline:
  	mov rdi, .fs_newline
  	mov rsi, rbx.printf:
  	push rax
  	push rcx
  	mov rax, 0
  	call printf
  	pop rcx
  	pop rax

  	dec rcx
  	inc rax
  	jmp .loop.done:
  	mov rax, 0
  	mov rdi, .double_quote
  	call printf

  	leave
  	retsection .data
  .double_quote:
  	db '"', 0
  .fs_simple_char:
  	db "%c", 0
  .fs_hex_char:
  	db "x%02x;", 0
  .fs_tab:
  	db "	", 0
  .fs_page:
  	db "", 0
  .fs_return:
  	db "", 0
  .fs_newline:
  	db "
", 0write_sob_pair:
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
  	retsection .data
  .open_paren:
  	db "(", 0
  .close_paren:
  	db ")", 0write_sob_pair_on_cdr:
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
  	jmp .done.cdrIsPair:
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
  	add rsp, 1*8.done:
  	leave
  	retsection .data
  .space:
  	db " ", 0
  .dot:
  	db " . ", 0write_sob_vector:
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
  	add rax, 8.loop:
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
  	jmp .loop.done:
  	mov rax, 0
  	mov rdi, .fs_close_vector
  	call printf

  	leave
  	retsection	.data
  .fs_open_vector:
  	db "#(", 0
  .fs_close_vector:
  	db ")", 0
  .fs_space:
  	db " ", 0write_sob_symbol:
  	push rbp
  	mov rbp, rsp

  	leave
  	retwrite_sob_fraction:
  	push rbp
  	mov rbp, rsp

  	leave
  	retwrite_sob_closure:
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
  	retsection .data
  .closure:
  	db "#<closure [env:%p, code:%p]>", 0write_sob:
  	mov rax, qword [rsp + 1*8]
  	TYPE rax
  	jmp qword [.jmp_table + rax * 8]section .data
  .jmp_table:
  	dq write_sob_undefined, write_sob_void, write_sob_nil
  	dq write_sob_integer, write_sob_fraction, write_sob_bool
  	dq write_sob_char, write_sob_string, write_sob_symbol
  	dq write_sob_closure, write_sob_pair, write_sob_vectorsection .text
  write_sob_if_not_void:
  	mov rax, qword [rsp + 1*8]
  	cmp rax, SOB_VOID
  	je .continue

  	push rax
  	call write_sob
  	add rsp, 1*8
  	mov rax, 0
  	mov rdi, .newline
  	call printf.continue:
  	ret
  section .data
  .newline:
  	db CHAR_NEWLINE, 0