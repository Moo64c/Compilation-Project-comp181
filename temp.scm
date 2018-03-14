
define gen-vector
	(lambda()
		(string-append
            "custom_vector_body:\n"
        	"push rbp\n"
			"mov rbp, rsp\n"
            "mov rbx, arg_count\n"

	        "push rbx\n"
	        "sal rbx, 3\n"
	        "mov rdi, rbx\n"
	        "call malloc\n"
	        "pop rbx\n"

	        ;rax= pointer to address of rbx*8 bytes, rbx=length of vector
	        "mov r10, 0\n" ;counter
	        "for_vector:\n"
	        "cmp r10, rbx\n"
	        "je end_of_vector\n"

	        "mov rdx, An(r10)\n"
	        "mov qword [rax+r10*8], rdx\n"
	        "inc r10\n"
	        "jmp for_vector\n"
	        "end_of_vector:\n"

	        "mov rcx, rax\n"
	        "sal rbx, 3\n"
	        "MAKE_LITERAL_VECTOR_WITH_REGS rcx, rbx\n" ;at the end of macro, rax is a literal vector
	        "mov rcx, rax\n"
	        "push rcx\n"
	        "mov rdi, 8\n"
	        "call malloc\n"
	        "pop rcx\n"
	        "mov [rax], rcx\n"







	        "custom_vector_finish:\n"
	        "leave\n"
	        "ret\n"
	        "custom_vector_exit:\n" ))
          "mov rcx,rax\n\t\t\t\t"
          "sub rcx,start_of_data\n\t\t\t\t"
          "ALLOCATE(8)\n\t\t\t\t"
          "mov qword [rax],rsi\n\t\t\t\t"
          "sal qword [rax],30\n\t\t\t\t"
          "or qword [rax],rcx\n\t\t\t\t"
          "sal qword [rax],4\n\t\t\t\t"
          "or qword [rax],T_VECTOR\n\t\t\t\t"

          "mov rax,qword [rax]\n\t\t\t\t"
          "leave\n\t\t\t\t"
          "ret\n\t\t\t\t"



          (define genMakeVector
            (lambda ()
              (string-append
                (start_assembly_library_functions 'make-vector)
                (getLabelFvar 'make-vector) "_body:\n\t"
                "push rbp\n\t"
                "mov rbp, rsp\n\t"

                 ;check number of arguments
                 "mov rcx, qword [rbp + 3*8]\n\t"
                 "cmp rcx, 2\n\t"
                 "jne make_vector_not_two_args\n\t"

                 ;";;get first argument to rax \n\t"
                 ;"mov rax, qword [rbp + 4*8]\n\t"
                 ;"mov rax, qword [rax]\n\t"

                 ";;get second argument to rdx\n\t"
                 "mov rdx,qword [rbp + 5*8]\n\t"
                 "jmp make_vector_create_vector\n\t"

                 "make_vector_not_two_args:\n\t\t"
                 "mov rdx,0\n\t\t"
                 "ALLOCATE(8)\n\t\t"
                 "mov rbx, rdx \n\t\t"
                 "sal rbx, TYPE_BITS \n\t\t"
                 "or rbx, T_INTEGER \n\t\t"
                 "mov qword[rax], rbx\n\t\t"
                 "mov rdx,rax\n\t\t"

                 "make_vector_create_vector:\n\t\t\t"
                 "mov rsi, qword [rbp + 4*8]\n\t\t\t"
                 "DATA rsi\n\t\t\t"
                 "mov rdi, rbx\n\t"
                 "sal rdi, 3\n\t"
                 "ALLOCATE(rdi)\n\t"

                 ";;rdi is the index in the loop\n\t\t\t"
                 "mov rdi,0\n\t\t\t"
                 "make_vector_loop:\n\t\t\t\t"
                 "cmp rsi,rdi\n\t\t\t\t"
                 "je make_vector_loop_exit\n\t\t\t\t"
                 "mov qword [rax+rdi*8],rdx\n\t\t\t\t"
                 "inc rdi\n\t\t\t\t"
                 "jmp make_vector_loop\n\t\t\t"



                 "make_vector_loop_exit:\n\t\t\t"
                 "mov rcx, rax\n\t\t"
                 "sal rbx, 3\n\t\t"
                 "MAKE_LITERAL_VECTOR_REGS rcx, rsi\n\t\t" ;rax = literal vector
                 "mov rcx, rax\n\t\t"
                 "push rcx\n\t\t"
                 "ALLOCATE(8)\n\t\t"
                 "pop rcx\n\t"
                 "mov [rax], rcx\n\t\t"

                 "leave\n\t\t"
                 "ret\n\t\t"
              )
            )
          )
