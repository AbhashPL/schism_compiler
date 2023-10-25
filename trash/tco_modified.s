
section .text
extern error
extern equal
extern print
extern input
global our_code_starts_here
  
  f:










  mov eax, [esp-8]
  mov [esp-20], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, 2
  mov [esp-24], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, [esp-20]
  add eax, [esp-24]
  jo error_overflow
  mov [esp-12], eax ;; [esp - 12] = 10
  mov eax, [esp-8]
  mov [esp-24], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, 4
  mov [esp-28], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, [esp-24]
  add eax, [esp-28]
  jo error_overflow
  mov [esp-16], eax ;; [esp - 16] = 12
  mov eax, [esp-8]
  mov [esp-28], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, 6
  mov [esp-32], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, [esp-28]
  add eax, [esp-32]
  jo error_overflow
  mov [esp-20], eax ;; [esp - 20] = 14
  
  
  jmp g
  ret
  
  
  
  g:
  mov eax, [esp-8]
  mov [esp-20], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, [esp-12]
  mov [esp-24], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, [esp-20]
  sar eax, 1
  imul eax, [esp-24]
  jo error_overflow
  mov [esp-20], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, [esp-16]
  mov [esp-24], eax
  and eax, 1
  cmp eax, 0
  jne error_non_int
  mov eax, [esp-20]
  sar eax, 1
  imul eax, [esp-24]
  jo error_overflow
  ret
  our_code_starts_here:
  mov ebx, [esp+4]
  add ebx, 4
  and ebx, 0xFFFFFFFC
  mov [esp-4], DWORD temp_after_call_1
  mov [esp-8], esp
  mov eax, 8
  mov [esp-12], eax
  sub esp, 4
  jmp f
  temp_after_call_1:
  mov esp, [esp-8]
  ret
  error_non_int:
  push DWORD 1
  call error
  error_non_bool:
  push DWORD 2
  call error
  error_overflow:
  push DWORD 3
  call error
  error_non_tuple:
  push DWORD 4
  call error
  error_too_small:
  push DWORD 5
  call error
  error_too_large:
  push DWORD 6
  call error

