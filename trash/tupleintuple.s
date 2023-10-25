
section .text
extern error
extern equal
extern print
extern input
global our_code_starts_here
  our_code_starts_here:
  mov ebx, [esp+4]
  add ebx, 4
  and ebx, 0xFFFFFFFC
  

; let ebx = 0x44 in the start

; [0x44 + 8] = false,       [0x4C] = false
; [0x44 + 4] = 18,          [0x48] = 18
; [0x44] = 2,               [0x44] = 2


;;;; ebx = 0x4C, eax = 0x44

; [0x4C + 8] = 0x44
; [0x4C + 4] = 22
; [0x4C] = 2   ///////// bruh we overwrote the 'false'

  mov eax, 22
  mov [esp-4], eax
    
    ;; Creating the (9,false) tuple      
        mov eax, 18
        mov [esp-8], eax
        mov eax, 0x7FFFFFFF
        mov [esp-12], eax
        mov eax, [esp-12]
        mov [ebx+8], eax
        mov eax, [esp-8]
        mov [ebx+4], eax
        mov [ebx+0], DWORD 4
        mov eax, ebx
        or eax, 1     
        add ebx, 8


    ;; Creating the tuple (11, (address_of_9_fls))
    mov [esp-8], eax
    mov eax, [esp-8]
    mov [ebx+8], eax
    mov eax, [esp-4]
    mov [ebx+4], eax
    mov [ebx+0], DWORD 4
    mov eax, ebx
    or eax, 1
    add ebx, 8
  

  ;; Check for tuple-ness
  mov [esp-4], eax ;; [esp-4] = address_of (11,address_9_fls)
  and eax, 3
  cmp eax, 1
  jne error_non_tuple


  ;; just check if the idx is a number
  mov eax, 4
  mov [esp-8], eax  ;; [esp-8] = 4
  and eax, 1
  cmp eax, 0
  jne error_non_int


  mov eax, [esp-4] ;; eax = address_of (11,address_9_fls)
  mov ecx, [esp-8] ;; ecx = 4
  shr ecx, 1       ;; ecx = 2
  mov eax, [eax + ecx * 4 - 1]  ;; this instr should probably get address_of_9fls
  
  
  ;; check for tuple-ness
  mov [esp-4], eax  ;; [esp-4] = address_9_fls
  and eax, 3
  cmp eax, 1
  jne error_non_tuple
  
  ;; just check if the idx is a number
  mov eax, 4
  mov [esp-8], eax ;; [esp-8] = 4
  and eax, 1
  cmp eax, 0
  jne error_non_int

  
  mov eax, [esp-4] ;; eax = address_9_fls
  mov ecx, [esp-8] ;; ecx = 4
  shr ecx, 1       ;; ecx = 2
  mov eax, [eax + ecx * 4 - 1]  ;; [address_9_fls + 8 - 1]
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

