open Compile
open Printf



let () = 
      let x86_program = (compile_to_string tuple_eg11) in
      printf "%s\n" x86_program;;