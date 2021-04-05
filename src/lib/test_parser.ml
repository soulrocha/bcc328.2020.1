(* Test syntax analyser *)

module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    (*let tree = Absyntree.flat_nodes (Absyntree.tree_of_lfundec ast) in*)
    let tree = Absyntree.flat_nodes (Absyntree.tree_of_lfundecs ast) in
    let box = Tree.box_of_tree tree in
    Format.printf "%s\n\n%!" (Box.string_of_box box);
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg



let%expect_test _ =
  (* function declaration and constant expression *)
  check "int f(int x) = 100";
  [%expect{|
                 ╭───────╮
                 │Program│
                 ╰────┬──╯
                   ╭──┴╮
                   │Fun│
                   ╰──┬╯
         ╭───────────┬┴───────────╮
    ╭────┴────╮  ╭───┴───╮  ╭─────┴────╮
    │    f    │  │Formals│  │IntExp 100│
    │Absyn.Int│  ╰───┬───╯  ╰──────────╯
    ╰─────────╯ ╭────┴────╮
                │    x    │
                │Absyn.Int│
                ╰─────────╯ |}];

  check "int f(int x, int y, bool z) = 100";
  [%expect{|
                              ╭───────╮
                              │Program│
                              ╰───┬───╯
                                ╭─┴─╮
                                │Fun│
                                ╰─┬─╯
         ╭────────────────────────┴────────────────────────╮
    ╭────┴────╮              ╭────┴──╮               ╭─────┴────╮
    │    f    │              │Formals│               │IntExp 100│
    │Absyn.Int│              ╰────┬──╯               ╰──────────╯
    ╰─────────╯      ╭───────────┬┴───────────╮
                ╭────┴────╮ ╭────┴────╮ ╭─────┴────╮
                │    x    │ │    y    │ │    z     │
                │Absyn.Int│ │Absyn.Int│ │Absyn.Bool│
                ╰─────────╯ ╰─────────╯ ╰──────────╯ |}];

  check "int f() = 100";
  [%expect{| :1.7 error: syntax |}];

  check "foo f(int x) = 100";
  [%expect{| :1.3 error: syntax |}];

  (* binary operators *)
  check "bool f(int x) = 2 + 3 + 4 < 5 + 6";
  [%expect{|
                                       ╭───────╮
                                       │Program│
                                       ╰───┬───╯
                                         ╭─┴─╮
                                         │Fun│
                                         ╰─┬─╯
          ╭───────────┬────────────────────┴────────────╮
    ╭─────┴────╮  ╭───┴───╮                        ╭────┴──╮
    │    f     │  │Formals│                        │OpExp <│
    │Absyn.Bool│  ╰───┬───╯                        ╰────┬──╯
    ╰──────────╯ ╭────┴────╮                 ╭──────────┴───────────────╮
                 │    x    │            ╭────┴──╮                   ╭───┴───╮
                 │Absyn.Int│            │OpExp +│                   │OpExp +│
                 ╰─────────╯            ╰────┬──╯                   ╰───┬───╯
                                       ╭─────┴──────────╮          ╭────┴─────╮
                                   ╭───┴───╮       ╭────┴───╮ ╭────┴───╮ ╭────┴───╮
                                   │OpExp +│       │IntExp 4│ │IntExp 5│ │IntExp 6│
                                   ╰───┬───╯       ╰────────╯ ╰────────╯ ╰────────╯
                                  ╭────┴─────╮
                             ╭────┴───╮ ╭────┴───╮
                             │IntExp 2│ │IntExp 3│
                             ╰────────╯ ╰────────╯ |}
  ];

  check "bool f(int x) = 2 < 3 < 4";
  [%expect{| :1.23 error: syntax |}];

  check "int f(int x, int y) = x + y";
  [%expect{|
                        ╭───────╮                       
                        │Program│                       
                        ╰───┬───╯                       
                          ╭─┴─╮                         
                          │Fun│                         
                          ╰─┬─╯                         
      ╭─────────────────┬───┴─────────────────╮         
  ╭────┴────╮        ╭───┴───╮             ╭───┴───╮     
  │    f    │        │Formals│             │OpExp +│     
  │Absyn.Int│        ╰───┬───╯             ╰───┬───╯     
  ╰─────────╯      ╭─────┴─────╮          ╭────┴────╮    
              ╭────┴────╮ ╭────┴────╮ ╭───┴───╮ ╭───┴───╮
              │    x    │ │    y    │ │IdExp x│ │IdExp y│
              │Absyn.Int│ │Absyn.Int│ ╰───────╯ ╰───────╯
              ╰─────────╯ ╰─────────╯
  |}];

  check "int f(int a, bool b) = if a then b else 10 + 2";
  [%expect{|
                                    ╭───────╮                                   
                                    │Program│                                   
                                    ╰───┬───╯                                   
                                      ╭─┴─╮                                     
                                      │Fun│                                     
                                      ╰─┬─╯                                     
      ╭──────────────────┬──────────────┴──────────────────╮                    
  ╭────┴────╮        ╭────┴──╮                        ╭─────┴────╮               
  │    f    │        │Formals│                        │CondExp if│               
  │Absyn.Int│        ╰────┬──╯                        ╰─────┬────╯               
  ╰─────────╯      ╭──────┴─────╮          ╭─────────┬──────┴─────────╮          
              ╭────┴────╮ ╭─────┴────╮ ╭───┴───╮ ╭───┴───╮       ╭────┴──╮       
              │    a    │ │    b     │ │IdExp a│ │IdExp b│       │OpExp +│       
              │Absyn.Int│ │Absyn.Bool│ ╰───────╯ ╰───────╯       ╰────┬──╯       
              ╰─────────╯ ╰──────────╯                          ╭─────┴─────╮    
                                                          ╭────┴────╮ ╭────┴───╮
                                                          │IntExp 10│ │IntExp 2│
                                                          ╰─────────╯ ╰────────╯
  |}];

  check "bool b(int a) = 1 < 2";
  [%expect{|
                    ╭───────╮                   
                    │Program│                   
                    ╰────┬──╯                   
                      ╭──┴╮                     
                      │Fun│                     
                      ╰──┬╯                     
        ╭───────────┬────┴───────────╮          
  ╭─────┴────╮  ╭───┴───╮        ╭───┴───╮      
  │    b     │  │Formals│        │OpExp <│      
  │Absyn.Bool│  ╰───┬───╯        ╰───┬───╯      
  ╰──────────╯ ╭────┴────╮      ╭────┴─────╮    
              │    a    │ ╭────┴───╮ ╭────┴───╮
              │Absyn.Int│ │IntExp 1│ │IntExp 2│
              ╰─────────╯ ╰────────╯ ╰────────╯
  |}];

  check "bool cond(int a, int b, int c) = if a+b < 5 then c else b + a";
  [%expect{|
                                                    ╭───────╮                                                  
                                                    │Program│                                                  
                                                    ╰───┬───╯                                                  
                                                      ╭─┴─╮                                                    
                                                      │Fun│                                                    
                                                      ╰─┬─╯                                                    
        ╭───────────────────────┬───────────────────────┴────────────────────────╮                             
  ╭─────┴────╮              ╭───┴───╮                                      ╭─────┴────╮                        
  │   cond   │              │Formals│                                      │CondExp if│                        
  │Absyn.Bool│              ╰───┬───╯                                      ╰─────┬────╯                        
  ╰──────────╯      ╭───────────┴───────────╮                     ╭──────────────┴────┬──────────────╮         
              ╭────┴────╮ ╭────┴────╮ ╭────┴────╮           ╭────┴──╮            ╭───┴───╮      ╭───┴───╮     
              │    a    │ │    b    │ │    c    │           │OpExp <│            │IdExp c│      │OpExp +│     
              │Absyn.Int│ │Absyn.Int│ │Absyn.Int│           ╰────┬──╯            ╰───────╯      ╰───┬───╯     
              ╰─────────╯ ╰─────────╯ ╰─────────╯          ╭─────┴─────────╮                   ╭────┴────╮    
                                                        ╭───┴───╮      ╭────┴───╮           ╭───┴───╮ ╭───┴───╮
                                                        │OpExp +│      │IntExp 5│           │IdExp b│ │IdExp a│
                                                        ╰───┬───╯      ╰────────╯           ╰───────╯ ╰───────╯
                                                      ╭────┴────╮                                             
                                                  ╭───┴───╮ ╭───┴───╮                                         
                                                  │IdExp a│ │IdExp b│                                         
                                                  ╰───────╯ ╰───────╯  
  |}];

  check"int f(int a) = let x = 10 in var int j(int b) = let y = 10 in var + 5 + 10";
  [%expect{|
                                                        ╭───────╮                                                       
                                                        │Program│                                                       
                                                        ╰────┬──╯                                                       
                        ╭───────────────────────────────────┴───────────────────────╮                                  
                      ╭─┴─╮                                                      ╭──┴╮                                 
                      │Fun│                                                      │Fun│                                 
                      ╰─┬─╯                                                      ╰──┬╯                                 
      ╭───────────┬─────┴───────────╮                 ╭───────────┬─────────────────┴───────────╮                      
  ╭────┴────╮  ╭───┴───╮        ╭────┴───╮        ╭────┴────╮  ╭───┴───╮                    ╭────┴───╮                  
  │    f    │  │Formals│        │DecExp x│        │    j    │  │Formals│                    │DecExp y│                  
  │Absyn.Int│  ╰───┬───╯        ╰────┬───╯        │Absyn.Int│  ╰───┬───╯                    ╰────┬───╯                  
  ╰─────────╯ ╭────┴────╮      ╭─────┴─────╮      ╰─────────╯ ╭────┴────╮      ╭─────────────────┴─────╮                
              │    a    │ ╭────┴────╮ ╭────┴────╮             │    b    │ ╭────┴────╮             ╭────┴──╮             
              │Absyn.Int│ │IntExp 10│ │IdExp var│             │Absyn.Int│ │IntExp 10│             │OpExp +│             
              ╰─────────╯ ╰─────────╯ ╰─────────╯             ╰─────────╯ ╰─────────╯             ╰────┬──╯             
                                                                                                ╭─────┴──────────╮     
                                                                                            ╭────┴──╮        ╭────┴────╮
                                                                                            │OpExp +│        │IntExp 10│
                                                                                            ╰────┬──╯        ╰─────────╯
                                                                                          ╭─────┴─────╮                
                                                                                      ╭────┴────╮ ╭────┴───╮            
                                                                                      │IdExp var│ │IntExp 5│            
                                                                                      ╰─────────╯ ╰────────╯
  |}];

  check"int f(int a) = y(if 1 < 2 then 3 else 4, let x = 10 in x)";
  [%expect{|
                                          ╭───────╮                                        
                                          │Program│                                        
                                          ╰───┬───╯                                        
                                            ╭─┴─╮                                          
                                            │Fun│                                          
                                            ╰─┬─╯                                          
      ╭───────────┬──────────────────────────┴───────────╮                                
  ╭────┴────╮  ╭───┴───╮                           ╭──────┴──────╮                         
  │    f    │  │Formals│                           │FuncCallExp y│                         
  │Absyn.Int│  ╰───┬───╯                           ╰──────┬──────╯                         
  ╰─────────╯ ╭────┴────╮                      ╭──────────┴─────────────────────╮          
              │    a    │                ╭─────┴────╮                      ╭────┴───╮      
              │Absyn.Int│                │CondExp if│                      │DecExp x│      
              ╰─────────╯                ╰─────┬────╯                      ╰────┬───╯      
                                    ╭──────────┴─────┬──────────╮          ╭────┴─────╮    
                                ╭───┴───╮       ╭────┴───╮ ╭────┴───╮ ╭────┴────╮ ╭───┴───╮
                                │OpExp <│       │IntExp 3│ │IntExp 4│ │IntExp 10│ │IdExp x│
                                ╰───┬───╯       ╰────────╯ ╰────────╯ ╰─────────╯ ╰───────╯
                              ╭────┴─────╮                                                
                          ╭────┴───╮ ╭────┴───╮                                            
                          │IntExp 1│ │IntExp 2│                                            
                          ╰────────╯ ╰────────╯
  |}];

  check "int g(string a) = 100";
  [%expect{|:1.12 error: syntax|}];

  check "int x(int g) = int z";
  [%expect{|:1.18 error: syntax|}];

  check "int x(int y) = ++ y";
  [%expect{|:1.23 error: syntax|}];

  check "int x (int y) = < y";
  [%expect{|:1.23 error: syntax|}];

  check"int f(int x) = y(let var x = 10 in var, if x < 12 then x+3 else x)";
  [%expect{| :1.26 error: syntax  |}];

  check "int f(int a) = if 1 < 2 then 2 else 1 int g(in b) = if 1 + 1 < 9 then 2 else 9";
  [%expect{| :2.8 error: syntax |}];

  check "int f(int a, int b) = (if a then b else 1) + 0";
  [%expect{| :1.23 error: syntax |}];

  check "bool g(bool a, bool b) = if a then b else (0 + 0)";
  [%expect{| :1.23 error: syntax |}];