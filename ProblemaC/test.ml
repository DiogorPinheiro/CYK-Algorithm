open String
open Printf

let file = read_line()
let tamanho = length file

let a_hash : (string, string) Hashtbl.t = Hashtbl.create 10

let contar_words palavra length =   (* Contar número de palavras numa linha do ficheiro *)
    let cont = ref 0 in
    let () =
      for i= 0 to (length-1) do
          if ((get palavra i) = ' ') then (cont:=(!cont+1)) else (cont:=(!cont+0)) 
      done in
    (!cont+1)

let num_words = contar_words file tamanho


let rec separar palavra hashtab contador i base cont baseCheck = (* Colocar produções numa hashtable*)
  if cont <> 2 then                                  (* Última palavra é um caso especial*)
    let comprimento = (String.index_from palavra i ' ') - i in  (*Diferença entre posição atual e a primeira posição onde aparece um espaço *)
    let word = String.sub palavra i comprimento in
    if word = "->" then
      separar palavra hashtab contador (i+comprimento+1) base (cont+1) 1
    else
      separar palavra hashtab contador (i+comprimento+1) word (cont+1) 0
  else
    let size = (String.length palavra) - i in
    let palav = (String.sub palavra i size) in
    let remove_blanks = Str.global_replace (Str.regexp " ") "" palav in
    let () = Hashtbl.add hashtab remove_blanks base in hashtab 

(*let rec separar palavra tabela contador i base cont baseCheck =
  let comprimento = (String.index_from palavra i ' ') - i in
  let word = String.sub palavra i comprimento in
  if word = "->" then
      separar palavra tabela contador (i+comprimento+1) base (cont+1) 1
  else if baseCheck = 1 then
    let size = (String.length palavra) - i in
    let palav = (String.sub palavra i size) in
    let () = printf "Entrou Else if\n" in
    if (contador < 4) then
        let () = printf "Entrou\n" in
        let () = Hashtbl.add tabela palav base in tabela
    else
        let () = printf "Entrou contador = %d\n" contador in
        let remove_blanks = Str.global_replace (Str.regexp " ") "" palav in
        let () = printf "%s\n" remove_blanks in
        let () = Hashtbl.add tabela remove_blanks base in tabela
  else
      separar palavra tabela contador (i+comprimento+1) word (cont+1) 0 *)


let a_hash = separar file a_hash num_words 0 "" 0 0 

let carac = Hashtbl.find_all a_hash "a"

let rec print_hash lista =
  match lista with
  | [] -> printf "\n"
  | (x)::resto -> let () = printf "%s " x in print_hash resto
let () = print_hash carac
(*
let cartesian l l' = 
  List.concat (List.map (fun e -> List.map (fun e' -> (e^e')) l') l)

let word = "ab"

let tabela = Array.make_matrix (length word) (length word) [] 
let () = tabela.(0).(0)<- ["S"]
let () = tabela.(0).(0)<-tabela.(0).(0)@["NA"]
let () = printf "%d\n" (List.length tabela.(0).(0))
let () = tabela.(0).(1)<- ["S"]
let () = tabela.(0).(1)<-tabela.(0).(1)@["K"]
let produto = cartesian tabela.(0).(0) tabela.(0).(1) 

let rec printar lista =
  match lista with
  | [] -> printf "\n"
  | a::resto -> let () = printf "%s " a in printar resto
let () = printar produto*)

(*
let product elem_a elem_b hashtable=
  let produto = cartesian elem_a elem_b in 
  let join = [] in
  for i=0 to (List.length produto)-1 do
    let join = join @ (Hashtbl.find_all hashtable (Char.escaped produto i) ) in
  done*)

(*let rec printar lista =
  match lista with
  | [] -> printf "\n"
  | a::resto -> let () = printf "%s " a in printar resto
let () = printar produto*)

(*
let nenhumdestino tabela  =
  for i=(8-1) downto 0 do
      for j=0 to i do
        if tabela.(j).(i)=0 then tabela.(j).(i)<-(-1);
        let () = printf "(%d,%d) " j i in()
      done;
      let () = printf "\n" in ()
  done
let () = nenhumdestino tabela *)
(*
let nenhumdestino   =
  for j=0 to 7 do
      for i=0 to (7-j) do
        let () = printf "(%d,%d) " j i in()
      done;
      let () = printf "\n" in ()
  done
let () = nenhumdestino *)

