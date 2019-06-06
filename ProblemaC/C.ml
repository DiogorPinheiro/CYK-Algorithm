open Printf
open Scanf
open String

let a_hash : (string, string) Hashtbl.t = Hashtbl.create 50     (* Inicializar HashTable*)

let word = read_line()                                          (* Ler palavra *)
let tam_word = length word                                      (* Tamanho da palavra *)

let n = int_of_string(read_line())                              (* Número de produções *)

let contar_words palavra length =   (* Contar número de palavras numa linha do ficheiro *)
    let cont = ref 0 in
    let () =
      for i= 0 to (length-1) do
          if ((get palavra i) = ' ') then (cont:=(!cont+1)) else (cont:=(!cont+0)) 
      done in
    (!cont+1)

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

let leitura hasht =              (* Leitura das produções*)
  for i = 0 to (n-1) do
    let file = read_line() in
    let num_words = contar_words file (length file) in 
    let _hasht = separar file hasht num_words 0 "" 0 0 in ()      (* Colocar numa Hashtable *)
  done
let () = leitura a_hash 

let matriz = Array.make_matrix tam_word tam_word []    (*Inicializar tabela com tamanho definido pelo comprimento da palavra *)

let colocar_iniciais matriz hashtable n word =          (* Colocar primeira linha de terminais *)
  for i = 0 to (n-1) do
      let () = matriz.(0).(i) <- (Hashtbl.find_all hashtable (Char.escaped (String.get word i)) ) in ()
  done
let () = colocar_iniciais matriz a_hash tam_word word

let cartesiano a l = List.concat (List.map (fun b -> List.map (fun c -> (b^c)) l) a)     (* Produto Cartesiano das duas listas *)

let rec printar lista tamanho cont=      (* Imprimir lista*)
  match lista with
  | [] -> printf "\n"
  | a::resto -> if cont = (tamanho-1) then 
      let () = printf "%s" a in printar resto tamanho (cont+1) 
    else let () = printf "%s " a in printar resto tamanho (cont+1)
    
let preencher_restantes matriz hashtable n word =
  for j=1 to (n-1) do
      for i=0 to ((n-1)-j) do
          for k=0 to (j-1) do
              let produto = cartesiano  (matriz.(k).(i)) (matriz.(j-k-1).(i+k+1)) in 
              for m=0 to ((List.length produto)-1) do
                  let () = matriz.(j).(i) <- ( (matriz.(j).(i)) @ List.flatten[Hashtbl.find_all hashtable (List.nth produto m)] ) in ()
              done
          done
      done
    done
let () = preencher_restantes matriz a_hash tam_word word

let rec output_simplificado lista final =   (* Remover elementos repetidos na lista para output*)
    match lista with
    | [] -> final
    | l::resto -> if List.mem l final then output_simplificado resto final else let final = final@[l] in output_simplificado resto final

let rec dif l=          (* Verificar se lista está vazia*)
    match l with
    | [] -> false
    | tt::resto -> if tt <> "" then true else dif resto

let descobrir_linha matriz tam =  (* Descobrir a primeira linha que tenha um valor *)
  try
  for i=tam-1 downto 0 do
    for j=0 to (tam-1)-i do
    
        if dif matriz.(i).(j) = true then
        let () = printf "%d\n" (tam-i) in
        let tr = true in
        if tr = true then
          raise Exit
    done
  done
  with
        | Exit -> printf ""
        
let final matriz tam =
  if List.mem "S" matriz.(tam-1).(0) then
    let () = printf "YES\n" in 
    let final = output_simplificado matriz.(tam-1).(0) [] in
    let () = printar final (List.length final) 0 in ()
  else
    let () = printf "NO\n" in  
    let () = descobrir_linha matriz tam in ()
let () = final matriz tam_word

(*---------------------------------Auxiliares------------------------------------------------- *)
(*let print_tabelacheck tabela =
  for i=0 to (n-1) do
    for j=0 to (n-1) do
      let () = printf "%s " tabela.(i).(j) in ()
    done;
    let () = printf "\n" in ()
  done;;
let () = print_tabelacheck matriz*)
(*
let carac = Hashtbl.find_all a_hash "A"
let rec print_hash lista =
  match lista with
  | [] -> printf "\n"
  | (x)::resto -> let () = printf "%s " x in print_hash resto
let () = print_hash carac  *)
  
