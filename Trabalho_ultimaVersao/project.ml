(* Projeto da Matéria - Conceitos de Linguagem de Programação(CLP) *)

(* A ideia da execução do trabalho do grupo de Ocaml foi utilizar *)
(* uma lista de expressoes onde cada termo eh uma linha *)
(* de codigo da linguagem L,a funcao "compila" ira percorrer *)
(* esta lista verificando a respectiva sintaxe e a executando *)
(* OBS: Necessária a inclusão da biblioteca Str !! *)
(* OBS2: Para os booleanos foi utilizado true e false *)
(* OBS3: Os parametros de saida,assim como os erros *)
(* foram inseridos em um arquivo txt*)
(* para que o grupo da interface pudesse utilizar *)

open Utilidades;;
open AvaliaSintaxe;;

let compila = fun listaAritmetico listaLogico listaPalavraChave listaComparativo ->
  let listaVariaveis = ref [] in
  let listaValores = ref [] in
  let listaTipos = ref [] in
  let arquivo = "teste.txt" in
	let arquivo2 = "saida.txt" in
	let arquivo3 = "traducao.txt" in
  let oc = open_in arquivo in   
	let oc2 = open_out arquivo2 in
	let oc3 = open_out arquivo3 in
	let linha = input_line oc in
    let lista = ref [] in
    let lista_final = ref [] in
    let var_atribuida = ref false in
		let resposta_parenteses = ref true in
		(* Verifica se a ordem e a quantidade dos parenteses esta correta *)
		verifica_parenteses linha resposta_parenteses;
		if !resposta_parenteses then
			begin
      verifica_sintaxeFun linha lista;
      print_list !lista;
      Printf.printf "\n";
      token lista listaAritmetico listaLogico listaPalavraChave listaComparativo lista_final;
      print_list !lista_final;
			print_list_file oc3 !lista_final;
      Printf.printf "\n";
      executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
			print_list !lista_final;
      Printf.printf "\n";
      print_list !listaVariaveis;
      Printf.printf "\n";
      print_list !listaValores;
      Printf.printf "\n";
      print_list !listaTipos;
      Printf.printf "\n";
			end
		else
			begin
			Printf.printf	"Erro de sintaxe: Ordem ou quantidade de parenteses invalida";
			Printf.fprintf oc2 "Erro de sintaxe: Ordem ou quantidade de parenteses invalida";
			exit 0;
			end;;

(* Declaração das Listas *)
let listaLogico = ["!";"&";"@"];;
let listaAritmetico = ["+";"-";"*";"/";"%";"^"];;
let listaComparativo = ["<";">";"="];;
let listaPalavraChave = ["if";"or";"do"];;
compila listaAritmetico listaLogico listaPalavraChave listaComparativo;;

(* Exemplos testados para P2 *)
(* IF -> ["if ( 5 > 6 , x := 2 )or( x := 3 )"] *)
(* DO -> ["x := 5";"do ( i , 0 , 2 , x := x + 5 )"] *)