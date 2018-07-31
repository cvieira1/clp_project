(* Projeto da Matéria - Conceitos de Linguagem de Programação(CLP) *)
(* OBS: Necessaŕia a inclusão da biblioteca Str !!*)

open Utilidades;;
open AvaliaSintaxe;;

let compila = fun listaExpressoes listaAritmetico listaLogico listaPalavraChave listaComparativo ->
	let tamanho = List.length listaExpressoes in
  let listaVariaveis = ref [] in
  let listaValores = ref [] in
  let listaTipos = ref [] in
	for i=0 to (tamanho-1) do
    let lista = ref [] in
    let lista_final = ref [] in
    let var_atribuida = ref false in
		let resposta_parenteses = ref false in
		verifica_parenteses (List.nth listaExpressoes i) resposta_parenteses;
		if not !resposta_parenteses then
			begin
      verifica_sintaxeFun (List.nth listaExpressoes i) lista;
      print_list !lista;
      Printf.printf "\n";
      token lista listaAritmetico listaLogico listaPalavraChave listaComparativo lista_final;
      print_list !lista_final;
      Printf.printf "\n";
      executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
      print_list !listaVariaveis;
      Printf.printf "\n";
      print_list !listaValores;
      Printf.printf "\n";
      print_list !listaTipos;
			end
		else
			begin
			Printf.printf	"Erro de sintaxe: Ordem ou quantidade de parenteses inválida";
			exit 0;
			end;		
	done;;	

(* Declaração das Listas *)
let listaLogico = ["!";"&";"@"];;
let listaAritmetico = ["+";"-";"*";"/";"%";"^"];;
let listaComparativo = ["<";">";"="];;
let listaPalavraChave = ["if";"or"];;
let listaExpressoes = ["var1 := (1)";"var2 := -var1"];;
compila listaExpressoes listaAritmetico listaLogico listaPalavraChave listaComparativo;;