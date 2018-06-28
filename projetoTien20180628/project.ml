(* Projeto da Matéria - Conceitos de Linguagem de Programação(CLP) *)

open Utilidades;;

(* Declaração das Listas *)
let listaLogico = ["!";"&";"@"];;
let listaAritmetico = ["+";"-";"*";"/";"%";"^"];;
let listaComparativo = ["<";">";"="];;
let listaPalavraChave = ["if";"or"];;
let expressao = "if        a  =   1 x + 2 or y + 3";;
let resposta = ref '0';;
let lista = ref [];;
let lista_final = ref [];;
verifica_sintaxeFun expressao lista;;
token lista listaAritmetico listaLogico listaPalavraChave lista_final;; 
print_list !lista;;
Printf.printf "\n";;
print_list !lista_final;;
