(* UMA POSS�VEL L�GICA SERIA USAR A TABELA ASCII, POIS VOU CONCATENANDO AT� ENCONTRAR ALGO QUE N�O SEJA UM D�GITO OU CHAR*)
(* E POSSO ARMAZENA-LOS NUMA TUPLA CASO N�O D� PARA TER UMA TULPLA DIN�MICA O QUE � BEM PROV�VEL EU POSSO COLOCAR *)
(* (CHAR,POS_REAL), ONDE POS_REAL � A POSI��O NA LISTA DE CARACTERES*)
open Separa_expressao;;
open Verifica_variavel;;

(* express�o para teste *)
let expressao = ref "camelo = 100";;

let listaLogico = ['!';'&';'@'];;
let listaAritmetico = ['+';'-';'*';'/';'%';'^'];;
let lista = ref [""];;

(*========================================IN�CIO DA DECLARA��O DE FUN��ES===================================================================================*)

(* mostra a lista *)
let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l;;

(*fun��o para tirar todos espa�os *)
let tira_espace exp =
	Str.global_replace (Str.regexp " \\|\t") "" exp;;

(*========================================FIM DA DECLARA��O DE FUN��ES===================================================================================*)

		
expressao := tira_espace !expressao;;
let lista_char = explode !expressao;;

let palavra = ref "";;
let letra = ref ' ';;
(* !!!!ELE N�O EST� CONCATENANDO OS VALORES!!!! *)
for indice=0 to ((List.length lista_char)-1) do
	letra := List.nth lista_char indice; 
	if e_variavel_min(!letra) || e_variavel_max(!letra) || e_digito !letra then
		palavra := (!palavra)^(String.make 1 !letra);
		if indice = ((List.length lista_char)-1) then 
			begin
				if (e_variavel_min(!letra) = false) && (e_variavel_max(!letra)=false) && (e_digito !letra = false)  then begin
					lista := List.append [!palavra] !lista;
					palavra := ""	;
					lista := List.append [String.make 1 !letra] !lista;end
				else begin
					lista := List.append [!palavra] !lista;
					palavra := ""; 
				end
			end	
		else
			if (e_variavel_min(!letra) = false) && (e_variavel_max(!letra)=false) && (e_digito !letra = false)  then begin
				lista := List.append [!palavra] !lista;
				palavra := ""	;
				lista := List.append [String.make 1 !letra] !lista;end
			else begin
				lista := List.append [!palavra] !lista;
				palavra := ""; 
			end
done;;

print_list !lista;;