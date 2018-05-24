(*DECLARA��O DAS FUN��ES*)

(* VERIFICA SE � LETRA MIN�SCULA *)
let e_variavel_min c = ((int_of_char(c) >= int_of_char('a')) && (int_of_char(c) <= int_of_char('z')));;

(* VERIFICA SE � LETRA MAI�SCULA *)
let e_variavel_max c = ((int_of_char(c) >= int_of_char('A')) && (int_of_char(c) <= int_of_char('Z')));;

(* VERIFICA SE � D�GITO *)
let e_digito c = let zero = int_of_char('0') in 
	int_of_char(c) - zero >= 0 && int_of_char(c) - zero <= 9;;

(*=================================================================================================================================*)			

(*vari�vel para teste apenas*)		
let expr = "X1y564";;

(*A L�GICA USADA FOI QUE SE A VARI�VEL CONTROLE FOR IGUAL A STRING ENT�O ELA PERCORREU TODA A STRING OBEDECENDO AS REGRAS, LOGO*)
(*ELA � UMA VARI�VEL, CASO CONTR�RIO N�O *)

(* esse valor tem que ser SEMPRE true para a l�gica funcionar *)
let retorno_const = true;; 

(*verifica se � uma vari�vel ou n�o*)
let rec ehVariavel exp pos controle retorno = 
	(*POS INDICA A POSI��O ATUAL, EXP � UMA STRING*)
	if (pos = (String.length exp)) then 
		if (controle = (String.length exp)) then 
			true
		else 
			false
	else 
		(*VERIFICA SE O PRIMEIRO CHAR � MINUSCULO*)
		if ((e_variavel_min (String.get exp pos)) && (pos = 0)) then
			retorno = ehVariavel exp (pos+1) (controle+1) retorno
	  else 
			if ((e_variavel_max (String.get exp pos) || e_variavel_min (String.get exp pos) || e_digito (String.get exp pos)) && (pos <> 0)) then
				retorno = ehVariavel exp (pos+1) (controle+1) retorno
			else
				retorno = ehVariavel exp (pos+1) (controle) retorno;;

(* converte o boolean para string *)
print_string( string_of_bool(ehVariavel expr 0 0 retorno_const) );;
				 	

(*======================================================== FIM =========================================================================*)			
