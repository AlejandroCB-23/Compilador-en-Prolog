
%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS CORRESPONDIENTE AL FUNCIONAMIENTO DEL COMPILADOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compilar(Fichero) :- tokenizar(Tokens,Fichero),parse(Tokens,Estructura),computar(dict{},Estructura,_).

tokenizar(Tokens,Fichero) :- obtener_listado_car(Fichero,Lista_char),
                            procesar_tokens(Lista_char,Tokens).

parse(Source,Estructura) :- phrase(pl_program_parser(Estructura),Source),!.
% Llamada principal para el computo de instrucciones del programa
computar(Dicc,Estructura,DiccF) :- parse_list(Estructura,L),computar_instr(Dicc,L,DiccF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado para leer un archivo caracter a caracter
leer_caracteres(Stream, Lista) :-
    at_end_of_stream(Stream), !, % Verificar si hemos llegado al final del archivo
    Lista = []. % Si hemos llegado al final, la lista está vacía
    
leer_caracteres(Stream, [Caracter|Cola]) :-
    get_char(Stream, Caracter), % Leer el siguiente carácter como un atomo
    leer_caracteres(Stream, Cola). % Leer el resto de los caracteres


obtener_listado_car(Fichero,Lista_char) :- open(Fichero, read, Stream),
   leer_caracteres(Stream,Lista_char),
   close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PASO 1: CREACIÓN DEL TOKENISER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

procesar_tokens(Caracteres,Tokens) :- concatenar_caracteres_al_reves(Caracteres,[],Tokens).

concatenar_caracteres_al_reves([],[],[]) :- !.

%Caso base: ultimo token
concatenar_caracteres_al_reves([],Acumulador,[Token|Tokens]) :-
    reverse(Acumulador, Reversa), atomic_list_concat(Reversa, Token), concatenar_caracteres_al_reves([],[],Tokens).

%------------------------Caso general/es-----------------------------------

%Caso de ser un caracter especial (; , ( ,),...) monocaracter
concatenar_caracteres_al_reves([Caracter|Caracteres], Acumulador, Tokens) :- 
    member(Caracter, [';',':','=','(', ')','+','-','*','/',',','<','>','.']), !,
    reverse(Acumulador, Reversa),
    ( Reversa \= [] ->
        atomic_list_concat(Reversa, Atomo),
        ( atom_number(Atomo, Numero) ->
            Tokens = [Numero,Caracter|RestoTokens]
            ;
            Tokens = [Atomo,Caracter|RestoTokens]
        )
        ;
        Tokens = [Caracter|RestoTokens]
    ),
    concatenar_caracteres_al_reves(Caracteres, [], RestoTokens).

%Caso de no encontrar un espacio,tabulador, en general delimitadores de espacio: se acumula
concatenar_caracteres_al_reves([Caracter|Caracteres],Acumulador,Tokens) :- 
    % Si en el acumulador tenemos el '\'' eso significa que aceptamos cualquier tipo de cadena
    ( member('\'',Acumulador) ->
        concatenar_caracteres_al_reves(Caracteres,[Caracter|Acumulador],Tokens)
        ;
        \+member(Caracter,[' ','\n','\t']),!,concatenar_caracteres_al_reves(Caracteres,[Caracter|Acumulador],Tokens)
    ).

%Caso de encontrar un espacio: se junta los caracteres (no tiene que ser vacio el acum) y se mete en la lista Tokens
concatenar_caracteres_al_reves([_|Caracteres],Acumulador,[Token|Tokens]):- 
    reverse(Acumulador, Reversa), 
    ( Acumulador \= [] ->
        atomic_list_concat(Reversa, Atomo),
        ( atom_number(Atomo,Numero) ->
            Token = Numero
            ;
            Token = Atomo
        )
    ),
    concatenar_caracteres_al_reves(Caracteres,[],Tokens).
   
%Caso especifica donde exite 1 caracter que debemos omitir
concatenar_caracteres_al_reves([_|Caracteres],_,Tokens) :- concatenar_caracteres_al_reves(Caracteres,[],Tokens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PASO 2: CREACIÓN DEL PARSER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse(Tokens,Estructura) donde Estructura representa la lista de tokens parseada correctamente

pl_program_parser(S) --> [program], identifier(_),[';'],statement(S).

statement((S;Ss);S3) --> [var], statement_var(S), rest_var(Ss), statement(S3). %Multiples variables
statement((S;S3)) --> [var], statement_var(S), statement(S3). % Unica variable

statement((S;Ss)) --> [begin] , statement(S), rest_statements(Ss).

statement(assign(X,V)) --> identifier(X) ,[':'],['='], expression(V).

statement(if(T,S1,S2)) --> [if],['('],test(T),[')'],[then] , statement(S1), [else] , statement(S2).
statement(if(T,S1)) --> [if],['('],test(T),[')'],[then],statement(S1).

statement(while(T,S)) --> [while],['('],test(T),[')'],[do],statement(S).

statement(for(T,S1,S2)) --> [for], expression(T), [to],expression(S1), [do], statement(S2).
statement(for(T,S1,S2)) --> [for], statement(T), [to],expression(S1), [do], statement(S2).

statement(read(X)) --> [read] ,['('],identifier(X),[')'].

statement(write(X)) --> [write] ,['('],expression(X),[')'].
statement(writeln(X)) --> [writeln] ,['('],expression(X),[')'].

statement_var(var(X)) --> identifier(X).
rest_var((S;Ss)) --> [';'], statement_var(S), rest_var(Ss).
rest_var(void) --> [';'].

rest_statements((S;Ss)) --> [';'] , statement(S), rest_statements(Ss).
rest_statements(void) --> [';'], [end], ['.'].
rest_statements(void) --> [';'] , [end].
rest_statements(void) --> [';'] , [end], [';'].
rest_statements(void) --> [end],['.'].
rest_statements(void) --> [end].

expression(X) --> pl_constant(X).
expression(expr(Op,X,Y)) --> pl_constant(X), arithmetic_op(Op), expression(Y).
arithmetic_op('+') --> ['+'].
arithmetic_op('-') --> ['-'].
arithmetic_op('*') --> ['*'].
arithmetic_op('/') --> ['/'].

pl_constant(X) --> pl_integer(X).
pl_constant(X) --> identifier(X).
pl_integer(number(X)) --> [X],{integer(X)}.
identifier(name(X)) --> [X], {atom(X)}.

test(compare(Op,X,Y)) --> expression(X), comparison_op(Op), expression(Y).
comparison_op('=') --> ['='].
comparison_op('<') --> ['<'].
comparison_op('>') --> ['>'].
comparison_op('>=') --> ['>'],['='].
comparison_op('<=') --> ['<'],['='].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PASO 3: MÁQUINA ABSTRACTA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Importamos la librería para la gestión del estado de la maquina
:- use_module(library(dicts)).

% 3.1.- Construir la lista de instrucciones a partir del árbol
parse_list(Estructura,List) :- compound(Estructura),compound_name_arguments(Estructura, ';', Arguments),
!, split_compound(Arguments,List).

parse_list(X,[X]) :- not(is_list(X)).

split_compound([],[]).
split_compound([T|Ts],L) :- parse_list(T,L1),split_compound(Ts,L2),append(L1,L2,L).

% 3.2.- Ir ejecutando instruccion por instruccion
computar_instr(Dicc,[],Dicc).
computar_instr(Dicc,[I|Is],DiccF) :- eval(Dicc,I,Dicc2),computar_instr(Dicc2,Is,DiccF).

% Proceso de evaluación de instrucciones
eval(Dicc,void,Dicc).

eval(Dicc,write(name(X)),Dicc) :- 
    (es_cadena_char(X) ->
        write(X)
        ;
        get_dict(X,Dicc,Val),write(Val)
    ).
eval(Dicc,writeln(name(X)),Dicc) :- 
    (es_cadena_char(X) ->
        writeln(X)
        ;
        get_dict(X,Dicc,Val),writeln(Val)
    ).

eval(Dicc,var(name(X)),DiccF) :- DiccF=Dicc.put(X,0). %Declaracion de variable
eval(Dicc,read(name(X)),DiccF) :- read(N),DiccF=Dicc.put(X,N).

eval(Dicc,assign(name(X),number(N)),DiccF) :- DiccF=Dicc.put(X,N).
eval(Dicc,assign(name(X),name(Y)),DiccF) :- get_dict(Y, Dicc, N), DiccF=Dicc.put(X,N).
eval(Dicc,assign(name(X),Expr),DiccF) :- calcular_expr(Expr,Dicc,N), DiccF=Dicc.put(X,N).

eval(Dicc,if(Cond,Acc),DiccF) :- (comparar_aux(Cond,Dicc) -> computar(Dicc,Acc,DiccF);DiccF=Dicc),!. % if sin else
eval(Dicc,if(Cond,Acc1,Acc2),DiccF) :- (comparar_aux(Cond,Dicc) -> computar(Dicc,Acc1,DiccF);computar(Dicc,Acc2,DiccF)),!. % if else

eval(Dicc,while(Cond,Ins),DiccF) :- comparar_aux(Cond,Dicc),computar(Dicc,Ins,Dicc2),eval(Dicc2,while(Cond,Ins),DiccF);DiccF=Dicc.

eval(Dicc,for(name(I),name(X),Ins),DiccF) :- 
    (comparar_aux(compare(<,name(I),name(X)),Dicc)
    -> computar(Dicc,Ins,D2),eval(D2,assign(name(I),expr(+,name(I),number(1))),D3),eval(D3,for(name(I),name(X),Ins),DiccF)
    ;
    DiccF = Dicc
    ).

eval(Dicc,for(assign(name(I),number(N)),name(X),Ins),DiccF) :- 
    eval(Dicc,assign(name(I),number(N)),D2),
    (comparar_aux(compare(<,name(I),name(X)),D2) 
    -> computar(D2,Ins,D3),eval(D3,assign(name(I),expr(+,name(I),number(1))),D4),eval(D4,for(name(I),name(X),Ins),DiccF)
    ;
    DiccF = Dicc
    ).
    
    
%Metodos auxiliares
calcular_expr(expr(Op,number(X),number(Y)),_,N) :- calcular(Op,X,Y,N).
calcular_expr(expr(Op,name(X),number(Y)),Dicc,N) :- get_dict(X,Dicc,Val),calcular(Op,Val,Y,N).
calcular_expr(expr(Op,number(X),name(Y)),Dicc,N) :- get_dict(Y,Dicc,Val),calcular(Op,X,Val,N).
calcular_expr(expr(Op,name(X),name(Y)),Dicc,N) :- get_dict(X,Dicc,ValX),get_dict(Y,Dicc,ValY),calcular(Op,ValX,ValY,N).
calcular_expr(expr(Op,name(X),Expr),Dicc,N) :- calcular_expr(Expr,Dicc,N1),get_dict(X,Dicc,Val),calcular(Op,Val,N1,N).
calcular_expr(expr(Op,number(X),Expr),Dicc,N) :- calcular_expr(Expr,Dicc,N1), calcular(Op,X,N1,N).

calcular(+,X,Y,N) :- N is X + Y.
calcular(-,X,Y,N) :- N is X - Y.
calcular(*,X,Y,N) :- N is X * Y.
calcular(/,X,Y,N) :- N is X / Y.

comparar_aux(compare(OpL,number(X),number(Y)),_) :- comparar(OpL,X,Y),!.
comparar_aux(compare(OpL,number(X),name(Y)),Dicc) :- get_dict(Y,Dicc,Val), comparar(OpL,X,Val),!.
comparar_aux(compare(OpL,name(X),number(Y)),Dicc) :- get_dict(X,Dicc,Val),comparar(OpL,Val,Y),!.
comparar_aux(compare(OpL,name(X),name(Y)),Dicc) :- get_dict(X,Dicc,ValX),get_dict(Y,Dicc,ValY), comparar(OpL,ValX,ValY),!.

comparar(=,X,Y) :- X =:= Y.
comparar(<,X,Y) :- X < Y.
comparar(>,X,Y) :- X > Y.
comparar(<=,X,Y) :- X =< Y.
comparar(>=,X,Y) :- X >= Y.

es_cadena_char(Atomo) :- atom_chars(Atomo, [Primero|_]),Primero == '\''.




