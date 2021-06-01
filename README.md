# Sostenuto 

El languaje esta estructurado de la siguiente manera:
```
composition <name>


<global var declaration>


<functions>

main
|>

<vardeclaration>

<|

```
## Declaración de variables
Para declarar variables se utiliza la palabra reservada var  y parentesis.
```
var ( <Type> <ID>  , .... )
```


## Declaración de funciones

``` 
func <ID> :: <args> -> <return type> 
|> 
<Statements> 
<return exp>
<|
```
## Tipos de dato
Los tipos de dato soportados son Number, String, Note, Bool y Silence (Sólo en algunos casos)

##Arreglos y Listas

Para declarar una lista o matriz es necesario usar corchetes cuadrados.

```
Number arr [2][5]
```

##Funciones especiales
Para desplegar a consola
```
display( arg1, arg2, ...)
```
Para recibir input
```
input(<type>)
```

## IF y IF ELSE

Para el IF se utiliza la siguiente estructura
```
if :: <exp> 
|> 

<|

else
|>

<|
```
El bloque de else es opcional.
Existe una forma diferente.
```
if :: <exp> -> <restricted statement>
```
Esta sintaxis solo soporta asignación y display.

## WHILE

```
while :: exp 
|>

<|
```
## Ritornello
Tiene un parecido al Repeat n times. Tiene dos formas.
```
[:

/* Esto se repite dos veces */
:]
```
O con expresiones
```
[: (n) ->
/* Esto se repite n veces*/
:]
```

## Play

Para usar el modulo de música tiene que tener este orden.

```
Streams(<numero>)

Play(<note>, <stream #>)
/* logica y muchos play despues */

MusicSheet()
```

Para introducir silencios se usa:
```
Play(Silence, <duration>, <stream #>)
```


## Operadores
+ , - , * , / , & , | , > , < , == , !=

## Asignación
```
a = b ;
```



