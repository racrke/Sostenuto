
# coding: utf-8

# In[1]:

#faltan comentarios
#Retorno de matrices
#Creo que el tipo de retorno de una funcion no se checa al retornar


import ply.lex as lex
import ply.yacc as yacc
import re
from VM import vm as VM


reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'print' : 'PRINTF',
    "Silence":"SILENCE",
    "var":"VAR",
    "func":"FUNC",
    "list":"LISTTYPE",
    "Number":"NUMBERTYPE",
    "Note":"NOTETYPE",
    "String" : "STRINGTYPE",
    "composition" : "COMP",
    "return": "RETURN",
    "main" : "MAIN",
    "Bool" : "BOOLTYPE",
    "input":"INPUT",
    "display":"DISP",
    "while":"WHILE",

    "Play": "PLAYT",
    "Stream":"STREAMT",
    "MusicSheet":"MST",
    "Midi":"MIDIT"
 }
 
tokens =["AND","OR","BOOL","COMMA","PLUS","MINUS","TIMES","DIVIDE","NUMBER","STRING","ID","NOTE",
"LIST","LPAR","RPAR","LARROW","RARROW","LTRIAN",
"RTRIAN","DDOT","SEMICOLON","LSQUARE","RSQUARE","EQ","ASSIGNATION","GT","LT",
"DIFF","LRITT","RRITT",
         "COLON",
] + list(reserved.values())




t_OR = r'&'
t_AND = r'\|'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAR = r'\('
t_RPAR = r'\)'
t_COMMA = r','
t_LARROW = r'\->'
t_RARROW = r'<\-'
t_LTRIAN = r'\|>'
t_RTRIAN = r'<\|'
t_DDOT = r'::'
t_COLON = r':'
t_SEMICOLON = r';'
t_LSQUARE = r'\['
t_RSQUARE = r'\]'
t_EQ = r'=='
t_ASSIGNATION = r'='
t_GT = r'>'
t_LT = r'<'
t_DIFF = r'!='
t_LRITT = r'\[:'
t_RRITT = r':\]'
#t_LHAPPY
#t_RHAPPY
#t_LCURLY = r'\{'
#t_RCURLY = r'\}'
t_ignore  = ' \t'

def t_NOTE(t):
    r'[A-Z][1-9][#b]?(.[1-9]\/[1-9])?'
    return t

def t_BOOL(t):
    r'(true)|(false)'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value,'ID')    
    return t
 
def t_NUMBER(t):
    r'([0-9]*[.])?[0-9]+'
    return t

def t_STRING(t):
    r'\"[a-zA-Z_][a-zA-Z0-9_]*\"'
    t.value = str(t.value)    
    return t


def t_error(t):
    print("Illegal character '%s'" % t.value[0], t)
    t.lexer.skip(1)
    raise SyntaxError
    



#
#  Constants / Commands of VM
#
#


_MOV_ = 0
_ADD_ = 1
_SUBS_= 2
_MULT_ = 3
_DIVIDE_ = 4
_LTHAN_ = 5
_GTHAN_  = 6
_EQ_= 7
_DIFF_ = 8
_AND_ = 9
_OR_ = 10
_GOTO_ = 11
_GOTOF_= 12
_GOTOV_= 13
_MOVH_ = 14
_PARAM_ = 15
_FUNC_ = 16
_ENDFUNC_ = 17
_CALL_ = 18
_NEW_ = 19
_DESTROY_ = 20
_RETURN_ = 21
_GETRET_ = 22
_INDEX_ = 23
_MOVI_ = 24
_PRINT_= 30
_PLAYN_ = 42
_PLAYC_ = 43
_PLAYS_ =44
_STREAM_ = 45
_MSHEET_ = 46
_MIDI_ = 47




reverse_op = {0:"MOV",1:"ADD",2:"SUBS",3:"MULT",4:"DIVIDE",5:"LTHAN",6:"GTHAN",7:"EQUALS",
8:"DIFF",9:"AND",10:"OR",11:"GOTO",12:"GOTOF",13:"GOTOV",14:"MOVH",15:"PARAM",16:"FUNC",
17:"ENDFUNC",18:"CALL",19:"NEW",20:"DESTROY",21:"RETURN",22:"GETRET",24:"MOVI",30:"PRINT",
 42:"PLAYN",43:"CHORD",44:"SILENCE",45:"STREAM",46:"MSHEET",47:"MIDI",23:"INDEX"}

op_dict = {
    "*" :_MULT_,"+":_ADD_,"-":_SUBS_,"/":_DIVIDE_,">":_GTHAN_,"<":_LTHAN_,"==":_EQ_,"!=":_DIFF_,"&":_AND_,"|":_OR_
}

lexer = lex.lex()


# # Parser



def p_main(p):
    """PROGRAM : COMP ID G1 F_VARDEC G2 F_FUNCDEC F1 MAIN_BLOCK"""
    p[0] = True
    
    
def p_F_FUNCDEC(p):
    """F_FUNCDEC : FUNCDEC F_FUNCDEC
                | e """
    p[0] = True


def p_VARDEC(p):
    """VARDEC : VAR LPAR TYPE ID VARIDB VARDECB RPAR"""
    global global_mode,vm
    if len(p[5]) > 2:
        print("Error: You cannot declare tridimensional arrays or higher")
        raise SyntaxError
    declare_var(p[4], p[3],p[5],global_mode,False)
    print("declaring...",p[4])
    pass

def p_VARID(p):
    """VARID : LSQUARE NUMBER RSQUARE VARIDB 
             """
    p[0] = [int(p[2])] + p[4]
def p_VARIDB(p):
    """VARIDB : LSQUARE NUMBER RSQUARE VARIDB 
             | e
             """
    if p[1] == None:
        p[0] = []
        return
    p[0] = [int(p[2])] + p[4]

def p_EXPVARID(p):
    """EXPVARID : LSQUARE C1 EXP C2 RSQUARE EXPVARIDB """
    global stack_prim, stack_type, gen_code

    type_t = stack_type.pop()
    value = stack_prim.pop()


    if type_t == "Number":    
        p[0] = [value] + p[6]
    else:
        print("Error: Index should be Number but got",type_t)
        raise SyntaxError
def p_EXPVARIDB(p):
    """EXPVARIDB : LSQUARE C1 EXP  C2 RSQUARE EXPVARIDB 
             | e
             """

    global stack_prim, stack_type
            
    if p[1] == None:
        p[0] = []
        return
    type_t = stack_type.pop()
    value = stack_prim.pop()
    if type_t == "Number":    
        p[0] = [value] + p[6]
    else:
        print("Error: Index should be Number but got",type_t)
        raise SyntaxError
    
def p_VARDECB(p):
    """VARDECB : COMMA TYPE ID VARIDB VARDECB 
               | e"""
    global global_mode
    #VARID access matrix
    if p[1] != None:
        declare_var(p[3],p[2],p[4],global_mode,False)
    pass
def p_FUNCDEC(p):
    """FUNCDEC : FUNC F1 AUXFUNCDEC  FUNCBLOCK"""
    global gen_code,stack_var, temp
    gen_code.append([ _ENDFUNC_ ])
    fill_goto()
    
    pass

def p_auxFUNDEC(p):
    """AUXFUNCDEC : ID DDOT F_ARGS LARROW RETTYPE"""
    global gen_code, stack_jump, temp,stack_param
    gen_code.append([_GOTO_,None,None])
    stack_jump.append(current_line())
    gen_code.append((_FUNC_,p[1]))
    temp = current_line()
    p[0] = p[1]
    declare_func(p[1], p[5], temp)
    stack_param = []


    
def p_F_ARGS(p):
    """F_ARGS : ARGS 
              | e"""
    p[0] = p[1]
def p_FUNCBLOCK(p):
    """FUNCBLOCK :  LTRIAN F_VARDEC STATEMENTS F_RETURN RTRIAN""" 
    p[0] = True

    destroy_local_context()
def p_F_VARDEC(p):
    """F_VARDEC : VARDEC 
                | e"""
    p[0] = True
def p_F_RETURN(p):
    """F_RETURN : RETURN EXP SEMICOLON 
                | e"""
    global gen_code,stack_prim, stack_type
    if p[1] != None:
        value = stack_prim.pop() #Exp address
        datatype = stack_type[-1]
        #Validacion de tipo de retorno

        gen_code.append((_RETURN_,value))
    pass                

    p[0] = True
def p_STATEMENTS(p):
    """STATEMENTS :  STATEMENT N0 STATEMENTS 
                  | e"""
    p[0] = True

def p_N0(p):
    """N0 : e"""

    global stack_type, stack_prim, stack_sec

    stack_type = []
    stack_prim = []
    stack_sec = []

def p_MAIN(p):
    """MAIN_BLOCK : MAIN FUNCBLOCK"""
    pass
    
def p_STATEMENT(p):
    """STATEMENT : ASSIGN SEMICOLON
                | FUNCCALL SEMICOLON
                | WHILE_BLOCK
                | RITORNELLO
                | IF_BLOCK
                | PRINT SEMICOLON
                | PLAYFUNCS SEMICOLON
                | MUSICFUNCS SEMICOLON
                """
    p[0] = True
def p_ARGS(p): #FUNCTION DECLARATION ARGS
    """ARGS : TYPE ID H_ARGS"""
    global stack_param
    p[0] = True

    #No toma arreglos
    declare_var(p[2], p[1],[],False,True)
    stack_param.append((p[2],get_var(p[2],[]),p[1])) #ID, Adress,Type <String> No soporta matriz

    
def p_H_ARGS(p):
    """H_ARGS : COMMA ARGS 
              | e"""
    
    p[0] = "" if p[1] == None else p[2]
def p_CALLARGS(p):
    """CALLARGS : C1 EXP C2 H_CALLARGS 
                | e"""
    global stack_param, stack_prim, stack_type,gen_code
    if p[1] != None:
        value = stack_prim.pop()
        type_t = stack_type.pop()
        stack_param.append((value,type_t))

    pass

def p_C1(p):
    """C1 : e"""
    global stack_sec
    stack_sec.append("[")
    p[0] = True

def p_C2(p):
    """C2 : e"""
    global stack_sec
    value = stack_sec.pop()

def p_H_CALLARGS(p):
    """H_CALLARGS :  COMMA CALLARGS
                  | e"""
    pass
    
def p_FUNCCALL(p):
    """FUNCCALL : ID LPAR CALLARGS RPAR """

    global stack_param, gen_code, global_func,stack_type
    param_i = 0

    if len(stack_param) < len(global_func[p[1]]["params_order"]):
        print("Function",p[1],"requires)",len(global_func[p[1]]["params_order"]),"params but got",len(stack_param))
        raise SyntaxError

    for param in stack_param: #Stack param Call2
        address = param[0]
        type_t = param [1]
        type_param_dec = global_func[p[1]]["params_order"][param_i]

        if type_t != type_param_dec:
            print("Error: Type mistmach, expected",type_param_dec, "but got",type_t)
            raise SyntaxError

        gen_code.append((_PARAM_,address,global_func[p[1]]["address_order"][param_i])) #From to
        param_i += 1
    stack_param = []
    gen_code.append((_CALL_,global_func[p[1]]["line"]))
    print("NEW CONTX")
    p[0] = "<Funccall>"

    ret_type = global_func[p[1]]["return"]
    if ret_type != "Silence":
        c_space = vm.request("local",ret_type)
        gen_code.append((_GETRET_,c_space))
        stack_prim.append(c_space)
        stack_type.append(ret_type)
    else:
        stack_prim.append(None)
        stack_type.append(ret_type)

#def p_READ(p):
 #   """READ : INPUT LPAR CALLARGS RPAR """
  #  p[0] = True
def p_PRINT(p):
    """PRINT :  DISP LPAR EXP RPAR """
    global gen_code

    value = stack_prim.pop()
    type_p = infer_type(value)
    gen_code.append((_PRINT_,value))

    p[0] = True


def p_MUSICFUNCS(p):
    """MUSICFUNCS : STREAMT LPAR EXP RPAR 
                    | MIDIT LPAR RPAR 
                    | MST LPAR RPAR """
    global gen_code, stack_prim, stack_type
    if p[1] == "Stream":

        type_t = stack_type.pop()
        value_t = stack_prim.pop()
        if type_t != "Number":
            print("Error: Expected number in Stream()")
            raise SyntaxError
        gen_code.append((_STREAM_, value_t ))

    if p[1] == "Midi":
        gen_code.append([_MIDI_])
    if p[1] == "MusicSheet":
        gen_code.append([_MSHEET_])

#e.g play(note,1) , play(silence,4,1), play(chord,2)
def p_PLAY(p):
    """PLAYFUNCS : PLAYT LPAR EXP COMMA EXP RPAR 
                | PLAYT LPAR SILENCE COMMA EXP COMMA EXP RPAR """
    global stack_type, stack_prim, gen_code
    if p[3] == "Silence":
        type_stream = stack_type.pop()
        type_note = stack_type.pop()

        if type_stream != "Number":
            print("Error: Expected Number in Stream param")
            raise SyntaxError

        value_stream = stack_prim.pop()
        value_duration = stack_prim.pop()

        gen_code.append((_PLAYS_,value_duration,value_stream))
    else:
        type_stream = stack_type.pop()
        type_note = stack_type.pop()

        if type_stream != "Number":
            print("Error: Expected Number in Stream param")
            raise SyntaxError 

        if type_note == "*Note":
            pass
        elif type_note =="Note":
            value_stream = stack_prim.pop()
            value_note = stack_prim.pop()
            gen_code.append((_PLAYN_,value_note,value_stream))
        else: 
            print("Error: Expected Note or *Note")
            raise SyntaxError 



    p[0] = True

def p_TYPE(p):
    """TYPE :  NUMBERTYPE 
            | LISTTYPE
            | NOTETYPE 
            | BOOLTYPE
            | STRINGTYPE"""
    p[0] = p[1]
    
def p_RETTYPE(p):
    """RETTYPE :  NUMBERTYPE 
                | LISTTYPE
                | NOTETYPE 
                | BOOLTYPE 
                | SILENCE"""
    p[0] = p[1]
def p_BLOCK(p):
    """BLOCK : LTRIAN STATEMENTS RTRIAN"""
    p[0] = True
def p_ASSIGN(p):
    """ASSIGN : ID EXPVARIDB ASSIGNATION EXP"""
    global stack_prim,gen_code,stack_type,stack_var, vm
    type_p = stack_type.pop()

    if len(p[2]) > 0:
        address = get_var(p[1],p[2])
        value = stack_prim.pop()
        if type_p == get_type(p[1],p[2]):

            sp = vm.get_scope(address)
            type_result = vm.get_datatype(address,sp)
            if type_result in ("*Number","*Note"):
                gen_code.append((_MOVI_, address, value)) #Indirect modification
            else:
                gen_code.append((_MOV_, address, value))
            vm.write(address,value)
        else:
            print("Error: Type mistmach",p[1])
            raise SyntaxError

    else:
        if type_p == get_type(p[1],p[2]):
            set_var(p[1],stack_prim.pop())
        else:
            print("Error: Type mistmach",p[1])
            raise SyntaxError
def p_IF(p):
    """IF_BLOCK : IF DDOT EXP A1 IF_B """
    p[0] = True
    
def p_A1(p):
    """A1 : e"""
    global stack_prim, stack_jump,gen_code 
    
    last_value = stack_prim.pop()
    type_t = stack_type.pop()

    if type_t == "Bool":
        gen_code.append([_GOTOF_,last_value,None])
        line = current_line()
        stack_jump.append(line)
    else:
        print("Error: Type mistmach at IF")
        raise SyntaxError

def p_A2(p):
    """A2 : e"""
    fill_goto()
    
def fill_goto():
    global stack_prim, stack_jump,gen_code 
    
    line_to_fill = stack_jump.pop()
    gen_code[line_to_fill][2] = current_line() + 1
    
def p_A3(p):
    """A3 : e"""
    global stack_prim, stack_jump,gen_code 
    gen_code.append([_GOTO_,None,None])
    
def p_A3B (p):
    """A3B : e"""
    global stack_prim, stack_jump,gen_code 
    line = current_line()
    stack_jump.append(line)
    
def p_IF_B(p):
    """IF_B : BLOCK F_ELSE A2
            | LARROW IF_ARROW SEMICOLON A2
            """
    p[0] = True
    
def p_IF_ARROW(p):
    """IF_ARROW : FUNCCALL 
                | ASSIGN 
                """
    p[0] = True
    
def p_F_ELSE(p):
    """F_ELSE : ELSE A3 A2 A3B ELSE_BLOCK 
              | e """
    p[0] = True
    
def p_ELSE_BLOCK(p):
    """ELSE_BLOCK : BLOCK 
                  | IF_BLOCK"""
    p[0] = True
    
def p_WHILE(p):
    """WHILE_BLOCK : WHILE W1 DDOT EXP A1 BLOCK W2"""
    p[0] = True
    
def p_W1(p):
    """W1 : e"""
    global stack_jump
    stack_jump.append(current_line()+1)
    
def p_W2(p):
    """W2 : e"""
    global stack_prim, stack_jump,gen_code #ELSEGOTO
    line_to_fill = stack_jump.pop()
    gen_code[line_to_fill][2] = current_line() + 2
    begin_while = stack_jump.pop()
    gen_code.append([_GOTO_,None,begin_while])
    
def p_RITORNELLO(p):
    """RITORNELLO : LRITT RITARG STATEMENTS RRITT """
    
    global stack_prim, stack_jump,gen_code 
    line_to_fill = stack_jump.pop()
    gen_code[line_to_fill][2] = current_line() + 2
    begin_while = stack_jump.pop()
    gen_code.append([_GOTO_,None,begin_while])
    
    p[0] = True

def p_RIT_ARG(p):
    """RITARG : LPAR EXP RPAR LARROW 
                | e"""
    global vm, gen_code,stack_jump,stack_prim,stack_type
    temp = vm.request("local","Number") 
    temp2 = vm.request("local","Number")
    temp3 = vm.request("local","Number")
    temp4 = vm.request("local","Number")

    if len(p[1:]) != 4:
        times = 2
        gen_code.append((_MOVH_,temp,times))
    else:

        times = stack_prim.pop()
        type_t = stack_type.pop()
        if type_t != "Number":
            print("Error: Ritornello needs a number, received",type_t)
            raise SyntaxError 
        gen_code.append((_MOV_,temp,times))
    gen_code.append((_MOVH_,temp2,-1))
    gen_code.append((_MOVH_,temp4,1))
    gen_code.append((_ADD_,temp2,temp4,temp2))
    stack_jump.append(current_line())
    gen_code.append((_LTHAN_,temp2,temp,temp3))
    gen_code.append([_GOTOF_,temp3,None])
    stack_jump.append(current_line())

def p_EXP(p):
    """EXP : EXPR EXP2 P7"""
    p[0] = True
    pass
def p_p7(p):
    """P7 : e"""
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in [">","<","==","!=","&","|"]:
        generate_code()
    
def p_EXP2(p):
    """EXP2 : e
            | EXP2_ops EXPR 
            """
            
    if p[1] == None:
        p[0] = ""
        return

def p_EXP2_ops(p):
    """EXP2_ops : GT 
            | LT 
            | DIFF 
            | EQ
            | OR 
            | AND 
            """
    global stack_sec
    stack_sec.append(p[1])
    
def p_EXPR(p):
    """EXPR : TERM P4 EXPR_B"""
    

def p_P4(p):
    """P4 : e"""
    
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in ["+","-"]:
        generate_code()
        
def p_EXPR_B(p):
    """EXPR_B : EXPR_B_OPS EXPR 
              | e"""
    if p[1] == None:
        p[0] = ""
        return

    
def p_EXPR_B_OPS(p):
    """EXPR_B_OPS : PLUS 
                  | MINUS"""
    global stack_sec
    stack_sec.append(p[1])

    
def p_TERM(p):
    """TERM : FACTOR P5 TERM_B"""
    pass

def p_P5(p):
    """P5 : e"""
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in ["*","/"]:
        generate_code()
        
def p_TERM_B(p):
    """TERM_B : TERM_B_OPS TERM 
              | e"""
    if p[1] == None:
        p[0] = ""
        return

    
def p_TERM_B_OPS(p):
    """TERM_B_OPS : TIMES
                 | DIVIDE"""
    global stack_sec
    stack_sec.append(p[1])

def p_FACTORVARID(p):
    """IDVARID : ID EXPVARID """
    global stack_prim, stack_sec, stack_type,vm,gen_code
    type_t = get_type(p[1],p[2])
    address = get_var(p[1],p[2])
    stack_prim.append(address)
    stack_type.append(type_t)
    p[0] = "<ID varid>"



def p_ARRAY(p):

    """ARRAY : LSQUARE C1 EXP C2 ARRAYB RSQUARE""" 
    global stack_array, gen_code, vm, stack_prim,stack_type

    print("ARRAY",len(stack_prim),stack_prim)
    
    stack_prim.reverse()
    stack_type.reverse()

    size = len(stack_prim)
    value_t = stack_prim.pop()
    first_type = stack_type.pop()

    origin,obj = vm.request_array("local","*"+first_type,first_type,size)

    gen_code.append((_MOVH_,origin,obj))
    gen_code.append((_MOV_,obj[0],value_t))

    print(origin,obj)
    for i in range(len(stack_prim)):
        value_t = stack_prim.pop()
        type_t = stack_type.pop()

        if type_t != first_type:
            print("Error: Array must be the same type")
            raise SyntaxError

        gen_code.append((_MOV_,1 + i + obj[0],value_t))

    stack_prim.append(origin)
    stack_type.append("*"+first_type)
    p[0] = "<list>"
    


def p_ARRAYB(p):

    """ARRAYB : COMMA C1 EXP C2 ARRAYB
             |  e    """  

def p_FACTOR(p):
    """FACTOR :  NUMBER 
              | ID
              | FACTOR_CTE 
              | LPAR P6 EXP RPAR 
              | IDVARID 
              | FUNCCALL
              | VARCTE
              | ARRAY
              """
    global stack_prim, stack_sec, stack_type,vm,gen_code

    if p[1] == "(":
        stack_sec.pop() #At the end of (exp) pop ")" , only when first is "(" 
        return


    if p[1] in ("<Funccall>","<ID varid>" ,"<factorcte>","<list>"):
        #Type remains in stacktype
        return
    inferred_type = infer_type(p[1])


    if inferred_type == "ID":
        stack_prim.append(get_var(p[1],[])) 
        stack_type.append(get_type(p[1],[]))
    elif inferred_type == "String" :
        c_space = vm.request("local","String")
        gen_code.append((_MOVH_,c_space,p[1]))
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    elif inferred_type == "Note":
        c_space = vm.request("local","Note")
        gen_code.append((_MOVH_,c_space,p[1]))
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    elif inferred_type == "Bool":
        c_space = vm.request("local","Bool")

        if p[1] == "true":
            b_value = True
        else:
            b_value = False

        gen_code.append((_MOVH_,c_space,b_value)) 
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    elif inferred_type == "Number":   
        c_space = vm.request("local","Number")
        gen_code.append((_MOVH_,c_space,float(p[1])))
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    else:
        print("Not implented")
        raise SyntaxError
        
def p_P6(p):
    """P6 : e"""
    global stack_sec
    stack_sec.append("(")
    
    
def p_FACTOR_CTE(p):
    """FACTOR_CTE :  PLUS NUMBER  
                  | MINUS NUMBER """
    global stack_prim, stack_sec, stack_type,vm,gen_code

    c_space = vm.request("local","Number")

    times = 1 if p[1] == "+" else -1
    gen_code.append((_MOVH_,c_space,float(p[2])*times))
    stack_prim.append(c_space)
    stack_type.append("Number")
    p[0] = "<factorcte>"
    
def p_VARCTE(p):
    """VARCTE : NOTE 
              | BOOL
              | STRING"""
    p[0] = str(p[1])
    return
   
def p_empty(p):
    """e : """





#semantic cube

cube = {
    "Number" : {
        "Number": { 
            "+" : "Number",
            "-" : "Number",
            "/" : "Number",
            "*" : "Number",
            "!=" : "Bool",
            "==" : "Bool",
            ">" : "Bool",
            "<" : "Bool"
        }
    },
    "String": {
        "String": {
            "+":"String",
            "!=":"Bool",
            "==":"Bool"
        }
    },
    "Bool": {
        "Bool": {
            "&": "Bool",
            "|": "Bool",
            "!=": "Bool",
            "==": "Bool",
        }
    },
    "Note": {
        "Number":{
            "+":"Note",
            "-":"Note",
            "!=":"Bool",
            "==":"Bool"
        },
        "Note":{
            "!=":"Bool",
            "==":"Bool"
        }
    },
    "*Number":{
        "Number":{
            "+":"*Number",
            "-":"*Number",
            "/":"*Number",
            "*":"*Number"
        },
        "*Number":{
            "+":"*Number"
        }
    },
    "*Note":{
        "Number":{
            "+":"*Note"
        },
        "Note":{
            "+":"*Note"
        },
        "*Note":{
            "+":"*Note"
        }
    },
    "**Number":{
        "**Number":{
            "+":"**Number",
            "-":"**Number",
            "/":"**Number",
            "*":"**Number",
        }
    }
}

def generate_code():
    global stack_sec, stack_prim, temp_mem,vm,op_dict,gen_code, stack_type
    op = stack_sec.pop()
    op_right = stack_prim.pop()
    op_left = stack_prim.pop()
    type_right = stack_type.pop()
    type_left = stack_type.pop()
    new_op = is_valid_binary_ops(type_left,type_right,op)
    if new_op != None:
        address = vm.request("local",new_op)
        gen_code.append((op_dict[op],op_left,op_right,address))
        stack_prim.append(address)
        stack_type.append(new_op)
        temp_mem.append(address)
    else:
        print("Error: Type mismatch")
        raise SyntaxError
    
def is_valid_binary_ops(opleft,opright,op):
    global cube
    if opleft in cube:
        if opright in cube[opleft]:
            if op in cube[opleft][opright]:
                return cube[opleft][opright][op]
    return None
    
def infer_type(string):
    note_pattern = '[A-Z][1-9][#b]?(.[1-9]\/[1-9])?'
    number_pattern =  '[0-9]+(.[0-9]+)?'
    id_pattern = '[a-zA-Z_][a-zA-Z0-9_]*'
    bool_pattern = '^(true)|(false)$'
    string_pattern = '\"[a-zA-Z_][a-zA-Z0-9_]*\"'

    if type(string) == float or type(string) == int:
        return "Number"
    if re.match(note_pattern, string):
        return "Note"
    if re.match(number_pattern, string):
        return "Number"
    if re.match(bool_pattern, string):
        return "Bool"
    if re.match(id_pattern, string):
        return "ID"
    if re.match(string_pattern, string):
        return "String"
    return None
def current_line():
    global gen_code
    return len(gen_code) - 1




#Erase local var directory
def p_f1(p):
    """ F1 : e"""
    global gen_code
    new_local_context()
    #gen_code.append([_NEW_])

def p_f2(p):
    """ F2 : e"""
    destroy_local_context()
    #gen_code.append([_DESTROY_])


def p_g1(p):
    """ G1 : e"""
    global global_mode
    global_mode = True
    
def p_g2(p):
    """ G2 : e"""
    global global_mode
    global_mode = False





def p_error(p):
    print("Syntax error in input!",p)
    raise SyntaxError

def is_float(s):
    try: 
        float(s)
        return True
    except ValueError:
        return False



def get_var(key,dim):
    global global_var, stack_var,vm, gen_code
    value = None
    found = False
    if len(stack_var) > 0:
        if key in stack_var[-1]:
            if len(dim) == 2:
                c_space = vm.request("local",stack_var[-1][key]["type"][1:])#Assumes **datatype
                gen_code.append((_INDEX_,stack_var[-1][key]["address"],dim[0],c_space))
                gen_code.append((_INDEX_,c_space,dim[1],c_space))
                value = c_space
                found = True
            elif len(dim) == 1:
                c_space = vm.request("local",stack_var[-1][key]["type"])#Assumes *datatype
                gen_code.append((_INDEX_,stack_var[-1][key]["address"],dim[0],c_space))
                value = c_space
                found = True
            else:
                value = stack_var[-1][key]["address"]
                found = True
    
    if key in global_var:
            if len(dim) == 2:
                c_space = vm.request("local",global_var[key]["type"][1:])#Assumes **datatype
                gen_code.append((_INDEX_,global_var[key]["address"],dim[0],c_space))
                gen_code.append((_INDEX_,c_space,dim[1],c_space))
                value = c_space
                found = True
            elif len(dim) == 1:
                c_space = vm.request("local",global_var[key]["type"])#Assumes *datatype
                gen_code.append((_INDEX_,global_var[key]["address"],dim[0],c_space))
                value = c_space
                found = True
            else:
                value = global_var[key]["address"]
                found = True
        
    if not found:
        print("Error: undeclared variable",key, "on get_var")
        raise SyntaxError
        
    return value

def set_var(key, value, is_global=False):
    global global_var, stack_var, gen_code,vm
    

    undeclared = True
    if len(stack_var) > 0:
        if key in stack_var[-1]:
            stack_var[-1][key]["value"] = value
            undeclared = False
            gen_code.append((_MOV_, stack_var[-1][key]["address"],value))
            vm.write(stack_var[-1][key]["address"],value)
    if key in global_var:
        global_var[key]["value"]  = value
        undeclared = False
        gen_code.append((_MOV_, global_var[key]["address"],value))
        vm.write(global_var[key]["address"],value)
    if undeclared:
        print("Error: undeclared variable",key, "on set_var")
        raise SyntaxError
    return value

def declare_var(key,type_var,dim,is_global=False,is_args=False):
    global global_var, stack_var, vm, gen_code
    
    if is_args:
        if key in stack_var[-1]:
            print("Error: Argument already declared", key)
            raise SyntaxError
        else:
            address = vm.request("local",type_var)
            print("DECLARE",address)
            vm.push_param(address,None)
            prefix = "*"*len(dim)
            stack_var[-1][key] = {"address":address,"type":prefix+type_var,"dim":dim}
        return

    if is_global:
        if key in global_var:
            print("Error: Already declared", key)
            raise SyntaxError
        else:
            prefix = "*"*len(dim)

            if prefix == "":
                address = vm.request("global",prefix+type_var)
            if prefix == "*":
                if dim[0] < 1: #Pointer declaration
                    address = vm.request("local",prefix+type_var)
                    gen_code.append((_MOVH_,address,None))
                else:
                    address,obj = vm.request_array("local",prefix+type_var,type_var,dim[0])
                    gen_code.append((_MOVH_,address,obj))
                    for i in range(dim[0]):
                        gen_code.append((_MOVH_,obj[0]+i,None))


            if prefix == "**":
                if dim[0] < 1 or dim[1] < 1:
                    print("Erro: Cannot declare matrix with less than one cell")
                    raise SyntaxError

                address,obj = vm.request_matrix("global",prefix+type_var,type_var,dim[0],dim[1])
                gen_code.append((_MOVH_,address,obj))
                for i in range(dim[0]):
                    obj2 = vm.read(obj[0]+i)
                    gen_code.append((_MOVH_,obj[0]+i,obj2))
                    for j in range(dim[1]):
                        gen_code.append((_MOVH_,obj2[0]+j,None))

            global_var[key] = {"address":address,"type":prefix+type_var,"dim":dim}
                
    else:
        if key in stack_var[-1]:
            print("Error: Already declared", key)
            raise SyntaxError
        else:
            prefix = "*"*len(dim)

            if prefix == "":
                address = vm.request("local",prefix+type_var)
            if prefix == "*":
                if dim[0] < 1:
                    print("Erro: Cannot declare array with less than one cell")
                    raise SyntaxError
                address,obj = vm.request_array("local",prefix+type_var,type_var,dim[0])
                gen_code.append((_MOVH_,address,obj))
                for i in range(dim[0]):
                    gen_code.append((_MOVH_,obj[0]+i,None))


            if prefix == "**":
                if dim[0] < 1 or dim[1] < 1:
                    print("Erro: Cannot declare matrix with less than one cell")
                    raise SyntaxError

                address,obj = vm.request_matrix("local",prefix+type_var,type_var,dim[0],dim[1])
                gen_code.append((_MOVH_,address,obj))
                for i in range(dim[0]):
                    obj2 = vm.read(obj[0]+i)
                    gen_code.append((_MOVH_,obj[0]+i,obj2))
                    for j in range(dim[1]):
                        gen_code.append((_MOVH_,obj2[0]+j,None))
                

            stack_var[-1][key] = {"address":address,"type":prefix+type_var,"dim":dim}
        
    
def new_local_context():
    global stack_var
    vm.new_context()
    stack_var.append({})

def destroy_local_context():
    global stack_var,vm
    addresses = [stack_var[-1][i]["address"] for i in stack_var[-1]]
    stack_var.pop() 
    vm.destroy_context()

def get_type(key,dim):
    global global_var, stack_var
    
    value = None
    if len(stack_var) > 0:
        if key in stack_var[-1]:      
            if len(dim) == 2:
                if stack_var[-1][key]["type"][:2] =="**":
                    value = stack_var[-1][key]["type"][2:]
                else:
                    print("Error: type is not matrix")
                    raise SyntaxError
            elif len(dim) == 1:
                if stack_var[-1][key]["type"][:1] =="*":
                    value = stack_var[-1][key]["type"][1:]
                else:
                    print("Error: type is not array")
                    raise SyntaxError
                
            else:
                value = stack_var[-1][key]["type"]
      
    
    if key in global_var:
        if len(dim) == 2:
            if global_var[key]["type"][:2] =="**":
                value = global_var[key]["type"][2:]
            else:
                print("Error: type is not matrix")
                raise SyntaxError
        elif len(dim) == 1:
            if global_var[key]["type"][:1] =="*":
                value = global_var[key]["type"][1:]
            else:
                print("Error: type is not array")
                raise SyntaxError
                
        else:
            value = global_var[key]["type"]
    
    if value == None:
        print("Error: undeclared variable",key, "on get_type")
        raise SyntaxError
        
    return value
    
def declare_func(key,return_type,line):
    global global_func, stack_param
    
    if key in global_func:
        print("Error: Function already declared",global_func)
        raise SyntaxError
    
    signature = "acn" #FIXME
    params_info = {}
    params_order = []
    address_order = []

    for i in stack_param: #ID, address local, type
        params_info[i[0]] = i[1]
        params_order.append(i[2])
        address_order.append(i[1])

    global_func[key] = {"return":return_type,"signature":signature,
        "params_info":params_info,"line":line,"params_order":params_order,"address_order":address_order}
    





global_mode = True
stack_var = [{}]
global_var = {}
global_func = {}
vm = None
temp = None
vm = VM.VirtualMachine()
stack_prim = []
stack_sec = []
stack_type = []
stack_jump = []
stack_param = []
temp_mem = []
gen_code = []
stack_array = []
start = "PROGRAM"
parser = yacc.yacc()
with open('input.sostenuto', 'r') as file:
    s = file.read().replace("\n"," ").replace("\t"," ")
result = parser.parse(s)

for i in [(x,reverse_op[y[0]],y[1:])for x,y in zip(range(len(gen_code)),gen_code)]:
    print(i)

print("Executing...")
vm = None
vm = VM.VirtualMachine()
vm.calculate(gen_code)

