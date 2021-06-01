
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
    "Silence":"SILENCE",
    "var":"VAR",
    "func":"FUNC",
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
    "MusicSheet":"MST"
 }
 
tokens =["AND","OR","BOOL","COMMA","PLUS","MINUS","TIMES","DIVIDE","NUMBER","STRING","ID","NOTE",
"LIST","LPAR","RPAR","LARROW","RARROW","LTRIAN",
"RTRIAN","DDOT","SEMICOLON","LSQUARE","RSQUARE","EQ","ASSIGNATION","GT","LT",
"DIFF","LRITT","RRITT",
         "COLON",
] + list(reserved.values())




t_AND = r'&'
t_OR = r'\|'
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
t_ignore  = ' \t'

def t_COMMENT(t):
    r'(?s)/\*.*?\*/'
    pass

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
    r'"(\.|[^"])*"'
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
_READ_ = 25
_PRINT_= 30
_PLAYN_ = 42
_PLAYC_ = 43
_PLAYS_ =44
_STREAM_ = 45
_MSHEET_ = 46
_MIDI_ = 47

_NUMBER = 1
_NOTE = 2
_BOOL = 3
_STRING = 4
_NUMBER_POINTER = 5
_NOTE_POINTER = 6 
_NUMBER_MATRIX = 7


reverse_op = {0:"MOV",1:"ADD",2:"SUBS",3:"MULT",4:"DIVIDE",5:"LTHAN",6:"GTHAN",7:"EQUALS",
8:"DIFF",9:"AND",10:"OR",11:"GOTO",12:"GOTOF",13:"GOTOV",14:"MOVH",15:"PARAM",16:"FUNC",
17:"ENDFUNC",18:"CALL",19:"NEW",20:"DESTROY",21:"RETURN",22:"GETRET",24:"MOVI",30:"PRINT",
 42:"PLAYN",43:"CHORD",44:"SILENCE",45:"STREAM",46:"MSHEET",47:"MIDI",23:"INDEX",25:"READ",}

op_dict = {
    "*" :_MULT_,"+":_ADD_,"-":_SUBS_,"/":_DIVIDE_,">":_GTHAN_,"<":_LTHAN_,"==":_EQ_,"!=":_DIFF_,"&":_AND_,"|":_OR_
}

lexer = lex.lex()

##
#  PARSER RULES SECTION
##


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


    if type_t == _NUMBER:    
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
    if type_t == _NUMBER:    
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
    """F_RETURN : RETURN EXPRESSION SEMICOLON 
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
                | READ SEMICOLON
                """
    p[0] = True

def p_ARGS(p): #FUNCTION DECLARATION ARGS
    """ARGS : TYPE e ID  H_ARGS"""
    global stack_param
    p[0] = True

    #No toma arreglos
    declare_var(p[3], p[1],[],False,True)
    stack_param.append((p[3],get_var(p[3],[]),get_type(p[3],[]))) 

    
def p_H_ARGS(p):
    """H_ARGS : COMMA ARGS 
              | e"""
    
    p[0] = "" if p[1] == None else p[2]

def p_CALLARGS(p):
    """CALLARGS : C1 EXPRESSION C2 H_CALLARGS 
                | e"""
    global stack_param, stack_prim, stack_type,gen_code
    if p[1] != None:
        value = stack_prim.pop()
        type_t = stack_type.pop()
        print("CALLARGS",value,type_t)
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

def p_READ(p):
    """READ : INPUT LPAR READAUX RPAR """
    global vm, gen_code, stack_prim, stack_type
    c_space = vm.request("local",trans_type(p[3]))
    gen_code.append((_READ_,c_space))
    stack_prim.append(c_space)
    stack_type.append(trans_type(p[3]))
    p[0] = "<Read>"

def p_READAUX(p):
    """READAUX : NUMBERTYPE 
                | STRINGTYPE 
                | NOTETYPE """
    p[0] = p[1]

def p_PRINT(p):
    """PRINT :  DISP LPAR CALLARGS RPAR """
    global gen_code,stack_param

    for value,type_t in stack_param[::-1]:
        gen_code.append((_PRINT_,value))
    stack_param = []
    p[0] = True


def p_MUSICFUNCS(p):
    """MUSICFUNCS : STREAMT LPAR EXP RPAR 
                    | MST LPAR RPAR """
    global gen_code, stack_prim, stack_type
    if p[1] == "Stream":

        type_t = stack_type.pop()
        value_t = stack_prim.pop()
        if type_t != _NUMBER:
            print("Error: Expected number in Stream()")
            raise SyntaxError
        gen_code.append((_STREAM_, value_t ))

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

        if type_stream != _NUMBER:
            print("Error: Expected Number in Play param")
            raise SyntaxError

        value_stream = stack_prim.pop()
        value_duration = stack_prim.pop()

        gen_code.append((_PLAYS_,value_duration,value_stream))
    else:
        type_stream = stack_type.pop()
        type_note = stack_type.pop()

        if type_stream != _NUMBER:
            print("Error: Expected Number in Play param")
            raise SyntaxError 

        if type_note == _NOTE_POINTER:
            value_stream = stack_prim.pop()
            value_note = stack_prim.pop()
            gen_code.append((_PLAYC_,value_note,value_stream))
        elif type_note ==_NOTE:
            value_stream = stack_prim.pop()
            value_note = stack_prim.pop()
            gen_code.append((_PLAYN_,value_note,value_stream))
        else: 
            print("Error: Expected Note or *Note")
            raise SyntaxError 

    p[0] = True

def p_TYPE(p):
    """TYPE :  NUMBERTYPE 
            | NOTETYPE 
            | BOOLTYPE
            | STRINGTYPE"""
    p[0] = p[1]
    
def p_RETTYPE(p):
    """RETTYPE :  NUMBERTYPE 
                | NOTETYPE 
                | BOOLTYPE 
                | SILENCE"""
    p[0] = p[1]

def p_BLOCK(p):
    """BLOCK : LTRIAN STATEMENTS RTRIAN"""
    p[0] = True

def p_ASSIGN_HELPER(p):
    """ASSIGNH : EXPRESSION 
                | ARRAY"""

def p_ASSIGN(p):
    """ASSIGN : ID EXPVARIDB ASSIGNATION ASSIGNH"""
    global stack_prim,gen_code,stack_type,stack_var, vm
    type_p = stack_type.pop()

    if len(p[2]) > 0:
        address = get_var(p[1],p[2])
        value = stack_prim.pop()
        if type_p == get_type(p[1],p[2]):

            sp = vm.get_scope(address)
            type_result = vm.get_datatype(address,sp)
            if type_result in (_NUMBER_POINTER,_NOTE_POINTER):
                gen_code.append((_MOVI_, address, value)) #Indirect modification
            else:
                gen_code.append((_MOV_, address, value))
            vm.write(address,value)
        else:
            print("Error: Type mistmach on assignation",p[1])
            raise SyntaxError

    else:
        if type_p == get_type(p[1],p[2]):
            set_var(p[1],stack_prim.pop())
        else:
            print("Error: Type mistmach on assignation",p[1])
            raise SyntaxError

def p_IF(p):
    """IF_BLOCK : IF DDOT EXPRESSION A1 IF_B """
    p[0] = True
    
def p_A1(p):
    """A1 : e"""
    global stack_prim, stack_jump,gen_code 
    
    last_value = stack_prim.pop()
    type_t = stack_type.pop()

    if type_t == _BOOL:
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
                | PRINT
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
    """WHILE_BLOCK : WHILE W1 DDOT EXPRESSION A1 BLOCK W2"""
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
    """RITARG : LPAR EXPRESSION RPAR LARROW 
                | e"""
    global vm, gen_code,stack_jump,stack_prim,stack_type
    temp = vm.request("local",_NUMBER) 
    temp2 = vm.request("local",_NUMBER)
    temp3 = vm.request("local",_NUMBER)
    temp4 = vm.request("local",_NUMBER)

    if len(p[1:]) != 4:
        times = 2
        gen_code.append((_MOVH_,temp,times))
    else:

        times = stack_prim.pop()
        type_t = stack_type.pop()
        if type_t != _NUMBER:
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


#WRAPPER
def p_EXPRESSION(p):
    """EXPRESSION : EXPOR"""

#OR OP
def p_EXPOR(p):
    """ EXPOR : EXPAND P10 EXPORB"""
def p_EXPORB(p):
    """ EXPORB : EXPOR_OPS EXPOR 
                | e"""
def p_EXPOR_OPS(p):
    """ EXPOR_OPS : OR """
    global stack_sec
    stack_sec.append(p[1])
def p_P10(p):
    """P10 : e """
    p[0] = True
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in ["|"]:
        generate_code()

#AND OP
def p_EXPAND(p):
    """ EXPAND : EXPEQ P9 EXPANDB"""
def p_EXPANDB(p):
    """ EXPANDB : EXPAND_OPS EXPAND
                 | e"""
def p_EXPAND_OPS(p):
    """ EXPAND_OPS : AND """
    global stack_sec
    stack_sec.append(p[1])

def p_P9(p):
    """P9 : e """
    p[0] = True
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in ["&"]:
        generate_code()

#Equality and difference
def p_EXPEQ(p):
    """ EXPEQ : EXP P8 EXPEQB"""
def p_EXPEQB(p):
    """ EXPEQB : EXPEQ_OPS EXPEQ 
               | e"""
def p_EXPEQB_OPS(p):
    """ EXPEQ_OPS : EQ 
                    | DIFF"""
    global stack_sec
    stack_sec.append(p[1])
def p_P8(p):
    """P8 : e """
    p[0] = True
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in ["==","!="]:
        generate_code()


def p_EXP(p):
    """EXP : EXPR P7 EXPB"""
    
def p_EXPB(p):
    """EXPB : e
            | EXP2_ops EXP"""
def p_EXP2_ops(p):
    """EXP2_ops : GT 
            | LT 
            """
    global stack_sec
    stack_sec.append(p[1])
def p_P7(p):
    """P7 : e """
    p[0] = True
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in [">","<"]:
        generate_code()

    
def p_EXPR(p):
    """EXPR : TERM P4 EXPR_B"""
def p_EXPR_B(p):
    """EXPR_B : EXPR_B_OPS EXPR 
              | e"""
def p_EXPR_B_OPS(p):
    """EXPR_B_OPS : PLUS 
                  | MINUS"""
    global stack_sec
    stack_sec.append(p[1])
def p_P4(p):
    """P4 : e"""
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in ["+","-"]:
        generate_code()
    

def p_TERM(p):
    """TERM : FACTOR P5 TERM_B"""        
def p_TERM_B(p):
    """TERM_B : TERM_B_OPS TERM 
              | e"""
def p_TERM_B_OPS(p):
    """TERM_B_OPS : TIMES
                 | DIVIDE"""
    global stack_sec
    stack_sec.append(p[1])

def p_P5(p):
    """P5 : e"""
    if len(stack_sec) == 0:
        return
    if stack_sec[-1] in ["*","/"]:
        generate_code()

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
    
    stack_prim.reverse()
    stack_type.reverse()
    print(stack_prim,len(stack_prim))
    print(stack_type)
    size = len(stack_prim)
    value_t = stack_prim.pop()
    first_type = stack_type.pop()

    origin,obj = vm.request_array("local",pointer_of(first_type),first_type,size)

    gen_code.append((_MOVH_,origin,obj))
    gen_code.append((_MOV_,obj[0],value_t))


    for i in range(len(stack_prim)):
        value_t = stack_prim.pop()
        type_t = stack_type.pop()

        if type_t != first_type:
            print("Error: Array must be the same type",value_t,type_t,first_type)
            raise SyntaxError

        gen_code.append((_MOV_,1 + i + obj[0],value_t))

    stack_prim.append(origin)
    stack_type.append(pointer_of(first_type))
    p[0] = "<list>"
    
def pointer_of(value):
    t = { _NUMBER_POINTER : _NUMBER_MATRIX,
        _NUMBER: _NUMBER_POINTER,
        _NOTE:_NOTE_POINTER
        }
    return t[value]

def p_ARRAYB(p):

    """ARRAYB : COMMA C1 EXP C2 ARRAYB
             |  e    """  

def p_FACTOR(p):
    """FACTOR :  NUMBER 
              | ID
              | FACTOR_CTE 
              | LPAR P6 EXPRESSION RPAR 
              | IDVARID 
              | FUNCCALL
              | VARCTE
              | READ
              """
    global stack_prim, stack_sec, stack_type,vm,gen_code

    if p[1] == "(":
        stack_sec.pop() #At the end of (exp) pop ")" , only when first is "(" 
        return


    if p[1] in ("<Funccall>","<ID varid>" ,"<factorcte>","<list>","<Read>"):
        #Type remains in stacktype
        return
    inferred_type = infer_type(p[1])


    if inferred_type == "ID":
        stack_prim.append(get_var(p[1],[])) 
        stack_type.append(get_type(p[1],[]))
    elif inferred_type == _STRING :
        c_space = vm.request("local",_STRING)
        gen_code.append((_MOVH_,c_space,p[1].replace('"',"")))
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    elif inferred_type == _NOTE:
        c_space = vm.request("local",_NOTE)
        gen_code.append((_MOVH_,c_space,p[1]))
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    elif inferred_type == _BOOL:
        c_space = vm.request("local",_BOOL)

        if p[1] == "true":
            b_value = True
        else:
            b_value = False

        gen_code.append((_MOVH_,c_space,b_value)) 
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    elif inferred_type == _NUMBER:   
        c_space = vm.request("local",_NUMBER)
        gen_code.append((_MOVH_,c_space,float(p[1])))
        stack_prim.append(c_space)
        stack_type.append(inferred_type)
    else:
        print("Not implemented",inferred_type, "on factor")
        raise SyntaxError
        
def p_P6(p):
    """P6 : e"""
    global stack_sec
    stack_sec.append("(")
    
    
def p_FACTOR_CTE(p):
    """FACTOR_CTE :  PLUS NUMBER  
                  | MINUS NUMBER """
    global stack_prim, stack_sec, stack_type,vm,gen_code

    c_space = vm.request("local",_NUMBER)

    times = 1 if p[1] == "+" else -1
    gen_code.append((_MOVH_,c_space,float(p[2])*times))
    stack_prim.append(c_space)
    stack_type.append(_NUMBER)
    p[0] = "<factorcte>"
    
def p_VARCTE(p):
    """VARCTE : NOTE 
              | BOOL
              | STRING"""
    p[0] = str(p[1])
    return
   
def p_empty(p):
    """e : """




##
# SEMANTIC CUBE
##
cube = {
    _NUMBER : {
        _NUMBER: { 
            "+" : _NUMBER,
            "-" : _NUMBER,
            "/" : _NUMBER,
            "*" : _NUMBER,
            "!=" : _BOOL,
            "==" : _BOOL,
            ">" : _BOOL,
            "<" : _BOOL
        }
    },
    _STRING: {
        _STRING: {
            "+":_STRING,
            "!=":_BOOL,
            "==":_BOOL
        }
    },
    _BOOL: {
        _BOOL: {
            "&": _BOOL,
            "|": _BOOL,
            "!=": _BOOL,
            "==": _BOOL,
        }
    },
    _NOTE: {
        _NUMBER:{
            "+":_NOTE,
            "-":_NOTE,
            "!=":_BOOL,
            "==":_BOOL
        },
        _NOTE:{
            "!=":_BOOL,
            "==":_BOOL
        }
    },
    _NUMBER_POINTER:{
        _NUMBER:{
            "+":_NUMBER_POINTER,
            "-":_NUMBER_POINTER,
            "/":_NUMBER_POINTER,
            "*":_NUMBER_POINTER
        },
        _NUMBER_POINTER:{
            "+":_NUMBER_POINTER
        }
    },
    _NOTE_POINTER:{
        _NUMBER:{
            "+":_NOTE_POINTER
        },
        _NOTE:{
            "+":_NOTE_POINTER
        },
        _NOTE_POINTER:{
            "+":_NOTE_POINTER
        }
    },
    _NUMBER_MATRIX:{
        _NUMBER_MATRIX:{
            "+":_NUMBER_MATRIX,
            "-":_NUMBER_MATRIX,
            "/":_NUMBER_MATRIX,
            "*":_NUMBER_MATRIX,
        }
    }
}


def generate_code():
    """
    This function automatically creates a quadruple using the stacks
    Does not need parameters.
    Stack Prim needs at least two values (left and right)
    Stack Type needs at least two datatypes (left and right)
    
    """
    global stack_sec, stack_prim,op_dict,gen_code, stack_type
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
    else:
        print("Error: Type mismatch")
        raise SyntaxError
    
def is_valid_binary_ops(opleft,opright,op):
    """
    Validates according to the semantic cube if two operands generate a valid operation
    Receives left and right datatypa and operation. E.g.  Number , Number , +
    """
    global cube
    if opleft in cube:
        if opright in cube[opleft]:
            if op in cube[opleft][opright]:
                return cube[opleft][opright][op]
    return None
    
def infer_type(string):
    """
    From a String, infers the datatype  using the same regex patterns
    Returns a datatype
    """
    note_pattern = '[A-Z][1-9][#b]?(.[1-9]\/[1-9])?'
    number_pattern =  '[0-9]+(.[0-9]+)?'
    id_pattern = '[a-zA-Z_][a-zA-Z0-9_]*'
    bool_pattern = '^(true)|(false)$'
    string_pattern = '"(\.|[^"])*"'

    if type(string) == float or type(string) == int:
        return _NUMBER
    if re.match(note_pattern, string):
        return _NOTE
    if re.match(number_pattern, string):
        return _NUMBER
    if re.match(bool_pattern, string):
        return _BOOL
    if re.match(id_pattern, string):
        return "ID"
    if re.match(string_pattern, string):
        return _STRING
    print("Patter unrecognized",string)
    return None

def current_line():
    """
    Gets the current numer of lines of code
    """
    global gen_code
    return len(gen_code) - 1


def p_f1(p):
    """ F1 : e"""
    global gen_code
    new_local_context()

def p_f2(p):
    """ F2 : e"""
    destroy_local_context()

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
    """
    Obtains real address of variable
    Receives the ID and the Dimensions of calling. E.g.  array , [1]
    
    """
    global global_var, stack_var,vm, gen_code
    value = None
    found = False
    if len(stack_var) > 0:
        if key in stack_var[-1]:
            if len(dim) == 2:
                c_space = vm.request("local",dereference(stack_var[-1][key]["type"]))#Assumes **datatype
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
                c_space = vm.request("local",dereference(global_var[key]["type"]))#Assumes **datatype
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
    """
    Sets a value to a variable. Generates quadruple ASSIGNATION
    Receives ID of variable
    Value to set
    and optional parameter is_global
    """
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

        if len(global_var[key]["dim"]) > 1:
            print("Error: Cannot set a whole matrix",key)
            raise SyntaxError

        elif len(global_var[key]["dim"]) == 1:

            print("Error: You have to clone the array yourself on Global arrays",key)
            raise SyntaxError

        else:
            gen_code.append((_MOV_, global_var[key]["address"],value))
            vm.write(global_var[key]["address"],value)


    if undeclared:
        print("Error: undeclared variable",key, "on set_var")
        raise SyntaxError
    return value

def helper_dim(value,dim):
    """
    Calculates the ponter of a variable according to the dimensions called
    Receives Datatype and dimension.
    Returns The datatype or pointer of datatype.

    """
    size = len(dim)
    if size == 2:
        return pointer_of(pointer_of(value))
    elif size == 1:
        return pointer_of(value)
    elif size == 0:
        return value
    else:
        print("Cannot declare tridimensional variables")
        raise SyntaxError

def is_matrix(type_t):
    """
    Checks if data type is matrix
    return BOOL
    """
    return type_t == _NUMBER_MATRIX

def is_array(type_t):
    """
    Checks if data type is Array
    return BOOL
    """
    return type_t in (_NOTE_POINTER,_NUMBER_POINTER)

def is_atomic(type_t):
    """
    Checks if data type is atomic
    return BOOL
    """
    return not(is_matrix(type_t) or is_array(type_t))

def declare_var(key,type_var,dim,is_global=False,is_args=False):
    """
    Declares a variable in the local stack. Generates MOVH initialization
    Receives ID
    Data type : Number
    Dim (dimensions) : list 
    is_global : BOOL
    is_args : BOOL
    """
    global global_var, stack_var, vm, gen_code
    
    print(key,dim)
    if is_args:
        if key in stack_var[-1]:
            print("Error: Argument already declared", key)
            raise SyntaxError
        else:
            address = vm.request("local",trans_type(type_var))
            vm.push_param(address,None)
            type_in_number = helper_dim(trans_type(type_var),dim)
            stack_var[-1][key] = {"address":address,"type":type_in_number,"dim":dim}
        return

    if is_global:
        if key in global_var:
            print("Error: Already declared", key)
            raise SyntaxError
        else:
            type_var = trans_type(type_var)
            new_type = helper_dim(type_var,dim)

            if is_atomic(new_type):
                address = vm.request("global",new_type)
            if is_array(new_type):
                if dim[0] < 1: #Pointer declaration
                    address = vm.request("global",new_type)
                    gen_code.append((_MOVH_,address,None))
                else:
                    address,obj = vm.request_array("global",new_type, dereference(new_type),dim[0])
                    gen_code.append((_MOVH_,address,obj))
                    for i in range(dim[0]):
                        gen_code.append((_MOVH_,obj[0]+i,None))


            if is_matrix(new_type):
                if dim[0] < 1 or dim[1] < 1:
                    print("Error: Cannot declare matrix with less than one cell")
                    raise SyntaxError

                address,obj = vm.request_matrix("global",new_type,dereference(dereference(new_type)),dim[0],dim[1])
                gen_code.append((_MOVH_,address,obj))
                for i in range(dim[0]):
                    obj2 = vm.read(obj[0]+i)
                    gen_code.append((_MOVH_,obj[0]+i,obj2))
                    for j in range(dim[1]):
                        gen_code.append((_MOVH_,obj2[0]+j,None))

            global_var[key] = {"address":address,"type":new_type,"dim":dim}
                
    else:
        if key in stack_var[-1]:
            print("Error: Already declared", key)
            raise SyntaxError
        else:
            type_var = trans_type(type_var)
            new_type = helper_dim(type_var,dim)
            if is_atomic(new_type):
                address = vm.request("local",new_type)
            if is_array(new_type):
                if dim[0] < 1: #Pointer declaration
                    address = vm.request("local",new_type)
                    gen_code.append((_MOVH_,address,None))
                else:
                    address,obj = vm.request_array("local",new_type, dereference(new_type),dim[0])
                    gen_code.append((_MOVH_,address,obj))
                    print(dim[0])
                    for i in range(dim[0]):
                        gen_code.append((_MOVH_,obj[0]+i,None))


            if is_matrix(new_type):
                if dim[0] < 1 or dim[1] < 1:
                    print("Error: Cannot declare matrix with less than one cell")
                    raise SyntaxError

                address,obj = vm.request_matrix("local",new_type,dereference(dereference(new_type)),dim[0],dim[1])
                gen_code.append((_MOVH_,address,obj))
                for i in range(dim[0]):
                    obj2 = vm.read(obj[0]+i)
                    gen_code.append((_MOVH_,obj[0]+i,obj2))

                    for j in range(dim[1]):
                        gen_code.append((_MOVH_,obj2[0]+j,None))
            stack_var[-1][key] = {"address":address,"type":new_type,"dim":dim}

    
def new_local_context():
    """
    Creates a new context for local variables
    """
    global stack_var
    vm.new_context()
    stack_var.append({})

def destroy_local_context():
    """
    Destroys the latest local context
    """
    global stack_var,vm
    addresses = [stack_var[-1][i]["address"] for i in stack_var[-1]]
    stack_var.pop() 
    vm.destroy_context()


def dereference(value):
    """
    Converts Pointer to Datatype. E.g  **Number -> *Number
    Receives Datatype : Number
    """
    if value == _NUMBER_MATRIX:
        return _NUMBER_POINTER
    if value == _NUMBER_POINTER:
        return _NUMBER
    if value == _NOTE_POINTER:
        return _NOTE

    print("Error : datatype does not support dereference",value)
    raise SyntaxError

def get_type(key,dim):
    """
    Gets datatype of variable, local or global.
    Receives ID and dimension of call
    """
    global global_var, stack_var
    
    value = None
    if len(stack_var) > 0:
        if key in stack_var[-1]:      
            if len(dim) == 2:
                if stack_var[-1][key]["type"] == _NUMBER_MATRIX:
                    value = dereference(dereference(stack_var[-1][key]["type"]))
                else:
                    print("Error: type is not matrix")
                    raise SyntaxError
            elif len(dim) == 1:
                if stack_var[-1][key]["type"] in (_NUMBER_POINTER,_NOTE_POINTER):
                    value = dereference(stack_var[-1][key]["type"])
                else:
                    print("Error: type is not array")
                    raise SyntaxError
                
            else:
                value = stack_var[-1][key]["type"]
      
    
    if key in global_var:
        if len(dim) == 2:
            if global_var[key]["type"] == _NUMBER_MATRIX:
                value = dereference(dereference(global_var[key]["type"]))
            else:
                print("Error: type is not matrix")
                raise SyntaxError
        elif len(dim) == 1:
            if global_var[key]["type"] in (_NUMBER_POINTER,_NOTE_POINTER):
                value = dereference(global_var[key]["type"])
            else:
                print("Error: type is not array",key,global_var[key]["type"])
                raise SyntaxError
                
        else:
            value = global_var[key]["type"]
    
    if value == None:
        print("Error: undeclared variable",key, "on get_type")
        raise SyntaxError
        
    return value
    
def declare_func(key,return_type,line):
    """
    Declares a function in the local stack.
    Receives ID
    Return type in String.
    Line, begin LOC of function
    """
    global global_func, stack_param
    
    if key in global_func:
        print("Error: Function already declared",global_func)
        raise SyntaxError
    
    params_info = {}
    params_order = []
    address_order = []

    for i in stack_param: #ID, address local, type
        params_info[i[0]] = i[1]
        params_order.append(i[2])
        address_order.append(i[1])

    global_func[key] = {"return":trans_type(return_type),
        "params_info":params_info,"line":line,"params_order":params_order,"address_order":address_order}
    
def trans_type(string_type):
    """
    Receives STRING datatype and converts it to 
    numeric datatype

    """
    t = {
    "Bool": _BOOL ,
    "Number":_NUMBER,
    "Note" :_NOTE,
    "String":_STRING,
    "*Note":_NOTE_POINTER,
    "*Number":_NUMBER_POINTER,
    "**Number":_NUMBER_MATRIX,
    "Silence": _BOOL
     }
    return t[string_type]




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
gen_code = []
stack_array = []
start = "PROGRAM"
parser = yacc.yacc()
with open('input.sostenuto', 'r') as file:
    s = file.read().replace("\n"," ").replace("\t"," ")
result = parser.parse(s)

for i in [(x,reverse_op[y[0]],y[1:])for x,y in zip(range(len(gen_code)),gen_code)]:
    print(i)

#for i in gen_code:
    #print(i)

print("Executing...")
vm = None
vm = VM.VirtualMachine()
vm.calculate(gen_code)

