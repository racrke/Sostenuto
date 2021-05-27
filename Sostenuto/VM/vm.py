from music21 import *

#Puede estar mal el temporal porque no tiene tipo de dato
class VirtualMachine:
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
    _PRINT_= 30
    _PLAY_ = 42
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


    BASE_CONS = 0
    BASE_GLOBAL = 10000
    BASE_LOCAL = 20000

    BASE_TYPE_NUMBER = 0
    BASE_TYPE_NOTE = 1000
    BASE_TYPE_BOOL = 2000
    BASE_TYPE_STRING = 3000

    BASE_TYPE_NUMBER_ARRAY = 4000 #Pointers
    BASE_TYPE_NOTE_ARRAY = 5000
    BASE_TYPE_NUMBER_MATRIX = 6000 #Pointer of Pointer

    DATA_LIMIT = 999

    stack_params = []
    stack_jump = []

    
    scope_map = {"const":BASE_CONS, "global":BASE_GLOBAL,"local":BASE_LOCAL}
    type_map = {"Number":BASE_TYPE_NUMBER,"Note":BASE_TYPE_NOTE,
                "Bool":BASE_TYPE_BOOL,"String":BASE_TYPE_STRING,
                "*Number":BASE_TYPE_NUMBER_ARRAY,"**Number":BASE_TYPE_NUMBER_MATRIX,
                "*Note":BASE_TYPE_NOTE_ARRAY}

    streams = []
    keys = {"C":0,"D":2,"E":4,"F":5,"G":7,"A":9,"B":11}

    def __init__(self):
        self.stack_params = []
        self.stack_jump = []
        self.memory = {"const":{ 0:0 },"global":{0:0},"local":[{}]}
        self.counters = {"Number":0,"Note":0,
                "Bool":0,"String":0,
                "*Number":0,"**Number":0,
                "*Note":0}

        self.return_value = None
        
    def request(self,location,datatype):

        current = self.counters[datatype]
        self.counters[datatype] += 1   
        address = self.scope_map[location] + self.type_map[datatype] + current + 1
        self.write(address,None)
        return address


    def request_array(self,location,pointer_type,datatype,dim):
        pointer = self.request(location,pointer_type)

        cell_origin = None
        for _ in range(dim):
            if cell_origin == None:

                cell_origin = self.request(location,datatype)
                self.write(cell_origin,None)
            else:
                origin = self.request(location,datatype)
                self.write(origin,None)
        self.write(pointer,(cell_origin,dim))
        return pointer,(cell_origin,dim)

    def request_matrix(self,location,pointer_type,datatype,dim1,dim2):
        pointer = self.request(location,pointer_type)

        cell_origin = None
        for _ in range(dim1):
            origin, obj = self.request_array(location,pointer_type[1:],datatype,dim2)

            if cell_origin == None:
                cell_origin = origin 

            self.write(origin,obj)

        self.write(pointer,(cell_origin,dim1,dim2))
        return pointer, (cell_origin,dim1,dim2)


    def get_scope(self,address):
        if address > self.BASE_LOCAL:
            return "local"

        elif address > self.BASE_GLOBAL:
            return "global"

        elif address > self.BASE_CONS:
            return "const"
    def get_datatype(self,address,scope):
        base = self.scope_map[scope]

        if address - base > self.BASE_TYPE_NUMBER_MATRIX :
            return "**Number"
        elif address - base > self.BASE_TYPE_NOTE_ARRAY :
            return "*Note"
        elif address - base > self.BASE_TYPE_NUMBER_ARRAY :
            return "*Number"
        elif address - base > self.BASE_TYPE_STRING :
            return "String"
        elif address- base > self.BASE_TYPE_BOOL :
            return "Bool"
        elif address - base > self.BASE_TYPE_NOTE :
            return "Note"
        elif address - base > self.BASE_TYPE_NUMBER :
            return "Number"    




    def new_context(self):
        self.memory["local"].append({0:1,1:None})
    def destroy_context(self):
        self.memory["local"].pop()
    def push_param(self,address,new_address):
        self.stack_params.append((self.read(address),new_address)) #Value

    def write(self,address,value):

        try:
            if type(address) != int and type(address) != float:
                address = address[0]

            if address > self.BASE_LOCAL:
                self.memory["local"][-1][address] = value

            elif address > self.BASE_GLOBAL:
                self.memory["global"][address] = value

            elif address > self.BASE_CONS:
                self.memory["const"][address] = value
            else:
                print("Memory Error: Invalid Range in Write address",address,value)
                raise Exception
        except:
            print("Fatal Memory Error: Invalid write address",address,value)
            raise Exception
            return None
    

    def calculate_address(self,address,dim1): #Assumess twin cell addresses
        obj = self.read(address)
        dim1 = self.read(dim1)

        if type(obj) == float: #pointer without boundaries
            obj = self.read(obj)

        if  obj[0] + dim1 >= obj[0] + obj[1] or  obj[0] + dim1 < obj[0] : #Pointer with boundaries
                print("Error: Out of bounds",address,dim1,obj[0] + obj[1])
                raise Exception

        elif dim1 >= 0:
            return obj[0] + dim1
        else:   
            print("Error: Error with index",address)
            raise Exception


        print("Error: Cannot index")
        raise Exception



    def read(self,address):

        try:
            if type(address) != int and type(address) != float:
                address = address[0]

            if address > self.BASE_LOCAL:
                return self.memory["local"][-1][address]

            elif address > self.BASE_GLOBAL:
                return self.memory["global"][address]

            elif address > self.BASE_CONS:
                return self.memory["const"][address]
            else:
                print("Memory Error: Invalid address",address)
                raise Exception
        except Exception as e:
            print("Memory Error: Address does not exist",address)
            raise Exception
            return None
    

    def calculate(self,lines):

        current_line = 0
        limit = 0
        while current_line < len(lines):
            limit += 1

            if limit > 9999:
                print("Avoided possible Infite Cycle")
                break

            #print("$",current_line)
            line = lines[current_line]
            current_line += 1
            op = line[0]
            if op == self._MOV_: #Memory to memory
                address = line[1]

                sp = self.get_scope(line[2])
                type_t = self.get_datatype(line[2],sp)
                if type_t in ("*Number","*Note"):
                    if type(self.read(line[2])) == float:
                        address2 = self.read(self.read(line[2]))
                    else:
                        address2 = self.read(line[2])
                else:
                    address2 = self.read(line[2])
                self.write(address,address2);

            if op == self._MOVH_: #Value to Memory
                value = self.MOVH(line[1],line[2])
                self.write(line[1],value)
            if op == self._ADD_:
                self.write(line[3], self.addition(line[1],line[2]))
            if op == self._SUBS_:
                self.write(line[3], self.substraction(line[1],line[2]))
            if op == self._DIVIDE_:
                self.write(line[3], self.division(line[1],line[2]))
            if op == self._MULT_:
                self.write(line[3], self.times(line[1],line[2]))
            if op == self._LTHAN_:
                self.write(line[3],self.ind(line[1])<self.ind(line[2]))
            if op ==self._GTHAN_:
                self.write(line[3],self.ind(line[1])>self.ind(line[2]))
            if op ==self._EQ_:
                self.write(line[3],self.ind(line[1])==self.ind(line[2]))
            if op ==self._DIFF_:
                self.write(line[3],self.ind(line[1])!=self.ind(line[2]))
            if op ==self._AND_:
                self.write(line[3],self.ind(line[1]) and self.ind(line[2]))
            if op ==self._OR_:
                self.write(line[3],self.ind(line[1]) or self.ind(line[2]))
            if op ==self._GOTOF_: #expect (GOTOF,value,line)
                if not bool (self.read(line[1])):
                    current_line = int(line[2])
            if op ==self._GOTOV_: #expect (GOTOF,Value,line)
                if bool (self.read(line[1])):
                    current_line = int(line[2])
            if op ==self._GOTO_: #expect (GOTOF,None,line)
                current_line = int(line[2])
            if op == self._PRINT_:

                sp = self.get_scope(line[1])
                type_t = self.get_datatype(line[1],sp)
                if  type_t in ( "*Number","*Note"):
                    value = self.read(self.read(line[1]))
                else:
                    value = self.read(line[1])

                if value == None:
                    print("None")
                else:
                    print(value)

            if op == self._NEW_:
                self.new_context()
            if op == self._DESTROY_:
                self.destroy_context()
    
            if op == self._PARAM_:
                #print("PARAM",line,self.read(line[1]))

                sp = self.get_scope(line[1])
                type_t = self.get_datatype(line[1],sp)
                if  type_t in ( "*Number","*Note"):
                    address = self.read(line[1])
                else:
                    address = line[1]
                self.push_param(address,line[2])

            if op == self._CALL_:
                self.stack_jump.append(current_line)
                current_line = int(line[1])
                self.new_context()
                for p in self.stack_params: 
                    self.write(p[1],p[0])
                self.stack_params  = []
            if op == self._ENDFUNC_:
                current_line = self.stack_jump.pop() 
                self.destroy_context()
            if op == self._RETURN_:
                self.return_value = self.read(line[1]) #Maybe should be a stack

            if op == self._GETRET_:
                self.write(line[1],self.return_value)
            if op == self._STREAM_:
                amount = self.read(line[1])

                for _ in range(int(amount)):
                    self.streams.append(stream.Stream())

            if op == self._PLAYN_:
                pitch,time = self.read(line[1]) #Necesita conversion
                n = self.numerical_to_note(pitch,time)
                st = self.read(line[2])
                self.streams[int(st)].append(n)
            if op == self._MSHEET_:
                piano = stream.Score()

                hands = []
                for st in self.streams:
                    hand = stream.Part()
                    hand.append(st)
                    hands.append(hand)

                for h in hands:
                    piano.append(h)
                piano.show()
            if op == self._PLAYS_ :
                duration = self.read(line[1])
                st = self.read(line[2])
                n = note.Rest(quarterLength=duration)
                self.streams[int(st)].append(n)
            if op == self._INDEX_:
                generated_address = self.calculate_address(line[1],line[2])
                self.write(line[3],generated_address)
            if op == self._MOVI_:
                address = self.read(line[1])

                sp = self.get_scope(line[2])
                type_t = self.get_datatype(line[2],sp)
                if type_t in ("*Number","*Note"):
                    address2 = self.read(line[2])
                else:
                    address2 = self.read(line[2])

                self.write(address,address2);



    def numerical_to_note(self,pitch,time):
        return note.Note(pitch,quarterLength=time)

    def str_to_note_numerical(self,value):

        #e.g C4#.4/4 , A5 , B2b
        if value.find(".") != -1:
            temp = value.split(".")
            temp2 = temp[1].split("/")
            div1 = int(temp2[0])
            div2 = int(temp2[1])
            time = div1 / div2

            octave = int(temp[0][1])
            key = self.keys[temp[0][0]]
        else:
            time = 1
            temp = value
            octave = int(temp[1])
            key = self.keys[temp[0]]

        if len(value) > 2:
            if value[2] == "b":
                alteration = -1
            elif value[2] == "#":
                alteration = 1
            else:
                alteration = 0
        else:
            alteration = 0

       
        note_number =  key + (octave - 1) * 12 + 24 + alteration
        return (note_number,time)
        
    def MOVH (self,address,value):
        scope = self.get_scope(address)
        right_type = self.get_datatype(address,scope)
        if right_type == "Note":
            return self.str_to_note_numerical(value)
        return value

    def addition(self,left,right):

        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)

        if left_type == "Number":
            if right_type == "Number":
                return left + right
            if right_type == "*Number":
                obj = self.read(right)

                if type(obj) == float:
                    return left + obj
                else:
                    # number plus array, incorrect
                     pass    


        if left_type == "String":
            if right_type == "String":
                return left + right

        if left_type == "Note":
            if right_type == "Number":
                return (left[0] + right,left[1])

        if left_type == "*Number":
            obj = self.read(left)
            if type(obj) != float:
                if right_type == "Number":
                    #Array plus number
                    pass

                if right_type == "*Number":
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == "Number":
                    return obj + right

                if right_type == "*Number":
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj + obj2 


        if left_type == "*Note":
            if right_type == "Number":
                pass 
            if right_type == "Note":
                pass  
            if right_type == "*Note":
                pass 
        if left_type == "**Note":
            if right_type == "**Number":
                pass 

        print("Invalid operand + between",left,left_type,"and",right,right_type)
        raise Exception 

    def substraction(self,left,right):
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)

        if left_type == "Number":
            if right_type == "Number":
                return left - right
            if right_type == "*Number":
                obj = self.read(right)

                if type(obj) == float:
                    return left - obj
                else:
                    # number plus array, incorrect
                     pass

        if left_type == "Note":
            if right_type == "Number":
                return (left[0] - right,left[1])    

        if left_type == "*Number":
            obj = self.read(left)
            if type(obj) != float:
                pass
            else:
                if right_type == "Number":
                    return obj - right

                if right_type == "*Number":
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj - obj2 

        print("Invalid operand - between",left,left_type,"and",right,right_type)
        raise Exception

    def division(self,left,right):
        
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)

        if left_type == "Number":
            if right_type == "Number":
                return left / right
            if right_type == "*Number":
                obj = self.read(right)

                if type(obj) == float:
                    return left / obj
                else:
                    # number plus array, incorrect
                     pass    

        if left_type == "*Number":
            obj = self.read(left)
            if type(obj) != float:
                pass
            else:
                if right_type == "Number":
                    return obj / right

                if right_type == "*Number":
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj / obj2 

        print("Invalid operand / between",left,left_type,"and",right,right_type)
        raise Exception
    def times(self,left,right):
        
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)    


        if left_type == "Number":
            if right_type == "Number":
                return left * right
            if right_type == "*Number":
                obj = self.read(right)

                if type(obj) == float:
                    return left * obj


        if left_type == "*Number":
            obj = self.read(left)
            if type(obj) != float:
                if right_type == "Number":
                    #Array plus number
                    pass

                if right_type == "*Number":
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == "Number":
                    return obj * right

                if right_type == "*Number":
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj * obj2 


        print("Invalid operand * between",left,left_type,"and",right,right_type)
        raise Exception


    def more_than(self,left,right):
        pass
    def less_than(self,left,right):
        pass
    def equals(self,left,right):
        pass
    def different(self,left,right):
        pass
    def and_op(self,left,right):
        pass
    def or_op(self,left,right):
        pass

    def ind(self,opt):
        #assumes always addresses
            return self.read(opt)

