from music21 import *

_NUMBER = 1
_NOTE = 2
_BOOL = 3
_STRING = 4
_NUMBER_POINTER = 5
_NOTE_POINTER = 6 
_NUMBER_MATRIX = 7

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
    type_map = {_NUMBER:BASE_TYPE_NUMBER,
                _NOTE:BASE_TYPE_NOTE,
                _BOOL:BASE_TYPE_BOOL,
                _STRING:BASE_TYPE_STRING,
                _NUMBER_POINTER:BASE_TYPE_NUMBER_ARRAY,
                _NUMBER_MATRIX:BASE_TYPE_NUMBER_MATRIX,
                _NOTE_POINTER:BASE_TYPE_NOTE_ARRAY}

    streams = []
    keys = {"C":0,"D":2,"E":4,"F":5,"G":7,"A":9,"B":11}

    def __init__(self):
        self.stack_params = []
        self.stack_jump = []
        self.memory = {"const":{},"global":{},"local":[{}]}
        self.counters = {_NUMBER:0,_NOTE:0,
                _BOOL:0,_STRING:0,
                _NUMBER_POINTER:0,_NUMBER_MATRIX:0,
                _NOTE_POINTER:0}

        self.return_value = None
        
    def request(self,location,datatype):
        """
        Allocates a memory address.
        Receives Location of memory: const, local, global
        and datatype
        returns Numeric address

        """
        current = self.counters[datatype]
        self.counters[datatype] += 1   
        address = self.scope_map[location] + self.type_map[datatype] + current + 1
        self.write(address,None)
        return address


    def request_array(self,location,pointer_type,datatype,dim):
        """
        Allocates memory to an array.
        Receives Location of memory: const, local, global
        datatype of variable,
        datatype of cells
        returns origin of array and dimensions
        """
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
        """
        Allocates memory to an Matrix.
        Receives Location of memory: const, local, global
        datatype of variable,
        datatype of cells
        return origin of matrix and dimensions
        """
        pointer = self.request(location,pointer_type)
        cell_origin = None
        for _ in range(dim1):
            origin, obj = self.request_array(location,self.data_type_point_to(pointer_type),datatype,dim2)
            if cell_origin == None:
                cell_origin = origin 

            self.write(origin,obj)

        self.write(pointer,(cell_origin,dim1,dim2))
        return pointer, (cell_origin,dim1,dim2)

    def data_type_point_to(self,value):
        """
        Derefences a datatype.
        Receives numeric datatype 
        returns numeric datatype
        """
        if value == _NUMBER_MATRIX:
            return _NUMBER_POINTER
        if value == _NUMBER_POINTER:
            return _NUMBER
        if value == _NOTE_POINTER:
            return _NOTE

        print("Value is not a valid pointer",value)
        raise Exception

    def get_scope(self,address):
        """
        Obtains location of address
        Returns Location in STRING
        """
        if address > self.BASE_LOCAL:
            return "local"

        elif address > self.BASE_GLOBAL:
            return "global"

        elif address > self.BASE_CONS:
            return "const"

    def get_datatype(self,address,scope):
        """
        Obtains datatype of address
        Receives address and scope
        returns numeric datatype
        """
        base = self.scope_map[scope]
        if address - base > self.BASE_TYPE_NUMBER_MATRIX :
            return _NUMBER_MATRIX
        elif address - base > self.BASE_TYPE_NOTE_ARRAY :
            return _NOTE_POINTER
        elif address - base > self.BASE_TYPE_NUMBER_ARRAY :
            return _NUMBER_POINTER
        elif address - base > self.BASE_TYPE_STRING :
            return _STRING
        elif address- base > self.BASE_TYPE_BOOL :
            return _BOOL
        elif address - base > self.BASE_TYPE_NOTE :
            return _NOTE
        elif address - base > self.BASE_TYPE_NUMBER :
            return _NUMBER  


    def new_context(self):
        self.memory["local"].append({})
    def destroy_context(self):
        self.memory["local"].pop()

    def push_param(self,address,new_address):
        """
        Adds a element to Stack param
        """
        self.stack_params.append((self.read(address),new_address)) #Value

    def write(self,address,value):
        """
        Writes in a given address the value
        Receives numeric address 
        Receives any value
        """

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
    

    def calculate_address(self,address,dim1):
        """
        Verifies and calculates the index of non-atomic variables
        Receives numeric address 
        returns numeric address
        """
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
        """
        Reads a memory slot
        Receives numeric address 
        returns the value of the cell
        """
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
            print(self.memory)
            raise Exception
            return None
    

    def calculate(self,lines):
        """
        Calculates all the operations available in the Virtual machine
        Receives an array of quadruples
        """
        current_line = 0
        limit = 0
        while current_line < len(lines):
            limit += 1

            if limit > 9999:
                print("Sostenuto VM : Avoided possible Infite Cycle")
                break

            #print("$",current_line)
            line = lines[current_line]
            current_line += 1
            op = line[0]
            if op == self._MOV_: #Memory to memory
                address = line[1]

                sp = self.get_scope(line[2])
                type_t = self.get_datatype(line[2],sp)
                if type_t in (_NUMBER_POINTER,_NOTE_POINTER):
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
                self.write(line[3], self.less_than(line[1],line[2]))
            if op ==self._GTHAN_:
                self.write(line[3], self.more_than(line[1],line[2]))
            if op ==self._EQ_:
                self.write(line[3], self.equals(line[1],line[2]))

            if op ==self._DIFF_:
                self.write(line[3], self.different(line[1],line[2]))
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
                if  type_t in ( _NUMBER_POINTER,_NOTE_POINTER):
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
                sp = self.get_scope(line[1])
                type_t = self.get_datatype(line[1],sp)
                if  type_t in (_NUMBER_POINTER,_NOTE_POINTER):
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
                self.return_value = self.read(line[1]) 

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

            if op == self._PLAYC_:
                origin, dim = self.read(line[1])
                st = self.read(line[2])
                notes_t = []
                for i in range(dim):
                    pitch,time = self.read(origin + i) #Necesita conversion
                    n = self.numerical_to_note(pitch,time)
                    notes_t.append(n)
                self.streams[int(st)].append(chord.Chord(notes_t))

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
            if op == self._MOVI_:  #Memory to indirect memory
                address = self.read(line[1])

                sp = self.get_scope(line[2])
                type_t = self.get_datatype(line[2],sp)
                if type_t in (_NUMBER_POINTER,_NOTE_POINTER):
                    if type(self.read(line[2])) == float:
                        address2 = self.read(self.read(line[2]))
                    else:
                        address2 = self.read(line[2])
                else:
                    address2 = self.read(line[2])
                self.write(address,address2);
            if op == self._READ_:
                temp = input()

                dt = self.get_datatype(line[1],self.get_scope(line[1]))
                if dt == _NUMBER:
                    temp = float(temp)
                self.write(line[1],temp)


    def numerical_to_note(self,pitch,time):
        return note.Note(pitch,quarterLength=time)

    def str_to_note_numerical(self,value):
        """
        Creates Music21 Object from String
        returns Music21 Note object
        """
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
        """
        Converts String Notes to Music21 Notes
        returns the value
        """
        scope = self.get_scope(address)
        right_type = self.get_datatype(address,scope)
        if value == None:
            return None
        if right_type == _NOTE:
            return self.str_to_note_numerical(value)
        return value


    ###
    #   Operations and semantic cube
    ###
    def addition(self,left,right):

        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)

        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left + right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left + obj
                else:
                    # number plus array, incorrect
                     pass    


        if left_type == _STRING:
            if right_type == _STRING:
                return left + right

        if left_type == _NOTE:
            if right_type == _NUMBER:
                return (left[0] + right,left[1])

        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                if right_type == _NUMBER:
                    #Array plus number
                    pass

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == _NUMBER:
                    return obj + right

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj + obj2 


        if left_type == _NOTE_POINTER:
            if right_type == _NUMBER:
                pass 
            if right_type == _NOTE:
                pass  
            if right_type == _NOTE_POINTER:
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

        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left - right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left - obj
                else:
                    # number plus array, incorrect
                     pass

        if left_type == _NOTE:
            if right_type == _NUMBER:
                return (left[0] - right,left[1])    

        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                pass
            else:
                if right_type == _NUMBER:
                    return obj - right

                if right_type == _NUMBER_POINTER:
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

        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left / right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left / obj
                else:
                    # number plus array, incorrect
                     pass    

        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                pass
            else:
                if right_type == _NUMBER:
                    return obj / right

                if right_type == _NUMBER_POINTER:
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


        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left * right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left * obj


        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                if right_type == _NUMBER:
                    #Array plus number
                    pass

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == _NUMBER:
                    return obj * right

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj * obj2 


        print("Invalid operand * between",left,left_type,"and",right,right_type)
        raise Exception


    def more_than(self,left,right):
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)    


        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left > right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left > obj


        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                if right_type == _NUMBER:
                    #Array plus number
                    pass

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == _NUMBER:
                    return obj > right

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj > obj2 
        print("Not Implemented")
        raise Exception

    def less_than(self,left,right):
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)    


        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left < right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left < obj


        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                if right_type == _NUMBER:
                    #Array plus number
                    pass

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == _NUMBER:
                    return obj < right

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj < obj2 
        print("Not Implemented")
        raise Exception

    def equals(self,left,right):
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)    


        if left_type ==_BOOL:
            if right_type == _BOOL:
                return left == right

        if left_type ==_STRING:
            if right_type == _STRING:
                return left == right
        if left_type == _NOTE:
            if right_type == _NOTE:
                return left == right

        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left == right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left == obj


        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                if right_type == _NUMBER:
                    #Array plus number
                    pass

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == _NUMBER:
                    return obj == right

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj == obj2

        print("Not Implemented")
        raise Exception

    def different(self,left,right):

        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)    


        if left_type ==_BOOL:
            if right_type == _BOOL:
                return left != right

        if left_type == _NUMBER:
            if right_type == _NUMBER:
                return left != right
            if right_type == _NUMBER_POINTER:
                obj = self.read(right)

                if type(obj) == float:
                    return left != obj

        if left_type ==_STRING:
            if right_type == _STRING:
                return left != right

        if left_type == _NOTE:
            if right_type == _NOTE:
                return left == right

        if left_type == _NUMBER_POINTER:
            obj = self.read(left)
            if type(obj) != float:
                if right_type == _NUMBER:
                    #Array plus number
                    pass

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) != float:
                        # Array plus Array
                         pass
            else:
                if right_type == _NUMBER:
                    return obj != right

                if right_type == _NUMBER_POINTER:
                    obj2 = self.read(right)
                    if type(obj2) == float:
                        return obj != obj2 
        print("Not Implemented")
        raise Exception

    def and_op(self,left,right):
        
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)    


        if left_type ==_BOOL:
            if right_type == _BOOL:
                return left and right
        print("Not Implemented")
        raise Exception

    def or_op(self,left,right):
        left_scope = self.get_scope(left)
        right_scope = self.get_scope(right)
        left_type = self.get_datatype(left,left_scope)
        right_type = self.get_datatype(right,right_scope)

        left = self.read(left)
        right = self.read(right)    


        if left_type ==_BOOL:
            if right_type == _BOOL:
                return left or right

        print("Not Implemented")
        raise Exception

    def ind(self,opt):
        #assumes always addresses
            return self.read(opt)

