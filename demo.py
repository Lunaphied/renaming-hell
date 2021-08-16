# Todo: Use ABC to do proper abstract classes
class Sync:
    """Defines a process with synchronous characteristics"""
    def __init__(self):
        # List of modules to clock with this module
        self.submodules = []
    def clock(self):
        """Handle clocking to define new outputs"""
        # By definition on_clock for this module must be handled before submodules
        # May change to pre_submodule and post_submodule if necessary since on_clock
        # being after submodules somewhat also makes more sense
        self.on_clock()
        for module in self.submodules:
            module.clock()
    def on_clock(self):
        """Default implementation just does nothing"""
        pass
class Register(Sync):
    """Defines a helper for defining a pipeline stage register"""
    def __init__(self, reset=None, hack=False):
        super().__init__()
        self.hack = hack
        self.next = None
        self.value = reset
        self.already_set = False
    def set(self, value):
        if self.hack:
            raise RuntimeError("Finding hack")
        if self.already_set:
            raise RuntimeError("Multiple writes to same reg attempted")
        self.already_set = True
        self.next = value
    def get(self):
        return self.value
    def on_clock(self):
        """Clocks the pipeline register sending input to output value"""
        if self.already_set:
            # Only update regs that get set
            self.value = self.next
            # Allow a new value to be set
            self.already_set = False
    def __str__(self):
        return f"{{value = {self.value}, next={self.next}}}"
class Fifo(Sync):
    # TODO: Write a memory implementation and make this use that since that will have more
    # real memory like semantics (reads are "instant", writes are at least 1 cycle later)
    """A buffer that allows filling and reading sequentially at different rates"""
    def __init__(self, capacity=1024):
        super().__init__()
        # HACK: We use this to allow setting initial state and capacity
        if isinstance(capacity, int):
            self.capacity = capacity
            # Don't think this needs to be registered for now (if reads and writes to a single location need)
            # to be double pumped this will change...
            self.items = [0] * capacity
            self.wptr = Register(0)
            self.rptr = Register(0)
            self.size = Register(0)
        elif isinstance(capacity, list):
            self.capacity = len(capacity)
            self.items = capacity.copy()
            # Start full so write pointer has "wrapped around" in theory
            self.wptr = Register(0)
            # Start full and unread so reads start at 0 too
            self.rptr = Register(0)
            # Full so size == capacity
            self.size = Register(capacity)
        # Goes up +1 for writes, -1 for reads, decides how size changes
        # during a clock
        self.incr = 0
        # Make sure all our submodules update properly
        #self.submodules += self.items
        self.submodules += [self.wptr, self.rptr, self.size]
    def read(self):
        # Todo: Handle forwarding somehow...
        if self.size.get() == 0:
            raise RuntimeError("Fifo was empty on read")
        # We will lose an entry during reads (space should be free immediately, but will be delayed)
        # a clock because no forwarding or whatever
        self.incr -= 1
        # We need multiple writes so cache it locally
        rptr = self.rptr.get()
        # read the actual value
        result = self.items[rptr]
        # Attempt to increment
        rptr += 1
        # Handle overflow wraparound
        if rptr >= self.capacity:
            rptr = 0
        # Store the read pointer location back
        self.rptr.set(rptr)
        return result
    def write(self, value):
        # TODO: Handle forwarding somehow...
        if self.size.get() == self.capacity:
            raise RuntimeError("Fifo was full on write")
        # We will gain an entry during writes (valid next clock since no forwarding)
        self.incr += 1
        # Store locally for same reason as read
        wptr = self.wptr.get()
        self.items[wptr] = value
        wptr += 1
        if wptr >= self.capacity:
            wptr = 0
        self.wptr.set(wptr)
    def is_full(self):
        return self.size.get() == self.capacity
    def is_empty(self):
        return self.size.get() == 0
    def flush(self):
        # Checking to make sure flush isn't attempted after read/write is done by rptr and wptr themselves!
        self.rptr.set(0)
        self.wptr.set(0)
        # HACK: We really should handle this more like actual RTL sim stuff
        self.incr = -self.size.get()
    def on_clock(self):
        # Handle the fact that we want multiple write detection but size would be updated based on which combination of read and write enables were high
        self.size.set(self.size.get() + self.incr)
        # Make sure self.incr is reset so it doesn't fill if nothing happens
        self.incr = 0
class Instr:
    def __init__(self):
        self.in_registers = []
        self.out_registers = []
    def __str__(self) -> str:
        # Print the class name and registers
        return f"{type(self).__name__}(in_registers={self.in_registers}, out_registers={self.out_registers})"
class Nop(Instr):
    def __init__(self):
        """No operation"""
        super().__init__()
class Load(Instr):
    def __init__(self, target):
        """Create a 'load' into target"""
        super().__init__()
        self.out_registers += target,
class Store(Instr):
    def __init__(self, source):
        """Create a 'store' from source"""
        super().__init__()
        self.in_registers += source,
class Add(Instr):
    def __init__(self, r1, r2):
        """Stores r1+r2 in r1"""
        super().__init__()
        self.in_registers = [r1, r2]
        self.out_registers = [r1]
class Abort(Instr):
    def __init__(self):
        """Forces fetch to only return Nops after this"""
        super().__init__()

class FetchStage(Sync):
    def __init__(self, memory):
        super().__init__()
        # Store a reference to "instruction memory"
        self.memory = memory
        # Keep track of local PC for stuff
        self.pc = Register(reset=0)
        # TODO: If the PC is changed we need to flush the fifo
        # self.pc_set = False
        # Fifo for holding instructions ready for decode phase
        self.fifo = Fifo()
        # "Exported" signal to signify we need to wait because next stage not ready
        self.stall = Register(False)
        self.submodules += [self.pc, self.fifo, self.stall]
    def eval(self):
        """Eval performs the brunt of the work of a stage in the pipeline"""
        # If we're not stalled (because say decoder needs more words for current instruction)
        if not self.stall.get():
            if not self.fifo.is_full():
                # Check pc for validity
                if self.pc.get() < len(self.memory):
                    # Read the next entry from "instruction memory" while there's space and pc is valid
                    self.fifo.write(self.memory[self.pc.get()])
                    # If there's space we can increment the "PC" which for now is just a "fetch" pointer
                    self.pc.set(self.pc.get()+1)
                else:
                    # Fill the buffer with noops because we don't want to keep reporting stalls
                    self.fifo.write(Nop())

class RegisterSlot(Sync):
    """Represents a register slot in a renamable file"""
    def __init__(self):
        super().__init__()
        # The register itself
        self.reg = Register(0)
        
        # Flag set when register is allocated during decode (indicating it cannot be used)
        self.allocated = Register(False)
        
        # Flag set when register is written by EU (indicating dependents can dispatch)
        self.valid = Register(False)

        # Index of real register that this one is mapped to
        # self.index = Register(0)
        self.submodules += [self.reg, self.allocated, self.valid]
    def set_valid(self, value):
        if self.allocated.get() == False:
            raise RuntimeError("Cannot make an unallocated register valid")
        # TODO: This is probably dumb since in reality you might not constantly be churning to a new allocation? idk maybe you have to be to make any of this work?
        if self.valid.get() == True:
            raise RuntimeError("Tried to set a value for a register that's already been written")
        self.reg.set(value)
        self.valid.set(True)
    def is_valid(self):
        return self.valid.get()
    def is_allocated(self):
        return self.allocated.get()
    def allocate(self):
        self.allocated.set(True)
        self.valid.set(False)
    def get(self):
        if not self.is_allocated():
            raise RuntimeError("Tried to read unallocated register value")
        if not self.is_valid():
            raise RuntimeError("Tried to read unset register value (non-valid)")
        return self.reg.get()
    def __str__(self):
        return f"RegisterSlot(reg = {self.reg}, allocated = {self.allocated}, valid = {self.valid})"

class DecodeStage(Sync):
    """Decode just handles performing register renaming and filling the dispatch queue for the dispatch unit"""
    def __init__(self, fetch):
        super().__init__()
        # Store reference to the fetch stage so we can fetch and fill up the queue in eval
        self.fetch = fetch

        # TODO: Support changing dispatch queue capacity
        # Dispatch queue holds instructions from fetch after register renaming has been performed
        self.dispatch_queue = Fifo(4)
        # Holds our collection of rename backing registers
        # (in real hardware this would probably be some sort of fifo with random access or something) but this is far easier to integrate with our Sync
        # submodule system (since RegisterSlots being shoved into a fifo at reset can't be easily clocked for now)
        self.register_file = [RegisterSlot() for _ in range(32)]
        # "Fifo" used to hold indexes of unallocated registers to use in future mappings (starts with all but the initial mapping)       
        self.unallocated = list(range(8, 32))

        # Mark allocated and valid the initial register mappings
        for i in range(8):
            # Hack to overwrite without clock (instead of making reset valid)
            register = self.register_file[i]
            register.allocated.value = True
            register.valid.value = True
            #register.index.value = i
        # Mapping of "named" register to their actual slot
        self.name_map = list(range(8))

        # Update dispatch queue during clocks
        self.submodules += self.dispatch_queue,
        # Update each register in the register file on clock
        self.submodules += self.register_file
    def eval(self):
        # We have to stall ourselves if we run out of target registers to use as renaming targets, since we cannot clear this until downstream loads free up the allocation
        # otherwise we will pull the next instruction to "decode" and rename, but there will be no registers to use anymore
        # I think real hardware has a saner idea of how to free up registers to avoid this (pushing stalls to later stages and using unrenamed registers), however if we don't
        # always assume that a rename target after decode is free, I cannot think of a good solution to solve the interdependency right now
        if len(self.unallocated) > 0:
            # We cannot dispatch so do not fetch
            if not self.dispatch_queue.is_full():
                # We cannot fetch new instructions when the fetch fifo is empty
                if not self.fetch.fifo.is_empty():
                    # Read an instruction
                    next_inst = self.fetch.fifo.read()
                    # store original print for output later
                    inst_orig_debug = str(next_inst)
                    # Rename the inputs first since the output renaming will screw up our map
                    for (i, unrenamed_input) in enumerate(next_inst.in_registers):
                        next_inst.in_registers[i] = self.name_map[unrenamed_input]
                    # Theoretically loads "free up" a previously allocated register, since future instructions will not be able to name the previous register anymore to access it
                    # however the next load in a real processor (and likely even here) can easily be decoded before the previous instructions depending on it have finished dispatching
                    # meaning that the "allocated+valid" combination we need to free up a register will not be possible
                    # For now we assume that we will never run without enough allocatable registers (in this case we only have self modifying instructions so that number is just 1)
                    for (i,unrenamed_output) in enumerate(next_inst.out_registers):
                        # Loads pull a new register free and change our name target
                        new_name = self.unallocated.pop()
                        # Get the register and set it as allocated and invalid
                        reg = self.register_file[new_name]
                        print(f"Attempting to allocate register #{new_name}, reg {reg}")
                        reg.allocate()
                        print(f"Allocated register #{new_name}, reg {reg}")
                        # Rename the register for future uses
                        self.name_map[unrenamed_output] = new_name
                        # Rename the output of the instruction
                        next_inst.out_registers[i] = new_name
                    # HACK: Make aborts depend on every value before them that's been allocated
                    # TODO: Above
                    # Now that the instruction has had inputs and outputs renamed properly we can send it for dispatch
                    self.dispatch_queue.write(next_inst)
                    print(f"Decoded {inst_orig_debug} to {next_inst}")
                else:
                    print("Decode is stalled on fetch queue being empty")
            else:
                print("Decode is stalled on dispatch queue being full")
        else:
            print("Decode is stalled on no unallocated registers")

class ExecutionUnit(Sync):
    """Represents an execution unit that can process inputs when dependencies are valid and mark outputs as valid"""
    def __init__(self, decode, name="Unnamed"):
        super().__init__()
        # Store decode stage to access register file
        self.decode = decode
        self.name = name 
        # Stores current inputs and outputs, updated on fetch from queue which happens when non-empty and output gets written
        self.inputs = []
        self.outputs = []
        # Queue of instructions to potentially execute
        self.queue = Fifo(4)
        self.submodules += [self.queue]
    def eval(self):
        # When the inputs are empty we know that this is fresh after reset and we should try to execute more instructions
        if len(self.inputs) == 0:
            # Cannot execute if nothing has been dispatched to us yet
            if not self.queue.is_empty():
                # Read a dispatched instruction
                instr = self.queue.read()
                # Fill our inputs and outputs
                self.inputs = instr.in_registers
                self.outputs = instr.out_registers
            else:
                print(f"Execution unit {self.name} is stalled on dispatch")
                return
        # Keep track of failures and stop trying if we hit one
        has_failed = False
        # Cannot execute if our input registers are not valid yet
        for input in self.inputs:
            if self.decode.register_file[input].is_valid() == False:
                print(f"Execution unit {self.name} (writing to {self.outputs}) is stalled on input reg {input} becoming valid")
                has_failed = True
                break
        # Theoretically we should check if we can write our output yet somehow to allow future support for stalling in EUs instead of decode
        if not has_failed:
            # If all of our inputs are valid we can now write to our outputs
            print(f"Execution unit {self.name} is writing to {self.outputs}")
            for output in self.outputs:
                # We don't actually write a value (which is what the argument takes), but set_valid(0) looks like setting invalid
                # TODO: Rename method or something so above doesn't happen
                self.decode.register_file[output].set_valid(1)
                print(f"Register written to is {self.decode.register_file[output]}")
            # At this point we can say the instruction has been "executed" and the EU's resources are freed up to handle another
            # Clear inputs and outputs which represent not having anything yet and needing to fetch the next
            self.inputs.clear()
            self.outputs.clear()

class DispatchStage(Sync):
    """Dispatches instructions to execution units"""
    def __init__(self, decode):
        """decode is a module that provides the instruction with it's list of input and output registers (corrected after register renaming)"""
        super().__init__()
        # Store decode to pull instructions in eval
        self.decode = decode
        # TODO: Move ExecutionUnits to a seperate argument or something
        self.load_store_units = [ExecutionUnit(decode, "Load_Store_0")]
        self.arithmetic_units = [ExecutionUnit(decode, "ALU_0")]
        # We have to potentially hold an instruction since if we get stalled we cannot put it back in the dispatch queue to try again
        self.stall_hold = None
        self.submodules += self.load_store_units
        self.submodules += self.arithmetic_units
    def eval(self):
        # Try to pull a new instruction or use our current stall instruction if it exists
        next_inst = self.stall_hold
        # If no stall instruction exists we have to try and pull a new one from the dispatch queue
        if next_inst is None:
            # Must not be empty first though
            if not self.decode.dispatch_queue.is_empty():
                next_inst = self.decode.dispatch_queue.read()
            else:
                print("Dispatch stage stalled on empty dispatch queue")
        if next_inst is not None:
            self.have_inst(next_inst)
        self.post_eval()
    def have_inst(self, next_inst):
        # We have a valid instruction to try and dispatch
        # Dispatch to different EUs depending on what type of instruction it is
        eu_type = None
        if isinstance(next_inst, (Load, Store)):
            eu_list = self.load_store_units
            eu_type = "LoadStore"
        elif isinstance(next_inst, (Add,)):
            eu_list = self.arithmetic_units
            eu_type = "Alu"
        elif isinstance(next_inst, (Nop, Abort)):
            # Nops don't need to be dispatched
            eu_list = None
        else:
            raise RuntimeError(f"Instruction {next_inst} did not match a type with a valid queue")
        if eu_list is not None:
            # Try to dispatch to an EU with space
            found_eu = False
            for eu in eu_list:
                if not eu.queue.is_full():
                    found_eu = True
                    # Actually do the dispatch
                    eu.queue.write(next_inst)
                    print(f"Dispatched {next_inst} to EU {eu.name}")
                    break
            if not found_eu:
                self.stall_hold = next_inst
                print(f"Dispatch stalled because no free EUs existed to handle type {eu_type}")
        else:
            print(f"Dispatched f{next_inst} as a nop")
    def post_eval(self):
        # Eval all our execution units at the end since this would really then jump into the execution phase
        for unit in self.load_store_units:
            unit.eval()
        for unit in self.arithmetic_units:
            unit.eval()
memory = [
    Load(0),  # allocate register for 0
    Load(1),  # allocate register for 1
    Add(0,1), # allocate register for result, 0 is now safe to write over since new register has been allocated
    Store(0), # 
    Load(2),  # allocate register for 2
    Add(2,1), # allocate register for result, 2 is now safe to write over since new register has been allocated
    Store(2), # 
    Add(2,0), # allocate register for result, 2 is now safe to write over since new register has been allocated
    Store(2), #
    Abort(),  # Ender
]
# Unallocated - Unused register
# Allocated but invalid - location has been allocated for a result but result has not been placed there yet
# Allocated and valid - location has been allocated for a result and the result is valid for use by dependencies
# Allocated and valid is the stating value of the initial mappings for the named registers

# After Load(0)
# Named idx - Backing idx
# 0 - 0 (allocated,invalid) - 0 has been reallocated (since a load allows us to clear dependencies) but isn't valid since load hasn't completed
# 1 - 1 (allocated, valid) - initial allocation
# 2 - 2 (allocated, valid) - initial allocation

# After Load(1)
# 0 - 0 (allocated,?) - we don't know yet but we know it's still allocated
# ...
fetch = FetchStage(memory)
decode = DecodeStage(fetch)
dispatch = DispatchStage(decode)
stages = [fetch, decode, dispatch]
cycle_count = 0
while True:
    print(f"Cycle {cycle_count}")
    for stage in stages:
        stage.eval()
    for stage in stages:
        stage.clock()
    print()
    cycle_count += 1
