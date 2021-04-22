# IntermittentJPGEncoder
VHDL implementation of a JPEG/JFIF Encoder under Transient Computing constraints 

# What You Need
- Vivado 2021.2 to simulate the project
- Go to run the scripts
- [optional] [VSG](https://github.com/jeremiah-c-leary/vhdl-style-guide) to format the code

# Todos

- [] AC_CR_ROM and AC_ROM can be taken from new but all instantiations must drop the reset  
- [] export files in bpm p3  
- [] change ramsim to sub_ramz  
- [x] In CntrlSM this line:  ` if idle(NUM_STAGES downto 1) = (NUM_STAGES-1 downto 0 => '1') then` matches this: 
```VHDL
idle: std_logic_vector(NUM_STAEGS+1 downto 1) is a 7 bit vector 
idle                          : [7,6,5,4,3,2,1]
idle(NUM_STAGES downto 1)     :   [6,5,4,3,2,1] is a 6 bit vector
(NUM_STAGES-1 downto 0 => '1'):   [5,4,3,2,1,0] is a 6 bit vector
                                  [1,1,1,1,1,1] filled with 1
```                                
hence `idle(NUM_STAGES downto 1) = (NUM_STAGES-1 downto 0 => '1')` checks if `idle(NUM_STAGES downto 1)` is filled with ones. A better version would be:
```VHDL
idle(NUM_STAGES downto 1) = (NUM_STAGES downto 1 => '1')
```
that would be the same thing, so change such line to this.
