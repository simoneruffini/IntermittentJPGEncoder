# IntermittentJPGEncoder
VHDL implementation of a JPEG/JFIF Encoder under Transient Computing constraits 

# What You Need
- Vivado 2021.2 to simulate the project
- Go to run the scripts
# Todos

TODO AC_CR_ROM and AC_ROM can be taken from new but all instantiations must drop the reset  
TODO check ctrlSM  
TODO export files in bpm p3  
TODO change ramsim to sub_ramz  
TODO  In CntrlSM this line: 
`if idle(NUM_STAGES downto 1) = (NUM_STAGES-1 downto 0 => '1') then` matches this 
```
idle: std_logic_vector(NUM_STAEGS+1 downto 1) is a 7 bit vector 
idle                          : [7,6,5,4,3,2,1]
idle(NUM_STAGES downto 1)     :   [6,5,4,3,2,1] is a 6 bit vector
(NUM_STAGES-1 downto 0 => '1'):   [5,4,3,2,1,0] is a 6 bit vector
                                  [1,1,1,1,1,1] filled with 1
                                
hence idle(NUM_STAGES downto 1) = (NUM_STAGES-1 downto 0 => '1') checks if idle(NUM_STAGES downto 1) is filled with ones.
A better version would be:

hence idle(NUM_STAGES downto 1) = (NUM_STAGES downto 1 => '1') 
that would be the same thing
``` 
So change to the last example that line. The new change instead is:
`if idle(NUM_STAGES+1 downto 1) = (NUM_STAGES+1 downto 1 => '1') then` 
which checks idle(7) that is not used by anyone not event by sn7
