#!/bin/bash

#
# Intcode implementation in bash.
#

INPUTFILE=./aoc_erlang/apps/aoc_erlang/priv/inputs/2019/input09.txt

function read_program()
{
    IFS=","
    i=0
    for n in $(cat $INPUTFILE); do
        echo -n " [$i]=$n "
        let i=i+1
    done
}

# Load program into PROG
eval "declare -A PROG=( $(read_program) )"

# Program counter
PC=0

# Relative base register
RELBASE=0

# Addressing modes
MODE_POS=0
MODE_IMM=1
MODE_REL=2

# Opcodes
OP_ADD=1
OP_MUL=2
OP_INPUT=3
OP_OUTPUT=4
OP_JUMP_IF_TRUE=5
OP_JUMP_IF_FALSE=6
OP_LESS_THAN=7
OP_EQUALS=8
OP_ADJ_RELBASE=9
OP_END=99

function read_mem()
{
    ADDR=$PC
    echo ${PROG[$ADDR]}
}

while true; do
    # PCX is PC + offset X
    PC0=$PC
    (( PC1=$PC + 1 ))
    (( PC2=$PC + 2 ))
    (( PC3=$PC + 3 ))

    # MEMX is memory read from PC in positional mode + offset X
    (( MEM0=$(read_mem $PC0 $MODE_POS) ))
    (( MEM1=$(read_mem $PC1 $MODE_POS) ))
    (( MEM2=$(read_mem $PC2 $MODE_POS) ))
    (( MEM3=$(read_mem $PC3 $MODE_POS) ))

    (( OP0=MEM0 % 100 ))
    (( M1=(MEM1 / 100) % 10 ))
    (( M2=(MEM2 / 1000) % 10 ))
    (( M3=(MEM3 / 10000) % 10 ))

    echo $MEM0
    echo $OP0
    echo $M1
    echo $M2
    echo $M3

    case $OP0 in
    esac
done
