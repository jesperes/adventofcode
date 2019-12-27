#!/bin/bash

#
# Intcode implementation in bash.
#

# Treat undefined variables as errors
set -u

INPUTFILE=$1
INPUT=$2

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
    OP=$1
    MOD=$2
    case $MOD in
        $MODE_POS)
            echo ${PROG[$OP]}
            ;;
        $MODE_IMM)
            echo $OP
            ;;
        $MODE_REL)
            (( RELADDR=$OP+$RELBASE ))
            echo ${PROG[$RELADDR]}
            ;;
    esac
}

function write_addr() {
    OP=$1
    MOD=$2

    case $MOD in
        $MODE_POS)
            echo $OP
            ;;
        $MODE_REL)
            (( RELADDR=$OP+$RELBASE ))
            echo $RELADDR
            ;;
    esac
}

while true; do
    # PCX is PC + offset X
    PC0=$PC
    MEM0=$(read_mem $PC0 $MODE_POS)
    (( OP0=($MEM0 % 100) ))

    # echo "TRACE: ${PC}=${MEM0}"

    case $OP0 in
        $OP_ADD)
            (( PC1=$PC + 1 ))
            (( PC2=$PC + 2 ))
            (( PC3=$PC + 3 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            OP2=$(read_mem $PC2 $MODE_POS)
            OP3=$(read_mem $PC3 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            (( M2=($MEM0 / 1000) % 10 ))
            (( M3=($MEM0 / 10000) % 10 ))
            V1=$(read_mem $OP1 $M1)
            V2=$(read_mem $OP2 $M2)
            (( V3=$V1+$V2 ))
            ADDR=$(write_addr $OP3 $M3)
            PROG[$ADDR]=$V3
            (( PC=$PC+4 ))
            ;;
        $OP_MUL)
            (( PC1=$PC + 1 ))
            (( PC2=$PC + 2 ))
            (( PC3=$PC + 3 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            OP2=$(read_mem $PC2 $MODE_POS)
            OP3=$(read_mem $PC3 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            (( M2=($MEM0 / 1000) % 10 ))
            (( M3=($MEM0 / 10000) % 10 ))
            V1=$(read_mem $OP1 $M1)
            V2=$(read_mem $OP2 $M2)
            (( V3=$V1*$V2 ))
            ADDR=$(write_addr $OP3 $M3)
            PROG[$ADDR]=$V3
            (( PC=$PC+4 ))
            ;;
        $OP_JUMP_IF_TRUE)
            (( PC1=$PC + 1 ))
            (( PC2=$PC + 2 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            OP2=$(read_mem $PC2 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            (( M2=($MEM0 / 1000) % 10 ))
            V1=$(read_mem $OP1 $M1)
            V2=$(read_mem $OP2 $M2)
            if [ $V1 = 0 ]; then
                (( PC=$PC+3 ))
            else
                PC=$V2
            fi
            ;;
        $OP_JUMP_IF_FALSE)
            (( PC1=$PC + 1 ))
            (( PC2=$PC + 2 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            OP2=$(read_mem $PC2 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            (( M2=($MEM0 / 1000) % 10 ))
            V1=$(read_mem $OP1 $M1)
            V2=$(read_mem $OP2 $M2)
            if [ $V1 = 0 ]; then
                PC=$V2
            else
                (( PC=$PC+3 ))
            fi
            ;;
        $OP_LESS_THAN)
            (( PC1=$PC + 1 ))
            (( PC2=$PC + 2 ))
            (( PC3=$PC + 3 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            OP2=$(read_mem $PC2 $MODE_POS)
            OP3=$(read_mem $PC3 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            (( M2=($MEM0 / 1000) % 10 ))
            (( M3=($MEM0 / 10000) % 10 ))
            V1=$(read_mem $OP1 $M1)
            V2=$(read_mem $OP2 $M2)
            ADDR=$(write_addr $OP3 $M3)
            if (( $V1<$V2 )); then
                PROG[$ADDR]=1
            else
                PROG[$ADDR]=0
            fi
            (( PC=$PC+4 ))
            ;;
        $OP_EQUALS)
            (( PC1=$PC + 1 ))
            (( PC2=$PC + 2 ))
            (( PC3=$PC + 3 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            OP2=$(read_mem $PC2 $MODE_POS)
            OP3=$(read_mem $PC3 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            (( M2=($MEM0 / 1000) % 10 ))
            (( M3=($MEM0 / 10000) % 10 ))
            V1=$(read_mem $OP1 $M1)
            V2=$(read_mem $OP2 $M2)
            ADDR=$(write_addr $OP3 $M3)
            if (( $V1==$V2 )); then
                PROG[$ADDR]=1
            else
                PROG[$ADDR]=0
            fi
            (( PC=$PC+4 ))
            ;;
        $OP_ADJ_RELBASE)
            (( PC1=$PC + 1 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            V1=$(read_mem $OP1 $M1)
            (( RELBASE=$RELBASE+$V1 ))
            (( PC=$PC+2 ))
            ;;
        $OP_INPUT)
            (( PC1=$PC + 1 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))

            # TODO get input
            ADDR=$(write_addr $OP1 $M1)
            PROG[$ADDR]=$INPUT
            (( PC=$PC+2 ))
            ;;
        $OP_OUTPUT)
            (( PC1=$PC + 1 ))
            OP1=$(read_mem $PC1 $MODE_POS)
            (( M1=($MEM0 / 100) % 10 ))
            V1=$(read_mem $OP1 $M1)
            (( PC=$PC+2 ))
            echo "OUTPUT: $V1"
            ;;
        $OP_END)
            exit 0
            ;;
        *)
            echo "Unknown op: $OP0"
            exit 1
            ;;
    esac
done
