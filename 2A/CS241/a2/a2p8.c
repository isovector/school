beq $0, $0, program
DECLARE(index)


fnNthIndex:
    PUSH(2, $27, $28)
    VAR(index, $27)
    CONST($28, 4)
    mult $27, $28
    mflo $28
    add $28, $1, $28
    RETURN($28, 2, $27, $28)

fnNodeHeight:
    PUSH(10, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)
    PUSH(10, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29)
    CONST($21, -1)
    CONST($24, 4)
    
    VAR(index, $10)
    CALL(fnNthIndex)
    VAR(return, $11)
    
    ; with left side
    lw $12, 4($11)
    bne $12, $21, lrecurs
    sub $18, $0, $21
    beq $0, $0, rside
    
    lrecurs:
    STORE(index, $12)
    CALL(fnNodeHeight)
    VAR(return, $18)
    sub $18, $18, $21
    
    rside:
    ; with right side
    lw $12, 8($11)
    bne $12, $21, rrecurs
    sub $19, $0, $21
    beq $0, $0, compare
    
    rrecurs:
    STORE(index, $12)
    CALL(fnNodeHeight)
    VAR(return, $19)
    sub $19, $19, $21
    
    ; check which is bigger
    compare:
    slt $11, $18, $19
    bne $11, $0, eighteen
    POP(10, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)
    RETURN($19, 10, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29)
    
    eighteen:
    POP(10, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)
    RETURN($18, 10, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29)

program:
    STORE(index, $0)
    CALL(fnNodeHeight)
    VAR(return, $3)
    jr $31