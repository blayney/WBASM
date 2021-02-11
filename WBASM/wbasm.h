#include <iostream>
#include <string>
#include <bitset>
#include <map>
/*
    Code by Will Blayney 2021 - adapted from my own assembler for SAP-3 from 2019.
    from http://nparker.llx.com/a2/opcodes.html (decoded instruction reference): 
    Most instructions that explicitly reference memory locations have bit patterns of the form aaabbbcc.
    The aaa and cc bits determine the opcode, and the bbb bits determine the addressing mode.
*/
namespace wbasm
{
    using d_OPCODE      = char[3];
    using d_OPERAND     = char[8];
    using d_INSTRUCTION = char[64];

    using a_OPCODE      = std::bitset<3>;      // up to 3 bits
    using a_DMOD        = std::bitset<2>;      // up to 2 bits, opcode space 2^5
    using a_DMOX        = std::bitset<5>;      // concat a_DMOD, a_OPCODE (aaa...cc)
    using a_OPERAND     = std::bitset<16>;     // up to 16 bit address space
    using a_INSTRUCTION = std::bitset<32>;     // up to aaabbbcc

    enum d_OPCODE {
        ADC, AND, ASL, BCC,
        BCS, BEQ, BIT, BMI,
        BNE, BPL, BRK, BVC,
        BVS, CLC, CLD, CLI,
        CLV, CMP, CPX, CPY,
        DEC, DEX, DEY, EOR,
        INC, INX, INY, JMP,
        JSR, LDA, LDX, LDY,
        LSR, NOP, ORA, PHA,
        PHP, PLA, PLP, ROL,
        ROR, RTI, RTS, SBC,
        SEC, SED, SEI, STA,
        STX, STY, TAX, TAY,
        TSX, TXA, TXS, TYA
    };

    using d_a_OPCODES = std::map<d_OPCODE, a_DMOX>;
    struct d_a_map;
}

struct wbasm::d_a_map
{                                               // desc                         register transfer logic
    d_OPCODE ADC = { 'A', 'D', 'C' };           // add with carry               A,Z,C,N = A+M+C
    d_OPCODE AND = { 'A', 'N', 'D' };           // alu and                      A,Z,N = A&M
    d_OPCODE ASL = { 'A', 'S', 'L' };           // arithmetic shift left        A,Z,C,N = M*2 or M,Z,C,N = M*2

    d_OPCODE BCC = { 'B', 'C', 'C' };           // branch if carry clear
    d_OPCODE BCS = { 'B', 'C', 'S' };           // branch if carry set 
    d_OPCODE BEQ = { 'B', 'E', 'Q' };           // branch if equal 
    d_OPCODE BIT = { 'B', 'I', 'T' };           // bit test                     A & M, N = M7, V = M6
    d_OPCODE BMI = { 'B', 'M', 'I' };           // branch if compliment
    d_OPCODE BNE = { 'B', 'N', 'E' };           // branch if ! equal
    d_OPCODE BPL = { 'B', 'P', 'L' };           // branch if +ve
    d_OPCODE BRK = { 'B', 'R', 'K' };           // force interrupt (as fsi--)
    d_OPCODE BVC = { 'B', 'V', 'C' };           // branch if overflow clear
    d_OPCODE BVS = { 'B', 'V', 'S' };           // branch if overflow set

    d_OPCODE CLC = { 'C', 'L', 'C' };           // clear carry flag             C=0
    d_OPCODE CLD = { 'C', 'L', 'D' };           // clear decimal mode           D=0
    d_OPCODE CLI = { 'C', 'L', 'I' };           // clear interrupt disable      I=0
    d_OPCODE CLV = { 'C', 'L', 'V' };           // clear overflow flag          V=0
    d_OPCODE CMP = { 'C', 'M', 'P' };           // compare                      Z,C,N = A-M
    d_OPCODE CPX = { 'C', 'P', 'X' };           // compare x register           Z,C,N = X-M
    d_OPCODE CPY = { 'C', 'P', 'Y' };           // compare y register           Z,C,N = Y-M

    d_OPCODE DEC = { 'D', 'E', 'C' };           // decrement memory             M,Z,N = M-1
    d_OPCODE DEX = { 'D', 'E', 'X' };           // decrement x register         X,Z,N = X-1
    d_OPCODE DEY = { 'D', 'E', 'Y' };           // decrement y register         Y,Z,N = Y-1
    
    d_OPCODE EOR = { 'E', 'O', 'R' };           // exclusive or                 A,Z,N = A^M

    d_OPCODE INC = { 'I', 'N', 'C' };           // increment memory             M,Z,N = M+1
    d_OPCODE INX = { 'I', 'N', 'X' };           // increment x register         X,Z,N = X+1
    d_OPCODE INY = { 'I', 'N', 'Y' };           // increment y register         Y,Z,N = Y+1

    d_OPCODE JMP = { 'J', 'M', 'P' };           // jump!                        (pc -> n)
    d_OPCODE JSR = { 'J', 'S', 'R' };           // jump to subroutine           (pc -> n[sr])

    d_OPCODE LDA = { 'L', 'D', 'A' };           // load accumulator             A,Z,N = M
    d_OPCODE LDX = { 'L', 'D', 'X' };           // load x register              X,Z,N = M
    d_OPCODE LDY = { 'L', 'D', 'Y' };           // load y register              Y,Z,N = M
    d_OPCODE LSR = { 'L', 'S', 'R' };           // logical shift right          A,C,Z,N = A/2 or M,C,Z,N = M/2
   
    d_OPCODE NOP = { 'N', 'O', 'P' };           // no operation                 (pc++)

    d_OPCODE ORA = { 'O', 'R', 'A' };           // logical ior                  A,Z,N = A|M

    d_OPCODE PHA = { 'P', 'H', 'A' };           // push accumulator             stack << A
    d_OPCODE PHP = { 'P', 'H', 'P' };           // push processor status        stack << C,Z,I,D,B,V,N
    d_OPCODE PLA = { 'P', 'L', 'A' };           // pull accumulator             stack >  A
    d_OPCODE PLP = { 'P', 'L', 'P' };           // pull processor status        stack >  C,Z,I,D,B,V,N

    d_OPCODE ROL = { 'R', 'O', 'L' };           // rotate left              
    d_OPCODE ROR = { 'R', 'O', 'R' };           // rotate right
    d_OPCODE RTI = { 'R', 'T', 'I' };           // return from interrupt
    d_OPCODE RTS = { 'R', 'T', 'S' };           // return from subroutine

    d_OPCODE SBC = { 'S', 'B', 'C' };           // subtract with carry          A,Z,C,N = A-M-(1-C)
    d_OPCODE SEC = { 'S', 'E', 'C' };           // set carry flag               C = 1
    d_OPCODE SED = { 'S', 'E', 'D' };           // set decimal flag             D = 1
    d_OPCODE SEI = { 'S', 'E', 'I' };           // set interrupt disable        I = 1
    d_OPCODE STA = { 'S', 'T', 'A' };           // store accumulator            M = A 
    d_OPCODE STX = { 'S', 'T', 'X' };           // store x register             M = X
    d_OPCODE STY = { 'S', 'T', 'Y' };           // store y register             M = Y

    d_OPCODE TAX = { 'T', 'A', 'X' };           // transfer accumulator to X    X = A
    d_OPCODE TAY = { 'T', 'A', 'Y' };           // transfer accumulator to Y    Y = A
    d_OPCODE TSX = { 'T', 'S', 'X' };           // transfer stack pointer to X  X = S
    d_OPCODE TXA = { 'T', 'A', 'Y' };           // transfer X to accumulator    A = X
    d_OPCODE TXS = { 'T', 'X', 'S' };           // transfer X to stack pointer  S = X
    d_OPCODE TYA = { 'T', 'Y', 'A' };           // transfer Y to accumulator    A = Y

    std::map<d_OPCODE, a_OPCODE> d_a_opmap
    {
        {ADC, 000},
    };
};
