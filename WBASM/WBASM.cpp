#include <iostream>
#include <string>
#include <bitset>
/*
    Code by Will Blayney 2021 - adapted from my own assembler for SAP-3 from 2019. 

    from http://nparker.llx.com/a2/opcodes.html: 
    Most instructions that explicitly reference memory locations have bit patterns of the form aaabbbcc. 
    The aaa and cc bits determine the opcode, and the bbb bits determine the addressing mode.
*/
namespace wbasm
{
    using d_OPCODE = char[3];
    using d_OPERAND = char[8];
    using d_INSTRUCTION = char[64];

    using a_OPCODE = std::bitset<3>;
    using a_OPERAND = std::bitset<16>; // up to 16 bit address space
    using a_INSTRUCTION = std::bitset<32>; // up to aaabbbcc

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
}

int main()
{
    std::cout << "WBASM!\n";
}

int assemble()
{
    
}