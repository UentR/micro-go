open Printf

type asm =
  | Nop
  | S of string
  | C of asm * asm

let (@@) x y = C (x, y)

type program = { text: asm; data: asm; }


let s0 = "$s0"
let t0 = "$t0"
let t1 = "$t1"
let t2 = "$t2"
let a0 = "$a0"
let v0 = "$v0"
let sp = "$sp"
let ra = "$ra"
let fp = "$fp"
let zero = "$zero"

(* From : http://arch2.scienceontheweb.net/lezioni/MIPS_SET_INSTRUCTION_SYSCALL.pdf *)
(* Arithmetic Instructions *)
(* abs *)
let add  r1 r2 r3  = S(sprintf "  add  %s, %s, %s" r1 r2 r3)
(* addu *)
let addi r1 r2 i   = S(sprintf "  addi %s, %s, %d" r1 r2 i)
(* adiu *)
let sub  r1 r2 r3  = S(sprintf "  sub  %s, %s, %s" r1 r2 r3)
(* subu *)
let mul  r1 r2 r3  = S(sprintf "  mul  %s, %s, %s" r1 r2 r3)
(* mulo *)
(* mulou *)
(* mult *)
(* multu *)
let div  r1 r2 r3  = S(sprintf "  div  %s, %s\n  mflo %s" r2 r3 r1)
(* divu *)
(* neg *)
(* negu *)


(* Logical Operations *)
let and_ r1 r2 r3  = S(sprintf "  and  %s, %s, %s" r1 r2 r3)
(* andi *)
(* nor *)
let not_ r1 r2     = S(sprintf "  not  %s, %s"     r1 r2)
let or_  r1 r2 r3  = S(sprintf "  or   %s, %s, %s" r1 r2 r3)
(* ori *)
let xor  r1 r2 r3  = S(sprintf "  xor  %s, %s, %s" r1 r2 r3)
(* xori *)
let rem  r1 r2 r3  = S(sprintf "  div  %s, %s\n  mfhi %s" r2 r3 r1)
(* remu *)


(* Rotate and Shift Instructions *)
(* rol *)
(* ror *)
(* sll *)
(* sllv *)
(* sra *)
(* srav *)
(* srl *)
(* srlv *)


(* Constant-Manipulating Instructions *)
let li   r1 i      = S(sprintf "  li   %s, %i"     r1 i)
(* lui *)



(* Comparison Instructions *)
let seq  r1 r2 r3  = S(sprintf "  seq  %s, %s, %s" r1 r2 r3)
let sge  r1 r2 r3  = S(sprintf "  sge  %s, %s, %s" r1 r2 r3)
(* sqeu *)
let sgt  r1 r2 r3  = S(sprintf "  sgt  %s, %s, %s" r1 r2 r3)
(* sgtu *)
let sle  r1 r2 r3  = S(sprintf "  sle  %s, %s, %s" r1 r2 r3)
(* sleu *)
let slt  r1 r2 r3  = S(sprintf "  slt  %s, %s, %s" r1 r2 r3)
(* sltu *)
(* slti *)
(* sltiu *)
let sne  r1 r2 r3  = S(sprintf "  sne  %s, %s, %s" r1 r2 r3)

(* Branch and Jump Instructions *)
(* b *)
(* bczt *)
(* bczf *)
(* beq *)
let beqz r1 l      = S(sprintf "  beqz %s, %s"     r1 l)
let bge  r1 r2 l   = S(sprintf "  bge  %s, %s, %s" r1 r2 l)
(* bgeu *)
(* bgez *)
(* bgezal *)
(* bgt *)
(* bgtu *)
(* bgtz *)
(* ble *)
(* bleu *)
(* blez *)
(* bltzal *)
(* blt *)
let bltz r1 l      = S(sprintf "  bltz %s, %s"     r1 l)
(* bne *)
let bnez r1 l      = S(sprintf "  bnez %s, %s"     r1 l)
let j    l         = S(sprintf "  j    %s"         l)
let jal  l         = S(sprintf "  jal  %s"         l)
(* jalr *)
let jr   r1        = S(sprintf "  jr   %s"         r1)


(* Load Instructions *)
let la   r1 x      = S(sprintf "  la   %s, %s"     r1 x)
(* lb *)
let lbu  r1 o r2   = S(sprintf "  lbu  %s, %d(%s)" r1 o r2)
(* ld *)
(* lh *)
(* lhu *)
(* lui *)
let lw   r1 o r2   = S(sprintf "  lw   %s, %d(%s)" r1 o r2)
(* lwcz *)
(* lwl *)
(* lwr *)
(* ulh *)
(* ulhu *)
(* ulw *)


(* Store Instructions *)
(* sb *)
(* sd *)
(* sh *)
let sw   r1 o r2   = S(sprintf "  sw   %s, %d(%s)" r1 o r2)
(* swl *)
(* swr *)
(* ush *)
(* usw *)


(* Data Movement Instructions *)
let move r1 r2     = S(sprintf "  move %s, %s"     r1 r2)
(* mfhi *)
(* mflo *)
(* mthi *)
(* mtlo *)
(* mfcz *)
(* mfc1.d *)
(* mtcz *)

(* Pass Floating points *)

(* Exception and Trap Instructions *)
(* rfe *)
let syscall        = S("  syscall")
(* break *)
let nop            = Nop


let label l        = S(sprintf "%s:" l)
let comment s      = S(sprintf "  # %s" s)


let rec ilist = function
    | []     -> ""
    | [i]    -> sprintf "%d" i
    | i :: l -> sprintf "%d, %s" i (ilist l)
let dword l  = S(sprintf "  .word %s" (ilist l))
let asciiz s = S(sprintf "  .asciiz %S" s) 

let push r = addi sp sp (-4) @@ sw r 0(sp)
let pop r = lw r 0(sp) @@ addi sp sp 4

let rec print_asm fmt a =
    match a with
    | Nop        -> ()
    | S s        -> fprintf fmt "%s\n" s
    | C (a1, a2) -> let () = print_asm fmt a1 in print_asm fmt a2

let print_program fmt p =
    fprintf fmt ".text\n";
    print_asm fmt p.text;
    fprintf fmt ".data\n";
    print_asm fmt p.data