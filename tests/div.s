.text
div1:
  addi $sp, $sp, -4
  sw   $ra, 0($sp)
  addi $sp, $sp, -4
  sw   $fp, 0($sp)
  move $fp, $sp
  addi $t0, $fp, 16
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 20
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  slt  $t0, $t0, $t1
  bnez $t0, _label_0
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 16
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 20
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sub  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 16
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  jal  div1
  addi $sp, $sp, 16
  li   $t0, 1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  add  $t0, $t0, $t1
  lw   $t1, 12($fp)
  sw   $t0, 0($t1)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  lw   $t1, 8($fp)
  sw   $t0, 0($t1)
  j    exit_div1
  j    _label_1
_label_0:
  li   $t0, 0
  lw   $t1, 12($fp)
  sw   $t0, 0($t1)
  addi $t0, $fp, 20
  lw   $t0, 0($t0)
  lw   $t1, 8($fp)
  sw   $t0, 0($t1)
  j    exit_div1
_label_1:
exit_div1:
  move $sp, $fp
  lw   $fp, 0($sp)
  addi $sp, $sp, 4
  lw   $ra, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
div2:
  addi $sp, $sp, -4
  sw   $ra, 0($sp)
  addi $sp, $sp, -4
  sw   $fp, 0($sp)
  move $fp, $sp
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  j    _label_2
_label_3:
  addi $t0, $fp, -4
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  lw   $t0, 0($t0)
  addi $t0, $t0, 1
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t0, 0($t1)
  addi $t0, $fp, 16
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 20
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sub  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 20
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
_label_2:
  addi $t0, $fp, 16
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 20
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sge  $t0, $t0, $t1
  bnez $t0, _label_3
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  lw   $t1, 12($fp)
  sw   $t0, 0($t1)
  addi $t0, $fp, 20
  lw   $t0, 0($t0)
  lw   $t1, 8($fp)
  sw   $t0, 0($t1)
  j    exit_div2
exit_div2:
  move $sp, $fp
  lw   $fp, 0($sp)
  addi $sp, $sp, 4
  lw   $ra, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
div3:
  addi $sp, $sp, -4
  sw   $ra, 0($sp)
  addi $sp, $sp, -4
  sw   $fp, 0($sp)
  move $fp, $sp
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $a0, 8
  li   $v0, 9
  syscall
  move $t0, $v0
  li   $t1, 0
_label_6:
  li   $t2, 8
  bge  $t1, $t2, _label_7
  add  $t2, $t0, $t1
  sw   $zero, 0($t2)
  addi $t1, $t1, 4
  j    _label_6
_label_7:
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 0
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  j    _label_4
_label_5:
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  lw   $t0, 0($t0)
  addi $t0, $t0, 1
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t0, 0($t1)
  addi $t0, $fp, 8
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 12
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sub  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 12
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
_label_4:
  addi $t0, $fp, 8
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, 12
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sge  $t0, $t0, $t1
  bnez $t0, _label_5
  addi $t0, $fp, 12
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  move $v0, $t0
  j    exit_div3
exit_div3:
  move $sp, $fp
  lw   $fp, 0($sp)
  addi $sp, $sp, 4
  lw   $ra, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
main:
  move $fp, $sp
  addi $sp, $sp, -4
  sw   $zero, 0($sp)
  addi $sp, $sp, -4
  sw   $zero, 0($sp)
  li   $t0, 45
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 6
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 12
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 12
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  jal  div1
  addi $sp, $sp, 16
  lw   $t0, 4($sp)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $a0, 32
  li   $v0, 11
  syscall
  lw   $t0, 0($sp)
  move $a0, $t0
  li   $v0, 1
  syscall
  addi $sp, $sp, 8
  li   $t0, 0
  la   $t0, _label_11
  move $a0, $t0
  li   $v0, 4
  syscall
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $zero, 0($sp)
  addi $sp, $sp, -4
  sw   $zero, 0($sp)
  li   $t0, 45
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 6
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 12
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 12
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  jal  div2
  addi $sp, $sp, 16
  lw   $t0, 4($sp)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $a0, 32
  li   $v0, 11
  syscall
  lw   $t0, 0($sp)
  move $a0, $t0
  li   $v0, 1
  syscall
  addi $sp, $sp, 8
  li   $t0, 0
  la   $t0, _label_10
  move $a0, $t0
  li   $v0, 4
  syscall
  li   $t0, 0
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 45
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 6
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  jal  div3
  addi $sp, $sp, 8
  move $t0, $v0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 0
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $a0, 32
  li   $v0, 11
  syscall
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 4
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
  la   $t0, _label_9
  move $a0, $t0
  li   $v0, 4
  syscall
  li   $t0, 0
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  move $a0, $t0
  jal  print_struct_res
  li   $t0, 0
  la   $t0, _label_8
  move $a0, $t0
  li   $v0, 4
  syscall
  li   $t0, 0
main_exit:
  li   $v0, 10
  syscall
print_struct_res:
  beqz $a0, _label_12
  addi $sp, $sp, -4
  sw   $s0, 0($sp)
  move $s0, $a0
  la   $a0, _label_13
  li   $v0, 4
  syscall
  lw   $a0, 0($s0)
  li   $v0, 1
  syscall
  la   $a0, _label_16
  li   $v0, 4
  syscall
  lw   $a0, 4($s0)
  li   $v0, 1
  syscall
  la   $a0, _label_14
  li   $v0, 4
  syscall
  lw   $s0, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
_label_12:
  la   $a0, _label_15
  li   $v0, 4
  syscall
  jr   $ra
.data
_label_16:
  .asciiz " "
_label_15:
  .asciiz "<nil>"
_label_14:
  .asciiz "}"
_label_13:
  .asciiz "&{"
_label_11:
  .asciiz "\n"
_label_10:
  .asciiz "\n"
_label_9:
  .asciiz "\n"
_label_8:
  .asciiz "\n"
