.text
main:
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
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 2
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 9
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -12
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  j    _label_0
_label_1:
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 2
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  div  $t0, $t1
  mfhi $t0
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sne  $t0, $t0, $t1
  bnez $t0, _label_2
  j    _label_3
_label_2:
  addi $t0, $fp, -12
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  mul  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -12
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
_label_3:
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  mul  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 2
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  div  $t0, $t1
  mflo $t0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
_label_0:
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sne  $t0, $t0, $t1
  bnez $t0, _label_1
  addi $t0, $fp, -12
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
exit_main:
  move $sp, $fp
  lw   $fp, 0($sp)
  addi $sp, $sp, 4
  lw   $ra, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
.data
