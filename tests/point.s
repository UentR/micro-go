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
  li   $a0, 8
  li   $v0, 9
  syscall
  move $t0, $v0
  li   $t1, 0
_label_2:
  li   $t2, 8
  bge  $t1, $t2, _label_3
  add  $t2, $t0, $t1
  sw   $zero, 0($t2)
  addi $t1, $t1, 4
  j    _label_2
_label_3:
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $a0, 8
  li   $v0, 9
  syscall
  move $t0, $v0
  li   $t1, 0
_label_0:
  li   $t2, 8
  bge  $t1, $t2, _label_1
  add  $t2, $t0, $t1
  sw   $zero, 0($t2)
  addi $t1, $t1, 4
  j    _label_0
_label_1:
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 0
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 2
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 3
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  addi $t0, $t0, 0
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 4
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  addi $t0, $t0, 4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  addi $t0, $t0, 4
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  addi $t0, $t0, 0
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 4
  lw   $t0, 0($t0)
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 0
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  add  $t0, $t0, $t1
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  add  $t0, $t0, $t1
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  add  $t0, $t0, $t1
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
print_struct_point:
  beqz $a0, _label_4
  addi $sp, $sp, -4
  sw   $s0, 0($sp)
  move $s0, $a0
  la   $a0, _label_5
  li   $v0, 4
  syscall
  lw   $a0, 0($s0)
  li   $v0, 1
  syscall
  la   $a0, _label_8
  li   $v0, 4
  syscall
  lw   $a0, 4($s0)
  li   $v0, 1
  syscall
  la   $a0, _label_6
  li   $v0, 4
  syscall
  lw   $s0, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
_label_4:
  la   $a0, _label_7
  li   $v0, 4
  syscall
  jr   $ra
.data
_label_8:
  .asciiz " "
_label_7:
  .asciiz "<nil>"
_label_6:
  .asciiz "}"
_label_5:
  .asciiz "&{"
