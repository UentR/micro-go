.text
test:
  addi $sp, $sp, -4
  sw   $ra, 0($sp)
  addi $sp, $sp, -4
  sw   $fp, 0($sp)
  move $fp, $sp
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 4
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
exit_test:
  move $sp, $fp
  lw   $fp, 0($sp)
  addi $sp, $sp, 4
  lw   $ra, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
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
  addi $sp, $sp, -8
  move $t1, $sp
  li   $t0, 45
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 6
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 8
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 12
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  jal  div1
  addi $sp, $sp, 16
  lw   $a0, 0($sp)
  li   $v0, 1
  syscall
  lw   $a0, 4($sp)
  li   $v0, 1
  syscall
  addi $sp, $sp, 8
  li   $t0, 0
  la   $t0, _label_9
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
  addi $sp, $sp, -8
  move $t1, $sp
  li   $t0, 45
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 6
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 8
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $sp, 12
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  jal  div2
  addi $sp, $sp, 16
  lw   $a0, 0($sp)
  li   $v0, 1
  syscall
  lw   $a0, 4($sp)
  li   $v0, 1
  syscall
  addi $sp, $sp, 8
  li   $t0, 0
  la   $t0, _label_8
  move $a0, $t0
  li   $v0, 1
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
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  addi $t0, $t0, 4
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
  la   $t0, _label_7
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
  la   $t0, _label_6
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
main_exit:
  li   $v0, 10
  syscall
.data
_label_9:
  .asciiz "\n"
_label_8:
  .asciiz "\n"
_label_7:
  .asciiz "\n"
_label_6:
  .asciiz "\n"
