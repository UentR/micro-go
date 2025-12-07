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
  li   $t0, 1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 6
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 2
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  add  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  li   $t0, 4
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  add  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  mul  $t0, $t0, $t1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -8
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  addi $t0, $fp, -4
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $a0, 32
  li   $v0, 11
  syscall
  addi $t0, $fp, -8
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
  li   $t0, 0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  la   $t0, _label_0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  addi $t0, $fp, -12
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sw   $t1, 0($t0)
  addi $t0, $fp, -12
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 4
  syscall
  li   $a0, 32
  li   $v0, 11
  syscall
  addi $t0, $fp, -12
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 4
  syscall
  li   $a0, 32
  li   $v0, 11
  syscall
  addi $t0, $fp, -12
  lw   $t0, 0($t0)
  move $a0, $t0
  li   $v0, 4
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
_label_0:
  .asciiz "Test"
