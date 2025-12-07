.text
main:
  addi $sp, $sp, -4
  sw   $ra, 0($sp)
  addi $sp, $sp, -4
  sw   $fp, 0($sp)
  move $fp, $sp
  li   $t0, 6
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 8
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 4
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 1
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 3
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  add  $t0, $t0, $t1
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  mul  $t0, $t0, $t1
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  div  $t0, $t1
  mflo $t0
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 2
  addi $sp, $sp, -4
  sw   $t0, 0($sp)
  li   $t0, 40
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sub  $t0, $t0, $t1
  lw   $t1, 0($sp)
  addi $sp, $sp, 4
  sub  $t0, $t0, $t1
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
.data
