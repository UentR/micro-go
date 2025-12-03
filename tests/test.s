.text
main:
  move $fp, $sp
main_exit:
  li   $v0, 10
  syscall
print_struct_S:
  beqz $a0, _label_0
  addi $sp, $sp, -4
  sw   $s0, 0($sp)
  move $s0, $a0
  la   $a0, _label_1
  li   $v0, 4
  syscall
  lw   $a0, 0($s0)
  li   $v0, 1
  syscall
  la   $a0, _label_2
  li   $v0, 4
  syscall
  lw   $s0, 0($sp)
  addi $sp, $sp, 4
  jr   $ra
_label_0:
  la   $a0, _label_3
  li   $v0, 4
  syscall
  jr   $ra
.data
_label_4:
  .asciiz " "
_label_3:
  .asciiz "<nil>"
_label_2:
  .asciiz "}"
_label_1:
  .asciiz "&{"
