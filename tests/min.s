.text
main:
  move $fp, $sp
  li   $t0, 42
  move $a0, $t0
  li   $v0, 1
  syscall
  li   $t0, 0
main_exit:
  li   $v0, 10
  syscall
.data
