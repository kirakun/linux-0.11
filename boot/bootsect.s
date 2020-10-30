!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!  指编译连接后system模块的大小。
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors  setup程序的扇区数（setup－sectors）值 setup有四个扇区
BOOTSEG  = 0x07c0			! original address of boot-sector  bootsect的原始地址（是段地址，以下同）
INITSEG  = 0x9000			! we move boot here - out of the way 将bootsect移到这里
SETUPSEG = 0x9020			! setup starts here  setup程序从这里开始
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).  system模块加载到10000(64kB)处.
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading    停止加载的段地址0x4000

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc  根文件系统设备在第一个硬盘的第一个分区上
ROOT_DEV = 0x306
;/* ************************************************************************
;	boot被bios－启动子程序加载至7c00h（31k）处，并将自己移动到了
;	地址90000h（576k）处，并跳转至那里。
;	它然后使用BIOS中断将'setup'直接加载到自己的后面（90200h）（576.5k），
;	并将system加载到地址10000h处。
;
;	注意：目前的内核系统最大长度限制为（8*65536）（512kB）字节，即使是在
;	将来这也应该没有问题的。我想让它保持简单明了。这样512k的最大内核长度应该
;	足够了，尤其是这里没有象minix中一样包含缓冲区高速缓冲。
;
;	加载程序已经做的够简单了，所以持续的读出错将导致死循环。只能手工重启。
;	只要可能，通过一次取取所有的扇区，加载过程可以做的很快的。
;************************************************************************ */
entry _start !入口地址 第一个扇区512byte
_start:
	mov	ax,#BOOTSEG
	mov	ds,ax     !  ds=0x07c0
	mov	ax,#INITSEG 
	mov	es,ax     !  es=0x9000
	mov	cx,#256
	sub	si,si    
	sub	di,di     ! si=0,di=0
	rep        
	movw      ! rep movw 重复执行传送指令cx=256次,每次一个word=2byte总共512byte,从ds:si->es:di 也就是0x07c0:0x0000->0x9000:0x0000  
	jmpi	go,INITSEG  ! 移动完成后跳转到0x9000:go执行指令（段内跳转）
go:	mov	ax,cs
	mov	ds,ax    ! ds=0x9000
	mov	es,ax    ! es=0x9000
! put stack at 0x9ff00.
	mov	ss,ax    ! ss=0x9000
	mov	sp,#0xFF00		! arbitrary value >>512 sp=0xFF00  栈指针指向0x9000:0xFF00位置 这个位置离bootsect模块很远

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.
!  开始加载后面四个扇区的setup模块进入内存 位置在bootsect后面 大小是512byte*4=2kb
load_setup:   !加载过程中如果出错 进位标识CF=1 
	mov	dx,#0x0000		! drive 0, head 0  磁头号dh=0 驱动器号dl=0
	mov	cx,#0x0002		! sector 2, track 0   ch = 磁道（柱面）号的低8位；  cl = 开始扇区（0－5位），磁道号高2位（6－7） 开始扇区2 磁道号0
	mov	bx,#0x0200		! address = 512, in INITSEG es:bx=0x9000:0x0200 目标内存位置
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors   扇区数量al=4 ah=0x02,即为读磁盘扇区到内存
	int	0x13			! read it   0x13号中断是读磁盘IO 上面的代码是0x13中断的参数
	jnc	ok_load_setup		! ok - continue 进位标识CF=0 跳转到ok_load_setup
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette  重置dx ax 重新发起0x13中断
	int	0x13
	j	load_setup

ok_load_setup: !

! Get disk drive parameters, specifically nr of sectors/track  获取磁盘驱动参数特别是每道的扇区数量 为后面读入system模块做准备

	mov	dl,#0x00        ! dl=0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters ah=8表示要读取磁盘驱动器参数
	int	0x13            ! 调用完成后 bx cx dx es:di 都会有变化
	mov	ch,#0x00
	seg cs
	mov	sectors,cx    ! seg cs mov sectors,cx 加起来等价于 mov cs:sectors,cx cx的值是每磁道扇区数
	mov	ax,#INITSEG
	mov	es,ax         ! es的值重置为 0x9000

! Print some inane message 加载system模块耗时较长 准备在屏幕上打印一段字符串

	mov	ah,#0x03		! read cursor pos 功能号0x03读取光标各种信息 
	xor	bh,bh         ! bh=0  表示页码0 就是读光标位置
	int	0x10          ! 0x10号中断 读取光标信息 读取后存在dx光标位置 cx
	
	mov	cx,#24         !显示字符个数 36
	mov	bx,#0x0007		! page 0, attribute 7 (normal) attribute就是字符颜色透明度等属性
	mov	bp,#msg1        ! es:bp 指向要显示的字符串
	mov	ax,#0x1301		! write string, move cursor 功能号ah=0x13
	int	0x10            ！ 打印字符

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG  ! ax=0x1000 system模块的加载位置
	mov	es,ax		! segment of 0x010000 es=0x1000
	call	read_it  ! 调用read_it 加载system
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track  bootsect+setup占第一个磁道5个扇区
head:	.word 0			! current head   磁头号只有0 1 两种可能
track:	.word 0			! current track
! 磁盘寻址模式 CHS 也就是 track->head->sector 柱面->磁头->扇区
read_it:
	mov ax,es           ! ax=0x1000
	test ax,#0x0fff     ! 如果ax=0x1000 test的结果ZF=0 不会进入die死循环 所以 es的值必须是0x1000
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment  bx=0  es:bx
;// 判断是否已经读入全部数据。比较当前所读段是否就是系统数据末端所处的段（#ENDSEG），如果
;// 不是就跳转至下面ok1_read标号处继续读数据。否则退出子程序返回。
rp_read:
	mov ax,es          ! ax=es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read   ! ax<#ENDSEG 则跳转 ok1_read
	ret
;// 计算和验证当前磁道需要读取的扇区数，放在ax寄存器中。
;// 根据当前磁道还未读取的扇区数以及段内数据字节开始偏移位置，计算如果全部读取这些
;// 未读扇区，所读总字节数是否会超过64KB段长度的限制。若会超过，则根据此次最多能读
;// 入的字节数（64KB - 段内偏移位置），反算出此次需要读取的扇区数。
ok1_read:
	seg cs
	mov ax,sectors  !ax=每磁道扇区数
	sub ax,sread     ! sread表示当前磁道已读扇区数 ax-sread=剩余扇区数
	mov cx,ax       ! cx=剩余扇区数
	shl cx,#9       ! 左移 cx=剩余扇区数*512  剩余字节
	add cx,bx        ! cx=cx+bx
	jnc ok2_read      ! 无进位跳转 CF=0
	je ok2_read        !
	xor ax,ax         !若加上此次将读磁道上所有未读扇区时会超过64KB，则计算此时最多能读入的字节数（64KB － 段内读偏移位置），再转换成需要读取的扇区数。
	sub ax,bx         ! 先令ax=0 然后再ax=ax-bx(bx是0~64kb中间的一个值) 计算结果是64K-bx 最后ax右移9位相当与 ax/512个扇区数
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax    ;// cx = 该此操作已读取的扇区数。
	add ax,sread  ;// 当前磁道上已经读取的扇区数。
	seg cs
	cmp ax,sectors   ;// 如果当前磁道上的还有扇区未读，则跳转到ok3_read处。
	jne ok3_read  
	!读该磁道的下一磁头面（1号磁头）上的数据。如果已经完成，则去读下一磁道。   
	mov ax,#1
	sub ax,head  
	jne ok4_read      ;// 如果是0磁头，则再去读1磁头面上的扇区数据
	inc track         ;// 否则去读下一磁道。 当前磁道加1
ok4_read:
	mov head,ax ! 修改当前磁头head=1
	xor ax,ax   ;// 清当前磁道已读扇区数。 ax=0
ok3_read:
	mov sread,ax  
	shl cx,#9
	add bx,cx
	jnc rp_read !若小于64KB边界值，则跳转到rp_read处，继续读数据。
	mov ax,es
	add ax,#0x1000
	mov es,ax   ! 将段基址调整为指向下一个64KB段内存。
	xor bx,bx   ! bx=0
	jmp rp_read
;// 读当前磁道上指定开始扇区和需读扇区数的数据到es:bx开始处。
;// al － 需读扇区数； es:bx － 缓冲区开始位置。
read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track ;// 取当前磁道号。
	mov cx,sread ;// 取当前磁道上已读扇区数。
	inc cx ;// cl = 开始读扇区。
	mov ch,dl ;// ch = 当前磁道号
	mov dx,head ;// 取当前磁头号。
	mov dh,dl ;// dh = 磁头号。
	mov dl,#0 ;// dl = 驱动器号（为0表示当前驱动器）。
	and dx,#0x0100 ;// 磁头号不大于1
	mov ah,#2 ;// ah = 2, 读磁盘扇区功能号。
	int 0x13
	jc bad_rt  ! 读取过程中如果报错重新读取
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

!/*
! * This procedure turns off the floppy drive motor, so
! * that we enter the kernel in a known state, and
! * don't have to worry about it later.
! */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
