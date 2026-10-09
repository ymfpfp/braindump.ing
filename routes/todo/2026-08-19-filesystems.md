---
date: 2026-08-19
title: A Tour Of Modern Filesystems, Part I: How? 
layout: writing
---

_Typos from the Linux kernel are left as is._

_Originally, this was going to be one whole read, but I decided it would be better to split it up _\*

For a pet project I'm working on, I was researching device mapper and LVM[^0] so I could figure out how to write. Somewhere along the way, I got sucked into reading about filesystems and wanting. There's a lot of filesystems out there and associated terms and I personally would like to have some understanding of how they work. This is also useful

[^0]: It's pretty likely that I'll write about this as well.

Famously, as they say about Linux, everything is a file.

A brief table of contents:

- What happens when we call `open()`? This will give us an idea of the abstractions that Unix (Linux specifically) has for filesystems.
- Then, we'll walk through RAID as an example of what we want out of data storage, and how the filesystems we use play a part in that.
- Then, we'll move onto FFS (Fast File System) as a good starting point.
- Then, we'll talk about ext and its evolutions (ext2, ext3, ext4); this is the most commonly used filesystem on Linux. We'll walk through the ext2 and ext4 file drivers to understand what's going on behind the scenes, and play around with ext4. We'll also talk about userspace tools, like `fsck`.
- One of the features added to a later version of ext, ext3, is journaling. Notably, the implementation for this is implemented through the jbd2 subsystem, of which we will also take a look at the source code.
- Then, we'll take a look at XFS, a filesystem that makes use of B+ trees for efficiency. We'll also take a look at the Linux filesystem driver for it.
- Finally, let's take a look at some other filesystems that are commonly used today: FAT, which is used in SD cards, and APFS, which is Apple's filesystem.

I've structured this to be as cohesive as possible. It's ordered such that we'll There are tons of features in each individual filesystem that I got nerdsniped by — for example, we'll learn that ZFS makes use of its own page cache, called ARC, and we'll take a look at the respective paper on it. But you don't have to get nerdsniped with me... or maybe you do :) My suggestion for reading is to skip and jump around.

## What happens when you call open()?

A typical program that writes to a file might look like this:

```c
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

int main() {
  int fd = open("hello.txt", O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (fd < 0)
    return 1;

  char *msg = "Hello, filesystems!\n";
  write(fd, msg, strlen(msg));
  close(fd);
  return 0;
}
```

We use `open`, which gives us back a **file descriptor**. Uh -

1. What exactly is a file descriptor?
2. What does `open` do, exactly?

What exactly is a file descriptor? We know it's an `int`, so that's helpful. We can intuit that a file descriptor must describe a file. Of course, a number isn't sufficient to do all that, so we can probably intuit that it's an index into something.

And you would be right! Each process has a file descriptor table:

todo(jc): Here goes a visual, process points into per-process fd table and then that points into a global fd table which is contained of `struct file`.

File descriptors 0, 1, and 2 are local and typically are stdin, stderr, and stdout. But file descriptors onwards point into a system-wide kernel table, as depicted above. That means: file descriptors are unique per process, but the actual file representation is shared.

This makes intuitive sense. I won't go too much more into the weeds of how the actual data representation looks like but you can take a look at [`include/linux/fs.h`](https://github.com/torvalds/linux/blob/66498c75b4f8017f62d720d9b59675bdf3abce91/include/linux/fs.h) in the kernel source!

So let's move on to question two. What does `open` do? musl is relatively small, so let's take a look at its definition for `open` inside [`src/fcntl/open.c`](https://github.com/kraj/musl/blob/kraj/master/src/fcntl/open.c):

```c
#include <fcntl.h>
#include <stdarg.h>
#include "syscall.h"

int open(const char *filename, int flags, ...)
{
  mode_t mode = 0;

  if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) {
    va_list ap;
    va_start(ap, flags);
    mode = va_arg(ap, mode_t);
    va_end(ap);
  }

  int fd = __sys_open_cp(filename, flags, mode);
  if (fd>=0 && (flags & O_CLOEXEC))
    __syscall(SYS_fcntl, fd, F_SETFD, FD_CLOEXEC);

  return __syscall_ret(fd);
}
```

It's just a syscall wrapper, not much to it! Specifically, `__sys_open_cp` is a macro, defined in [`src/fcntl/syscall.h`](https://github.com/kraj/musl/blob/1c82569f4cc53f5bb25c820051c2a02463f46cab/src/internal/syscall.h):

```c
#define __SYSCALL_NARGS_X(a,b,c,d,e,f,g,h,n,...) n
#define __SYSCALL_NARGS(...) __SYSCALL_NARGS_X(__VA_ARGS__,7,6,5,4,3,2,1,0,)
#define __SYSCALL_CONCAT_X(a,b) a##b
#define __SYSCALL_CONCAT(a,b) __SYSCALL_CONCAT_X(a,b)
#define __SYSCALL_DISP(b,...) __SYSCALL_CONCAT(b,__SYSCALL_NARGS(__VA_ARGS__))(__VA_ARGS__)
// ...
#define __sys_open_cp(...) __SYSCALL_DISP(__sys_open_cp,,__VA_ARGS__)
```

If we trace this macro and try to reconstruct it by hand:

```c
// #define __sys_open_cp(...) __SYSCALL_DISP(__sys_open_cp,,__VA_ARGS__)
__SYSCALL_DISP(__sys_open_cp,,filename,flags,mode)
// #define __SYSCALL_DISP(b,...) __SYSCALL_CONCAT(b,__SYSCALL_NARGS(__VA_ARGS__))(__VA_ARGS__)
__SYSCALL_CONCAT(__sys_open_cp,__SYSCALL_NARGS(,filename,flags,mode))(,filename,flags,mode)
// #define __SYSCALL_NARGS(...) __SYSCALL_NARGS_X(__VA_ARGS__,7,6,5,4,3,2,1,0,)
__SYSCALL_CONCAT(__sys_open_cp, __SYSCALL_NARGS_X(,filename,flags,mode,7,6,5,4,3,2,1,0))(,filename,flags,mode)
// #define __SYSCALL_NARGS_X(a,b,c,d,e,f,g,h,n,...) n
__SYSCALL_CONCAT(__sys_open_cp,3)(,filename,flags,mode)
// #define __SYSCALL_CONCAT(a,b) __SYSCALL_CONCAT_X(a,b)
__SYSCALL_CONCAT_X(__sys_open_cp,3)(,filename,flags,mode)
// #define __SYSCALL_CONCAT_X(a,b) a##b
__sys_open_cp3(,filename,flags,mode)
```

Macros are always so confusing for me but that's because I don't really interact with C very often. The `__SYSCALL_NARGS_X` is ingenious though! All this just to come up with `__sys_open_cp3`, which is defined in the same header file:

```c
#ifndef __scc
#define __scc(X) ((long) (X))
typedef long syscall_arg_t;
#endif
// ...
// ...
#define __syscall_cp3(n,a,b,c) (__syscall_cp)(n,__scc(a),__scc(b),__scc(c),0,0,0)
#define __syscall_cp4(n,a,b,c,d) (__syscall_cp)(n,__scc(a),__scc(b),__scc(c),__scc(d),0,0)
// ...
#define __syscall6(n,a,b,c,d,e,f) __syscall6(n,__scc(a),__scc(b),__scc(c),__scc(d),__scc(e),__scc(f))
// ...
#ifdef SYS_open
#define __sys_open_cp3(x,pn,fl,mo) __syscall_cp3(SYS_open, pn, (fl)|O_LARGEFILE, mo)
#else
#define __sys_open_cp3(x,pn,fl,mo) __syscall_cp4(SYS_openat, AT_FDCWD, pn, (fl)|O_LARGEFILE, mo)
#endif
// ...
#define __syscall_cp(...) __SYSCALL_DISP(__syscall_cp,__VA_ARGS__)
```

Let's just assume that `SYS_open` is defined; on my MacBook it's defined as `5` in the system headers so let's go with that. That means:

```c
__sys_open_cp3(,filename,flags,mode)
// #define __sys_open_cp3(x,pn,fl,mo) __syscall_cp3(SYS_open, pn, (fl)|O_LARGEFILE, mo)
__syscall_cp3(5, filename, (flags)|O_LARGEFILE, mode)
// #define __syscall_cp3(n,a,b,c) (__syscall_cp)(n,__scc(a),__scc(b),__scc(c),0,0,0)
(__syscall_cp)(5,__scc(filename),__scc((flags)|O_LARGEFILE),__scc(mode),0,0,0)
// #define __scc(X) ((long) (X))
(__syscall_cp)(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0)
// #define __syscall_cp(...) __SYSCALL_DISP(__syscall_cp,__VA_ARGS__)
__SYSCALL_DISP(__syscall_cp,5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0)
// #define __SYSCALL_DISP(b,...) __SYSCALL_CONCAT(b,__SYSCALL_NARGS(__VA_ARGS__))(__VA_ARGS__)
__SYSCALL_CONCAT(__syscall_cp,__SYSCALL_NARGS(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0))(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0)
// #define __SYSCALL_NARGS(...) __SYSCALL_NARGS_X(__VA_ARGS__,7,6,5,4,3,2,1,0,)
__SYSCALL_CONCAT(__syscall_cp,__SYSCALL_NARGS_X(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0,7,6,5,4,3,2,1,0,))(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0)
// #define __SYSCALL_NARGS_X(a,b,c,d,e,f,g,h,n,...) n
__SYSCALL_CONCAT(__syscall_cp,6)(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0)
// #define __SYSCALL_CONCAT(a,b) __SYSCALL_CONCAT_X(a,b)
__SYSCALL_CONCAT_X(__syscall_cp,6)(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0)
// #define __SYSCALL_CONCAT_X(a,b) a##b
__syscall_cp6(5,((long) (filename)),((long) ((flags)|O_LARGEFILE)),((long) (mode)),0,0,0)
```

Sorry for dragging you through all that.

So now we know all the heavy duty stuff is happening in the kernel. There's nothing particularly special in userspace happening, we just execute a syscall.

So what happens in the kernel?

## VFS

Well, it turns out the Linux kernel implements something called the _virtual filesystem_. Devices, like your disks, are able to expose bytes as a structured hierarchy of data — this is called a **filesystem**, and that structure typically looks like tree you might expect:

todo(jc): Here goes a visual

On disk, though, what does this look like?

todo(jc): Here goes a visual. Probably like hard drive / SSD / SD card

At the end of the day, it's all just bits! My data could be anywhere, in any form! How am I supposed to get anything done? How do I move a file between filesystems?

The solution, as is the case with a lot of computer science, is to introduce a level of abstraction. That abstraction in Linux comes in the form of the virtual filesystem, or VFS for short:

todo(jc): Here goes a visual. Hard drive / SSD / SD card feeds into black box that is VFS and then out comes the filesystem (there's a arrow saying "this is the filesystem"

Specifically, there are four building blocks that make up VFS.

First off, this hierarchy is made up of **files**. Intuitively, you might expect a file to be a stream of bytes, and you would be correct. It's just bits, after all! However, you've probably also come to expect that files come with some sort of attached metadata, properties like the timestamp for when this was created or edited, or who has access to this file, or what kind of access they have. This sort of metadata is stored in what Linux calls a **inode**, which is sort for index node. We'll see why it's called index node soon, but for now this is just a structure that holds metadata about its respective file.

And naturally you'll also notice that we have the notion of **directories**. Well, in line with "Everything is a file", directories are... also just files. They just happen to be files that contain an encoded list of the files within. Encoded how? Well, it depends on the filesystem...

### The bigger picture

Before we go further, let's take a look at the bigger picture of writing a file. With a userspace program, we make a syscall, we go to

It turns out there's a couple steps in between, all to do with the fact that writing to disk is _expensive_ and we should try to avoid doing it as much as possible!

At the disk level, each disk has a driver that we use to write to it. Similar to the abstraction that the VFS, we'll hide the details of the way the disk sends and receives instructions for operations to perform, and hide it behind a **block device** — it's just a device that has a array of data, and we can index, or _seek_, to anywhere in that array and read or write those bits.

A sort of interesting question to ask is: what if we want to separate our filesystem over multiple disks? Maybe our filesystem is too big, or we want to have some redundancy (which we'll look at in just a moment!) or

Quite literally, I'll leave you to take a look at a very big picture, taken from

![https://www.thomas-krenn.com/en/wiki/Linux_Storage_Stack_Diagram](https://www.thomas-krenn.com/en/wikiEN/images/d/d1/Linux-storage-stack-diagram_v6.18.png)

## What do we want?

Ok, we know that we want filesystems to store our data, but it turns out that there are plenty of extra features we take for granted that up until now, I haven't really talked about. Roughly, these features try to solve two kinds of problems:

1. Durability/reliability. Ideally, I would like my data not to go missing, or get corrupted. That would defeat the whole purpose of storing them.
2. Low latency/high throughput. Ideally, operations on both singular files and many files should go brrr.

RAID (Redundant Array of Inexpensive Disks) is a great example of how these features are accounted for at the _bit-level_. Specifically, given a bunch of (inexpensive) disks, I should be able to solve these two problems, right?

Yeah, to some extent!

For example, given two disks, I can use disk two as a redundant disk and just have it store the same exact bits as disk one. This is called _mirroring_, and RAID level 1 does just this. This is a way to alleviate the issue of durability/reliability.

todo(jc): Animation of mirroring.

Or, maybe I want to alleviate the issue of low latency/high throughput. If I have multiple disks, I don't want my disks to be idle while one of the disks is handling all the requests. I can split data across the disks, using a function $S :: Int$ to decide how to split the data — e.g., round robin! This is called _striping_, and RAID level 0 does this.

todo(jc): Animation of striping.

Or, maybe I want both. That's RAID level 1 + 0 = RAID level 10 — striping and mirroring. Maybe I can have two disks that are striped, and then mirror those two disks over to another two disks.

todo(jc): Animation of this.

But, mirroring — that sounds like a lot of space! The solution to this is to figure out a compact representation of the data that also tells us how to reconstruct the data, should something happen. This is what a checksum does — $f(\text{data}[x]) ::  $, where $y < x$; we store this somewhere on a disk, and when the time comes, we can run this function to make sure that the data on the disk does actually match the _parity bits_.

One example of such a function is the XOR function. It'll take in data of at least two bits, and then XOR the bits next

todo(jc): Table of XOR, and then an interactive example.

What RAID level 4 does is a variation of this. First off, we'll expect at least $N = 2$ disks for the usual operations. Then, we'll have the $N + 1$ be our parity disk. For each bit at location $x$ on each of the $N$ disks, we'll compact them and pass them into our XOR function. That'll give us a parity bit, which we'll store in disk $N + 1$.

todo(jc): Depiction of RAID

In exchange for this, you will have to make some sacrifices. Notably, every time you write data, you'll have to write at least two disks — the disk with the actual disk, and the disk with the parity data! This problem is called the _small write problem_ — since in large writes this gets amortized and is less of an issue.

RAID level 5 doesn't fix this issue, but it does handle the issue of there being only one parity disk. To increase throughput, we rotate the parity bit — or more commonly, blocks — across multiple disks:

todo(jc): Viz

Finally, another common RAID level is RAID level 6 — instead of using one parity function on one parity block — use $X$ parity disks at a time, each with a different parity function. This reduces reliance on one parity

todo(jc): Viz this

Of course, if all of our disks magically get destroyed — maybe they all get burned in a fire — there's not much we can do about that. Or if there's another bottleneck in the system — maybe — there's not much changing our data storage methods can do about that. Consider this a level of insurance, we'll just be able to statistically reduce our chances of bad things happening.

Filesystems, unsurprisingly, are designed with a similar mindset in mind. In fact, we'll notice a lot of shared patterns and obvious evolutions as we go along. We'll look at each filesystem from the perspective of how to create/read/update/delete {files, folders} from them, while trying to exemplify how they're designed to also provide a compromise between these two features.

## The simplest filesystem

The first Unix filesystem driver that Ken Thompson wrote was really simple. Essentially, imagine the disk as one split into fixed-size **blocks**:

todo(jc): Viz this

Each time you write data, you'll usually be writing at least one block at a time. Now, let's split up the disk in a different way:

todo(jc): [Metadata, data] -> [Superblock, inode table, data]

We'll have the superblock at the beginning of the disk. It'll have some info about the filesystem. Importantly, it'll have a pointer to the beginning of the inode table, which can be anywhere on disk but usually right after the superblock.

From the superblock, we'll jump to the inode table. The inode table is, as you would expect, an array of inodes. Each of these inodes will be metadata about its respective file. Importantly, it'll have a pointer to the data block of this file.

So there's a lot of indirection here. How do we create a file, then?

1. We'll jump from the superblock to the inode table.
2. We'll look at second inode in the table, or the inode at index 1; the inode at index 0 is reserved for special use.
3. We'll jump to the respective file. Remember — directories are files! In Thompson's filesystem, dentries were encoded as an array of (inode number, name), totaling 36 bytes. The filename was a fixed size of 14 bytes.
4. We'll recursively do this jumping, reading through the dentries, over and over again until we get to the spot we want to create a file.
5. We'll create a inode for our file, searching the inode table for a free inode. How do we know if an inode is free? Each inode has an attached _link count_ property, which is just a number. I'll explain what this is — for now, just know that if this number goes down to zero, that means that it's a free inode and can be written.
6. We'll add an dentry to the spot where we want to create a file, with our inode number. Now we have a file!

todo(jc): Viz of this, specifically

You can imagine that reading a file follows a similar pattern.

Writing to a file is a bit more complicated. We need to find a free block that we can write to. The superblock can help us — it has a fixed size array with a list of the free block numbers, and the occupied size of this array. We can pop a free block number off this array, and update our inode to contain the pointer to this data block.

todo(jc): Viz of this

As for deleting, I want to draw your attention to the mention of link counts: it turns out that link counts are not just useful for knowing whether or not an inode is free, but forms the basis for how deletion works. Having link count makes deletion an operation that happens automatically. When a user "deletes" a file, we'll decrement the link count. When the link count hits zero, it's as if that file is simply no longer a thing.

### All kinds of links

What the link count number represents is really how many active

The default kind of link is a _hard link_: there are multiple references to an inode within dentries:

todo(jc): Viz

This means that when you delete

Some filesystems even have the concept of reflinks!

---

Ok, so all of this seems really straightforward! But let's talk about one problem that might arise: **fragmentation**. Say I want to update a file — let's say I want to append some content to it. This is fine if the length of the resulting data doesn't extend the block it's been assigned to:

todo(jc): Viz this

But imagine it does. If the blocks next to it have already been taken up by some other file:

todo(jc): Viz this

We'll have to find another block:

todo(jc): Viz this

The first obvious issue is how to set up this indirection. The answer is that

Over time, this sort of fragmentation accumulates. We do have userspace programs, defragmentation tools, to alleviate this issue: they will rearrange the data blocks and update the metadata to reduce the numbers.

## FFS

FFS, which stands for Fast File System, was designed to fix this by keeping in account that a filesystem should be disk aware. Designed by a group at Berkeley, it was later standardized back into Thompson's file system, which is now referred to as UFS, or Unix File System.

The core concept is that data that have common relationships should be located closer to each other, to take advantage of [_spatial locality_]().

So FFS works by dividing the disk into a number of _cylinder groups_: translated to disk, this usually tends to be a cluster of sectors next to each other.

todo(jc): Viz this

The data within each cylinder group tends to be more "related" to one another to one another, in the sense that

<details>
    <summary><h3>How do hard disks work?</h3></summary>

Hard disks are probably the most common long-term bulk storage. They're made of a bunch of round magnetic disks, called _platters_, set on top of each other. Each magnetic disk has a read/write _head_ assigned to it:

todo(jc): Depiction of this.

As you can see, each of these heads are attached to a arm, and the arms are all conjoined to a central management unit. (Specifically, this is an [actuator](https://en.wikipedia.org/wiki/Actuator) with arms.)

A lot of the terminology around reading/writing disks is based on how hard disks store data. Each of the platters have a thin magnetic layer, and then the platter is made up of nested circles called _tracks_. Each of these tracks are then split into parts, called _sectors_, which contain the data (along with some other metadata used for )

todo(jc): Depiction of this.

The read/write heads are able to write to appropriate sector by

You'll see "sector" used very often, even when referring to something like

For a even better more in-depth explanation, see this [post](https://www.makingsoftware.com/chapters/how-is-data-stored#hard-disk-drives); it has useful details as to how SSDs work, in addition to how data storage works in other parts of the memory hierarchy.

To admire how crazy it is that hard disks actually work so well:

> Imagine a hard drive head as a 747 flying over a grassy field at 75 miles per hour. The air gap between the bottom of the plane and the top of the grass is two sheets of paper. Now, if we measure bits on the disk as blades of grass, the track width would be 4.6 blades of grass wide and the bit length would be one blade of grass. As the plane flew over the grass it would count blades of grass and only miss one blade for every 25 thousand times the plane circled the Earth.
>
> — [Building and operating a pretty big storage system called S3](https://www.allthingsdistributed.com/2023/07/building-and-operating-a-pretty-big-storage-system.html)

Isn't that insane?

</details>

Each cylinder group begins with a copy of the superblock:

todo(jc): Look at the superblock!

Each cylinder group is still divided into blocks, as you might expect:

todo(jc): Look at that

After the superblock, we'll have encoded metadata about this cylinder group. For example, we'll want to know where the inode table on disk:

todo(jc): Rectangle of superblock, followed by cylinder group metadata pointing to somewhere on disk that is the inode table.

Let's take a look at what sort of metadata we're storing, from the UFS (Unix File System) driver inside the Linux kernel at [`fs/ufs/ufs_fs.h`]():

```c
struct	ufs_cylinder_group {
	__fs32	cg_link;		        /* linked list of cyl groups */
	__fs32	cg_magic;		        /* magic number */
	__fs32	cg_time;		        /* time last written */
	__fs32	cg_cgx;			        /* we are the cgx'th cylinder group */
	__fs16	cg_ncyl;		        /* number of cyl's this cg */
	__fs16	cg_niblk;		        /* number of inode blocks this cg */
	__fs32	cg_ndblk;		        /* number of data blocks this cg */
	struct	ufs_csum cg_cs;		    /* cylinder summary information */
	__fs32	cg_rotor;		        /* position of last used block */
	__fs32	cg_frotor;		        /* position of last used frag */
	__fs32	cg_irotor;		        /* position of last used inode */
	__fs32	cg_frsum[UFS_MAXFRAG];	/* counts of available frags */
	__fs32	cg_btotoff;		        /* (__u32) block totals per cylinder */
	__fs32	cg_boff;		        /* (short) free block positions */
	__fs32	cg_iusedoff;		    /* (char) used inode map */
	__fs32	cg_freeoff;		        /* (u_char) free block map */
	__fs32	cg_nextfreeoff;		    /* (u_char) next available space */
	union {
		struct {
			__fs32	cg_clustersumoff;	/* (u_int32) counts of avail clusters */
			__fs32	cg_clusteroff;		/* (u_int8) free cluster map */
			__fs32	cg_nclusterblks;	/* number of clusters this cg */
			__fs32	cg_sparecon[13];	/* reserved for future use */
		} cg_44;
		struct {
			__fs32	cg_clustersumoff;/* (u_int32) counts of avail clusters */
			__fs32	cg_clusteroff;	/* (u_int8) free cluster map */
			__fs32	cg_nclusterblks;/* number of clusters this cg */
			__fs32   cg_niblk; /* number of inode blocks this cg */
			__fs32   cg_initediblk;	/* last initialized inode */
			__fs32   cg_sparecon32[3];/* reserved for future use */
			__fs64   cg_time;	/* time last written */
			__fs64	cg_sparecon[3];	/* reserved for future use */
		} cg_u2;
		__fs32	cg_sparecon[16];	/* reserved for future use */
	} cg_u;
	__u8	cg_space[1];		/* space for cylinder group maps */
/* actually longer */
};
```

This time around though, we'll also have an _inode bitmap_ and a _block bitmap_.

This is followed by the inode bitmap and the block bitmap. Bitmaps, bitfields, whatever you want to call them... just an array of bits that describe whether this inode or this block is free to write. The max bitmap size is $2^{15}$ and is, as expected, dictated by the metadata.

Ok, let's walk through creating/reading/updating/deleting. First off, the first time we format some partition of a disk to be FFS, we'll set inode number 2 to be the inode

## Ext{2, 3, 4}

We can still do better than FFS. So that takes us to Ext, which is the default Linux filesystem.

In addition,

Ok, let's walk through creating/reading/updating/deleting.

### mkfs, mount, fsck

Wait, back up. All this talk and we still haven't actually

### ext3: Journaling with jbd2

Another way we can introduce some resiliency is through _journaling_. It sounds like exactly what you might expect it to be — let's keep a log of all the transactions! We can either choose to store this log on the same disk — i.e., in a different partition, usually somewhere near the center of the disk[^98] — or on a separate disk entirely.

[^98]: This is because it's faster to seek to and write to the middle of the platter, where the head is most likely to be.

The most straightforward way to do journaling is to probably log all the data. This includes all the metadata, of course!

You might be surprised to learn that this is not the default option for ext. Actually, it's unordered journaling, or `data=unordered`, when. In this case, only metadata is journaled.

ext3 introduces journaling as a feature, and calculates the size of the journal based on the size of the filesystem:

todo(jc): Table that shows this

In terms of implementation, ext journaling is done through jbd2, Journal Block Device 2, which is

---

Another notable feature that ext3 introduces is an indexed directory format. This was originally designed for ext2, but [never made it]() in the spec.

What does the HTree look like?

To refresh, we access

### ext4: Extents

Now we get to ext4, which is the modern day version of ext. Up until this point, we've stored file data as a set of indirect pointers. There's a specific situation

The other feature that ext4 introduces is _delayed allocation_. Instead of allocating
