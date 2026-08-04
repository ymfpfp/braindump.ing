---
date: 2026-08-04
title: Comparing filesystems
layout: writing
---

I'm going to talk about Linux's VFS, then compare some common filesystems by taking a look at each of them. This is handy as a reference guide, and is useful if you're trying to get a big picture understanding. If you understand (kernel/user space, syscalls) you should be able to read through this relatively easily.

tl;dr, here is a comparison table of some common filesystems and what they offer:

<table>
  <tr>
    <th>Filesystem</th>
    <th>Compression</th>
  </tr>
  <tr>
    <td>ext</td>
    <td></td>
  </tr>
  <tr>
    <td>btrfs</td>
    <td></td>
  </tr>
</table>

Some of the graphics here are intended to be simplified depictions for conveying a specific concept, not the thing in actuality. Where this is the case, I will note it.

## VFS

The Linux kernel implements something called the _virtual filesystem_. Devices are able to expose bytes as a structured hierarchy of data - this is called a **filesystem**, and that structure typically looks like a tree:

<div class="demo">
@components/filetree.html
</div>

Specifically, this hierarchy is made up of **files**. Intuitively, you might expect a file to be a stream of bytes, and you would be correct. However, you might also expect there to be _directories_ (aka folders), but: it turns out that directories are also just files, they just happen to be files that contain an encoded list of the files within.

<div class="demo">
todo: trees.software example, but clicking on folders results in the metadata within
</div>

The cool thing about this is that the Linux kernel has abstractions for all of this, so that any filesystem (remember, it's just a bunch of bytes!) can provide functions to expose themselves as this sort of hierarchy.

To support this, there's four abstractions represented by actual data structures in the kernel.

The first is the file, which we've already talked about! Just a stream of bytes. Files are open per process, as you might already know - file descriptors point into a process' table of open files, which are represented by `*struct file`.

Files have attached metadata, such as file type, access rights, timestamps, etc. Rather than keep it together with the file, we store it in a separate data structure, called the **inode**, short for index node. What does the inode look like? Here's a trimmed down definition of the actual type definition in [`include/linux/fs.h`](https://github.com/torvalds/linux/blob/31996e14bd59840692d6c1c6e41ef878b77a2967/include/linux/fs.h#L762):

```c
struct inode {
	umode_t			i_mode;
	unsigned short	i_opflags;
	kuid_t			i_uid;
	kgid_t			i_gid;
	unsigned int	i_flags;

    // ...

    loff_t			i_size;
	time64_t		i_atime_sec;
	time64_t		i_mtime_sec;
	time64_t		i_ctime_sec;
	u32			    i_atime_nsec;
	u32			    i_mtime_nsec;
	u32			    i_ctime_nsec;

    // ...
} __randomize_layout;
```

As you can see, all sorts of attached metadata.

Files are organized in directories, forming a _path_. Linux represents this path by using a directory entry, or **dentry**, for each part of this path. For example, you might have a script at `/scripts/init_db.sh`. Both of these are files behind the scenes

With dentries, we are able to answer questions like, "How to lay out these files? Can I get a pointer to this file's parent?"

Finally, the structure that combines these three structures together into one super structure is the aptly named **superblock**.

This is represented by `super_block` in `include/linux/fs.h` (once again, a

Using these four building blocks you can construct a filesystem out of just arbitrary bytes.

Now, what if we want to write

If you looked closely at the code snippets, you'll notice that each of them have a pointer to some object `_ops`. Inodes have `i_ops`, dentries have `d_ops`, superblocks have `s_ops`... what are in these? What's the dilineation of operations? Let's take a look

### The bigger picture

For me personally, it's useful to have a bigger concept of how the VFS ties into overall I/O.

To go full circle, let's say you write a userspace program that makes a `write` syscall. Maybe you want to copy a file over from one filesystem to another:

<div class="demo">
</div>

What happens behind the scenes:

This assumes that there is no cache between the

The cool thing about all of these is the kernel can then represent memory-based structures as files. For example, processes can be represented as files; you can take a look at them

### ioctl

If you've written decently low-level code you've probably made use of `ioctl`. You might know `ioctl` as the grab-bag syscall on POSIX systems; its

You can now get an idea of how `ioctl` works.

## FUSE

The best way to internalize

You can write your own filesystems and expose functions for them. Rather than do it in the kernel by exposing a filesystem driver, which tends to be a bit more complicated than making things happen in userspace, you can make use of [FUSE](), which stands for *F*ilesystem in *USE*rspace. You can do all sorts of stuff with this, from proper serious stuff like [JuiceFS](), to [using Discord as the backing datastore]().

## Comparing filesystems

Now let's look at some common filesystems and why you might want to use them. In general, there are a couple of broad criteria for a filesystem:

- Durability/reliability: typically through anti-corruption measures, journaling, etc.
- Low latency, high throughput
- Minimal space taken up on disk: typically through measures like built-in compression, etc.
- Security: is encryption a feature of this filesystem?

Keep in mind that some filesystems will trade off some of these capabilities to the block device they are being stored on.

## ext

## btrfs

Pronounced "butter f-s".

## xfs

The test suite for XFS is used as the

## zfs

## fat

## apfs

---

Diagrams rendered with LLM assistance.
