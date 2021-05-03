POSIX Shared Memory API.

## Documentation

The `posix-shm` library allows the creation and access of POSIX shared
memory objects. A POSIX shared memory object is in effect a file
handle which can be used by several processes to access the same
region of shared memory.

<procedure>posix-shm? :: BOOLEAN</procedure>

`#t` if the current platform supports POSIX shared memory, `#f` otherwise.

<procedure>shm-open:: PATH * OFLAGS  [* MODE] -> FD</procedure>

`shm-open` is a wrapper around `shm_open`, which is analogous to the
UNIX system call `open(2)`.

Argument `PATH` specifies the shared memory object to be created or
opened. For portable use, name should have an initial slash (/) and
contain no embedded slashes.

`OFLAGS` is a list of bit masks from the `chicken file posix` unit which will
be ORed together and passed to `shm_open`. It must contains exactly
one of `open/rdonly` or `open/rdwr` and any of the other flags
listed here:

- `open/creat` : Creates the shared memory object if it does not exist. The user and group ownership of the object are taken from the corresponding effective IDs of the calling process, and the object's permission bits are set according to the low-order 9 bits of mode, except that those bits set in the process file mode creation mask (see `umask(2)`) are cleared for the new object.  A new shared memory object initially has zero length. The size of the object can be set using `file-truncate`. The newly allocated bytes of a shared memory object are automatically initialised to 0.
- `open/excl` : If `open/create` was also specified, and a shared memory object with the given name already exists, returns an error. 
- `open/trunc` : If the shared memory object already exists, truncate it to zero bytes.

`MODE` should be a bitmask composed of one or more permission
values like `perm/irusr` and is only relevant when a new file is
created. The default mode is `perm/irwxu | perm/irgrp | perm/iroth`.

On successful completion `shm-open` returns a new file descriptor
referring to the shared memory object.

<procedure>shm-unlink:: PATH -> STATUS</procedure>

The operation of `shm-unlink` is analogous to `unlink(2)`: it
removes a shared memory object name, and, once all processes have
unmapped the object, de-allocates and destroys the contents of the
associated memory region. After a successful `shm-unlink`, attempts
to `shm-open` an object with the same name will fail (unless
`open/creat` was specified, in which case a new, distinct object is
created).

## Examples

```scheme
  (let* ((str "Hello, world!")
         (path (sprintf "/shmtest~A" (random 100)))
         (fd (shm-open path (list open/rdwr open/creat))))
    (file-truncate fd (string-length str))
    (file-write fd str)
    (file-close fd)
    (let ((fd (shm-open path (list open/rdonly open/excl ))))
      (file-read fd (string-length str))
      (file-close fd)
      (shm-unlink path)))
```

## License

>
> Copyright 2011-2020 Ivan Raikov
>  
>  Redistribution and use in source and binary forms, with or without
>  modification, are permitted provided that the following conditions are
>  met:
>  
>  Redistributions of source code must retain the above copyright
>  notice, this list of conditions and the following disclaimer.
>  
>  Redistributions in binary form must reproduce the above copyright
>  notice, this list of conditions and the following disclaimer in the
>  documentation and/or other materials provided with the distribution.
>  
>  Neither the name of the author nor the names of its contributors may
>  be used to endorse or promote products derived from this software
>  without specific prior written permission.
>  
>  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
>  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
>  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
>  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
>  COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
>  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
>  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
>  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
>  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
>  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
>  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
>  OF THE POSSIBILITY OF SUCH DAMAGE.
